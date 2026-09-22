#!/usr/bin/env python3
"""Rebuild canonical events from cached ESPN summaries and reconcile, never impute.

The existing manual CSVs identify the covered games, not their correct totals.
Raw inputs are preserved. Network retrieval is explicit (--fetch / --refresh).
"""
import argparse
import collections
import csv
from datetime import datetime, timezone
import hashlib
import json
from pathlib import Path
import tempfile
import urllib.request
from zoneinfo import ZoneInfo

ROOT = Path(__file__).resolve().parents[2]
STATS = ('PTS', 'FGA', 'FGM', 'FTA', 'FTM', 'FGA3', 'FGM3', 'OREB', 'DREB', 'TOV')
SHOT_TYPES = {'JumpShot', 'LayUpShot', 'DunkShot', 'TipShot'}


# Source ordering, local files, and verified source snapshots

def clock_seconds(clock):
    minutes, seconds = clock.split(':')
    return int(minutes) * 60 + float(seconds)


def play_order(play):
    # Sequence numbers can reflect delayed data entry (even after End Game).
    return (int(play['period']['number']), -clock_seconds(play['clock']['displayValue']),
            int(play['sequenceNumber']))


def write_csv(path, rows, fields=None):
    path.parent.mkdir(parents=True, exist_ok=True)
    if fields is None:
        fields = list(rows[0]) if rows else []
    with tempfile.NamedTemporaryFile('w', newline='', dir=path.parent, delete=False) as stream:
        writer = csv.DictWriter(stream, fieldnames=fields)
        writer.writeheader()
        writer.writerows(rows)
        temporary_path = Path(stream.name)
    temporary_path.replace(path)


def read_csv(path):
    with path.open(newline='', encoding='utf-8-sig') as stream:
        return list(csv.DictReader(stream))


def source_url(game_id):
    return 'https://site.api.espn.com/apis/site/v2/sports/basketball/mens-college-basketball/summary?event=' + game_id


def get_source(game_id, cache_dir, fetch=False, refresh=False):
    """Read a hash-verified snapshot; fetch or replace it only when requested."""
    path = cache_dir / (game_id + '.json')
    metadata_path = cache_dir / (game_id + '.meta.json')
    if refresh or not path.exists():
        if not (fetch or refresh):
            raise ValueError(f'Missing cached source for {game_id}; run with --fetch')
        source_bytes = urllib.request.urlopen(source_url(game_id), timeout=30).read()
        source_data = json.loads(source_bytes)
        if str(source_data.get('header', {}).get('id')) != game_id or not source_data.get('plays'):
            raise ValueError(f'Wrong or empty ESPN response: {game_id}')
        cache_dir.mkdir(parents=True, exist_ok=True)
        path.write_bytes(source_bytes)
        metadata_path.write_text(json.dumps({
            'source_url': source_url(game_id),
            'retrieved_at_utc': datetime.now(timezone.utc).isoformat(),
            'sha256': hashlib.sha256(source_bytes).hexdigest()}, indent=2) + '\n')
    source_bytes = path.read_bytes()
    source_metadata = json.loads(metadata_path.read_text())
    if source_metadata['sha256'] != hashlib.sha256(source_bytes).hexdigest():
        raise ValueError(f'Source hash mismatch: {path}')
    source_data = json.loads(source_bytes)
    if str(source_data['header']['id']) != game_id:
        raise ValueError(f'Source identity mismatch: {game_id}')
    return source_data, source_metadata


# Canonical event statistics and game reconciliation

def official_box(data):
    """Extract official team totals and check that scoring arithmetic agrees."""
    result = {}
    scores = {str(team['id']): int(team['score']) for team in data['header']['competitions'][0]['competitors']}
    for team in data['boxscore']['teams']:
        team_id = str(team['team']['id'])
        stats = {statistic['name']: statistic['displayValue'] for statistic in team['statistics']}
        team_totals = {'PTS': scores[team_id]}
        for field, made, attempted in (
            ('fieldGoalsMade-fieldGoalsAttempted', 'FGM', 'FGA'),
            ('threePointFieldGoalsMade-threePointFieldGoalsAttempted', 'FGM3', 'FGA3'),
            ('freeThrowsMade-freeThrowsAttempted', 'FTM', 'FTA')):
            made_count, attempt_count = map(int, stats[field].split('-'))
            team_totals[made], team_totals[attempted] = made_count, attempt_count
        for key, name in (('OREB', 'offensiveRebounds'), ('DREB', 'defensiveRebounds'), ('TOV', 'totalTurnovers')):
            team_totals[key] = int(stats[name])
        if 2 * team_totals['FGM'] + team_totals['FGM3'] + team_totals['FTM'] != team_totals['PTS']:
            raise ValueError(f'Box-score arithmetic does not reconcile: {team_id}')
        result[team_id] = team_totals
    return result


def event_stats(play):
    """Translate one source play into the established team-stat columns."""
    event_type = play['type']['text']
    event_text = play.get('text', '').lower()
    is_scoring_play = bool(play.get('scoringPlay'))
    is_field_goal = event_type in SHOT_TYPES
    is_free_throw = 'free throw' in event_text and event_type == 'MadeFreeThrow'
    is_three_pointer = is_field_goal and ('three point' in event_text or '3-point' in event_text)
    points = int(play.get('scoreValue', 0)) if is_scoring_play else 0
    result = dict.fromkeys(STATS, 0)
    result.update(
        PTS=points, FGA=int(is_field_goal), FGM=int(is_field_goal and is_scoring_play),
        FTA=int(is_free_throw), FTM=int(is_free_throw and is_scoring_play),
        FGA3=int(is_three_pointer), FGM3=int(is_three_pointer and is_scoring_play),
        OREB=int(event_type == 'Offensive Rebound'), DREB=int(event_type == 'Defensive Rebound'),
        TOV=int('Turnover' in event_type),
    )
    if is_scoring_play and (
            not (is_field_goal or is_free_throw)
            or points != (1 if is_free_throw else 3 if is_three_pointer else 2)):
        raise ValueError(f'Unknown scoring event: {play["id"]}')
    return result


def canonical_game(game_id, game_file, source_data, source_metadata):
    """Return game totals, unique events, stat comparisons, and timeline issues."""
    competition = source_data['header']['competitions'][0]
    if not competition['status']['type']['completed']:
        raise ValueError(f'Game is not final: {game_id}')
    teams = competition['competitors']
    uconn = next(team for team in teams if str(team['id']) == '41')
    opponent = next(team for team in teams if str(team['id']) != '41')
    home_id = str(next(team for team in teams if team['homeAway'] == 'home')['id'])
    tipoff = datetime.fromisoformat(competition['date'].replace('Z', '+00:00'))
    game_date = tipoff.astimezone(ZoneInfo('America/New_York')).date().isoformat()
    game_identity = {
        'game_id': game_id, 'game_file': game_file, 'game_date': game_date,
        'opponent': opponent['team']['displayName'],
        'site_type': 'neutral' if competition['neutralSite'] else uconn['homeAway'],
    }
    events, seen_events, issues = [], {}, []
    previous_home_score, previous_away_score = 0, 0
    for play in sorted(source_data['plays'], key=play_order):
        play_id = str(play['id'])
        if play_id in seen_events:
            if play != seen_events[play_id]:
                raise ValueError(f'Conflicting source event: {game_id}/{play_id}')
            continue
        seen_events[play_id] = play
        team_id = str(play.get('team', {}).get('id', ''))
        home_score, away_score = int(play['homeScore']), int(play['awayScore'])
        stats = event_stats(play)
        if any(stats.values()) and team_id not in (str(uconn['id']), str(opponent['id'])):
            raise ValueError(f'Stat event has unknown team: {game_id}/{play_id}/{team_id}')
        home_score_delta, away_score_delta = home_score - previous_home_score, away_score - previous_away_score
        expected = (stats['PTS'], 0) if team_id == home_id else (0, stats['PTS'])
        if (home_score_delta, away_score_delta) != expected:
            issues.append({'game_id': game_id, 'play_id': play_id, 'issue': 'score_delta_mismatch',
                           'detail': f'home/away delta {home_score_delta}/{away_score_delta}, event points {stats["PTS"]}, team {team_id}'})
        row = dict(game_identity, play_id=play_id, sequence_number=str(play['sequenceNumber']),
                   event_order=len(events)+1,
                   period_number=int(play['period']['number']), clock_display_value=play['clock']['displayValue'],
                   team_id=team_id, type_text=play['type']['text'], text=play.get('text', ''),
                   home_score=home_score, away_score=away_score,
                   uconn_score=home_score if home_id == '41' else away_score,
                   opponent_score=away_score if home_id == '41' else home_score,
                   scoring_play=bool(play.get('scoringPlay')), shooting_play=bool(play.get('shootingPlay')),
                   points=stats['PTS'], is_uconn_offense=(team_id == '41') if team_id else '',
                   source_url=source_metadata['source_url'], source_sha256=source_metadata['sha256'])
        row.update(stats)
        events.append(row)
        previous_home_score, previous_away_score = home_score, away_score
    box_totals = official_box(source_data)
    comparisons = []
    for team_id in ('41', str(opponent['id'])):
        for stat in STATS:
            actual = sum(event[stat] for event in events if event['team_id'] == team_id)
            expected = box_totals[team_id][stat]
            comparisons.append({'game_id': game_id, 'game_file': game_file, 'team_id': team_id, 'stat': stat,
                                'event_total': actual, 'box_total': expected,
                                'difference': actual - expected, 'matched': actual == expected})
    score_reconciled = (
        events[-1]['uconn_score'] == int(uconn['score'])
        and events[-1]['opponent_score'] == int(opponent['score'])
        and all(comparison['matched'] for comparison in comparisons if comparison['stat'] == 'PTS')
    )
    game = dict(game_identity, tipoff_utc=tipoff.isoformat(), uconn_points=int(uconn['score']),
                opponent_points=int(opponent['score']), score_reconciled=score_reconciled,
                score_timeline_reconciled=not issues,
                event_stats_reconciled=all(comparison['matched'] for comparison in comparisons),
                event_count=len(events), source_url=source_metadata['source_url'],
                source_sha256=source_metadata['sha256'], retrieved_at_utc=source_metadata['retrieved_at_utc'],
                conference_competition=competition['conferenceCompetition'])
    for prefix, team_id in (('uconn', '41'), ('opponent', str(opponent['id']))):
        for stat in STATS[1:]:
            game[prefix + '_' + stat.lower()] = box_totals[team_id][stat]
    return game, events, comparisons, issues


# Legacy input audits; raw inputs remain unchanged

def audit_manual(manual, canonical):
    """Report duplicate manual rows and field differences against source events."""
    events_by_id = {(source_event['game_id'], source_event['play_id']): source_event for source_event in canonical}
    manual_rows_by_event = collections.defaultdict(list)
    for row in manual:
        manual_rows_by_event[(row['game_id'], row['play_id'])].append(row)
    duplicates, differences = [], []
    for key, rows in manual_rows_by_event.items():
        if len(rows) > 1:
            duplicates.append({'game_id': key[0], 'play_id': key[1], 'copies': len(rows),
                               'extra_copies': len(rows)-1,
                               'source_paths': '|'.join(sorted({manual_row['_path'] for manual_row in rows}))})
        source_event = events_by_id.get(key)
        for manual_row in rows:
            if not source_event:
                differences.append({'game_id': key[0], 'play_id': key[1], 'source_path': manual_row['_path'],
                                    'field': 'play_id', 'manual': key[1], 'source': 'missing'})
                continue
            for field in ('PTS', 'FGA', 'FGM', 'FTA', 'FTM', 'FGA3', 'FGM3', 'TOV', 'team_id', 'game_date', 'text'):
                manual_value, source_value = str(manual_row.get(field, '')).strip(), str(source_event[field]).strip()
                if field == 'text':
                    manual_value, source_value = ' '.join(manual_value.split()), ' '.join(source_value.split())
                if manual_value != source_value:
                    differences.append({'game_id': key[0], 'play_id': key[1], 'source_path': manual_row['_path'],
                                        'field': field, 'manual': manual_row.get(field, ''), 'source': source_event[field]})
    return duplicates, differences


def audit_stints(root, games):
    """Compare stint point sums with game totals without certifying lineups."""
    stints = read_csv(root / '_data/01_core_inputs/uconn_stints_from_pbp.csv')
    stints_by_game = collections.defaultdict(list)
    for stint in stints:
        stints_by_game[stint['game_file']].append(stint)
    games_by_filename = {game['game_file']: game for game in games}
    audit_rows = []
    for game_file, rows in sorted(stints_by_game.items()):
        game = games_by_filename.get(game_file)
        stint_points_for = sum(float(stint['points_for']) for stint in rows)
        stint_points_against = sum(float(stint['points_against']) for stint in rows)
        audit_rows.append({'game_file': game_file, 'game_id': game['game_id'] if game else '', 'stint_rows': len(rows),
                    'stint_points_for': stint_points_for, 'stint_points_against': stint_points_against,
                    'source_points_for': game['uconn_points'] if game else '',
                    'source_points_against': game['opponent_points'] if game else '',
                    'score_totals_match': bool(game and stint_points_for == game['uconn_points'] and stint_points_against == game['opponent_points']),
                    'lineup_model_eligible': False,
                    'reason': 'No source event-to-stint mapping; possessions/lineups unverified' if game else 'No matching source game in current coverage'})
    return audit_rows


# Reconciliation outputs and command-line entry point

def run(root=ROOT, fetch=False, refresh=False):
    manual_events = []
    for competition_folder in ('_conf', '_nc', '_bet', '_ncaatourn'):
        for path in sorted((root / '_data/03_manual_game_csv' / competition_folder).glob('*.csv')):
            for row in read_csv(path):
                if not row.get('game_id') or not row.get('play_id'):
                    raise ValueError(f'Missing source event ID: {path}')
                row['_path'] = str(path.relative_to(root))
                manual_events.append(row)
    game_aliases = collections.defaultdict(set)
    for row in manual_events:
        game_aliases[row['game_id']].add(row['game_file'])
    if not game_aliases:
        raise ValueError('No source game IDs in manual inputs')
    games, events, comparisons, issues = [], [], [], []
    core_game_files = {row['game_file'] for row in read_csv(root / '_data/01_core_inputs/uconn_games_meta.csv')}
    for game_id, aliases in sorted(game_aliases.items()):
        preferred_filenames = sorted(aliases & core_game_files) or sorted(aliases)
        source_data, source_metadata = get_source(game_id, root / '_data/00_source/espn', fetch, refresh)
        game, game_events, game_comparisons, game_issues = canonical_game(
            game_id, preferred_filenames[0], source_data, source_metadata
        )
        game['input_game_aliases'] = '|'.join(sorted(aliases))
        games.append(game)
        events.extend(game_events)
        comparisons.extend(game_comparisons)
        issues.extend(game_issues)
    games.sort(key=lambda game: (game['game_date'], game['game_id']))
    events.sort(key=lambda event: (event['game_date'], event['game_id'], event['event_order']))
    duplicates, differences = audit_manual(manual_events, events)
    stint_audit = audit_stints(root, games)
    derived_dir = root / '_data/02_derived_inputs'
    quality_dir = root / '_outputs/00_qc'
    write_csv(derived_dir / 'reconciled_games.csv', games)
    write_csv(derived_dir / 'reconciled_events.csv', events)
    write_csv(quality_dir / 'source_stat_reconciliation.csv', comparisons)
    write_csv(quality_dir / 'duplicate_manual_events.csv', duplicates,
              ['game_id', 'play_id', 'copies', 'extra_copies', 'source_paths'])
    write_csv(quality_dir / 'manual_source_differences.csv', differences,
              ['game_id', 'play_id', 'source_path', 'field', 'manual', 'source'])
    write_csv(quality_dir / 'source_event_issues.csv', issues, ['game_id', 'play_id', 'issue', 'detail'])
    write_csv(quality_dir / 'core_stint_reconciliation.csv', stint_audit)
    summary = {'generated_at_utc': datetime.now(timezone.utc).isoformat(),
               'games': len(games), 'canonical_source_events': len(events),
               'manual_rows': len(manual_events), 'unique_manual_event_ids': len(manual_events)-sum(duplicate['extra_copies'] for duplicate in duplicates),
               'duplicate_extra_rows': sum(duplicate['extra_copies'] for duplicate in duplicates),
               'manual_source_field_differences': len(differences),
               'score_reconciled_games': sum(game['score_reconciled'] for game in games),
               'score_timeline_reconciled_games': sum(game['score_timeline_reconciled'] for game in games),
               'event_stats_reconciled_games': sum(game['event_stats_reconciled'] for game in games),
               'stat_checks': len(comparisons), 'stat_mismatches': sum(not comparison['matched'] for comparison in comparisons),
               'core_games': len(stint_audit), 'core_score_matching_games': sum(stint['score_totals_match'] for stint in stint_audit),
               'lineup_models_released': False,
               'source_scope': 'ESPN current historical summaries; not immutable pregame snapshots',
               'raw_inputs_preserved': True}
    summary['code_sha256'] = hashlib.sha256(Path(__file__).read_bytes()).hexdigest()
    summary['output_sha256'] = {str(path.relative_to(root)): hashlib.sha256(path.read_bytes()).hexdigest()
                               for path in (derived_dir / 'reconciled_games.csv', derived_dir / 'reconciled_events.csv')}
    (quality_dir / 'reconciliation_summary.json').write_text(json.dumps(summary, indent=2) + '\n')
    print(json.dumps(summary, indent=2))
    return summary


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--fetch', action='store_true', help='Fetch missing source snapshots')
    parser.add_argument('--refresh', action='store_true', help='Explicitly replace cached source snapshots')
    args = parser.parse_args()
    run(fetch=args.fetch, refresh=args.refresh)
