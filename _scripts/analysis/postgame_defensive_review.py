#!/usr/bin/env python3
"""Build an evidence-linked defensive review and record an actual human pilot.

Only the Python standard library is required. Source-event possession boundaries
are deliberately conservative; they are not film-verified possession labels.
"""
from __future__ import annotations

import argparse
import csv
import hashlib
import importlib.util
from html import escape
import json
from pathlib import Path
import re
import statistics
import time
from datetime import datetime, timezone
from uuid import uuid4

ROOT = Path(__file__).resolve().parents[2]
DEFAULT_OUTPUT = ROOT / '_outputs/08_staff_pilot'
DEFAULT_SOURCE_CACHE = ROOT / '_data/00_source/espn'
CATEGORIES = ('threes', 'close_shots', 'turnovers')


# Canonical event fields and conservative possession reconstruction

def read_csv_rows(path):
    with Path(path).open(newline='', encoding='utf-8-sig') as handle:
        return list(csv.DictReader(handle))


def integer_value(row, key):
    """Read an integer CSV field, treating a missing value as zero."""
    value = row.get(key, '')
    if value in ('', None):
        return 0
    return int(float(value))


def is_true(value):
    return str(value).lower() in ('true', '1')


def is_uconn_event(row):
    """Identify source events assigned to UConn rather than its opponent."""
    return is_true(row['is_uconn_offense'])


def clock_seconds(row):
    minute, second = row['clock_display_value'].split(':')
    return int(minute) * 60 + float(second)


def review_category(row):
    """Assign an opponent event to one of the three supported review themes."""
    if is_uconn_event(row):
        return None
    if integer_value(row, 'TOV'):
        return 'turnovers'
    if integer_value(row, 'FGA3'):
        return 'threes'
    if integer_value(row, 'FGA') and re.search(
            r'lay\s*up|dunk|tip\s*(?:shot|in)', row['text'] + ' ' + row['type_text'], re.I):
        return 'close_shots'
    return None


def is_possession_action(row):
    """Identify attempts, turnovers, and rebounds used to check control changes."""
    return any(integer_value(row, name) for name in ('FGA', 'FTA', 'TOV', 'OREB', 'DREB'))


def confirms_made_shot_end(events, index):
    """Reject and-ones and ambiguous possession retention after a made basket."""
    made_shot = events[index]
    for event in events[index + 1:]:
        if event['period_number'] != made_shot['period_number']:
            return False
        if re.search(r'foul|jump\s*ball', event['type_text'] + ' ' + event['text'], re.I):
            return False
        if is_possession_action(event):
            return is_uconn_event(event) and bool(integer_value(event, 'FGA') or integer_value(event, 'TOV'))
    return False


def reconstruct_possessions(events):
    """Return only sequences bounded by explicit source control changes.

    Start: opponent defensive rebound or UConn turnover. End: opponent turnover,
    UConn defensive rebound, or made opponent FG followed by UConn FGA/turnover
    before any further opponent action/foul. Fouls, FTs, jump balls, inconsistent
    control, period changes, and spans over 120 seconds abort reconstruction.
    """
    possessions, current_sequence = [], []
    for index, event in enumerate(events):
        starts_opponent_possession = (
            (not is_uconn_event(event) and integer_value(event, 'DREB'))
            or (is_uconn_event(event) and integer_value(event, 'TOV'))
        )
        if starts_opponent_possession:
            current_sequence = [event]
            continue
        if not current_sequence:
            continue
        elapsed_game_seconds = clock_seconds(current_sequence[0]) - clock_seconds(event)
        if (current_sequence[0]['period_number'] != event['period_number']
                or not 0 <= elapsed_game_seconds <= 120
                or integer_value(event, 'FTA')
                or re.search(r'foul|jump\s*ball', event['type_text'] + ' ' + event['text'], re.I)):
            current_sequence = []
            continue
        current_sequence.append(event)
        prior_actions = [row for row in current_sequence[1:-1] if is_possession_action(row)]
        awaiting_rebound = (prior_actions and integer_value(prior_actions[-1], 'FGA') and
                            not integer_value(prior_actions[-1], 'FGM'))
        if awaiting_rebound and is_possession_action(event) and not (
                is_uconn_event(event) and integer_value(event, 'DREB') or
                not is_uconn_event(event) and integer_value(event, 'OREB')):
            current_sequence = []
            continue
        end_reason = None
        if is_uconn_event(event):
            if integer_value(event, 'DREB'):
                end_reason = 'uconn_defensive_rebound'
            elif integer_value(event, 'FGA') or integer_value(event, 'OREB'):
                current_sequence = []
                continue
        elif integer_value(event, 'TOV'):
            end_reason = 'opponent_turnover'
        elif integer_value(event, 'OREB'):
            if not awaiting_rebound:
                current_sequence = []
                continue
        elif integer_value(event, 'FGM'):
            if confirms_made_shot_end(events, index):
                end_reason = 'opponent_made_field_goal'
            else:
                current_sequence = []
                continue
        if end_reason:
            if end_reason != 'uconn_defensive_rebound' or awaiting_rebound:
                possessions.append({
                    'possession_id': f"{event['game_id']}:{current_sequence[0]['play_id']}:{event['play_id']}",
                    'boundary_status': 'source_bounded_reconstruction_not_film_verified',
                    'start_reason': 'opponent_defensive_rebound' if integer_value(current_sequence[0], 'DREB') else 'uconn_turnover',
                    'end_reason': end_reason,
                    'events': current_sequence,
                })
            current_sequence = []
    return possessions


def choose_examples(possessions, category_name):
    """Select the earliest eligible make and miss, or the first two turnovers."""
    candidates = [
        possession for possession in possessions
        if any(review_category(event) == category_name for event in possession['events'])
    ]
    if category_name == 'turnovers':
        return candidates[:2]
    # Fixed rule: earliest reconstructed sequence with a make, then a miss.
    chosen = []
    for made in (True, False):
        match = next((
            possession for possession in candidates
            if possession not in chosen and any(
                review_category(event) == category_name and bool(integer_value(event, 'FGM')) == made
                for event in possession['events']
            )
        ), None)
        if match:
            chosen.append(match)
    return chosen


# Verified source loading and review packet generation

def write_csv(path, data, columns):
    with path.open('w', newline='', encoding='utf-8') as handle:
        writer = csv.DictWriter(handle, fieldnames=columns, extrasaction='ignore')
        writer.writeheader()
        writer.writerows(data)


def write_json(path, data):
    path.write_text(json.dumps(data, indent=2, allow_nan=False) + '\n', encoding='utf-8')


def load_game(events_path, games_path, game_id=None, source_cache=DEFAULT_SOURCE_CACHE):
    """Validate an eligible game and every event against its cached source."""
    games = read_csv_rows(games_path)
    if len({game_row['game_id'] for game_row in games}) != len(games):
        raise ValueError('Reconciled game IDs must be unique.')
    eligible_games = [game_row for game_row in games if is_true(game_row.get('score_reconciled')) and is_true(game_row.get('event_stats_reconciled'))]
    if game_id:
        eligible_games = [game_row for game_row in eligible_games if game_row['game_id'] == game_id]
    if not eligible_games:
        raise ValueError('No selected game passes both score and event-stat reconciliation.')
    game = max(eligible_games, key=lambda game_row: (game_row['game_date'], game_row['game_id']))
    events = [event_row for event_row in read_csv_rows(events_path) if event_row['game_id'] == game['game_id']]
    required_columns = {'play_id', 'sequence_number', 'event_order', 'period_number', 'clock_display_value', 'team_id',
                'type_text', 'text', 'is_uconn_offense', 'source_url', 'source_sha256',
                'FGA', 'FGM', 'FGA3', 'FGM3', 'FTA', 'FTM', 'OREB', 'DREB', 'TOV'}
    if not events or not required_columns <= events[0].keys():
        raise ValueError('Canonical source events are empty or missing required columns.')
    play_ids = [event_row['play_id'] for event_row in events]
    if any(not value for value in play_ids) or len(play_ids) != len(set(play_ids)):
        raise ValueError('Canonical play IDs must be present and unique within a game.')
    source_hashes = {event_row['source_sha256'] for event_row in events}
    if len(source_hashes) != 1 or not next(iter(source_hashes)) or any(not event_row['source_url'] for event_row in events):
        raise ValueError('All events must reference one hashed source and a source URL.')
    if game.get('source_sha256') and source_hashes != {game['source_sha256']}:
        raise ValueError('Event source hash does not match the reconciled game.')
    for event in events:
        if is_possession_action(event) and str(event['is_uconn_offense']).lower() not in ('true', 'false', '1', '0'):
            raise ValueError('Event ownership must be explicit.')
    event_orders = [integer_value(event_row, 'event_order') for event_row in events]
    if len(event_orders) != len(set(event_orders)) or min(event_orders) < 1:
        raise ValueError('Canonical event_order must be positive and unique.')
    if game.get('event_count') and integer_value(game, 'event_count') != len(events):
        raise ValueError('Canonical event count does not match the reconciled game.')
    for side in ('uconn', 'opponent'):
        team_events = [event_row for event_row in events if is_uconn_event(event_row) == (side == 'uconn')]
        for stat in ('FGA', 'FGM', 'FTA', 'FTM', 'FGA3', 'FGM3', 'OREB', 'DREB', 'TOV'):
            expected = f'{side}_{stat.lower()}'
            if expected in game and sum(integer_value(event_row, stat) for event_row in team_events) != integer_value(game, expected):
                raise ValueError(f'Canonical event totals no longer match game field {expected}.')
    events.sort(key=lambda event_row: integer_value(event_row, 'event_order'))
    # A claimed hash and matching totals alone cannot detect altered text, clocks,
    # ownership or ordering. Rebuild this game from the verified cached payload.
    reconciliation_spec = importlib.util.spec_from_file_location('pilot_reconciliation', ROOT / '_scripts/ops/reconcile_source_data.py')
    reconciliation_module = importlib.util.module_from_spec(reconciliation_spec)
    reconciliation_spec.loader.exec_module(reconciliation_module)
    source_data, source_metadata = reconciliation_module.get_source(game['game_id'], Path(source_cache))
    expected_game, expected_events, _, _ = reconciliation_module.canonical_game(game['game_id'], game['game_file'], source_data, source_metadata)
    if any(game.get(key) != str(value) for key, value in expected_game.items()):
        raise ValueError('Reconciled game differs from its cached source reconstruction.')
    if len(events) != len(expected_events) or any(
            any(actual.get(key) != str(value) for key, value in expected.items())
            for actual, expected in zip(events, expected_events)):
        raise ValueError('Canonical events differ from their cached source reconstruction.')
    return game, events


def write_html(output, game, examples, titles, questions, source_link):
    """Portable, printable artifact; no remote assets or web service required."""
    cards = []
    for index, category_name in enumerate(CATEGORIES, 1):
        sequences = []
        for possession in examples[category_name]:
            first_event, last_event = possession['events'][0], possession['events'][-1]
            action_events = [event for event in possession['events'] if is_possession_action(event)]
            steps = ''.join(f'<li><time>{escape(event["clock_display_value"])}</time> {escape(event["text"])}</li>' for event in action_events)
            play_ids = ' → '.join(event['play_id'] for event in action_events)
            sequences.append(f'<div class="possession"><h3>Period {escape(first_event["period_number"])} · {escape(first_event["clock_display_value"])}–{escape(last_event["clock_display_value"])}</h3><ol>{steps}</ol><div class="ids">Source plays {escape(play_ids)}</div></div>')
        if not sequences:
            sequences.append('<p>No sequence passed the boundary checks. Locate a complete possession in the event list before drawing a conclusion.</p>')
        cards.append(f'<section><div class="number">0{index}</div><div class="card-body"><h2>{escape(titles[category_name])}</h2><p class="question">{escape(questions[category_name])}</p><div class="sequences">{"".join(sequences)}</div></div></section>')
    title = f'UConn vs {game["opponent"]}'
    document = '''<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1"><title>Postgame defensive review</title><style>
*{box-sizing:border-box}body{background:#eef1f4;color:#15243b;font-family:system-ui,-apple-system,sans-serif;line-height:1.45;margin:0}main{max-width:1120px;margin:32px auto;background:white;padding:38px 44px;border-top:7px solid #122746;box-shadow:0 5px 30px #1020390d}.eyebrow{color:#526581;font-size:11px;letter-spacing:2px;text-transform:uppercase;font-weight:750}header{display:flex;justify-content:space-between;gap:20px;align-items:start;border-bottom:1px solid #dce2e9;padding-bottom:20px;margin-bottom:20px}h1{font-size:28px;letter-spacing:-.7px;line-height:1.15;margin:7px 0}p{margin:8px 0}h2{font-size:17px;margin:0}h3{font-size:12px;margin:0 0 7px;color:#526581}.score{text-align:right;white-space:nowrap;font-weight:700;font-size:23px}.score small{font-size:11px;color:#526581;display:block;letter-spacing:1px;text-transform:uppercase}.status{border-left:3px solid #c15644;background:#fcf6f3;padding:11px 15px;font-size:12px;margin:18px 0}.task{font-size:14px}section{display:flex;gap:15px;padding:22px 0;border-bottom:1px solid #dce2e9;break-inside:avoid}.number{color:#aab7c8;font-size:24px;font-weight:650;line-height:1.1}.card-body{flex:1;min-width:0}.question{font-size:12px;color:#40536d;margin:7px 0 15px}.sequences{display:grid;grid-template-columns:1fr 1fr;gap:18px}.possession{background:#f5f7fa;padding:13px;border-radius:5px}.possession ol{padding:0;margin:0;list-style:none;font-size:12px}.possession li+li{margin-top:5px}time{font-variant-numeric:tabular-nums;font-weight:650}.ids{overflow-wrap:anywhere;font-size:9px;color:#6b788c;margin-top:9px}footer{font-size:11px;color:#526581;margin-top:18px}footer p{margin:7px 0}a{color:#164b85}.action{font-size:13px;color:#15243b}button{font:inherit;border:1px solid #bbc7d5;background:white;border-radius:4px;padding:5px 11px;cursor:pointer;margin-top:10px}@media(max-width:750px){main{margin:0;padding:25px 20px}.sequences{grid-template-columns:1fr}header{display:block}.score{text-align:left;margin-top:15px}}@media print{@page{size:landscape;margin:12mm}body{background:white;font-size:10px}main{margin:0;padding:0;max-width:none;box-shadow:none;border-top:4px solid #122746}header{padding:12px 0 10px;margin-bottom:10px}h1{font-size:23px}section{padding:11px 0}.question{margin-bottom:8px}.possession{padding:8px}.possession ol{font-size:10px}.ids{font-size:8px}.status{margin:9px 0;padding:7px 10px;font-size:10px}footer{font-size:9px;margin-top:10px}button{display:none}}
</style></head><body><main>'''
    document += f'<header><div><div class="eyebrow">Postgame defensive review · {escape(game["game_date"])}</div><h1>{escape(title)}</h1><div class="task">Choose at most one useful film review item from each theme.</div></div><div class="score">{integer_value(game,"uconn_points")} — {integer_value(game,"opponent_points")}<small>UConn · Opponent / Final</small><button onclick="window.print()">Print review</button></div></header>'
    document += '<div class="status"><strong>Reconciled source events. Film review pending.</strong> Scores and event statistics pass reconciliation. The sequences below use explicit event boundaries; ambiguous foul/free-throw sequences are excluded. Counts cover all relevant logged events. This is a review queue, not a defensive grade.</div>'
    document += ''.join(cards)
    document += f'<footer><p class="action"><strong>Next action:</strong> Locate these period/clock ranges in authorized film. Check each complete possession and record whether the observation is accurate, useful and actionable.</p><p>Examples use the first reconstructed make and miss for each shot theme, then the first two turnovers. No film, coverage or individual responsibility has been verified. Sequences are incomplete coverage and must not be used as possession denominators.</p><p><a href="{escape(source_link, quote=True)}">ESPN play-by-play</a> · <a href="possession_evidence.csv">Full sequence evidence</a> · <a href="defensive_events.csv">All counted events</a> · <a href="review.json">Provenance</a> · <a href="build_metrics.json">Generation timing</a></p><p>Staff time savings and usefulness are unmeasured until actual baseline and report sessions are recorded.</p></footer></main></body></html>'
    (output / 'defensive_review.html').write_text(document, encoding='utf-8')


def write_baseline_html(output, game, events, source_link):
    """Readable full source log with no selected observations or timeline scores."""
    periods = {}
    for event in events:
        periods.setdefault(event['period_number'], []).append(event)
    sections = []
    for period, period_events in periods.items():
        entries = ''.join(
            f'<tr class="event"><td class="clock">{escape(event["clock_display_value"])}</td>'
            f'<td>{escape(event["text"])}</td><td class="play-id">{escape(event["play_id"])}</td></tr>'
            for event in period_events)
        sections.append(f'<section><h2>Period {escape(str(period))}</h2>'
                        '<table><thead><tr><th scope="col">Clock</th><th scope="col">Source event</th>'
                        f'<th scope="col">Play ID</th></tr></thead><tbody>{entries}</tbody></table></section>')
    title = f'UConn vs {game["opponent"]}'
    document = '''<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1"><title>Baseline source packet</title><style>
*{box-sizing:border-box}body{margin:0;background:#eef1f4;color:#15243b;font-family:system-ui,-apple-system,sans-serif;line-height:1.5}main{max-width:1060px;margin:28px auto;padding:32px 38px;background:white;border-top:6px solid #122746}h1{font-size:27px;line-height:1.2;margin:8px 0}h2{font-size:19px;margin:30px 0 10px}.eyebrow{font-size:12px;color:#526581;font-weight:700;letter-spacing:1px;text-transform:uppercase}.task{padding:14px 17px;background:#f5f7fa;border-left:3px solid #7184a0;margin:20px 0}.task p{margin:5px 0}table{width:100%;border-collapse:collapse;text-align:left;font-size:14px}th{background:#f5f7fa;font-size:12px;color:#526581}td,th{padding:9px 10px;border-bottom:1px solid #dce2e9;vertical-align:top}.clock{white-space:nowrap;font-variant-numeric:tabular-nums;font-weight:650;width:75px}.play-id{font-size:11px;color:#526581;overflow-wrap:anywhere;width:180px}a{color:#164b85}footer{margin-top:24px;color:#526581;font-size:12px}@media(max-width:650px){main{margin:0;padding:22px 14px}td,th{padding:8px 5px}table{font-size:12px}.play-id{width:108px;font-size:10px}}@media print{body{background:white}main{margin:0;padding:0;border:0;max-width:none}thead{display:table-header-group}tr{break-inside:avoid}}
</style></head><body><main>'''
    document += f'<header><div class="eyebrow">Baseline source packet · {escape(game["game_date"])}</div><h1>{escape(title)}</h1><p>Final: UConn {integer_value(game,"uconn_points")}, {escape(game["opponent"])} {integer_value(game,"opponent_points")}.</p></header>'
    document += '<div class="task"><p><strong>Task:</strong> Prepare at most three defensive review items, each with a specific possession located by period, clock and source play ID. Zero useful items is a valid outcome.</p><p>Use the same authorized film access as the report condition. Do not open the defensive review before completing this baseline session.</p><p>This is the full chronological source event log. It contains no selected findings. Record actual time and usefulness with the pilot timer.</p></div>'
    document += ''.join(sections)
    document += f'<footer><a href="{escape(source_link, quote=True)}">Source play-by-play</a> · <a href="baseline_events.csv">Full event data</a><p>Game totals and event statistics are reconciled. Source events are not film-verified possession labels. Intermediate scoreboard values are omitted.</p></footer></main></body></html>'
    (output / 'baseline_packet.html').write_text(document, encoding='utf-8')


def build(args):
    """Create source-linked review artifacts without starting a human session."""
    generation_started_at = time.perf_counter()
    game, events = load_game(args.events, args.games, args.game_id, args.source_cache)
    output = Path(args.output) / game['game_id']
    output.mkdir(parents=True, exist_ok=True)
    possessions = reconstruct_possessions(events)
    examples = {category_name: choose_examples(possessions, category_name) for category_name in CATEGORIES}
    observations = []
    for category_name in CATEGORIES:
        category_events = [event for event in events if review_category(event) == category_name]
        observations.append({'id': category_name, 'attempts_or_events': len(category_events),
                             'makes': sum(integer_value(event, 'FGM') for event in category_events),
                             'all_supporting_play_ids': [event['play_id'] for event in category_events],
                             'example_possession_ids': [possession['possession_id'] for possession in examples[category_name]]})
    observations_by_category = {item['id']: item for item in observations}
    metadata = {
        'game_id': game['game_id'], 'game_date': game['game_date'], 'opponent': game['opponent'],
        'uconn_points': integer_value(game, 'uconn_points'), 'opponent_points': integer_value(game, 'opponent_points'),
        'score_reconciled': True, 'event_stats_reconciled': True,
        'source_url': events[0]['source_url'], 'source_sha256': events[0]['source_sha256'],
        'canonical_events_sha256': hashlib.sha256(Path(args.events).read_bytes()).hexdigest(),
        'reconciled_games_sha256': hashlib.sha256(Path(args.games).read_bytes()).hexdigest(),
        'source_validation': 'All game and event fields match a reconstruction from the hash-verified cached ESPN payload.',
        'observations': observations, 'human_pilot_status': 'not_started',
        'reconstructed_sequences_available': len(possessions),
        'possession_scope': 'Conservative source-event sequences; incomplete coverage; not film verified.',
        'selection_rule': 'First reconstructed make and miss per shot theme; first two turnovers.',
    }
    write_json(output / 'review.json', metadata)
    evidence = []
    for category_name in CATEGORIES:
        for possession in examples[category_name]:
            for event in possession['events']:
                evidence.append(dict(event, observation_id=category_name,
                                     reconstructed_possession_id=possession['possession_id'],
                                     boundary_status=possession['boundary_status'],
                                     start_reason=possession['start_reason'], end_reason=possession['end_reason']))
    columns = ['observation_id', 'reconstructed_possession_id', 'boundary_status', 'start_reason',
               'end_reason', 'game_id', 'play_id', 'period_number', 'clock_display_value',
               'sequence_number', 'team_id', 'type_text', 'text', 'source_url', 'source_sha256']
    write_csv(output / 'possession_evidence.csv', evidence, columns)
    write_csv(output / 'defensive_events.csv', [dict(event, observation_id=review_category(event)) for event in events if review_category(event)],
              ['observation_id', 'game_id', 'play_id', 'period_number', 'clock_display_value',
               'type_text', 'text', 'FGA', 'FGM', 'FGA3', 'TOV', 'source_url', 'source_sha256'])
    write_csv(output / 'baseline_events.csv', events, list(events[0]))
    game_title = f"UConn vs {game['opponent']} — {game['game_date']}"
    source_url = events[0]['source_url']
    source_link = f"https://www.espn.com/mens-college-basketball/playbyplay/_/gameId/{game['game_id']}"
    observation_counts = observations_by_category
    titles = {
        'threes': f"Opponent threes: {observation_counts['threes']['makes']}/{observation_counts['threes']['attempts_or_events']}",
        'close_shots': f"Logged layup/dunk/tip shots: {observation_counts['close_shots']['makes']}/{observation_counts['close_shots']['attempts_or_events']}",
        'turnovers': f"Opponent turnovers: {observation_counts['turnovers']['attempts_or_events']}",
    }
    questions = {
        'threes': 'Review the contest and the sequence before the shot. Decide whether either possession deserves a staff film cut.',
        'close_shots': 'Review how the ball reached the shot and whether the finish was contested. These text labels do not establish location or a coverage error.',
        'turnovers': 'Review what caused the turnover and whether a repeatable defensive action is visible. The log alone does not credit UConn pressure.',
    }
    report = [f'# Postgame defensive review: {game_title}', '',
              f"Final: UConn {integer_value(game, 'uconn_points')}, {game['opponent']} {integer_value(game, 'opponent_points')}. Score and event statistics pass reconciliation.", '',
              'Task: choose at most one useful film review item from each theme below. This is a review queue, not a defensive grade.', '',
              '**Evidence status:** Possessions below are conservatively reconstructed between explicit source events. They have not been checked against film; foul/free-throw and other ambiguous sequences are excluded. Aggregate counts use all relevant logged events, not just these examples.', '']
    for category_name in CATEGORIES:
        report.extend([f'## {titles[category_name]}', '', questions[category_name], ''])
        if not examples[category_name]:
            report.extend(['No sequence met the boundary checks for this theme. Use the event list to locate and verify a complete possession before making a staff observation.', ''])
        for possession in examples[category_name]:
            first_event, last_event = possession['events'][0], possession['events'][-1]
            action_events = [event for event in possession['events'] if is_possession_action(event)]
            texts = ' → '.join(f"{event['clock_display_value']} {event['text']} (play {event['play_id']})" for event in action_events)
            report.extend([f"- Period {first_event['period_number']}, {first_event['clock_display_value']}–{last_event['clock_display_value']}: {texts}"])
        report.append('')
    report.extend(['## Review action', '',
                   'Open the listed period/clock in authorized game film, check the complete possession and log whether the item was accurate, useful and actionable. No video links or film confirmation are available in this dataset.', '',
                   f'[Source play-by-play]({source_link}) · [Source API]({source_url}) · [Complete sequence evidence](possession_evidence.csv) · [All counted events](defensive_events.csv)', '',
                   'The make/miss examples are selected by a fixed chronological rule. They do not establish a trend relative to another game, assign individual blame or support a lineup change.', ''])
    (output / 'defensive_review.md').write_text('\n'.join(report), encoding='utf-8')
    baseline = [f'# Baseline source packet: {game_title}', '',
                f"Final: UConn {integer_value(game, 'uconn_points')}, {game['opponent']} {integer_value(game, 'opponent_points')}.", '',
                'Task: prepare at most three defensive review items, each with a specific possession located by period/clock and source play ID. Use the same authorized film access as the report condition.', '',
                f'[Source play-by-play]({source_link}) · [Source API]({source_url}) · [Full chronological event log](baseline_events.csv)', '',
                'Do not open the defensive review in Markdown or HTML before completing a baseline session. Record actual time and usefulness with the pilot timer. This packet has no selected observations.', '']
    (output / 'baseline_packet.md').write_text('\n'.join(baseline), encoding='utf-8')
    write_baseline_html(output, game, events, source_link)
    write_html(output, game, examples, titles, questions, source_link)
    generation_seconds = time.perf_counter() - generation_started_at
    metrics = {'game_id': game['game_id'], 'generated_at_utc': datetime.now(timezone.utc).isoformat(),
               'machine_generation_seconds': round(generation_seconds, 6),
               'timing_scope': 'Loading canonical data and writing review, baseline and evidence artifacts; excludes source retrieval and reconciliation.',
               'human_review_seconds': None, 'staff_usefulness': None,
               'human_pilot_status': 'not_started',
               'source_event_count': len(events), 'reconstructed_sequences_available': len(possessions),
               'selected_sequences': sum(len(selected_examples) for selected_examples in examples.values())}
    write_json(output / 'build_metrics.json', metrics)
    artifact_names = ('review.json', 'possession_evidence.csv', 'defensive_events.csv',
                      'baseline_events.csv', 'baseline_packet.md', 'baseline_packet.html', 'defensive_review.md',
                      'defensive_review.html', 'build_metrics.json')
    write_json(output / 'packet_manifest.json', {
        'game_id': game['game_id'],
        'artifact_sha256': {name: hashlib.sha256((output / name).read_bytes()).hexdigest()
                           for name in artifact_names},
    })
    print(json.dumps(dict(metrics, output=str(output)), indent=2))


# Human trial records and packet integrity checks

def ledger_path(args):
    return Path(args.output) / 'human_sessions.jsonl'


def read_sessions(args):
    path = ledger_path(args)
    return [json.loads(line) for line in path.read_text().splitlines() if line.strip()] if path.exists() else []


def append_record(args, record):
    path = ledger_path(args)
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open('a', encoding='utf-8') as handle:
        handle.write(json.dumps(record, allow_nan=False) + '\n')


def verify_packet(output):
    """Reject missing or changed packet artifacts before recording trial time."""
    manifest_path = output / 'packet_manifest.json'
    manifest = json.loads(manifest_path.read_text())
    required = {'review.json', 'possession_evidence.csv', 'defensive_events.csv',
                'baseline_events.csv', 'baseline_packet.md', 'baseline_packet.html', 'defensive_review.md',
                'defensive_review.html', 'build_metrics.json'}
    if set(manifest.get('artifact_sha256', {})) != required:
        raise ValueError('Packet manifest does not contain the required artifacts.')
    if manifest.get('game_id') != output.name:
        raise ValueError('Packet manifest game identity is inconsistent.')
    for name, digest in manifest['artifact_sha256'].items():
        if hashlib.sha256((output / name).read_bytes()).hexdigest() != digest:
            raise ValueError(f'Packet artifact changed after build: {name}')
    return hashlib.sha256(manifest_path.read_bytes()).hexdigest()


def start_session(args):
    """Record a real review start after checking packet and session integrity."""
    # Separate CLI invocations use wall time; source UTC timestamps remain auditable.
    output = Path(args.output) / args.game_id
    packet_path = output / ('baseline_packet.md' if args.condition == 'baseline' else 'defensive_review.md')
    if not packet_path.is_file():
        raise ValueError(f'Build the selected game first: missing {packet_path}')
    if not args.reviewer.strip() or not args.pair_id.strip():
        raise ValueError('Actual reviewer and preassigned pair IDs must be nonempty.')
    manifest_hash = verify_packet(output)
    session_records = read_sessions(args)
    completed_session_ids = {session['session_id'] for session in session_records if session['status'] == 'complete'}
    if any(session['status'] == 'started' and session['reviewer'] == args.reviewer
           and session['session_id'] not in completed_session_ids for session in session_records):
        raise ValueError('This reviewer has an unfinished session. Finish it before starting another.')
    if any(session['reviewer'] == args.reviewer and session['pair_id'] == args.pair_id and
           session['condition'] == args.condition for session in session_records):
        raise ValueError('This reviewer/pair already has this condition; use a new preassigned pair.')
    record = {'session_id': uuid4().hex[:12], 'status': 'started', 'game_id': args.game_id,
              'reviewer': args.reviewer, 'condition': args.condition, 'pair_id': args.pair_id,
              'started_at_utc': datetime.now(timezone.utc).isoformat(), 'started_epoch': time.time(),
              'packet_path': str(packet_path), 'packet_sha256': hashlib.sha256(packet_path.read_bytes()).hexdigest(),
              'packet_manifest_sha256': manifest_hash}
    append_record(args, record)
    print(json.dumps(record, indent=2))


def finish_session(args):
    """Validate actual findings and append a completed review session."""
    records = read_sessions(args)
    matching_starts = [
        session_record for session_record in records
        if session_record['session_id'] == args.session_id and session_record['status'] == 'started'
    ]
    if len(matching_starts) != 1 or any(
            session_record['session_id'] == args.session_id and session_record['status'] == 'complete'
            for session_record in records):
        raise ValueError('Session must exist and not already be complete.')
    counts = [args.items_reviewed, args.source_correct, args.worth_reviewing, args.film_confirmed, args.actionable]
    if min(counts) < 0 or args.items_reviewed > 3 or any(value > args.items_reviewed for value in counts[1:]):
        raise ValueError('Review at most three items; counts must be nonnegative and cannot exceed items reviewed.')
    if args.actionable > args.worth_reviewing or args.film_confirmed > args.source_correct:
        raise ValueError('Actionable items must be useful; film-confirmed items must have correct source evidence.')
    findings = json.loads(Path(args.findings).read_text())
    if not isinstance(findings, list) or len(findings) != args.items_reviewed:
        raise ValueError('Findings JSON must contain one entry per reviewed item.')
    started_session = matching_starts[0]
    output = Path(args.output) / started_session['game_id']
    if verify_packet(output) != started_session['packet_manifest_sha256']:
        raise ValueError('Packet was rebuilt or changed during the session; timing is not comparable.')
    source_events = {event['play_id']: event for event in read_csv_rows(output / 'baseline_events.csv')}
    outcomes = ('source_correct', 'worth_reviewing', 'film_confirmed', 'actionable')
    for item in findings:
        if not isinstance(item, dict) or not all(item.get(k) for k in ('observation', 'period', 'clock', 'play_id', 'assessment')):
            raise ValueError('Each actual finding requires observation, period, clock, play_id and assessment.')
        event = source_events.get(str(item['play_id']))
        if not event or str(item['period']) != event['period_number'] or item['clock'] != event['clock_display_value']:
            raise ValueError('Finding period, clock and play ID must match a source event in this game.')
        if any(type(item.get(key)) is not bool for key in outcomes):
            raise ValueError('Each finding requires boolean source_correct, worth_reviewing, film_confirmed and actionable outcomes.')
        if item['actionable'] and not item['worth_reviewing'] or item['film_confirmed'] and not item['source_correct']:
            raise ValueError('Finding outcome flags are inconsistent.')
    if any(sum(item[key] for item in findings) != getattr(args, key) for key in outcomes):
        raise ValueError('Outcome counts must agree with the individual findings.')
    review_seconds = time.time() - started_session['started_epoch']
    if review_seconds < 0:
        raise ValueError('System clock moved backward; elapsed time cannot be trusted.')
    record = dict(started_session, status='complete', completed_at_utc=datetime.now(timezone.utc).isoformat(),
                  elapsed_seconds=round(review_seconds, 3), items_reviewed=args.items_reviewed,
                  source_correct=args.source_correct, worth_reviewing=args.worth_reviewing,
                  film_confirmed=args.film_confirmed, actionable=args.actionable,
                  findings=findings, notes=args.notes)
    append_record(args, record)
    print(json.dumps(record, indent=2))


def summarize(args):
    """Summarize recorded sessions and descriptive baseline/report contrasts."""
    completed_sessions = [session for session in read_sessions(args) if session['status'] == 'complete']
    conditions = {}
    for condition in ('baseline', 'report'):
        condition_sessions = [session for session in completed_sessions if session['condition'] == condition]
        conditions[condition] = {'sessions': len(condition_sessions),
                                 'median_seconds': statistics.median(session['elapsed_seconds'] for session in condition_sessions) if condition_sessions else None,
                                 'items_reviewed': sum(session['items_reviewed'] for session in condition_sessions),
                                 'worth_reviewing': sum(session['worth_reviewing'] for session in condition_sessions),
                                 'source_correct': sum(session['source_correct'] for session in condition_sessions),
                                 'film_confirmed': sum(session['film_confirmed'] for session in condition_sessions),
                                 'actionable': sum(session['actionable'] for session in condition_sessions)}
    pairs = {}
    for row in completed_sessions:
        pairs.setdefault((row['reviewer'], row['pair_id']), []).append(row)
    contrasts = []
    for (reviewer, pair_id), pair in pairs.items():
        baseline = [session for session in pair if session['condition'] == 'baseline']
        reports = [session for session in pair if session['condition'] == 'report']
        if len(baseline) == len(reports) == 1:
            baseline_session, report_session = baseline[0], reports[0]
            contrasts.append({'reviewer': reviewer, 'pair_id': pair_id,
                              'baseline_game_id': baseline_session['game_id'], 'report_game_id': report_session['game_id'],
                              'same_game_reuse': baseline_session['game_id'] == report_session['game_id'],
                              'seconds_saved': round(baseline_session['elapsed_seconds'] - report_session['elapsed_seconds'], 3),
                              'useful_item_difference': report_session['worth_reviewing'] - baseline_session['worth_reviewing'],
                              'report_was_first': report_session['started_epoch'] < baseline_session['started_epoch']})
    result = {'completed_sessions': len(completed_sessions), 'conditions': conditions, 'paired_contrasts': contrasts,
              'status': 'no_human_results_yet' if not completed_sessions else 'descriptive_pilot_only',
              'interpretation': 'Machine generation time is not staff time saved. Same-game reuse is confounded by familiarity; different-game pairs may differ in difficulty. These are descriptive observations, not causal estimates.'}
    destination = Path(args.output) / 'human_pilot_summary.json'
    destination.parent.mkdir(parents=True, exist_ok=True)
    write_json(destination, result)
    print(json.dumps(result, indent=2))


# Command-line interface

def build_parser():
    cli = argparse.ArgumentParser(description=__doc__)
    cli.add_argument('--output', type=Path, default=DEFAULT_OUTPUT)
    commands = cli.add_subparsers(dest='command', required=True)
    build_cli = commands.add_parser('build', help='Build a short review from reconciled source events')
    build_cli.add_argument('--events', type=Path, default=ROOT / '_data/02_derived_inputs/reconciled_events.csv')
    build_cli.add_argument('--games', type=Path, default=ROOT / '_data/02_derived_inputs/reconciled_games.csv')
    build_cli.add_argument('--game-id')
    build_cli.add_argument('--source-cache', type=Path, default=DEFAULT_SOURCE_CACHE)
    build_cli.set_defaults(func=build)
    start_cli = commands.add_parser('start', help='Human operator starts an actual review timer')
    start_cli.add_argument('--game-id', required=True)
    start_cli.add_argument('--condition', choices=('baseline', 'report'), required=True)
    start_cli.add_argument('--reviewer', required=True, help='Actual anonymized reviewer ID')
    start_cli.add_argument('--pair-id', required=True, help='Actual preassigned crossover pair ID')
    start_cli.set_defaults(func=start_session)
    finish_cli = commands.add_parser('finish', help='Record actual time and evidence-backed findings')
    finish_cli.add_argument('--session-id', required=True)
    finish_cli.add_argument('--items-reviewed', required=True, type=int)
    finish_cli.add_argument('--source-correct', required=True, type=int)
    finish_cli.add_argument('--worth-reviewing', required=True, type=int)
    finish_cli.add_argument('--film-confirmed', required=True, type=int)
    finish_cli.add_argument('--actionable', required=True, type=int)
    finish_cli.add_argument('--findings', required=True, type=Path)
    finish_cli.add_argument('--notes', default='')
    finish_cli.set_defaults(func=finish_session)
    summary_cli = commands.add_parser('summary', help='Summarize actual human sessions; empty until run')
    summary_cli.set_defaults(func=summarize)
    return cli


if __name__ == '__main__':
    args = build_parser().parse_args()
    try:
        args.func(args)
    except (ValueError, KeyError, OSError, csv.Error) as error:
        raise SystemExit(f'Pilot error: {error}')
