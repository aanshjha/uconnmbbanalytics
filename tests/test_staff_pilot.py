"""Pilot checks use synthetic events and timers in temporary directories only."""
import contextlib
import csv
import importlib.util
import io
import json
from pathlib import Path
import tempfile
from types import SimpleNamespace
import unittest
from unittest.mock import patch


ROOT = Path(__file__).resolve().parents[1]


def module(name, path):
    spec = importlib.util.spec_from_file_location(name, path)
    result = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(result)
    return result


pilot = module('staff_pilot', ROOT / '_scripts/analysis/postgame_defensive_review.py')
reconcile = module('pilot_source_fixture', ROOT / '_scripts/ops/reconcile_source_data.py')


def event(pid, clock, own=False, period='1', text='', kind='', **stats):
    row = {key: '0' for key in ('FGA', 'FGM', 'FGA3', 'FGM3', 'FTA', 'FTM', 'OREB', 'DREB', 'TOV')}
    row.update(game_id='123', play_id=str(pid), clock_display_value=clock,
               period_number=period, is_uconn_offense=str(own), text=text, type_text=kind)
    row.update({key: str(value) for key, value in stats.items()})
    return row


class PossessionBoundaryTests(unittest.TestCase):
    def start(self):
        return event('start', '19:00', DREB=1)

    def test_explicit_miss_rebound_and_turnover_boundaries(self):
        miss = event('miss', '18:45', FGA=1, FGA3=1)
        rebound = event('end', '18:43', own=True, DREB=1)
        result = pilot.reconstruct_possessions([self.start(), miss, rebound])
        self.assertEqual(len(result), 1)
        self.assertEqual(result[0]['possession_id'], '123:start:end')
        self.assertEqual(result[0]['end_reason'], 'uconn_defensive_rebound')
        result = pilot.reconstruct_possessions([
            event('start', '19:00', own=True, TOV=1), event('end', '18:55', TOV=1)])
        self.assertEqual(result[0]['start_reason'], 'uconn_turnover')
        self.assertEqual(result[0]['end_reason'], 'opponent_turnover')

    def test_make_requires_subsequent_uconn_control(self):
        make = event('make', '18:45', FGA=1, FGM=1, FGA3=1)
        own_attempt = event('own', '18:30', own=True, FGA=1)
        self.assertEqual(len(pilot.reconstruct_possessions([self.start(), make, own_attempt])), 1)
        self.assertEqual(pilot.reconstruct_possessions([self.start(), make]), [])
        self.assertEqual(pilot.reconstruct_possessions([make, own_attempt]), [])
        self.assertEqual(pilot.reconstruct_possessions([self.start(), make,
                                                      event('opp', '18:30', FGA=1)]), [])

    def test_and_one_foul_and_free_throw_excluded(self):
        for disputed in [event('foul', '18:45', kind='Personal Foul'),
                         event('ft', '18:45', FTA=1, FTM=1),
                         event('jump', '18:45', kind='Jump Ball')]:
            with self.subTest(disputed=disputed['play_id']):
                self.assertEqual(pilot.reconstruct_possessions([
                    self.start(), event('make', '18:45', FGA=1, FGM=1), disputed,
                    event('own', '18:30', own=True, FGA=1)]), [])
                self.assertEqual(pilot.reconstruct_possessions([
                    self.start(), disputed, event('turnover', '18:30', TOV=1)]), [])

    def test_rebound_required_between_multiple_attempts(self):
        miss = event('miss', '18:45', FGA=1)
        make = event('make', '18:40', FGA=1, FGM=1)
        own_attempt = event('own', '18:30', own=True, FGA=1)
        rebound = event('oreb', '18:43', OREB=1)
        self.assertEqual(len(pilot.reconstruct_possessions([
            self.start(), miss, rebound, make, own_attempt])), 1)
        for sequence in ([self.start(), miss, make, own_attempt],
                         [self.start(), rebound, make, own_attempt],
                         [self.start(), miss, rebound, event('dreb', '18:40', own=True, DREB=1)],
                         [self.start(), miss, event('tov', '18:40', TOV=1)]):
            self.assertEqual(pilot.reconstruct_possessions(sequence), [])

    def test_inconsistent_and_incomplete_control_excluded(self):
        for ending in [event('end', '18:40', own=True, FGA=1),
                       event('end', '18:40', own=True, OREB=1),
                       event('end', '18:40', own=True, DREB=1),
                       event('end', '18:40', period='2', TOV=1),
                       event('end', '16:59', TOV=1),
                       event('end', '19:01', TOV=1)]:
            with self.subTest(ending=ending):
                self.assertEqual(pilot.reconstruct_possessions([self.start(), ending]), [])
        self.assertEqual(pilot.reconstruct_possessions([self.start(), event('miss', '18:45', FGA=1)]), [])

    def test_selection_is_fixed_and_categorization_is_text_limited(self):
        items = []
        for index, made in enumerate([False, True, True, False]):
            items.append({'events': [event(index, '18:00', FGA=1, FGA3=1, FGM=int(made))]})
        self.assertEqual(pilot.choose_examples(items, 'threes'), [items[1], items[0]])
        self.assertEqual(pilot.review_category(event(1, '18:00', FGA=1, text='Player made Layup.')), 'close_shots')
        self.assertIsNone(pilot.review_category(event(1, '18:00', FGA=1, text='Player made Jumper.')))

    def test_fractional_source_clocks_are_supported(self):
        possession = pilot.reconstruct_possessions([
            event('start', '0:08.5', DREB=1), event('end', '0:02.3', TOV=1)])
        self.assertEqual(len(possession), 1)
        self.assertAlmostEqual(pilot.clock_seconds(possession[0]['events'][0]), 8.5)


def source_fixture():
    teams = [{'id': '41', 'homeAway': 'home', 'score': '2', 'team': {'displayName': 'UConn'}},
             {'id': '57', 'homeAway': 'away', 'score': '3', 'team': {'displayName': 'Fixture opponent'}}]
    boxes = []
    for tid, fga, three, rebound in [('41', 2, 0, 0), ('57', 1, 1, 1)]:
        stats = {'fieldGoalsMade-fieldGoalsAttempted': f'1-{fga}',
                 'threePointFieldGoalsMade-threePointFieldGoalsAttempted': f'{three}-{three}',
                 'freeThrowsMade-freeThrowsAttempted': '0-0', 'offensiveRebounds': '0',
                 'defensiveRebounds': str(rebound), 'totalTurnovers': '1'}
        boxes.append({'team': {'id': tid}, 'statistics': [{'name': k, 'displayValue': v} for k, v in stats.items()]})
    plays = []
    details = [('41', '19:50', 'LayUpShot', 'Player missed Layup.', 0, 0, 0),
               ('57', '19:49', 'Defensive Rebound', 'Opponent Defensive Rebound.', 0, 0, 0),
               ('57', '19:30', 'JumpShot', 'Opponent made Three Point Jumper.', 3, 0, 3),
               ('41', '19:15', 'LayUpShot', 'Player made Layup.', 2, 2, 3),
               ('41', '19:00', 'Turnover', 'Player Turnover.', 0, 2, 3),
               ('57', '18:50', 'Turnover', 'Opponent Turnover.', 0, 2, 3)]
    for index, (team, clock, kind, text, points, home, away) in enumerate(details, 1):
        plays.append({'id': '40181279311571563' + str(index), 'sequenceNumber': str(index),
                      'type': {'text': kind}, 'text': text, 'period': {'number': 1},
                      'clock': {'displayValue': clock}, 'team': {'id': team},
                      'scoringPlay': bool(points), 'shootingPlay': kind.endswith('Shot'),
                      'scoreValue': points, 'homeScore': home, 'awayScore': away})
    return {'header': {'id': '123', 'competitions': [{'competitors': teams,
             'status': {'type': {'completed': True}}, 'date': '2026-01-02T01:00Z',
             'neutralSite': False, 'conferenceCompetition': False}]},
            'boxscore': {'teams': boxes}, 'plays': plays}


class PacketAndSessionTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.cache = self.root / 'cache'
        self.cache.mkdir()
        raw = json.dumps(source_fixture()).encode()
        meta = {'source_url': reconcile.source_url('123'),
                'sha256': pilot.hashlib.sha256(raw).hexdigest(),
                'retrieved_at_utc': '2026-09-16T00:00:00+00:00'}
        (self.cache / '123.json').write_bytes(raw)
        (self.cache / '123.meta.json').write_text(json.dumps(meta))
        self.game, self.events, _, _ = reconcile.canonical_game('123', 'fixture.pdf', source_fixture(), meta)
        self.games_path = self.root / 'games.csv'
        self.events_path = self.root / 'events.csv'
        self.rewrite_inputs()
        self.output = self.root / 'output'
        self.args = SimpleNamespace(output=self.output, games=self.games_path, events=self.events_path,
                                    game_id='123', source_cache=self.cache)

    def rewrite_inputs(self):
        reconcile.write_csv(self.games_path, [self.game])
        reconcile.write_csv(self.events_path, self.events)

    def load(self):
        return pilot.load_game(self.events_path, self.games_path, '123', self.cache)

    def build(self):
        with contextlib.redirect_stdout(io.StringIO()):
            pilot.build(self.args)

    def start(self, condition='baseline', epoch=1000, pair='test_pair'):
        args = SimpleNamespace(output=self.output, game_id='123', reviewer='test_reviewer',
                               condition=condition, pair_id=pair)
        with contextlib.redirect_stdout(io.StringIO()), patch.object(pilot.time, 'time', return_value=epoch):
            pilot.start_session(args)
        return pilot.read_sessions(args)[-1]

    def finish_args(self, start, **changes):
        finding = {'observation': 'Synthetic test turnover', 'period': 1, 'clock': '18:50',
                   'play_id': self.events[-1]['play_id'], 'assessment': 'Synthetic test assessment',
                   'source_correct': True, 'worth_reviewing': True, 'film_confirmed': False,
                   'actionable': False}
        path = self.root / 'test_findings.json'
        path.write_text(json.dumps([finding]))
        args = dict(output=self.output, session_id=start['session_id'], items_reviewed=1,
                    source_correct=1, worth_reviewing=1, film_confirmed=0, actionable=0,
                    findings=path, notes='Synthetic unit test only')
        args.update(changes)
        return SimpleNamespace(**args)

    def finish(self, args, epoch=1012.5):
        with contextlib.redirect_stdout(io.StringIO()), patch.object(pilot.time, 'time', return_value=epoch):
            pilot.finish_session(args)

    def test_source_identity_totals_and_exact_long_ids(self):
        game, events = self.load()
        self.assertEqual(game['game_id'], '123')
        self.assertEqual(events[0]['play_id'], '401812793115715631')
        self.assertEqual(len(events), 6)

    def test_reconciliation_flags_fail_closed(self):
        for key in ('score_reconciled', 'event_stats_reconciled'):
            self.game[key] = False
            self.rewrite_inputs()
            with self.assertRaisesRegex(ValueError, 'passes both'):
                self.load()
            self.game[key] = True

    def test_event_text_clock_order_and_provenance_tampering_rejected(self):
        for key, value in [('text', 'Altered observation'), ('clock_display_value', '18:59'),
                           ('event_order', 7), ('source_url', 'https://example.test'),
                           ('source_sha256', 'b' * 64), ('FGA', 4), ('is_uconn_offense', ''),
                           ('play_id', 'rounded-or-altered-id')]:
            with self.subTest(key=key):
                old = self.events[0][key]
                self.events[0][key] = value
                self.rewrite_inputs()
                with self.assertRaises(ValueError):
                    self.load()
                self.events[0][key] = old

    def test_game_score_duplicate_ids_and_source_payload_tampering_rejected(self):
        self.game['uconn_points'] = 999
        self.rewrite_inputs()
        with self.assertRaisesRegex(ValueError, 'game differs'):
            self.load()
        self.game['uconn_points'] = 2
        self.rewrite_inputs()
        reconcile.write_csv(self.games_path, [self.game, self.game])
        with self.assertRaisesRegex(ValueError, 'game IDs must be unique'):
            self.load()
        self.rewrite_inputs()
        self.events[1]['play_id'] = self.events[0]['play_id']
        self.rewrite_inputs()
        with self.assertRaisesRegex(ValueError, 'play IDs'):
            self.load()
        self.events[1]['play_id'] = '401812793115715632'
        self.rewrite_inputs()
        (self.cache / '123.json').write_text(json.dumps(source_fixture()) + '\n')
        with self.assertRaisesRegex(ValueError, 'Source hash mismatch'):
            self.load()

    def test_build_has_source_evidence_and_no_invented_human_results(self):
        self.build()
        packet = self.output / '123'
        review = json.loads((packet / 'review.json').read_text())
        metrics = json.loads((packet / 'build_metrics.json').read_text())
        self.assertEqual(review['human_pilot_status'], 'not_started')
        self.assertIsNone(metrics['human_review_seconds'])
        self.assertIsNone(metrics['staff_usefulness'])
        self.assertGreaterEqual(metrics['machine_generation_seconds'], 0)
        self.assertTrue(review['canonical_events_sha256'])
        self.assertEqual(review['observations'][0]['all_supporting_play_ids'], [self.events[2]['play_id']])
        self.assertEqual(len(pilot.read_csv_rows(packet / 'possession_evidence.csv')), 4)
        self.assertTrue(pilot.verify_packet(packet))
        self.assertFalse(pilot.ledger_path(self.args).exists())
        with contextlib.redirect_stdout(io.StringIO()):
            pilot.summarize(self.args)
        summary = json.loads((self.output / 'human_pilot_summary.json').read_text())
        self.assertEqual(summary['status'], 'no_human_results_yet')
        self.assertEqual(summary['completed_sessions'], 0)
        self.assertEqual(summary['paired_contrasts'], [])
        self.assertIsNone(summary['conditions']['report']['median_seconds'])

    def test_baseline_html_contains_every_event_without_selected_findings_or_intermediate_scores(self):
        self.build()
        packet = self.output / '123'
        document = (packet / 'baseline_packet.html').read_text()
        self.assertEqual(document.count('<tr class="event">'), len(self.events))
        for row in self.events:
            self.assertEqual(document.count(row['play_id']), 1)
            self.assertIn(row['clock_display_value'], document)
            self.assertIn(row['text'], document)
        self.assertIn('Period 1', document)
        self.assertNotIn('Opponent threes:', document)
        self.assertNotIn('Source plays ', document)
        for field in ('home_score', 'away_score', 'uconn_score', 'opponent_score'):
            self.assertNotIn(field, document)
        modified = dict(self.events[0], text='<script>alert("untrusted")</script>')
        pilot.write_baseline_html(packet, self.game, [modified], 'https://example.test')
        self.assertIn('&lt;script&gt;', (packet / 'baseline_packet.html').read_text())
        with self.assertRaisesRegex(ValueError, 'baseline_packet.html'):
            pilot.verify_packet(packet)

    def test_actual_timer_lifecycle_and_pair_summary(self):
        self.build()
        start = self.start()
        with self.assertRaisesRegex(ValueError, 'unfinished'):
            self.start(condition='report')
        args = self.finish_args(start)
        self.finish(args)
        completed = pilot.read_sessions(args)[-1]
        self.assertEqual(completed['elapsed_seconds'], 12.5)
        self.assertEqual(completed['packet_manifest_sha256'], start['packet_manifest_sha256'])
        with self.assertRaisesRegex(ValueError, 'not already be complete'):
            self.finish(args)
        with self.assertRaisesRegex(ValueError, 'already has this condition'):
            self.start()
        report = self.start(condition='report', epoch=2000)
        self.finish(self.finish_args(report), epoch=2010)
        with contextlib.redirect_stdout(io.StringIO()):
            pilot.summarize(args)
        summary = json.loads((self.output / 'human_pilot_summary.json').read_text())
        self.assertEqual(summary['completed_sessions'], 2)
        self.assertEqual(summary['paired_contrasts'][0]['seconds_saved'], 2.5)
        self.assertTrue(summary['paired_contrasts'][0]['same_game_reuse'])
        self.assertFalse(summary['paired_contrasts'][0]['report_was_first'])
        self.assertEqual(summary['status'], 'descriptive_pilot_only')

    def test_packet_tampering_rejected_before_start_and_after_start(self):
        self.build()
        path = self.output / '123' / 'defensive_review.md'
        old = path.read_text()
        path.write_text(old + 'Changed')
        with self.assertRaisesRegex(ValueError, 'changed after build'):
            self.start()
        path.write_text(old)
        start = self.start()
        evidence = self.output / '123' / 'baseline_events.csv'
        evidence.write_text(evidence.read_text() + '\n')
        with self.assertRaisesRegex(ValueError, 'changed after build'):
            self.finish(self.finish_args(start))
        self.assertEqual(len(pilot.read_sessions(self.args)), 1)

    def test_rebuild_during_session_and_backward_clock_rejected(self):
        self.build()
        start = self.start()
        with self.assertRaisesRegex(ValueError, 'clock moved backward'):
            self.finish(self.finish_args(start), epoch=999)
        self.build()
        with self.assertRaisesRegex(ValueError, 'rebuilt or changed'):
            self.finish(self.finish_args(start))

    def test_invalid_finding_locators_flags_and_count_claims_rejected(self):
        self.build()
        start = self.start()
        for key, value in [('play_id', 'not-real'), ('period', 2), ('clock', '18:49'),
                           ('source_correct', 'true'), ('worth_reviewing', False),
                           ('actionable', True), ('observation', '')]:
            with self.subTest(key=key):
                args = self.finish_args(start)
                data = json.loads(args.findings.read_text())
                data[0][key] = value
                args.findings.write_text(json.dumps(data))
                with self.assertRaises(ValueError):
                    self.finish(args)
        for changes in ({'items_reviewed': 4}, {'worth_reviewing': 2},
                        {'actionable': 1}, {'film_confirmed': 1}, {'source_correct': -1}):
            with self.assertRaises(ValueError):
                self.finish(self.finish_args(start, **changes))
        self.assertEqual(len(pilot.read_sessions(self.args)), 1)

    def test_zero_items_is_a_valid_actual_outcome(self):
        self.build()
        args = self.finish_args(self.start(), items_reviewed=0, source_correct=0, worth_reviewing=0)
        args.findings.write_text('[]')
        self.finish(args)
        self.assertEqual(pilot.read_sessions(args)[-1]['items_reviewed'], 0)


if __name__ == '__main__':
    unittest.main()
