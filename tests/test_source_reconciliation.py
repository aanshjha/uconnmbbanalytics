import copy
import importlib.util
from pathlib import Path
import unittest

SCRIPT = Path(__file__).resolve().parents[1] / '_scripts/ops/reconcile_source_data.py'
SPEC = importlib.util.spec_from_file_location('reconciliation', SCRIPT)
reconcile = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(reconcile)


def fixture():
    teams = [{'id': '41', 'homeAway': 'home', 'score': '3', 'team': {'displayName': 'UConn'}},
             {'id': '57', 'homeAway': 'away', 'score': '2', 'team': {'displayName': 'Opponent'}}]
    boxes = []
    for tid, three in [('41', 1), ('57', 0)]:
        stats = {'fieldGoalsMade-fieldGoalsAttempted': '1-1',
                 'threePointFieldGoalsMade-threePointFieldGoalsAttempted': f'{three}-{three}',
                 'freeThrowsMade-freeThrowsAttempted': '0-0', 'offensiveRebounds': '0',
                 'defensiveRebounds': '0', 'totalTurnovers': '0'}
        boxes.append({'team': {'id': tid}, 'statistics': [{'name': k, 'displayValue': v} for k, v in stats.items()]})
    plays = [
        {'id': '401812793115715635', 'sequenceNumber': '1', 'type': {'text': 'JumpShot'},
         'text': 'Player made Three Point Jumper.', 'period': {'number': 1},
         'clock': {'displayValue': '19:00'}, 'team': {'id': '41'},
         'scoringPlay': True, 'shootingPlay': True, 'scoreValue': 3, 'homeScore': 3, 'awayScore': 0},
        {'id': '401812793115715636', 'sequenceNumber': '2', 'type': {'text': 'LayUpShot'},
         'text': 'Player made Layup.', 'period': {'number': 1},
         'clock': {'displayValue': '18:00'}, 'team': {'id': '57'},
         'scoringPlay': True, 'shootingPlay': True, 'scoreValue': 2, 'homeScore': 3, 'awayScore': 2},
    ]
    return {'header': {'id': '123', 'competitions': [{'competitors': teams,
             'status': {'type': {'completed': True}}, 'date': '2026-01-02T01:00Z',
             'neutralSite': False, 'conferenceCompetition': False}]},
            'boxscore': {'teams': boxes}, 'plays': plays}


class SourceReconciliationTests(unittest.TestCase):
    def build(self, data):
        return reconcile.canonical_game('123', 'game.pdf', data,
                                        {'source_url': 'https://example.test', 'sha256': 'a'*64,
                                         'retrieved_at_utc': '2026-09-16T00:00:00Z'})

    def test_score_stats_and_string_ids(self):
        game, events, stats, issues = self.build(fixture())
        self.assertTrue(game['score_reconciled'])
        self.assertTrue(game['event_stats_reconciled'])
        self.assertEqual(game['game_date'], '2026-01-01')
        self.assertEqual(len({e['play_id'] for e in events}), 2)
        self.assertEqual(events[1]['play_id'], '401812793115715636')
        self.assertEqual(len(stats), 20)
        self.assertEqual(issues, [])

    def test_duplicate_source_event_is_not_counted_twice(self):
        data = fixture()
        data['plays'].append(copy.deepcopy(data['plays'][0]))
        self.assertEqual(len(self.build(data)[1]), 2)

    def test_conflicting_duplicate_fails(self):
        data = fixture()
        changed = copy.deepcopy(data['plays'][0]); changed['text'] = 'different'
        data['plays'].append(changed)
        with self.assertRaisesRegex(ValueError, 'Conflicting source event'):
            self.build(data)

    def test_missing_event_does_not_get_imputed(self):
        data = fixture(); data['plays'].pop(0)
        game, events, stats, _ = self.build(data)
        self.assertFalse(game['score_reconciled'])
        self.assertFalse(game['event_stats_reconciled'])
        self.assertEqual(len(events), 1)
        self.assertTrue(any(s['difference'] for s in stats))

    def test_delayed_source_entry_uses_period_and_clock(self):
        data = fixture()
        data['plays'][0]['sequenceNumber'] = '100'
        game, events, _, _ = self.build(data)
        self.assertTrue(game['score_reconciled'])
        self.assertEqual(events[0]['team_id'], '41')

    def test_bad_intermediate_score_does_not_certify_timeline(self):
        data = fixture(); data['plays'][0]['homeScore'] = 1
        game, _, _, issues = self.build(data)
        self.assertTrue(game['score_reconciled'])
        self.assertFalse(game['score_timeline_reconciled'])
        self.assertEqual(len(issues), 2)

    def test_wrong_box_arithmetic_rejected(self):
        data = fixture(); data['header']['competitions'][0]['competitors'][0]['score'] = '99'
        with self.assertRaisesRegex(ValueError, 'Box-score arithmetic'):
            self.build(data)

    def test_third_team_stat_event_rejected(self):
        data = fixture()
        extra = copy.deepcopy(data['plays'][0])
        extra.update(id='extra', team={'id': '999'})
        data['plays'].append(extra)
        with self.assertRaisesRegex(ValueError, 'unknown team'):
            self.build(data)

    def test_missed_free_throw_is_attempt_not_point(self):
        p = fixture()['plays'][0]
        p.update(type={'text': 'MadeFreeThrow'}, text='Player missed Free Throw.',
                 scoringPlay=False, scoreValue=1)
        stats = reconcile.event_stats(p)
        self.assertEqual((stats['FTA'], stats['FTM'], stats['PTS'], stats['FGA']), (1, 0, 0, 0))


if __name__ == '__main__':
    unittest.main()
