import csv
import importlib.util
from pathlib import Path
import sys
import tempfile
import unittest

OPS = Path(__file__).resolve().parents[1] / '_scripts/ops'
sys.path.insert(0, str(OPS))
SPEC = importlib.util.spec_from_file_location('lineup_evidence', OPS / 'audit_lineup_evidence.py')
audit = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(audit)


def event(play_id, clock, team, event_type, points=0, **stats):
    row = dict(game_id='fixture', play_id=play_id, period_number=1,
               clock_display_value=clock, team_id=team, type_text=event_type,
               text=event_type, points=points, PTS=points, source_sha256='a'*64,
               lineup_state_status='source_consistent_five_player_state',
               uconn_lineup_ids='1|2|3|4|5', opponent_lineup_ids='11|12|13|14|15')
    row.update(dict.fromkeys(('FGA', 'FGM', 'FTA', 'TOV', 'OREB', 'DREB'), 0))
    row.update(stats)
    return row


def athlete(athlete_id, starter=True):
    return {'athlete': {'id': athlete_id, 'displayName': athlete_id}, 'starter': starter}


class LineupEvidenceTests(unittest.TestCase):
    def test_substitution_clock_and_invalid_group_are_excluded(self):
        data = {
            'header': {'competitions': [{'competitors': [{'id': '41'}, {'id': '57'}]}]},
            'boxscore': {'players': [
                {'team': {'id': '41'}, 'statistics': [{'athletes': [athlete(str(n)) for n in range(1, 6)] + [athlete('6', False)]}]},
                {'team': {'id': '57'}, 'statistics': [{'athletes': [athlete(str(n)) for n in range(11, 16)]}]},
            ]},
            'plays': [
                {'id': 'score1', 'participants': [{'athlete': {'id': '1'}}]},
                {'id': 'out2', 'text': 'subbing out', 'participants': [{'athlete': {'id': '1'}}]},
                {'id': 'in2', 'text': 'subbing in', 'participants': [{'athlete': {'id': '6'}}]},
                {'id': 'score2', 'participants': [{'athlete': {'id': '6'}}]},
                {'id': 'score3', 'participants': [{'athlete': {'id': '6'}}]},
                {'id': 'out4', 'text': 'subbing out', 'participants': [{'athlete': {'id': '2'}}]},
                {'id': 'in4', 'text': 'subbing in', 'participants': [{'athlete': {'id': '6'}}]},
                {'id': 'score5', 'participants': [{'athlete': {'id': '3'}}]},
            ],
        }
        rows = [event('score1', '19:00', '41', 'JumpShot', 2),
                event('out2', '18:00', '41', 'Substitution'),
                event('in2', '18:00', '41', 'Substitution'),
                event('score2', '18:00', '41', 'JumpShot', 2),
                event('score3', '17:00', '41', 'JumpShot', 2),
                event('out4', '16:00', '41', 'Substitution'),
                event('in4', '16:00', '41', 'Substitution'),
                event('score5', '15:00', '41', 'JumpShot', 2)]
        states, issues, _ = audit.lineup_state_rows('fixture', data, rows)
        self.assertEqual([row['lineup_state_status'] for row in states],
                         ['source_consistent_five_player_state'] + ['same_clock_substitution']*3
                         + ['source_consistent_five_player_state'] + ['same_clock_substitution']*2
                         + ['unresolved_player_state'])
        self.assertIn('6', states[4]['uconn_lineup_ids'])
        self.assertEqual(states[7]['uconn_lineup_ids'], '')
        self.assertEqual(len(issues), 1)
        self.assertEqual(issues[0]['reason'], 'invalid_substitution_group')

    def test_possession_needs_explicit_end_and_stable_lineups(self):
        rows = [event('a', '10:00', '57', 'Turnover', TOV=1),
                event('b', '9:50', '41', 'JumpShot', 2, FGA=1, FGM=1),
                event('c', '9:30', '57', 'JumpShot', FGA=1),
                event('d', '9:00', '57', 'Turnover', TOV=1),
                event('e', '8:50', '41', 'Personal Foul')]
        included, excluded = audit.bounded_possessions(rows, '57')
        self.assertEqual([(row['start_play_id'], row['end_play_id'], row['points_scored'])
                          for row in included], [('a', 'b', 2)])
        self.assertEqual(excluded[0]['exclusion_reason'], 'foul_free_throw_or_jump')
        rows[1]['lineup_state_status'] = 'same_clock_substitution'
        included, excluded = audit.bounded_possessions(rows, '57')
        self.assertFalse(included)
        self.assertEqual(excluded[0]['exclusion_reason'], 'unresolved_or_changed_lineup')

    def test_core_stint_boundary_points_do_not_hide_interior_mismatch(self):
        stint = dict(game_file='fixture.pdf', period='1st Half', stint_index='1',
                     start_time='20:00', end_time='19:00', poss_est='4',
                     points_for='1', points_against='0')
        rows = [event('boundary', '20:00', '41', 'JumpShot', 2),
                event('interior', '19:30', '41', 'JumpShot', 2)]
        game = {'game_id': 'fixture', 'uconn_points': '4', 'opponent_points': '0'}
        result = audit.audit_core_stints([stint], {'fixture.pdf': game}, {'fixture': rows}, set(), False)
        self.assertEqual(result[0]['source_interior_points_for'], 2)
        self.assertEqual(result[0]['source_boundary_candidate_points_for'], 2)
        self.assertTrue(result[0]['definite_points_mismatch_for'])
        self.assertEqual(result[0]['historical_imputed_possessions'], 'unknown')
        game_result = audit.audit_core_games(result, {'fixture.pdf': game}, {'fixture': rows})
        self.assertEqual(game_result[0]['scoring_events_outside_all_stint_clocks'], 0)
        self.assertEqual(game_result[0]['legacy_minus_source_for'], -3)
        stint['points_for'] = '2'
        result = audit.audit_core_stints([stint], {'fixture.pdf': game}, {'fixture': rows}, set(), True)
        self.assertFalse(result[0]['definite_points_mismatch_for'])

    def test_historical_imputation_requires_matching_current_value(self):
        stint = dict(zip(audit.IDENTITY, ('fixture.pdf', '1st Half', '1', '20:00', '19:00')))
        stint['poss_est'] = '4'
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / 'uconn_stints_core_input_repair_report_1.csv'
            with path.open('w', newline='') as handle:
                writer = csv.DictWriter(handle, fieldnames=[*audit.IDENTITY, 'poss_est', 'reason_codes', 'new_poss_est'])
                writer.writeheader()
                writer.writerow({**stint, 'reason_codes': 'poss_est_invalid_repaired_from_points', 'new_poss_est': '4'})
            keys, report_count = audit.imputed_possession_keys(directory, [stint])
            self.assertEqual((len(keys), report_count), (1, 1))
            stint['poss_est'] = '5'
            keys, _ = audit.imputed_possession_keys(directory, [stint])
            self.assertFalse(keys)


if __name__ == '__main__':
    unittest.main()
