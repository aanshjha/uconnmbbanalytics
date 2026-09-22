"""Meaningful leakage and baseline tests; synthetic inputs need no private data."""

import copy
import csv
import importlib.util
import json
import statistics
import tempfile
import unittest
from datetime import date, timedelta
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location("pregame", ROOT / "_scripts/analysis/evaluate_pregame_defense.py")
evaluation = importlib.util.module_from_spec(spec)
spec.loader.exec_module(evaluation)


def fixture(n=14):
    return [{
        "game_id": str(1000 + i), "game_file": f"game{i}.pdf",
        "game_date": (date(2025, 11, 1) + timedelta(days=3 * i)).isoformat(),
        "tipoff_utc": (date(2025, 11, 1) + timedelta(days=3 * i)).isoformat() + "T20:00:00Z",
        "opponent": f"Opponent {i % 5}", "site_type": ("home", "away", "neutral")[i % 3],
        "uconn_points": 70 + (i * 7) % 25, "opponent_points": 55 + (i * 11) % 30,
        "score_reconciled": "true", "event_stats_reconciled": "true",
        "source_url": f"https://example.invalid/game/{1000+i}", "source_sha256": "a" * 64,
    } for i in range(n)]


def write_input(path, games):
    with path.open("w", newline="") as stream:
        writer = csv.DictWriter(stream, fieldnames=evaluation.REQUIRED)
        writer.writeheader()
        writer.writerows(games)


class PregameEvaluationTests(unittest.TestCase):
    def test_baselines_are_prior_only_and_scored_on_identical_games(self):
        games = fixture()
        predictions, folds, coverage = evaluation.evaluate(games)
        first = {row["model"]: row for row in predictions if row["game_id"] == games[5]["game_id"]}
        self.assertEqual(first["expanding_mean"]["predicted_opponent_points"], statistics.mean(row["opponent_points"] for row in games[:5]))
        self.assertEqual(first["last_three_mean"]["predicted_opponent_points"], statistics.mean(row["opponent_points"] for row in games[2:5]))
        self.assertEqual(len(coverage), 5)
        self.assertEqual(len(folds), len(games) - 5)
        for model in evaluation.MODELS:
            self.assertEqual({row["game_id"] for row in predictions if row["model"] == model}, {row["game_id"] for row in games[5:]})
        for fold in folds:
            self.assertLess(fold["train_through"], fold["game_date"])
            self.assertNotIn(fold["game_id"], fold["training_game_ids"])

    def test_current_and_future_results_cannot_change_frozen_prediction(self):
        original = fixture()
        _, original_folds, _ = evaluation.evaluate(original)
        for index in range(5, len(original)):
            changed = copy.deepcopy(original)
            for row in changed[index:]:
                row["opponent_points"] = 9999
                row["uconn_points"] = 8888
                row["source_sha256"] = "b" * 64
            _, changed_folds, _ = evaluation.evaluate(changed)
            self.assertEqual(original_folds[index - 5], changed_folds[index - 5])

    def test_future_metadata_and_appended_games_cannot_change_earlier_folds(self):
        games = fixture()
        _, reference, _ = evaluation.evaluate(games)
        changed = fixture(20)
        for row in changed[10:]:
            row["site_type"] = "neutral"
            row["opponent"] = "New future opponent"
            row["opponent_points"] = 1000
        _, altered, _ = evaluation.evaluate(changed)
        self.assertEqual(reference[:5], altered[:5])

    def test_same_day_result_is_embargoed(self):
        games = fixture()
        games[7]["game_date"] = games[6]["game_date"]
        _, folds, _ = evaluation.evaluate(games)
        heldout = next(fold for fold in folds if fold["game_id"] == games[7]["game_id"])
        self.assertNotIn(games[6]["game_id"], heldout["training_game_ids"])
        self.assertEqual(heldout["n_prior_games"], 6)

    def test_forecaster_rejects_target_outcome_and_nonprior_training(self):
        games = fixture()
        with self.assertRaisesRegex(ValueError, "pregame fields"):
            evaluation.forecast(games[:5], games[5])
        with self.assertRaisesRegex(ValueError, "non-prior"):
            evaluation.forecast(games[:6], evaluation.pregame_fields(games[5]))

    def test_scaling_uses_only_historical_feature_rows(self):
        games = fixture()
        _, folds, _ = evaluation.evaluate(games)
        expected = [evaluation.build_pregame_features(evaluation.pregame_fields(games[i]), games[:i]) for i in range(1, 5)]
        self.assertEqual(folds[0]["ridge_fit"]["feature_means"], [statistics.mean(row[j] for row in expected) for j in range(4)])

    def test_failed_reconciliation_excluded_and_duplicate_ids_rejected(self):
        with tempfile.TemporaryDirectory() as temp:
            path = Path(temp) / "input.csv"
            games = fixture()
            games[6]["score_reconciled"] = "false"
            write_input(path, games)
            loaded, excluded = evaluation.load_games(path)
            self.assertEqual(len(loaded), 13)
            self.assertEqual(excluded[0]["reason"], "final_score_not_reconciled")
            games.append(games[0])
            write_input(path, games)
            with self.assertRaisesRegex(ValueError, "duplicate game_id"):
                evaluation.load_games(path)

    def test_metrics_and_paired_comparisons_are_correct(self):
        predictions = []
        for model, errors in (("expanding_mean", [1, -3]), ("last_three_mean", [2, -4]), ("pregame_ridge", [0, -2])):
            for gid, error in enumerate(errors):
                predictions.append({"model": model, "game_id": str(gid), "error_points": error, "absolute_error_points": abs(error), "squared_error_points": error ** 2})
        metrics = evaluation.metrics_for(predictions)
        self.assertEqual(metrics[0]["mae_points"], 2)
        self.assertAlmostEqual(metrics[0]["rmse_points"], 5 ** 0.5)
        comparisons = evaluation.paired_comparisons(predictions)
        self.assertEqual(comparisons[0]["mae_difference_points_model_minus_baseline"], -1)
        self.assertEqual(comparisons[0]["bootstrap_p025_points"], -1)
        self.assertEqual(comparisons[1]["mae_difference_points_model_minus_baseline"], -2)

    def test_end_to_end_report_and_provenance(self):
        with tempfile.TemporaryDirectory() as temp:
            path = Path(temp) / "input.csv"
            output = Path(temp) / "output"
            write_input(path, fixture())
            manifest = evaluation.run(path, output)
            self.assertEqual(manifest["test_games"], 9)
            self.assertEqual(manifest["lineup_validation_status"], "not_evaluated_unreconciled_stint_inputs")
            folds = json.loads((output / "pregame_folds.json").read_text())
            for fold in folds:
                recorded = fold.pop("forecast_sha256")
                self.assertEqual(recorded, evaluation.canonical_hash(fold))
            self.assertEqual(len(list(output.iterdir())), 7)


if __name__ == "__main__":
    unittest.main()
