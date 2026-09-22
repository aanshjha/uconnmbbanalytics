#!/usr/bin/env python3
"""A small, auditable rolling evaluation using reconciled final game scores.

No third-party dependencies. This evaluates game points conceded, not lineup
quality, defensive efficiency, or the causal value of a coaching decision.
"""

from __future__ import annotations

import argparse
import csv
import hashlib
import json
import math
import random
import statistics
from datetime import date
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
MODELS = ("expanding_mean", "last_three_mean", "pregame_ridge")
FEATURES = ("is_home", "is_neutral", "prior_three_points_against", "prior_three_points_for")
MIN_PRIOR_GAMES = 5
RIDGE_PENALTY = 10.0  # Fixed before this evaluation; never selected on test outcomes.
SEED = 20260916
REQUIRED = (
    "game_id", "game_file", "game_date", "tipoff_utc", "opponent", "site_type",
    "uconn_points", "opponent_points", "score_reconciled", "event_stats_reconciled",
    "source_url", "source_sha256",
)
PREGAME_FIELDS = ("game_id", "game_date", "tipoff_utc", "opponent", "site_type")


# Reconciled inputs and historical feature construction

def canonical_hash(value):
    return hashlib.sha256(json.dumps(value, sort_keys=True, separators=(",", ":")).encode()).hexdigest()


def is_true(value):
    return str(value).strip().lower() in ("true", "1")


def load_games(path):
    """Load unique, reconciled games and record every eligibility exclusion."""
    with Path(path).open(newline="", encoding="utf-8-sig") as stream:
        reader = csv.DictReader(stream)
        missing = set(REQUIRED) - set(reader.fieldnames or ())
        if missing:
            raise ValueError("Missing reconciliation columns: " + ", ".join(sorted(missing)))
        rows = list(reader)
    games, exclusions, seen_game_ids = [], [], set()
    for row in rows:
        game_id = row["game_id"]
        if not game_id or game_id in seen_game_ids:
            raise ValueError(f"Missing or duplicate game_id: {game_id!r}")
        seen_game_ids.add(game_id)
        date.fromisoformat(row["game_date"])
        exclusion_reasons = []
        if not is_true(row["score_reconciled"]):
            exclusion_reasons.append("final_score_not_reconciled")
        if not is_true(row["event_stats_reconciled"]):
            exclusion_reasons.append("source_event_stats_not_reconciled")
        if not row["source_url"] or len(row["source_sha256"]) != 64:
            exclusion_reasons.append("missing_source_provenance")
        if exclusion_reasons:
            exclusions.append({"game_id": game_id, "game_date": row["game_date"], "reason": ";".join(exclusion_reasons)})
            continue
        for key in ("uconn_points", "opponent_points"):
            points = float(row[key])
            if not math.isfinite(points) or points < 0 or points != int(points):
                raise ValueError(f"Invalid {key} for {game_id}")
            row[key] = int(points)
        if row["site_type"] not in ("home", "away", "neutral"):
            raise ValueError(f"Unknown site_type for {game_id}: {row['site_type']!r}")
        games.append(row)
    games.sort(key=lambda row: (row["game_date"], row["tipoff_utc"], row["game_id"]))
    return games, exclusions


def prior_games(games, target):
    # Date-level embargo also excludes same-day games when completion timestamps
    # are unavailable. A later tipoff alone does not certify an earlier finish.
    return [game for game in games if game["game_date"] < target["game_date"]]


def pregame_fields(game):
    return {key: game[key] for key in PREGAME_FIELDS}


def build_pregame_features(target, prior):
    """Build site indicators and recent scoring averages from prior games only."""
    if not prior:
        return None
    recent_games = prior[-3:]
    return [
        float(target["site_type"] == "home"),
        float(target["site_type"] == "neutral"),
        statistics.mean(row["opponent_points"] for row in recent_games),
        statistics.mean(row["uconn_points"] for row in recent_games),
    ]


# Fixed ridge model and baseline forecasts

def solve_linear_system(matrix, vector):
    """Pivoted elimination for a tiny positive-definite ridge system."""
    system_size = len(vector)
    augmented_matrix = [list(row) + [value] for row, value in zip(matrix, vector)]
    for column in range(system_size):
        pivot_row = max(
            range(column, system_size),
            key=lambda row_index: abs(augmented_matrix[row_index][column]),
        )
        augmented_matrix[column], augmented_matrix[pivot_row] = (
            augmented_matrix[pivot_row], augmented_matrix[column],
        )
        pivot_value = augmented_matrix[column][column]
        if abs(pivot_value) < 1e-12:
            raise ValueError("Singular ridge system")
        augmented_matrix[column] = [value / pivot_value for value in augmented_matrix[column]]
        for row_index in range(system_size):
            if row_index != column:
                elimination_factor = augmented_matrix[row_index][column]
                augmented_matrix[row_index] = [
                    row_value - elimination_factor * pivot_row_value
                    for row_value, pivot_row_value in zip(
                        augmented_matrix[row_index], augmented_matrix[column]
                    )
                ]
    return [row[-1] for row in augmented_matrix]


def ridge_forecast(prior, target):
    """Fit feature scaling and ridge coefficients using earlier games only."""
    training_rows = []
    for game in prior:
        feature_values = build_pregame_features(pregame_fields(game), prior_games(prior, game))
        if feature_values is not None:
            training_rows.append((feature_values, game["opponent_points"], game["game_id"]))
    if len(training_rows) < 3:
        return statistics.mean(row["opponent_points"] for row in prior), {
            "status": "expanding_mean_fallback_insufficient_feature_rows", "n_fit_rows": len(training_rows),
        }
    feature_means = [
        statistics.mean(feature_values[index] for feature_values, _, _ in training_rows)
        for index in range(len(FEATURES))
    ]
    feature_scales = [
        statistics.pstdev(feature_values[index] for feature_values, _, _ in training_rows) or 1.0
        for index in range(len(FEATURES))
    ]
    standardized_training_features = [
        [(value - mean) / scale for value, mean, scale in zip(feature_values, feature_means, feature_scales)]
        for feature_values, _, _ in training_rows
    ]
    mean_training_points = statistics.mean(points_against for _, points_against, _ in training_rows)
    feature_count = len(FEATURES)
    ridge_system = [
        [
            sum(feature_values[index] * feature_values[other_index]
                for feature_values in standardized_training_features)
            + (RIDGE_PENALTY if index == other_index else 0)
            for other_index in range(feature_count)
        ]
        for index in range(feature_count)
    ]
    centered_feature_products = [
        sum(feature_values[index] * (training_row[1] - mean_training_points)
            for feature_values, training_row in zip(standardized_training_features, training_rows))
        for index in range(feature_count)
    ]
    coefficients = solve_linear_system(ridge_system, centered_feature_products)
    standardized_target_features = [
        (value - mean) / scale
        for value, mean, scale in zip(build_pregame_features(target, prior), feature_means, feature_scales)
    ]
    prediction = max(
        0.0,
        mean_training_points + sum(feature_value * coefficient
                                   for feature_value, coefficient in zip(standardized_target_features, coefficients)),
    )
    return prediction, {
        "status": "fit", "n_fit_rows": len(training_rows), "fit_game_ids": [row[2] for row in training_rows],
        "feature_names": list(FEATURES), "feature_means": feature_means, "feature_scales": feature_scales,
        "intercept": mean_training_points, "standardized_coefficients": coefficients, "ridge_penalty": RIDGE_PENALTY,
    }


def forecast(prior, target):
    """Accept only prior results and a deliberately restricted pregame record."""
    if set(target) != set(PREGAME_FIELDS):
        raise ValueError("Forecast target must contain only the declared pregame fields")
    if len(prior) < MIN_PRIOR_GAMES or any(row["game_date"] >= target["game_date"] for row in prior):
        raise ValueError("Insufficient or non-prior training games")
    ridge_prediction, ridge_fit = ridge_forecast(prior, target)
    return {
        "expanding_mean": statistics.mean(row["opponent_points"] for row in prior),
        "last_three_mean": statistics.mean(row["opponent_points"] for row in prior[-3:]),
        "pregame_ridge": ridge_prediction,
    }, ridge_fit


# Rolling evaluation and paired baseline comparison

def evaluate(games):
    """Freeze each forecast before attaching that game's held-out outcome."""
    predictions, folds, warmup_exclusions = [], [], []
    chronological_games = sorted(games, key=lambda row: (row["game_date"], row["tipoff_utc"], row["game_id"]))
    for game in chronological_games:
        prior = prior_games(chronological_games, game)
        if len(prior) < MIN_PRIOR_GAMES:
            warmup_exclusions.append({"game_id": game["game_id"], "game_date": game["game_date"], "reason": "warmup_fewer_than_five_prior_games"})
            continue
        target = pregame_fields(game)
        estimates, ridge_fit = forecast(prior, target)
        # Hash only the values actually available to the estimator; no full-file
        # digest or test-game source hash is included in a frozen forecast record.
        training_records = [dict(pregame_fields(row), uconn_points=row["uconn_points"], opponent_points=row["opponent_points"]) for row in prior]
        training_values_hash = canonical_hash(training_records)
        fold = {
            "game_id": game["game_id"], "game_date": game["game_date"],
            "forecast_cutoff": "before_game_date", "train_through": prior[-1]["game_date"],
            "n_prior_games": len(prior), "training_game_ids": [row["game_id"] for row in prior],
            "training_values_sha256": training_values_hash, "pregame_fields": target,
            "predictions": estimates, "ridge_fit": ridge_fit,
        }
        fold["forecast_sha256"] = canonical_hash(fold)
        folds.append(fold)
        # Held-out outcomes enter only after all forecasts are frozen above.
        actual_points = game["opponent_points"]
        for model, predicted_points in estimates.items():
            predictions.append({
                "game_id": game["game_id"], "game_date": game["game_date"], "opponent": game["opponent"],
                "model": model, "train_through": prior[-1]["game_date"], "n_prior_games": len(prior),
                "predicted_opponent_points": predicted_points, "actual_opponent_points": actual_points,
                "error_points": predicted_points - actual_points, "absolute_error_points": abs(predicted_points - actual_points),
                "squared_error_points": (predicted_points - actual_points) ** 2,
                "training_values_sha256": training_values_hash, "forecast_sha256": fold["forecast_sha256"],
            })
    return predictions, folds, warmup_exclusions


def metrics_for(predictions):
    result = []
    for model in MODELS:
        rows = [row for row in predictions if row["model"] == model]
        if not rows:
            continue
        result.append({
            "model": model, "test_games": len(rows),
            "mae_points": statistics.mean(row["absolute_error_points"] for row in rows),
            "rmse_points": math.sqrt(statistics.mean(row["squared_error_points"] for row in rows)),
            "mean_error_points": statistics.mean(row["error_points"] for row in rows),
        })
    return result


def percentile(values, quantile):
    ordered = sorted(values)
    position = (len(ordered) - 1) * quantile
    lower_index = math.floor(position)
    upper_index = math.ceil(position)
    return ordered[lower_index] + (ordered[upper_index] - ordered[lower_index]) * (position - lower_index)


def paired_comparisons(predictions):
    """Compare absolute errors on paired games with a fixed exploratory bootstrap."""
    predictions_by_model = {model: {row["game_id"]: row for row in predictions if row["model"] == model} for model in MODELS}
    result = []
    for baseline in MODELS[:2]:
        game_ids = sorted(predictions_by_model[baseline])
        if not game_ids:
            continue
        differences = [
            predictions_by_model["pregame_ridge"][game_id]["absolute_error_points"]
            - predictions_by_model[baseline][game_id]["absolute_error_points"]
            for game_id in game_ids
        ]
        random_generator = random.Random(SEED)
        bootstrap_means = [statistics.mean(random_generator.choices(differences, k=len(differences))) for _ in range(2000)]
        result.append({
            "model": "pregame_ridge", "baseline": baseline, "paired_test_games": len(game_ids),
            "mae_difference_points_model_minus_baseline": statistics.mean(differences),
            "bootstrap_p025_points": percentile(bootstrap_means, 0.025), "bootstrap_p975_points": percentile(bootstrap_means, 0.975),
            "games_model_lower_absolute_error": sum(difference < 0 for difference in differences),
            "interval_method": "paired_game_bootstrap_2000_seed_20260916_exploratory",
        })
    return result


# Evaluation artifacts and command-line entry point

def write_csv(path, rows, fields):
    with path.open("w", newline="", encoding="utf-8") as stream:
        writer = csv.DictWriter(stream, fieldnames=fields)
        writer.writeheader()
        writer.writerows(rows)


def run(input_path, output_dir):
    games, reconciliation_exclusions = load_games(input_path)
    predictions, folds, warmup_exclusions = evaluate(games)
    if not predictions:
        raise ValueError("No eligible test games after five-prior-game warmup")
    metrics = metrics_for(predictions)
    comparisons = paired_comparisons(predictions)
    output_dir = Path(output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)
    write_csv(output_dir / "pregame_predictions.csv", predictions, list(predictions[0]))
    write_csv(output_dir / "pregame_metrics.csv", metrics, list(metrics[0]))
    write_csv(output_dir / "pregame_baseline_comparisons.csv", comparisons, list(comparisons[0]))
    write_csv(output_dir / "pregame_exclusions.csv", reconciliation_exclusions + warmup_exclusions, ["game_id", "game_date", "reason"])
    (output_dir / "pregame_folds.json").write_text(json.dumps(folds, indent=2) + "\n")
    manifest = {
        "evaluation_version": "reconciled_game_points_v1", "status": "retrospective_rolling_evaluation",
        "input_path": str(input_path), "input_sha256": hashlib.sha256(Path(input_path).read_bytes()).hexdigest(),
        "code_sha256": hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),
        "eligible_games": len(games), "reconciliation_exclusions": len(reconciliation_exclusions),
        "warmup_games": len(warmup_exclusions), "test_games": len(folds), "min_prior_games": MIN_PRIOR_GAMES,
        "test_date_start": folds[0]["game_date"], "test_date_end": folds[-1]["game_date"],
        "target": "opponent_final_points_per_game", "models": list(MODELS), "ridge_penalty": RIDGE_PENALTY,
        "tuning": "none; fixed model, features, penalty, warmup and baselines",
        "lineup_validation_status": "not_evaluated_unreconciled_stint_inputs",
        "metrics": metrics, "paired_baseline_comparisons": comparisons,
        "limitations": [
            "Points conceded confounds pace, opponent strength, game length and defense; it is not defensive efficiency.",
            "Source snapshots were retrieved retrospectively; historical feed vintages and correction times are not archived.",
            "Prior finalized scores are treated as knowable before later game dates; same-day results are embargoed.",
            "No as-of opponent ratings, full-season profiles, test-game statistics, or test-selected thresholds are used.",
            "All models use the same test games; games with failed reconciliation are excluded and listed.",
            "Small single-team sample and overlapping training sets limit generalization; paired game bootstrap ignores serial dependence and is exploratory.",
            "No test set was reserved for future model changes; changes after viewing these results require fresh future games.",
            "This is a reproducible retrospective backtest, not a prospectively timestamped forecast or proof of staff utility.",
            "No lineup, player, causal coaching, or calibrated probability claim is supported by this evaluation.",
        ],
    }
    (output_dir / "pregame_manifest.json").write_text(json.dumps(manifest, indent=2) + "\n")
    report = [
        "# Reconciled pregame evaluation", "",
        f"Target: opponent final points. {len(folds)} held-out games, {folds[0]['game_date']} to {folds[-1]['game_date']}; five prior games required.", "",
        "| Model | Games | MAE (points) | RMSE (points) | Mean predicted − actual |",
        "|---|---:|---:|---:|---:|",
    ]
    report += [f"| {row['model']} | {row['test_games']} | {row['mae_points']:.3f} | {row['rmse_points']:.3f} | {row['mean_error_points']:.3f} |" for row in metrics]
    report += ["", "Ridge minus baseline paired MAE differences (negative favors ridge):", ""]
    report += [f"- {row['baseline']}: {row['mae_difference_points_model_minus_baseline']:+.3f} points; exploratory paired-game 95% bootstrap interval [{row['bootstrap_p025_points']:+.3f}, {row['bootstrap_p975_points']:+.3f}]." for row in comparisons]
    report += ["", "The model, features, penalty and baselines were fixed; no parameter selection uses held-out outcomes. Every fold records training IDs, last training date, scales, coefficients and a forecast hash. Held-out outcomes are attached after forecasting.", "", "Limitations:", ""]
    report += [f"- {item}" for item in manifest["limitations"]]
    (output_dir / "pregame_evaluation.md").write_text("\n".join(report) + "\n")
    return manifest


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--input", type=Path, default=ROOT / "_data/02_derived_inputs/reconciled_games.csv")
    parser.add_argument("--output-dir", type=Path, default=ROOT / "_outputs/08_reconciled_evaluation")
    args = parser.parse_args()
    result = run(args.input, args.output_dir)
    print(json.dumps({"test_games": result["test_games"], "metrics": result["metrics"], "output_dir": str(args.output_dir)}, indent=2))


if __name__ == "__main__":
    main()
