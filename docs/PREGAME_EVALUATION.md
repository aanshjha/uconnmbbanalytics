# Pregame Evaluation

[Project home](../README.md) / [Documentation](README.md) / Pregame evaluation

## Run the Evaluation

Run source reconciliation first, then:

```bash
python3 _scripts/analysis/evaluate_pregame_defense.py
python3 -m unittest discover -s tests -p 'test_pregame_evaluation.py' -v
```

The evaluator reads `_data/02_derived_inputs/reconciled_games.csv`. It requires unique ESPN game IDs, ISO game dates, site, final scores, passing score and event-stat reconciliation, and source URL/SHA-256 provenance. Failed games are listed in an exclusion file. It does not silently repair or impute scores. The first five eligible games are warmup; all subsequent eligible games are tested once.

## Forecast Target and Baselines

The target is **opponent final points per game**. This is a deliberately limited target supported by score reconciliation. It combines defense with pace, game length, and opponent strength. It is not defensive PPP, lineup quality, or evidence that changing a rotation will improve outcomes. Lineup validation remains unavailable until stint attribution and possession boundaries reconcile to source events.

Three methods forecast exactly the same games:

| Method | Inputs available before the game |
|---|---|
| Expanding mean | Mean points conceded in all earlier eligible games |
| Last-three mean | Mean points conceded in the last three earlier eligible games |
| Pregame ridge | Home/neutral indicators, mean scoring and conceding in the previous three games |

## Information Available Before Each Game

For each ridge training game, lagged features are built only from games preceding that training game. The first game has no lag history and does not enter the regression. Feature means, scales, intercept and coefficients are fitted again within each fold using prior games. The fixed ridge penalty is 10; it was not selected using the evaluation results. There is no calibration, threshold tuning, or model selection. Fewer than three usable regression rows causes an explicit expanding-mean fallback. A date-level embargo excludes same-day results because source data lacks game-completion timestamps. Every forecast receives only a restricted pregame record, and held-out outcomes are attached afterward.

This evaluates historical information availability under a stated assumption: finalized scores from earlier dates were knowable before a later game. The source snapshots were retrieved retrospectively. The repository does not preserve the original feed vintages or correction timestamps, so this is not a claim of prospectively saved forecasts or a perfect historical data archive. No undated opponent ratings or full-season profiles enter the evaluation.

## Evaluation Outputs

Generated outputs in `_outputs/08_reconciled_evaluation/`:

- `pregame_predictions.csv`: forecasts, held-out scores and errors for each game/method.
- `pregame_metrics.csv`: equal-game MAE, RMSE and prediction bias.
- `pregame_baseline_comparisons.csv`: paired differences in absolute error, with exploratory game-bootstrap intervals (2,000 resamples, seed 20260916).
- `pregame_folds.json`: exact training IDs, training cutoff, input-values hash, scaling, coefficients and frozen forecast hash.
- `pregame_exclusions.csv`: failed reconciliation and warmup games.
- `pregame_manifest.json`: source/code hashes, protocol, results and limitations.
- `pregame_evaluation.md`: concise report.

## Interpretation Limits

The uncertainty intervals resample games, not individual events or lineups. They ignore temporal dependence and shared training histories and are exploratory. This is a small single-team retrospective sample. Any model changes informed by these results need new future test games before claiming improvement. There is no reserved untouched test set for further iteration.

## Why Historical Validation Was Retired

The old R entrypoints now fail before loading data or cached fits. Their unreachable implementations remain in place for audit history. Existing generated outputs are historical artifacts and must not be treated as valid pregame evidence.

The rolling lineup backtest loaded full-season defensive leak posteriors, shot profiles, and player creation profiles. It normalized opponent context and scoring inputs over the entire evaluation set. Its parameter grid selected on the same “forward” outcomes later reported as validation. Its defensive PPP prediction also included the actual held-out game defensive PPP. These paths invalidate the pregame claim even though the inner Stan fit used preceding games.

The separate defensive holdout compared the posterior sign probability of a latent lineup residual against whether observed points per estimated possession exceeded the aggregate test-period mean. These are different probabilistic targets, and the threshold used future test outcomes. Together with unreconciled stint inputs, those outputs do not establish calibrated defensive risk.

The replacement reports only the reconciled game-score target. It does not rescue or relabel the historical lineup probabilities. The future-data invariance tests alter held-out and future scores, future metadata, and appended future games; earlier frozen predictions, training transforms and hashes must remain identical. Tests also verify same-day exclusion, rejection of outcome-bearing forecast inputs, train-only scaling, matched baseline coverage, reconciliation gating, duplicate rejection, metric arithmetic and an end-to-end output manifest.
