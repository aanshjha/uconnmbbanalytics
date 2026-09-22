# Regression checks

Run all available checks from the project root:

```bash
bash run_checks.sh
bash run_checks.sh --require-real-data # fail if private inputs are absent
bash run_checks.sh --fixtures-only     # syntax and fixture checks only
```

The launcher stops on failure. It requires Python 3.9+, R with `dplyr`, `readr`, `stringr`, and `rstan`, and Node.js. The first stage parses root shell launchers, all Python and R scripts, the three Stan models, and the dashboard JavaScript. The regression suites use fixtures and temporary files. They do not start a real trial or publish the dashboard.

When local private inputs are complete, the default run copies only the cached ESPN responses, manual game CSVs, and two core game/stint CSVs into a temporary directory. It rebuilds canonical data, forecast evaluation, the Florida review, and the source lineup-evidence audit there. It checks reconciliation flags, all team-stat comparisons, finite forecast errors, exact accounting of included and excluded lineup points, and that no human sessions were invented, then removes the temporary directory. Historical repair reports are deliberately absent from that isolated copy, so its imputed-possession count is unknown. It does not fetch from the network or modify saved project outputs. `--require-real-data` makes missing inputs a failure; the default reports a skip when they are absent.

The private inputs required for this stage are `_data/00_source/espn/`, `_data/03_manual_game_csv/`, `_data/01_core_inputs/uconn_games_meta.csv`, and `_data/01_core_inputs/uconn_stints_from_pbp.csv`. The source cache must contain the JSON and metadata for each game referenced by the manual files. These files are gitignored and are not part of a fresh checkout. The R fixture suite has a separate optional Florida input check when its two manual copies are present.

## Python suite

```bash
python3 -m unittest discover -s tests -p 'test_*.py' -v
```

| File | Behavior covered |
|---|---|
| `test_source_reconciliation.py` | Exact event IDs, duplicate/conflicting events, final-score arithmetic, and event-to-box totals. |
| `test_pregame_evaluation.py` | Prior-date training, future-data invariance, train-only scaling, matched baselines, metrics, and provenance. |
| `test_staff_pilot.py` | Conservative possession boundaries, source evidence, packet integrity, actual timer lifecycle, and human-result validation. |
| `test_lineup_evidence.py` | Substitution ambiguity and reversible same-clock chains, invalid five-player transitions, conservative possession endings, clock-bound point bounds, and historical-fill provenance. |

These tests use Python's standard-library `unittest` runner.

## R input guards

```bash
Rscript --vanilla _scripts/tests/test_source_input_guards.R
```

The R checks cover the retained input helpers: exact source IDs, duplicate copies and conflicts, backup requirements, invalid-denominator quarantine, stored-rate invariants, event counts, unverified lineup states, and the historical entrypoint guard. A real-data Florida check runs only when its local manual input copies are available; the fixture checks still run when those files are absent.

## What passing checks establishes

Syntax and fixture checks establish that inspected code parses and behaves as specified on test cases. The isolated real-data run additionally verifies that cached source totals reconcile in the current workflow and that the evidence audit accounts for every included and excluded point. This does not certify excluded five-player states, complete possession boundaries, forecasting improvement over the baseline, or staff usefulness. Current data findings and release limits are documented in [the reliability report](../docs/RELIABILITY_RESET.md).
