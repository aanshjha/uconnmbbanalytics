# Script directory

Use the launchers in the project root for routine work. Existing script names and paths are stable so commands, imports, and source provenance continue to resolve.

## Current workflow

| Purpose | Script | Output area |
|---|---|---|
| Reconcile scores, event counts, and source identity | `ops/reconcile_source_data.py` | `_data/02_derived_inputs/` and `_outputs/00_qc/` |
| Evaluate pregame forecasts against simple baselines | `analysis/evaluate_pregame_defense.py` | `_outputs/08_reconciled_evaluation/` |
| Build defensive reviews and record actual pilot sessions | `analysis/postgame_defensive_review.py` | `_outputs/08_staff_pilot/` |

Run the full workflow from the project root:

```bash
bash run_coaching_pipeline.sh
bash run_coaching_pipeline.sh --help
```

`run_everything.sh` remains a compatibility alias for the same workflow. Source snapshots are read locally by default; `--fetch` retrieves missing snapshots and `--refresh` replaces cached snapshots. Building a review does not start or complete a staff trial.

## Directory map

| Directory | Contents and status |
|---|---|
| `analysis/` | Current Python evaluation/review commands alongside retained R research analyses. |
| `ops/` | Current Python reconciliation plus historical R repair, cleanup, and batch-run tools. |
| `tests/` | R source-input regression guards; the Python suite is in root `tests/`. |
| `utils/` | Shared R path, loading, normalization, and input-repair helpers. |
| `pipeline/` | Historical lineup and coaching workflows; direct entrypoints are guarded. |
| `models/` | Retained R model-fitting scripts; direct fitting is guarded until source attribution is repaired. |
| `dashboard/` | Historical Shiny application and the suspended staff dashboard exporter. |

## Historical and research entrypoints

The R analyses, model fits, and stored outputs support audit and research. They do not establish current lineup recommendations. The shared bootstrap stops direct historical analysis, model fitting, pipeline, and batch commands before they regenerate apparent recommendations. Current source generation, maintenance commands, and the read-only historical Shiny view keep their paths.

`ops/run_runnable_entrypoints.R` and its CSV manifest are a retired historical batch workflow, not a test suite. `ops/cleanup_project.R` and `ops/repair_core_inputs.R` are maintenance tools; inspect their options before use.

The historical launchers are labeled in their help:

```bash
bash run_dashboard.sh --help
bash run_staff_dashboard.sh --help
```

See [reliability status](../docs/RELIABILITY_RESET.md), [evaluation protocol](../docs/PREGAME_EVALUATION.md), [staff pilot](../docs/STAFF_PILOT.md), and [dashboard status](../docs/STAFF_DASHBOARD.md).

## Verification

```bash
bash run_checks.sh
```

See the [test directory guide](../tests/README.md) for coverage and separate Python/R commands.
