# UConn Basketball Analytics

Source-checked game data, pregame evaluation, and a short postgame defensive review with source-linked possession examples.

[Documentation](docs/README.md) · [Project structure](docs/PROJECT_STRUCTURE.md) · [Current findings](docs/RELIABILITY_RESET.md) · [Staff pilot](docs/STAFF_PILOT.md) · [Dashboard](docs/STAFF_DASHBOARD.md)

**Historical lineup recommendations and probability-calibration claims are withdrawn.** The earlier pipeline passed schema checks despite duplicate events, incorrect stint totals, and future information in evaluation. Those outputs remain available for audit; they are not current validated findings.

## Quick Start

Python 3.9+ is sufficient for the current workflow:

```bash
bash run_coaching_pipeline.sh
```

This rebuilds canonical data from the cached ESPN source, runs the rolling pregame evaluation, creates the UConn–Florida demonstration review, and summarizes actual human pilot sessions. It does not start a timer or create human results. `run_everything.sh` is an alias for the same workflow. If source snapshots are missing, use `bash run_coaching_pipeline.sh --fetch`. Use `--refresh` only to explicitly replace cached source snapshots.

The local input CSVs and source snapshots are private working files and remain gitignored. A fresh checkout needs the manual game files containing source game IDs and the core game/stint files for the reconciliation audit.

## Current Results

- **32 games:** final scores and all **640 team-stat comparisons** match source events and ESPN box totals.
- **14,589 canonical source events:** exact string IDs, source URLs, retrieval timestamps, and content hashes.
- **1,032 redundant manual rows:** excluded from the canonical data; originals are preserved.
- **Eight covered games have incorrect core-stint point totals.** Two exhibition games have no matching source snapshot. Lineup and possession attribution remains unverified, so lineup recommendations are not released.
- **27 pregame test games:** the expanding-mean baseline has 8.599-point MAE; ridge has 8.679; last-three mean has 11.309. The model does not beat the expanding mean.
- **Staff trial prepared:** a Florida demonstration review, Butler baseline packet, DePaul report packet, and a timing/usefulness recorder. The assigned human trial has not started; staff time savings and usefulness remain unmeasured.

The forecast target is opponent final points, which includes pace, game length and opponent strength. It does not validate lineup effects or defensive efficiency. Historical data were retrieved retrospectively; original feed vintages are unavailable. Intermediate scoreboard inconsistencies are reported separately from reconciled final totals.

## Find Your Next Step

| Artifact | Location |
|---|---|
| All current guides | [Documentation index](docs/README.md) |
| Folder responsibilities and field labels | [Project structure and naming](docs/PROJECT_STRUCTURE.md) |
| Reconciliation and release status | [Data reconciliation](docs/RELIABILITY_RESET.md) |
| Pregame protocol and limitations | [Evaluation guide](docs/PREGAME_EVALUATION.md) |
| Staff trial instructions | [Pilot guide](docs/STAFF_PILOT.md) |
| Canonical game/event CSVs | `_data/02_derived_inputs/reconciled_games.csv`, `reconciled_events.csv` |
| Source checks and core exclusions | `_outputs/00_qc/` |
| Forecasts, metrics and fold provenance | `_outputs/08_reconciled_evaluation/` |
| Demonstration review and evidence | `_outputs/08_staff_pilot/401812793/defensive_review.html` |
| Assigned trial and packet locations | `_outputs/08_staff_pilot/trial_assignment.json` |

The [historical archive](docs/archive/README.md) retains earlier findings, submission materials, model documentation, and website instructions as audit records. Their old validation conclusions are superseded.

## Verification

```bash
bash run_checks.sh
# Require the private inputs and fail if any are missing:
bash run_checks.sh --require-real-data
```

The runner checks shell, Python, R, Stan, and website JavaScript syntax, then runs the Python and R regression suites. When private inputs are present, it also rebuilds the current pipeline in a temporary directory without changing saved outputs or human trial data. `--fixtures-only` skips that rebuild. See [check requirements and interpretation](tests/README.md). Passing checks does not validate lineup attribution, coaching recommendations, or staff usefulness.

## Historical Models and Website

The project retains R/Stan lineup, player, defensive-risk, scouting, and Shiny implementations. Their current model inputs are not certified for recommendations. Legacy R analysis, fitting, and batch entrypoints stop with an explanation before writing new recommendation files. The Shiny dashboard remains a clearly labeled historical view, and the historical staff exporter is suspended.

The separate staff presentation website in `staff-dashboard/` is documented in the [dashboard guide](docs/STAFF_DASHBOARD.md), including the local preview command. Its existing hosted snapshot has not been republished. `run_staff_dashboard.sh` is suspended by the exporter guard; use the review workflow for this pilot. The old Shiny dashboard is an archival view of existing outputs.

## License

[MIT](LICENSE) covers source code. Private data and third-party materials are excluded.
