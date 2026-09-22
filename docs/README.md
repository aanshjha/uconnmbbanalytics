# Documentation

[Project home](../README.md)

The current workflow reconciles source events, evaluates pregame forecasts, and prepares a short defensive review. Start with the guide for the task you need to perform.

## Current Guides

| Guide | What it covers |
|---|---|
| [Project structure and naming](PROJECT_STRUCTURE.md) | Folder responsibilities, supported commands, field names, and display labels |
| [Data reconciliation and release status](RELIABILITY_RESET.md) | Verified totals, source provenance, unresolved issues, and release limits |
| [Pregame evaluation](PREGAME_EVALUATION.md) | Earlier-game-only training, matched baselines, metrics, and limitations |
| [Staff review pilot](STAFF_PILOT.md) | Review packets, the assigned trial, actual timing, and usefulness recording |
| [Staff dashboard](STAFF_DASHBOARD.md) | Local presentation, release status, and the separate website project |

## Where to Find Results

These paths are relative to the repository root. Local data and generated results are gitignored; they are not included in a fresh checkout.

| Result | Location |
|---|---|
| Game and event data | `_data/02_derived_inputs/reconciled_games.csv` and `reconciled_events.csv` |
| Source reconciliation checks | `_outputs/00_qc/` |
| Forecasts, error metrics, and training provenance | `_outputs/08_reconciled_evaluation/` |
| Review packets and actual human trial results | `_outputs/08_staff_pilot/` |

## Historical Records

The [historical archive](archive/README.md) preserves earlier findings, submission materials, website instructions, and model documentation. Its lineup recommendations and validation claims are superseded. Local output folders `01_*` through `07_*` carry an `HISTORICAL_UNVALIDATED.txt` marker. Use the current guides above for commands and interpretation.
