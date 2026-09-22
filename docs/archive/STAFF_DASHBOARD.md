# Historical Staff Dashboard Guide — April 2026 Snapshot

[Documentation](../README.md) / [Historical archive](README.md)

> **Archive only.** This document preserves superseded claims and commands. Use the [current project guide](../../README.md) for supported commands. Code and data paths in the preserved text are relative to the repository root.

> **Publication suspended September 16, 2026.** The historical exporter now stops because its lineup recommendations and validation inputs are not reliable. Use [the reconciled workflow and postgame pilot](../RELIABILITY_RESET.md). The existing hosted snapshot has not been updated; the instructions below document the historical website and are not the current release workflow.

The presentation website is a separate checkout in `staff-dashboard/`. Its source and selected output snapshot were published privately through a hosting service. The existing Shiny app remains available through `run_dashboard.sh`.

## Open locally

```bash
bash run_staff_dashboard.sh
```

Open `http://127.0.0.1:4173`. Use an alternative port by passing it as the first argument. The launcher exports the existing analytical outputs, then starts a local server; it does not fit models or retrieve new games. Python 3.9+ is required. The website also needs the generated outputs listed in `_scripts/dashboard/export_staff_dashboard.py` and core game metadata.

## Update the information

1. Add or correct the project's source data and regenerate any opponent scout outputs that changed.
2. Run `bash run_coaching_pipeline.sh`. After the R pipeline succeeds, it exports the selected dashboard data automatically. Python/export errors are reported with a nonzero exit code.
3. For presentation-only changes or already-generated outputs, run `python3 _scripts/dashboard/export_staff_dashboard.py` without refitting models.
4. Use **Reload snapshot** in the local dashboard to load the new export.
5. Republish the existing hosted website to update its URL content. A browser reload does not fetch ESPN data, run R, or publish local changes.

The manifest runner `run_everything.sh` does not call the Bash coaching wrapper; run the export command after it finishes. A direct invocation of the R pipeline likewise requires a separate export.

The exporter checks required inputs and columns, rejects empty datasets and nonfinite JSON numbers, includes source hashes and file dates, and replaces the previous snapshot atomically after a successful export. It never rewrites analytical CSVs. Preserve the source input/output files when reproducing a snapshot; file modification times alone do not prove a shared model run.

## Meeting flow

1. **Overview:** explain the purpose and the distinction between 126 observed combinations and 14 sample-qualified lineups.
2. **Rotation lab:** compare observed margin and model evidence, open a lineup's uncertainty details, and try a hypothetical player-out scenario.
3. **Defense / Player roles:** explain probability, confidence, and lineup usage concentration.
4. **The project & methods:** use the short introduction and discuss the implemented methods and current limits.

Use **Present** to hide the sidebar. Exit presentation to switch sections. The rotation view includes a print action; browser printing also works on the other views.

## Verified snapshot and limits

The first website snapshot uses outputs dated April 8, 2026 with defensive game coverage through March 14, 2026. It is not live September data.

- Current scores use a defense-only fallback (defense/offense/style weights 1/0/0), not calibrated net-positive probabilities.
- Full prospective validation of the combined score remains unfinished: later backtest scoring uses full-output defensive, shot-diet, and player-creation context.
- Most lineups have limited samples. Posterior effects are adjusted associations, not causal effects or demonstrated improvements in wins.
- Defensive trend ranges average lineup posterior quantiles; they are not joint posterior intervals or actual defensive ratings.
- Some scout totals are inflated by duplicate input events. The website collapses duplicate library entries only; it does not repair the underlying events. Inspect summaries as unverified, not official box scores.
- Scheme matchup code has no completed results in this snapshot.

## The brief explanation

“I built a basketball analytics platform that turns game and lineup data into a dashboard for the staff. I used R to clean and organize the data, and Bayesian models in Stan to estimate player contribution and lineup fit while reducing overreaction to small samples. It brings together defensive signals, player roles, and lineup options when someone is unavailable. The goal is to make staff review faster, with the evidence and its limitations visible.”
