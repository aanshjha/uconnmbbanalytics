# Staff Dashboard Guide

[Project home](../README.md) / [Documentation](README.md) / Staff dashboard

The presentation website lives in the separate `staff-dashboard/` checkout. Its analytical snapshot is historical and unvalidated. Current source reconciliation and the staff review pilot are documented in [the release guide](RELIABILITY_RESET.md).

## Local Preview

Preview the existing website from the repository root:

```bash
python3 -m http.server 4173 --bind 127.0.0.1 --directory staff-dashboard/dist
```

Open `http://127.0.0.1:4173` in a browser. This serves the existing local snapshot. It does not reconcile data, fit a model, update the snapshot, start a staff trial, or publish the website.

`run_staff_dashboard.sh` remains suspended because its historical exporter is guarded. The current workflow, `run_coaching_pipeline.sh`, produces source reconciliation, a pregame evaluation, and review packets; it does not export historical model recommendations to the website.

## Dashboard Sections

| Section | Content |
|---|---|
| Season overview | Snapshot coverage and project context |
| Lineup comparison | Historical lineup summaries and player-availability exploration |
| Defensive signals | Historical model summaries and uncertainty |
| Player profiles | Historical player and role summaries |
| Opponent reports | Earlier opponent scouting reports |
| Methods and data quality | Explanations, provenance, and limits |

The section labels organize the presentation. They do not change the release status of its underlying results. Use the current [evaluation guide](PREGAME_EVALUATION.md) for validated information boundaries and the [staff pilot guide](STAFF_PILOT.md) for the narrow review workflow.

## Hosting Status

The existing Sites project has a hosted URL and is currently configured with custom access. The current task changes local organization only; it does not publish changes or change access. Local edits appear on the hosted website only after a separate publication.

The website's own [README](../staff-dashboard/README.md) documents the separate project. The [historical dashboard guide](archive/STAFF_DASHBOARD.md) retains the earlier export and presentation instructions for audit.

## Historical Shiny App

```bash
bash run_dashboard.sh
```

This launches the retained R/Shiny implementation and requires its R dependencies and local historical outputs. It is an archival view of existing outputs, not the current staff pilot.

## Checks

```bash
bash run_checks.sh
```

Use the root check runner after changing shared code or project organization. Preview website presentation changes separately before publishing.
