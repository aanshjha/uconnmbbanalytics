# Historical Submission Brief — April 2026

[Documentation](../README.md) / [Historical archive](README.md)

> **Archive only.** This document preserves superseded claims and commands. Use the [current project guide](../../README.md) for supported commands. Code and data paths in the preserved text are relative to the repository root.

> **Historical submission, superseded September 16, 2026.** The claims and counts below describe an older, unreconciled run. They are not current validated findings. See [source reconciliation, corrected evaluation, and staff pilot](../RELIABILITY_RESET.md). Staff utility is not yet measured and lineup recommendations remain unreleased.

## Problem

College staffs need faster, evidence-backed answers to rotation, defensive-risk, and opponent-prep questions. This project turns UConn game, lineup, player, and opponent-event data into coach-facing recommendations that separate stable signals from low-sample noise.

Primary use cases:

- Lineup optimization: identify groups that can survive defensively while still creating efficient offense.
- Defensive leak detection: flag lineups and game stretches most tied to points allowed.
- Player development and role planning: show role concentration, creation profile, and player-out contingency options.
- Opponent prep: convert public play-by-play into matchup, shot-profile, turnover, clutch, and lineup scout tables.

## Data Used

Implemented public-source path:

- ESPN public college basketball play-by-play, roster, and game data through `hoopR`, converted by `_scripts/analysis/generate_manual_game_csvs_from_espn.R`.
- Generated manual-game event CSVs under `_data/03_manual_game_csv`, then summarized by `_scripts/analysis/build_manual_game_opponent_scouts.R`.

Internal/local working-copy inputs:

- `_data/01_core_inputs/uconn_stints_from_pbp.csv`
- `_data/01_core_inputs/uconn_games_meta.csv`
- `_data/01_core_inputs/opponent_controls.csv`
- `_data/01_core_inputs/player_archetypes.csv`

Generated model and dashboard outputs:

- `_outputs/01_lineup_core/uconn_lineup_coach_view.csv`
- `_outputs/02_defense_leaks/uconn_lineup_def_leaks_coach_table.csv`
- `_outputs/03_players/uconn_player_rci_coach_table.csv`
- `_outputs/05_decision_audit/uconn_pred_pr_net_pos_calibration_metrics.csv`
- `_outputs/07_opps/manual_game_scouts/manual_game_scout_manifest.csv`

Note: BartTorvik, KenPom, and Basketball Reference are not required to run the current working product. The implemented public source is ESPN via `hoopR`; BartTorvik can be added as an external team/player context layer if the review specifically requires that source.

## Product

The functioning data product is a Shiny dashboard:

```sh
bash run_dashboard.sh
```

Default local URL:

```text
http://127.0.0.1:3838
```

Dashboard sections:

- `Coach View`: plain-language answers with source paths and evidence-row counts.
- `Game Board`: opponent/game filters, context strip, KPIs, lineup and shot evidence.
- `Player Out`: contingency planning when a selected player is unavailable.
- `Technical Data`: source status, schema checks, question coverage, and output tables.

The full pipeline entry point is:

```sh
bash run_coaching_pipeline.sh
```

The manifest runner is:

```sh
bash run_everything.sh
```

## Build

Core tools:

- R for ingestion, feature engineering, validation, dashboard data loading, and reporting.
- Shiny and DT for the interactive dashboard.
- Stan model files under `_models` for Bayesian lineup and matchup modeling.
- Bash entrypoints for repeatable execution.
- CSV contracts for data handoff between modeling, scouting, and dashboard layers.

Important implementation paths:

- Dashboard app: `_scripts/dashboard/app.R`
- Dashboard loader and schema checks: `_scripts/dashboard/data_loader.R`
- Coaching pipeline: `_scripts/pipeline/run_coaching_pipeline.R`
- Lineup model: `_scripts/models/fit_core_lineup_model.R`
- Defensive leak model: `_scripts/models/fit_lineup_defensive_leak_model.R`
- Rolling decision backtest: `_scripts/pipeline/run_rolling_lineup_decision_backtest.R`
- Calibration check: `_scripts/analysis/evaluate_net_probability_calibration.R`

## Verified Current Output

Current dashboard data load:

- Load errors: `0`
- Load warnings: `0`
- Dashboard questions available: `40 of 40`
- Manual-game event rows: `6,141`
- Game-board entries: `35`

Current model/output facts:

- Lineup coach-view rows: `126`
- Lineups with `sample_flag == ok`: `14`
- Low-sample lineup rows: `112`
- Defensive leak rows: `126`
- Player role rows: `14`
- Scout manifest rows: `35`
- Unique scout game files: `32`
- Unique scout opponents: `19`

Current decision validation:

- Backtest rows used: `415`
- Holdout possessions: `2,073`
- Weighted observed positive rate: `0.565`
- Weighted mean predicted probability: `0.394`
- Weighted Brier score: `0.266`
- Weighted log loss: `0.726`
- Weighted ECE by decile: `0.172`

Current notable examples:

- Highest-volume stable lineup: `Ball Solo|Demary Jr.,Silas|Karaban Alex|Mullins Braylon|Reed Jr.,Tarris`, with `560` possessions, `289.8` minutes, `23` games, and raw net PPP `0.180`.
- Top player by positive net probability: `Demary Jr.,Silas`, with `1,748` possessions, net mean `0.044`, and net positive probability `0.868`.

## Why It Helps a GM or Coaching Staff

This product is useful because it converts raw event and lineup data into decision-ready evidence:

- It prevents overreaction by labeling low-sample lineups and surfacing validation results.
- It helps a staff decide which lineups are safer, which are watch-list risks, and which players stabilize roles.
- It gives a GM or staff a player-out planning view before availability issues force rushed rotation decisions.
- It produces opponent scout outputs with shot profiles, lineup matchups, turnover creators/victims, and clutch-event evidence.
- It keeps source paths visible, so staff can trace each recommendation back to the underlying data.

## Limitations

- `cmdstanr` is not installed in the current local R environment, so the existing generated outputs can be reviewed and served, but full Stan refits may require installing that package.
- Most lineup rows are sample-limited: `112 of 126` are low sample.
- Probability estimates are useful signals, not exact truth; calibration still shows a gap between weighted mean prediction `0.394` and weighted observed positive rate `0.565`.
- Manual opponent scout outputs depend on the quality and completeness of the public play-by-play conversion and lineup reconstruction.
