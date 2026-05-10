# UConn Basketball README

Last updated: 2026-04-08

This repository is the working UConn basketball analytics workflow. It includes source code, Stan models, local data folders, and generated coaching outputs.

Run `bash run_coaching_pipeline.sh` for the standard lineup/defense/audit entry point once local data dependencies are available. For manifest-based "run everything", use `bash run_everything.sh`. The separate scheme matchup work uses `_models/uconn_scheme_matchup_ppp.stan`.

## Project Snapshot

This project:

- fits Bayesian lineup models for UConn lineup evaluation
- attributes defensive breakdown risk at the lineup and game level
- audits decision rules with calibration checks, holdouts, and rolling backtests
- turns model output into coach-facing scouting briefs and visuals

Tools used:

- `R` for pipeline orchestration, feature building, and reporting
- `Stan` for Bayesian lineup and matchup models
- `bash` for the command-line entry point
- structured `CSV` tables for ingest, tagging, and delivery contracts

Why it matters:

- the workflow is built to change coaching decisions, not just produce tables
- it separates stable groups from collapse-risk lineups
- it checks whether confidence is earned before a recommendation reaches staff
- it packages findings in formats coaches can use quickly

## What To Review First

1. `_outputs/05_decision_audit/uconn_pred_pr_net_pos_calibration_curve.png` for model reliability.
2. `_outputs/02_defense_leaks/uconn_game_level_def_leak_trend.png` for defensive diagnosis.
3. `_outputs/07_opps/manual_game_scouts/conference/marquette/2026-03-07__away/summary/report_summary.md` for a coach-facing output example.
4. `_data/04_templates/TEMPLATE FOR CSV GAMES.csv` for the manual game CSV contract template.

![Probability Calibration Curve](_outputs/05_decision_audit/uconn_pred_pr_net_pos_calibration_curve.png)

![Game-Level Defensive Leak Trend](_outputs/02_defense_leaks/uconn_game_level_def_leak_trend.png)

## Validation

Validation is built into the workflow, not bolted on afterward.

- Probability outputs are calibration-checked before decision tables are trusted.
- Lineup recommendations are stress-tested and rolling-backtested.
- Defensive leak signals are checked on holdout slices before they become coach-facing flags.

Public code paths for that work:

- `_scripts/analysis/evaluate_net_probability_calibration.R`
- `_scripts/pipeline/run_rolling_lineup_decision_backtest.R`
- `_scripts/analysis/validate_defensive_leak_signal_holdout.R`

## Data Availability

This working copy currently includes local/private data and generated outputs under `_data`, `_models`, and `_outputs`.

These folders are gitignored by default, so repository history stays source-code focused.

## License Note

The MIT license in this repo is intended for source code. Local/private data and excluded third-party materials are not part of that license.

## Start Here

Start with `Project Snapshot`, `Validation`, and `Quick Start`.

The rest of this README documents the full working system. Some file counts and paths below refer to the private working copy and are kept here to explain the architecture.

If you're using the private working copy, read these sections next:

1. `Project Overview and Operating Philosophy` for scope and intent.
2. `Active Script Runbook` for the run order and output checks.
3. `CSV Landscape and Contracts` and `Data Dictionary` for file contracts and field meanings.
4. `System Logic and Thought Process` for the modeling rationale.
5. `File Atlas` if you need the full script and output inventory.
6. `Tagging Vocabulary` and `Notes Knowledge Map` as references.

## Document Sections

1. [CSV Landscape and Contracts](#csv-landscape-and-contracts)
2. [Data Dictionary](#data-dictionary)
3. [Notes Knowledge Map](#notes-knowledge-map)
4. [Project Overview and Operating Philosophy](#project-overview-and-operating-philosophy)
5. [Tagging Vocabulary (Scheme Matchup Project)](#tagging-vocabulary-scheme-matchup-project)
6. [_data Folder Standard](#_data-folder-standard)
7. [File Atlas](#file-atlas)
8. [System Logic and Thought Process](#system-logic-and-thought-process)
9. [Active Script Runbook](#active-script-runbook)
10. [Scripts Standard](#scripts-standard)

## Quick Start (Working Copy)

1. Run `bash run_coaching_pipeline.sh` for the full lineup/defense/audit pipeline once local data inputs are in place.
2. Use `bash run_everything.sh` when you want the manifest-based required + optional run.
3. Open `_outputs/01_lineup_core/uconn_lineup_coach_view.csv` (main lineup action table).
4. Open `_outputs/05_decision_audit/uconn_pred_pr_net_pos_calibration_metrics.csv` (calibration check for this run).
5. Open `_outputs/02_defense_leaks/uconn_lineup_def_leaks_coach_table.csv` (defense risk watchlist by lineup).
6. If QC fails, fix the source issue and rerun (do not hand-edit output CSVs).

## Key Terms

- `PPP`: points per possession (efficiency per possession, not per game).
- `posterior`: the model's uncertainty distribution after fitting (not one fixed number).
- `calibration`: a reliability check/correction so predicted probabilities better match realized outcomes.
- `stabilizers`: small-sample safety rules that shrink extreme lineup estimates toward team baseline.
- `backtest`: train on past games, then test on later games to mimic real decision timing.
- `holdout`: data intentionally left out of training so evaluation is honest.
- `eligibility`: a gate that blocks weak-sample or unstable lineups from decision labels.

---


## CSV Landscape and Contracts

Last rebuilt from source files: 2026-03-05 (historical snapshot section)

This section maps the CSV files the pipeline reads and writes.

### Ground Rules

1. Source-of-truth inputs live under `_data/01_core_inputs`.
2. Derived inputs are rebuilt, not hand-edited.
3. `_outputs` files are generated artifacts.
4. Archive snapshots are currently disabled; `_outputs` should be treated as the active output state.
5. If a script writes a CSV, that script defines the schema contract.

### CSV Inventory Scale

Current counts from file scan:

- total CSV files: `109`
- `_data`: `41`
- `_outputs` (active): `22`
- `Notes`: `36`


### Core Input Contract

Required before pipeline run:

1. `_data/01_core_inputs/uconn_stints_from_pbp.csv`
2. `_data/01_core_inputs/uconn_games_meta.csv`
3. `_data/01_core_inputs/opponent_controls.csv`

Required derived file (rebuilt in pipeline):

4. `_data/02_derived_inputs/uconn_lineup_stabilizers.csv` (stabilizers file; see defined term above)

### Active Output Families

#### 1) Lineup Core

Folder: `_outputs/01_lineup_core`

Primary files:

1. `uconn_lineup_usage.csv`
2. `uconn_lineup_synergy_posterior.csv`
3. `uconn_lineup_coach_view.csv`
4. `uconn_lineup_core_model_diagnostics.csv`

#### 2) Defense Leaks

Folder: `_outputs/02_defense_leaks`

Primary files:

1. `uconn_lineup_def_leaks_posterior.csv`
2. `uconn_lineup_def_leaks_coach_table.csv`
3. `uconn_def_leak_lineups_by_game.csv`
4. `uconn_def_leak_primary_culprit_by_game.csv`
5. `uconn_def_leak_repeat_offenders.csv`
6. `uconn_game_level_def_leak_trend.csv`
7. holdout validation CSV set (holdout = unseen test slice; see defined term above)

#### 3) Player Layer

Folder: `_outputs/03_players`

Primary files:

1. `uconn_player_net_posterior.csv`
2. `uconn_player_rci.csv`
3. `uconn_player_rci_coach_table.csv`
4. `uconn_player_rci_three_windows.csv`

#### 4) Decision Audit

Folder: `_outputs/05_decision_audit`

Primary files:

1. `uconn_lineup_decision_rolling_backtest_rows.csv`
2. `uconn_lineup_decision_rolling_backtest_by_bucket.csv`
3. `uconn_lineup_decision_rolling_backtest_by_game_bucket.csv`
4. `uconn_lineup_decision_rule_v2_thresholds.csv`
5. `uconn_pred_pr_net_pos_calibration_metrics.csv`
6. `uconn_pred_pr_net_pos_calibration_model.csv`
7. `uconn_pred_pr_net_pos_calibration_deciles.csv`
8. `uconn_decision_eligibility_by_stint.csv`
9. `uconn_availability_stress_test_report.csv`

### Manual Game Tables

Folder: `_data/03_manual_game_csv`

Current split:

1. conference game CSVs in `_conf`
2. non-conference game CSVs in `_nc`
3. ESPN generation summaries in `_games` and `_nc`

These are ingestion-side tables, not model outputs.

### Scheme Project CSVs (Separate Project)

Folder: `_data/05_projects/scheme_matchup_project`

Files include:

1. `possessions.csv`
2. `scheme_tags.csv`
3. `events.csv`
4. `player_dev_tags.csv`
5. `breakdown_tags.csv`
6. `clip_playlist.csv`

These feed `_scripts/models/fit_scheme_matchup_model.R`, which is outside the default coaching pipeline.

### Historical Archive Behavior

`_outputs/_archive` has been removed in this cleanup pass.

If you want archived snapshots again, rerun with archive mode enabled in output organizer scripts.


---

## Data Dictionary

Last rebuilt from source files: 2026-03-05 (historical snapshot section)

This dictionary covers the core fields that drive model logic and coach-facing outputs.


### Global Conventions

1. Canonical lineup key:
`paste(sort(trimws(strsplit(uconn_lineup, "|"))), collapse = "|")`

2. Net points:
`net_pts = points_for - points_against`

3. Net PPP (see defined term above):
`net_ppp = net_pts / poss_est` when `poss_est > 0`

4. Defensive PPP target in leak model (same PPP definition):
`y_def = points_against / poss_est`

5. Tiny possession clamp:
if `0 < poss_est < 1`, clamp to `1` for modeling stability.

6. Weighted mean convention:
`sum(value * weight) / sum(weight)`

7. Pace conversion convention:
`per_40 = per_possession * 40 * PACE_UCONN`

8. Posterior probability fields (see defined term above):
`pr_*` means posterior share of draws satisfying the condition.

9. Posterior intervals:
`p05`, `p50`, `p95` are 5th, 50th, 95th percentiles (low/middle/high likely range from posterior draws).

### Core Inputs

#### `_data/01_core_inputs/uconn_stints_from_pbp.csv`

Grain: one stint segment.

Key fields:

1. `game_file`: join key to game metadata.
2. `game_date`: date string used for consistency checks.
3. `game_id`: sequential game index used in leakage-safe logic.
4. `period`, `stint_index`: within-game sequence fields.
5. `start_time`, `end_time`: clock values used for duration and elapsed-game reconstruction.
6. `uconn_lineup`: raw lineup string.
7. `lineup_size`: eligibility gate (see defined term above; must be `5` for lineup modeling rows).
8. `points_for`, `points_against`: stint scoring outcome.
9. `poss_est`: possession weight.

#### `_data/01_core_inputs/uconn_games_meta.csv`

Grain: one game.

Key fields:

1. `game_file`: join key to stint table.
2. `game_date`: date used with opponent controls.
3. `opponent`: join key to controls table.
4. `uconn_is_home`: site indicator.
5. `matchup_header`, `site_type`: metadata and quality checks.

#### `_data/01_core_inputs/opponent_controls.csv`

Grain: one `(game_date, opponent)` record.

Key fields:

1. `opp_adjO`
2. `opp_adjD`

Critical rule: duplicate `(game_date, opponent)` keys fail pre-QC.

### Derived Input

#### `_data/02_derived_inputs/uconn_lineup_stabilizers.csv`

Built by `_scripts/analysis/build_lineup_stability_baselines.R`.

Key fields:

1. `lineup_key`
2. `possessions`
3. `games_played`
4. `games_ok`
5. `sample_tier`
6. `team_avg_net_ppp`
7. `dev_from_team_avg`
8. `trust_baseline`
9. `collapse_risk_flag`

### Core Lineup Outputs

#### `uconn_lineup_usage.csv`

1. `uconn_lineup_canon`
2. `possessions`, `minutes`, `segments`, `games`
3. `raw_net_ppp`
4. `lineup_id`, `lineup_pretty`

#### `uconn_lineup_synergy_posterior.csv`

1. `synergy_mean`, `synergy_p05`, `synergy_p50`, `synergy_p95` (posterior summary columns; see defined term above)
2. `pr_synergy_pos`

#### `uconn_lineup_coach_view.csv`

This is the canonical coach decision layer.

Key decision fields:

1. `decision_pred_net_ppp_mean`
2. `decision_pred_pr_net_pos_raw`
3. `decision_pred_pr_net_pos` (calibrated/shrunk = reliability-adjusted probability; see defined terms above)
4. `decision_prob`
5. `Decision`

Decision labels used in post-QC:

1. `PLAY MORE`
2. `LEAN IN`
3. `NEUTRAL`
4. `LIMIT / WATCH`
5. `TOO SMALL`

### Defense Outputs

#### `uconn_lineup_def_leaks_posterior.csv`

1. `u_def_mean`
2. `u_def_p05`, `u_def_p50`, `u_def_p95`
3. `pr_leak`

Interpretation convention:
`u_def > 0` means worse-than-baseline defense (leak direction).

#### `uconn_lineup_def_leaks_coach_table.csv`

Adds context and action language:

1. `expected_pts_allowed_per_40`
2. `confidence`
3. `leak_flag`
4. `dont_overreact`
5. `lineup_type`

#### Game attribution outputs

1. `uconn_def_leak_lineups_by_game.csv`
2. `uconn_def_leak_primary_culprit_by_game.csv`
3. `uconn_def_leak_repeat_offenders.csv`

These files connect lineup leak signal to game-level defensive damage.

### Player Outputs

#### `uconn_player_net_posterior.csv`

1. `player`
2. `net_mean`, `net_p05`, `net_p50`, `net_p95`
3. `net_pr_pos`

#### Role stability files

1. `uconn_player_rci.csv`: usage concentration diagnostics.
2. `uconn_player_rci_coach_table.csv`: role + impact recommendation language.
3. `uconn_player_rci_three_windows.csv`: phase-by-phase role stability and current-phase impact check.

### Decision Audit Outputs

#### Rolling backtest files (see defined term above)

1. `uconn_lineup_decision_rolling_backtest_rows.csv`
2. `uconn_lineup_decision_rolling_backtest_by_bucket.csv`
3. `uconn_lineup_decision_rolling_backtest_by_game_bucket.csv`
4. `uconn_lineup_decision_rolling_backtest_fit_diagnostics.csv`

#### Calibration files (see defined term above)

1. `uconn_pred_pr_net_pos_calibration_model.csv`
2. `uconn_pred_pr_net_pos_calibration_deciles.csv`
3. `uconn_pred_pr_net_pos_calibration_metrics.csv`

#### Rule/eligibility/stress files

1. `uconn_lineup_decision_rule_v2_thresholds.csv`
2. `uconn_decision_eligibility_by_stint.csv`
3. `uconn_availability_stress_test_report.csv`

### Scheme Project Outputs (Separate)

Written by `_scripts/models/fit_scheme_matchup_model.R` into `_outputs/06_scheme_matchups`.

Key files:

1. `uconn_scheme_matchup_cell_summary.csv`
2. `uconn_scheme_matchup_opponent_prep_table.csv`
3. `uconn_scheme_matchup_do_not_run.csv`
4. `uconn_scheme_matchup_lineup_recommendations.csv`
5. `uconn_scheme_matchup_calibration.csv`
6. `uconn_scheme_matchup_oos_lift.csv`
7. `uconn_scheme_matchup_model_meta.csv`


---


## Notes Knowledge Map

Last updated: 2026-04-08

I use this in order when I make modeling decisions in the UConn project.

### 1) Notes Inventory Snapshot

- Total files: 63
- R scripts: 12
- Stan files: 2
- CSV files: 36
- PDF files: 12

### 2) How I Use Notes in Practice

My sequence is always:

1. Pick the modeling question in the project.
2. Map that question to one notes module.
3. Read that module's PDF + `*-FINAL.R` script.
4. Apply same logic in project scripts.
5. Validate with holdout/backtest outputs (see defined terms above) before claiming anything.

### 3) Module-by-Module Map

#### `00 - Course Introduction`

Files:

- `Intro-Sports-Analytics.pdf`

Use in this repo:

- framing only
- not used for direct code logic

#### `01 - Normal models for game outcomes`

Files:

- `Abilities-NormalModels.pdf`
- `ability_normal_model-FINAL.R`

What the code is doing:

- constrained linear ability estimates
- explicit home advantage term
- covariance-aware pairwise comparisons

How I apply it here:

- base intuition for lineup net-effect modeling
- compare lineup/player effects with uncertainty, not only point estimates

#### `02 - Count models for low-score games`

Files:

- `Abilities-PoissonModels.pdf`
- `ability_poisson_model-FINAL.R`

What the code is doing:

- Poisson scoring model
- separate attack/defense effects
- optional Dixon-Coles style handling for score dynamics

How I apply it here:

- defense-leak thinking as rate pressure, not just raw score noise

#### `03 - Paired comparison models`

Files:

- `Abilities-Paired-Comparisons.pdf`
- `ability_binary_model-FINAL.R`

What the code is doing:

- Bradley-Terry / Thurstone-Mosteller binary outcome models
- home advantage variants
- probability-based strength comparisons

How I apply it here:

- lineup decision probabilities
- threshold-based action labels

#### `04 - Regularization and Bayes`

Files:

- `Regularization-and-Bayes.pdf`
- `regularization-FINAL.R`
- `regularization-plus-stan-FINAL.R`
- `davidson-stan-FINAL.R`
- `johnny-timmy.R`
- `bt_nohfa_basic_model.stan`
- `davidson_model_hfa.stan`

What the code is doing:

- ridge and pseudo-game regularization
- Bayesian paired-comparison modeling in Stan
- posterior distributions and interval-based interpretation

How I apply it here:

- shrinkage for thin-sample lineups
- uncertainty-aware coach labels
- discipline around `TOO SMALL` and stability/eligibility checks

#### `05 - Multicompetitor Models`

Files:

- `Multicompetitor-and-ranking-models.pdf`
- `ranking_toy_example.R`
- `ranking_golf_FINAL.R`

What the code is doing:

- many-player ranking with normal and Plackett-Luce styles
- block effects and unequal exposure handling

How I apply it here:

- lineup-context ranking comparisons where exposure is uneven

#### `06 - Dynamic models`

Files:

- `Dynamic-models.pdf`
- `dynamic_model-FINAL.R`

What the code is doing:

- exponential down-weighting for older games
- rolling validation using season-aware splits

How I apply it here:

- rolling backtest design
- train-before-test discipline for holdout workflows

#### `07 - Rating systems`

Files:

- `Abilities-rating-systems.pdf`
- `rating_system_nfl-FINAL.R`

What the code is doing:

- Elo and Glicko with parameter tuning on validation data
- iterative update logic across seasons

How I apply it here:

- practical communication style: directional strength updates, not false certainty

#### `08 - Simulating tournaments`

Files:

- `SimulatingTournaments.pdf`
- `simulation-FINAL.R`

What the code is doing:

- propagate parameter uncertainty through simulation
- convert model uncertainty into scenario probabilities

How I apply it here:

- lineup availability stress tests
- scenario planning outputs in `_outputs/05_decision_audit/`

### 4) Direct Mapping to UConn Pipeline

- Core lineup model + decision framing:
  - driven by `01`, `03`, `04`
- Rolling backtest + calibration:
  - driven by `03`, `04`, `06`
- Defense leak model + holdout checks:
  - driven by `02`, `04`, `06`
- Stress test logic:
  - driven by `04`, `08`

### 5) Known Friction Points (So I Don't Forget)

- Folder name typo is original source and remains: `Regularizaton`.
- Some note scripts contain old sections; I only trust the current `*-FINAL.R` logic blocks.
- Some legacy install comments are historical and not needed for this project pipeline.

### 6) Bottom Line

These Notes are not decoration.

They are the model logic source.

Project scripts are where that logic gets operationalized for UConn decisions.


---


## Project Overview and Operating Philosophy


This document is the practical handoff for how this project actually works right now.
It was rebuilt from the code and data files (`.csv`, `.R`, `.stan`) and does not rely on older markdown text.

### Why This Exists

This project was built to solve one recurring problem: lineup decisions get distorted when memory and recent emotion take over.

The goal is not to replace coaching judgment.
The goal is to force discipline around it:

1. Use a full-season evidence trail, not one game.
2. Keep uncertainty visible instead of pretending weak samples are stable.
3. Separate predictive signal from descriptive noise.
4. Keep every recommendation auditable back to inputs and model assumptions.

### Design Philosophy

This system is old-school in values and modern in method.

Old-school side:

1. Rotation decisions are still coaching decisions.
2. Sample size matters.
3. Role clarity matters.
4. Defensive breakdowns are treated as recurring habits until disproven.

Modern side:

1. Bayesian partial pooling prevents overreacting to tiny samples.
2. Rolling holdout backtests force forward-looking evaluation.
3. Probability calibration is treated as mandatory, not optional.
4. QC gates can fail runs when evidence quality is not acceptable.

### What Is In Scope

The active coaching pipeline is the set of scripts run by:

```bash
bash run_coaching_pipeline.sh
```

That shell entrypoint calls:

- `_scripts/pipeline/run_coaching_pipeline.R`

The production pipeline includes:

1. core lineup model
2. rolling decision backtest
3. probability calibration diagnostics
4. player role stability tracking
5. defensive leak model
6. defensive holdout validation
7. defensive game attribution and trend
8. eligibility and stress-test guardrails
9. output organization and post-run QC

The scheme matchup model exists, but is intentionally separate from the default coaching run.

### Current Source Coverage Snapshot

Verified from the current tree on 2026-04-08:

- `_scripts/*.R` files: `31`
- `_models/*.stan` files: `3`
- `Notes/*.stan` files: `2`
- runnable entrypoints in `_scripts/ops/runnable_entrypoints_manifest.csv`: `4`

Breakdown:

- active production scripts: `_scripts/`
- active production Stan models: `_models/`
- data and manual game tables: `_data/`
- current outputs: `_outputs/`
- methods/reference coursework: `Notes/`

### Live Pipeline Order (As Coded)

`_scripts/pipeline/run_coaching_pipeline.R` currently runs this exact order:

1. `_scripts/models/fit_lineup_defensive_leak_model.R`
2. `_scripts/analysis/build_uconn_lineup_shot_diet.R`
3. `_scripts/analysis/build_uconn_player_creation_profile.R`
4. `_scripts/pipeline/run_rolling_lineup_decision_backtest.R`
5. `_scripts/analysis/evaluate_net_probability_calibration.R`
6. `_scripts/models/fit_core_lineup_model.R`
7. `_scripts/analysis/build_player_role_concentration_table.R`
8. `_scripts/analysis/analyze_player_role_stability_by_phase.R`
9. `_scripts/analysis/build_defensive_leak_coach_table.R`
10. `_scripts/analysis/validate_defensive_leak_signal_holdout.R`
11. `_scripts/analysis/build_lineup_stability_baselines.R`
12. `_scripts/analysis/attribute_defensive_leaks_by_game.R`
13. `_scripts/analysis/build_game_level_defensive_trend.R`
14. `_scripts/pipeline/run_lineup_availability_stress_test.R`
15. `_scripts/analysis/audit_lineup_decision_eligibility.R`

Then it organizes top-level outputs into bucket folders and runs post-flight QC.

### Why This Run Order

The order reflects how decisions are meant to be trusted:

1. Build defense leak, shot-diet, and creation primitives before downstream synthesis tables.
2. Run rolling backtest and calibration before trusting decision thresholds.
3. Fit core lineup and player/defense coach-facing layers after threshold evidence is generated.
4. Finish with attribution, stress test, and eligibility audit gates before final QC.

This is deliberate. Reliability is upstream of recommendation language.

### Non-Negotiable Operating Rules

1. Run through `bash run_coaching_pipeline.sh` unless you are debugging one script intentionally.
2. Do not hand-edit `_outputs/*.csv`.
3. If QC fails, fix the source condition and rerun.
4. If docs and code conflict, code is source of truth.
5. Treat archived runs as historical snapshots, not current truth.

### Key QC Guardrails

The orchestrator script has explicit pre-flight and post-flight checks.

Pre-flight checks include:

1. required file presence
2. required columns
3. date parsing quality
4. opponent control join integrity
5. non-exhibition sample sufficiency
6. model-eligible row sufficiency
7. clock parse-rate sanity

Post-flight checks include:

1. required output files and columns
2. probability bounds in decision and leak outputs
3. allowed decision labels only
4. minimum backtest evidence thresholds
5. calibration quality thresholds
6. stale-output detection through game_id consistency

### Output Buckets

The project routes outputs into these folders:

1. `_outputs/01_lineup_core`
2. `_outputs/02_defense_leaks`
3. `_outputs/03_players`
4. `_outputs/04_games_trends`
5. `_outputs/05_decision_audit`
6. `_outputs/06_scheme_matchups`

### What To Read First

For a coaching decision pass, read in this order:

1. `_outputs/01_lineup_core/uconn_lineup_coach_view.csv`
2. `_outputs/02_defense_leaks/uconn_lineup_def_leaks_coach_table.csv`
3. `_outputs/03_players/uconn_player_rci_coach_table.csv`
4. `_outputs/05_decision_audit/uconn_availability_stress_test_report.csv`

For reliability checks, read:

1. `_outputs/05_decision_audit/uconn_lineup_decision_rolling_backtest_by_bucket.csv`
2. `_outputs/05_decision_audit/uconn_pred_pr_net_pos_calibration_metrics.csv`
3. `_outputs/02_defense_leaks/uconn_def_leaks_holdout_validation_by_bucket.csv`


---


## Tagging Vocabulary (Scheme Matchup Project)

Last updated: 2026-04-08

This is the controlled vocabulary for the scheme project.

I keep this strict so tags stay model-usable and coach-usable at the same time.

### 1) Scope and Script Link

This vocab supports:

- `_scripts/models/fit_scheme_matchup_model.R`

Primary files in this project folder:

- `possessions.csv`
- `scheme_tags.csv`
- `events.csv` (optional but useful)
- `breakdown_tags.csv` (optional)
- `player_dev_tags.csv` (optional)
- `clip_playlist.csv` (optional)
- `games.csv` (optional)
- `teams.csv` (optional)
- `lineups.csv` (optional)

### 2) Hard Rules

1. Use IDs as join keys. Never join on free text names.
2. Use controlled codes in model-facing columns.
3. Keep free text in `notes` fields only.
4. Keep one primary `scheme_tags` row per possession.
5. Always include confidence for subjective tags.
6. Every possession should end with a terminal result (`SHOT`, `TURNOVER`, or shooting `FOUL`).

### 3) Minimum Columns Required by the Model

#### `possessions.csv` required columns

- `possession_id`
- `game_id`
- `offense_team_id`
- `defense_team_id`
- `points_scored`

#### `scheme_tags.csv` required columns

- `possession_id` (preferred)
- `offense_action`
- `defense_coverage`

Fallback allowed by script:

- if `possession_id` is missing in `scheme_tags.csv`, you can use `event_id` only if `events.csv` maps `event_id -> possession_id`.

### 4) `possessions.csv` (One Row Per Possession)

Purpose:

- backbone for all joins and matchup summaries

Recommended controlled fields:

#### `possession_result`

- `MADE_2`
- `MADE_3`
- `MISS_2`
- `MISS_3`
- `FT_TRIP`
- `TURNOVER`
- `SHOT_CLOCK`
- `END_PERIOD`

#### `special_situation`

- `NONE`
- `ATO`
- `BLOB`
- `SLOB`
- `EOG`
- `EOH`
- `PRESS_BREAK`

Definitions:

- `ATO`: first designed halfcourt action after timeout
- `BLOB`: baseline out-of-bounds
- `SLOB`: sideline out-of-bounds
- `EOG`: late game possession with game-state strategy
- `EOH`: late half possession
- `PRESS_BREAK`: possession starts in press-break context

#### `chance_type`

- `TRANSITION`
- `EARLY_OFFENSE`
- `HALFCOURT`

#### Boolean fields

Use one style consistently within file: `TRUE/FALSE` preferred.

- `turnover_flag`
- `transition_flag`
- `garbage_time_flag`
- `second_chance_flag`
- `paint_touch_flag`
- `post_touch_flag`

### 5) `scheme_tags.csv` (Primary Scheme Labels)

Purpose:

- model-facing possession scheme labels
- prep and postgame scheme review

#### `offense_action` (required)

- `PNR_HIGH`
- `PNR_ANGLE`
- `DHO`
- `ZOOM`
- `CHIN`
- `FLEX`
- `POST_UP`
- `ISO`
- `SPOT_UP_ATTACK`
- `OFF_REB_PUTBACK`
- `TRANSITION_DRAG`
- `TRANSITION_PITCH`
- `BLOB_ACTION`
- `SLOB_ACTION`
- `UNKNOWN_ACTION`

#### `offense_action_family`

- `PNR`
- `HANDOFF`
- `MOTION`
- `POST`
- `ISOLATION`
- `SPACING_ATTACK`
- `TRANSITION`
- `SPECIAL_SITUATION`
- `SECOND_CHANCE`
- `UNKNOWN`

#### `defense_coverage` (required)

- `DROP`
- `ICE`
- `SWITCH`
- `HEDGE`
- `SHOW`
- `BLITZ`
- `WEAK`
- `GAP_MAN`
- `ZONE_2_3`
- `ZONE_3_2`
- `ZONE_1_3_1`
- `MATCHUP_ZONE`
- `PRESS`
- `UNKNOWN_COVERAGE`

#### `defense_coverage_family`

- `MAN_PNR`
- `MAN_GAP`
- `ZONE`
- `PRESSURE`
- `UNKNOWN`

#### `screen_location`

- `NONE`
- `TOP`
- `LEFT_SLOT`
- `RIGHT_SLOT`
- `LEFT_WING`
- `RIGHT_WING`
- `ELBOW`
- `EMPTY_SIDE`

#### metadata fields

- `tag_source`: `MANUAL`, `MODEL`, `HYBRID`
- `tag_confidence`: numeric from `0.00` to `1.00`

### 6) `events.csv` (Rep-Level Event Detail)

Purpose:

- event-level context beyond possession summary
- shot profile, turnover type, foul type, rebounding accountability

#### `event_type`

- `SHOT`
- `PASS`
- `TURNOVER`
- `FOUL`
- `REBOUND`
- `SCREEN`
- `CUT`
- `DRIVE`
- `POST_TOUCH`
- `DEFLECTION`

#### `event_subtype` starter set

- `SHOT_ATTEMPT`
- `SHOT_MAKE`
- `SHOT_BLOCKED`
- `PASS_ADVANCE`
- `PASS_TO_SHOT`
- `TURNOVER_LIVE_BALL`
- `TURNOVER_DEAD_BALL`
- `FOUL_SHOOTING`
- `FOUL_OFFENSIVE`
- `FOUL_LOOSE_BALL`
- `REB_OFF`
- `REB_DEF`
- `SCREEN_ON_BALL`
- `SCREEN_OFF_BALL`
- `DRIVE_PAINT`
- `DRIVE_BASELINE`
- `CUT_45`
- `CUT_BACKDOOR`

#### Shot-related fields

`shot_zone`:

- `RIM`
- `SHORT_MID`
- `LONG_MID`
- `CORNER_3`
- `ABOVE_BREAK_3`
- `FT`

`shot_type`:

- `CATCH_SHOOT`
- `OFF_DRIBBLE`
- `AT_RIM`
- `FLOATER`
- `POST_HOOK`
- `POST_FADE`
- `TIP_IN`

`shot_contest`:

- `OPEN`
- `LIGHT`
- `HEAVY`
- `BLOCKED`

#### Turnover/foul/rebound fields

`turnover_type`:

- `BAD_PASS`
- `LOST_HANDLE`
- `CHARGE`
- `TRAVEL`
- `3SEC`
- `5SEC`
- `OFF_FOUL`
- `STRIP`

`foul_type`:

- `SHOOTING`
- `OFFENSIVE`
- `LOOSE_BALL`
- `REACH`
- `BLOCK`
- `HANDCHECK`
- `TECHNICAL`

`rebound_type`:

- `OREB`
- `DREB`
- `TEAM_REB`

`advantage_state`:

- `CREATED`
- `MAINTAINED`
- `LOST`
- `NONE`

### 7) `breakdown_tags.csv` (Coaching Accountability)

Purpose:

- tag what broke and what needs immediate teaching

#### `phase`

- `TRANSITION`
- `ON_BALL`
- `OFF_BALL`
- `SCREEN_DEFENSE`
- `HELP_ROTATION`
- `REBOUND`
- `FOUL_DISCIPLINE`
- `OFFENSE_EXECUTION`

#### `breakdown_type`

- `NO_FLOOR_BALANCE`
- `MISSED_MATCH`
- `SCREEN_NAV`
- `NO_TAG`
- `LATE_LOW_MAN`
- `BAD_CLOSEOUT`
- `NO_XOUT`
- `BALL_WATCH`
- `MISS_BOXOUT`
- `BAD_FOUL`
- `TURNOVER_DECISION`
- `POOR_SPACING`
- `MISSED_READ`

#### `severity`

Use `1` to `5`.

- `1`: minor issue
- `3`: clear correction point
- `5`: major immediate-teach clip

#### `teach_today_flag`

- `TRUE`
- `FALSE`

### 8) `player_dev_tags.csv` (Player Development Reps)

Purpose:

- convert film reps into practical next-drill assignments

#### `phase`

- `OFFENSE`
- `DEFENSE`
- `TRANSITION`
- `SPECIAL_SITUATION`

#### `skill_domain`

- `SHOOTING`
- `FINISHING`
- `BALL_SECURITY`
- `PNR_READ`
- `POST_READ`
- `OFF_BALL_SPACING`
- `SCREEN_SETTING`
- `SCREEN_NAV`
- `CONTAINMENT`
- `CLOSEOUT`
- `ROTATION`
- `REBOUNDING`

#### grades

`decision_grade`:

- `PLUS`
- `NEUTRAL`
- `MINUS`

`technique_grade`:

- `PLUS`
- `NEUTRAL`
- `MINUS`

`result_grade`:

- `WON_REP`
- `EVEN_REP`
- `LOST_REP`

#### `next_drill_family`

- `CATCH_SHOOT`
- `FINISH_CONTACT`
- `WEAK_HAND_FINISH`
- `PNR_READS`
- `CLOSEOUT_FOOTWORK`
- `SCREEN_NAV`
- `BOXOUT_HIT_FIND`
- `ROTATION_XOUT`

#### optional booleans

- `weak_hand_flag`
- `contact_flag`
- `balance_flag`
- `footwork_flag`

Use `TRUE/FALSE`. Leave blank if not evaluated.

### 9) `clip_playlist.csv` (Meeting Workflow)

Purpose:

- turn tags into actual film meeting playlists

#### `playlist`

- `HEAD_COACH`
- `ASSISTANT_DEFENSE`
- `ASSISTANT_OFFENSE`
- `PLAYER_DEV_TEAM`
- `PLAYER_DEV_INDIVIDUAL`

#### `status`

- `QUEUED`
- `REVIEWED`
- `TAUGHT`
- `CARRYOVER`

### 10) ID and Naming Rules

- `game_id`: stable key for each game
- `possession_id`: unique within full dataset
- `event_id`: unique event key
- `tag_id`, `breakdown_id`, `pdev_tag_id`, `clip_id`: UUID or deterministic key, but stay consistent

### 11) QA Checklist (Run Every Tagging Batch)

1. One row per `possession_id` in `scheme_tags.csv`.
2. No blanks in `offense_action` and `defense_coverage`.
3. Terminal event coverage exists for each possession.
4. Confidence fields stay inside `0-1`.
5. Controlled-code columns contain only allowed values.
6. Any `QUEUED` clips have timestamp fields populated.

### 12) Practical Rollout Order

1. Build `possessions.csv` first.
2. Fill primary scheme tags.
3. Add event-level terminal and shot-turnover detail.
4. Add major breakdown tags.
5. Add player development reps.
6. Build clip playlists for next meeting/practice.

That order keeps the process usable instead of trying to capture everything on day one.


---


## _data Folder Standard

Last updated: 2026-04-08

This folder is the project contract. If this structure drifts, models drift.

### 1) Allowed Root Contents Only

At `_data/` root, keep only:

- `01_core_inputs/`
- `02_derived_inputs/`
- `03_manual_game_csv/`
- `04_templates/`
- `05_projects/`

No loose CSVs at `_data/` root.
No symlinks at `_data/` root.

### 2) Folder Purpose (In Order)

#### `01_core_inputs/`

Core files required by the coaching pipeline:

- `uconn_stints_from_pbp.csv`
- `uconn_games_meta.csv`
- `opponent_controls.csv`

If one is missing, pipeline should fail fast.

#### `02_derived_inputs/`

Derived files used downstream:

- `uconn_lineup_stabilizers.csv`

This file is rebuilt by:

- `_scripts/analysis/build_lineup_stability_baselines.R`

#### `03_manual_game_csv/`

Manual or hand-curated raw game files:

- `_games/`

Treat this as manual staging/reference, not guaranteed model-ready input.

#### `04_templates/`

Template CSVs:

- `TEMPLATE FOR CSV GAMES.csv`

#### `05_projects/`

Side projects not required by the main coaching pipeline.

Current project:

- `scheme_matchup_project/`

Main script for that project:

- `_scripts/models/fit_scheme_matchup_model.R`

### 3) Rebuild Order I Follow

1. Update/check `01_core_inputs/`.
2. Run full coaching pipeline.
3. Confirm derived and output CSVs regenerated correctly.
4. Run cleanup only if needed.

### 4) Input Checks Before Running

#### `uconn_stints_from_pbp.csv`

Must have clean values for:

- `game_id`
- `poss_est`
- `uconn_lineup`
- `lineup_size`
- `period`, `start_time`, `end_time`
- `poss_est` is auto-repaired by `_scripts/ops/repair_core_inputs.R` (also invoked by `_scripts/ops/cleanup_project.R` unless disabled).

#### `uconn_games_meta.csv`

Must have clean values for:

- `game_file`
- `game_date`
- `opponent`
- `uconn_is_home`

#### `opponent_controls.csv`

Must have:

- one row per `(game_date, opponent)`
- numeric `opp_adjO`
- numeric `opp_adjD`

### 5) Commands

Full pipeline:

```bash
bash run_coaching_pipeline.sh
```

Run everything (entrypoint manifest):

```bash
bash run_everything.sh
```

Core stints repair (dry report only):

```bash
Rscript --vanilla _scripts/ops/repair_core_inputs.R --dry-run
```

Core stints repair (rewrite + backup):

```bash
Rscript --vanilla _scripts/ops/repair_core_inputs.R --rewrite
```

Cleanup:

```bash
Rscript --vanilla _scripts/ops/cleanup_project.R
```

### 6) Hard Rules

- Do not hand-edit output CSVs in `_outputs/`.
- Do not keep duplicate versions of core inputs in random folders.
- If docs and scripts conflict, trust scripts and rerun.
- `run_everything.sh` only executes manifest entrypoints from `_scripts/ops/runnable_entrypoints_manifest.csv`; utility/library files are intentionally excluded.
- Manual scout output generation runs in clean-rebuild mode (`--clean-out=true`) so stale files are removed before write.
- Manual scout root must contain exactly: `conference/`, `non_conference/`, and `manual_game_scout_manifest.csv`.


---

## File Atlas

Generated: 2026-03-05 19:38:03 EST (historical snapshot)

This atlas was rebuilt directly from `.csv`, `.R`, and `.stan` files.
No existing markdown was used as source truth.
If this section feels overwhelming on first read, skip it and come back later (this is reference inventory, not first-run guidance).
Run contracts above are current; this atlas can lag active data/output state.

- CSV files: 109
- R scripts: 30
- Stan models: 5

### CSV Files

| Path | Category | Rows | Cols | Header Preview |
|---|---:|---:|---:|---|
| `./_data/01_core_inputs/opponent_controls.csv` | other | 29 | 4 | game_date / opponent / opp_adjO / opp_adjD |
| `./_data/01_core_inputs/uconn_games_meta.csv` | other | 30 | 6 | game_file / game_date / matchup_header / opponent / uconn_is_home / site_type |
| `./_data/01_core_inputs/uconn_stints_from_pbp.csv` | other | 750 | 15 | game_file / period / stint_index / start_time / end_time / uconn_is_home / uconn_lineup / points_for / points_against / poss_est / lineup_size / net_pts |
| `./_data/02_derived_inputs/uconn_lineup_stabilizers.csv` | other | 108 | 14 | lineup_key / possessions / pts_for / pts_against / stints / net_pts / net_ppp / games_played / games_ok / sample_tier / team_avg_net_ppp / dev_from_team_avg |
| `./_data/03_manual_game_csv/_conf/UConn v Depaul Home.csv` | other | 162 | 35 | UsagePlayer / AssistPlayer / ReboundPlayer / StealPlayer / BlockPlayer / FGA / FGM / FTA / FTM / FGA3 / FGM3 / PTS |
| `./_data/03_manual_game_csv/_conf/UConn vs Butler Away.csv` | other | 154 | 35 | UsagePlayer / AssistPlayer / ReboundPlayer / StealPlayer / BlockPlayer / FGA / FGM / FTA / FTM / FGA3 / FGM3 / PTS |
| `./_data/03_manual_game_csv/_conf/UConn vs Butler Home.csv` | other | 186 | 35 | UsagePlayer / AssistPlayer / ReboundPlayer / StealPlayer / BlockPlayer / FGA / FGM / FTA / FTM / FGA3 / FGM3 / PTS |
| `./_data/03_manual_game_csv/_conf/UConn vs Creighton Away.csv` | other | 154 | 35 | UsagePlayer / AssistPlayer / ReboundPlayer / StealPlayer / BlockPlayer / FGA / FGM / FTA / FTM / FGA3 / FGM3 / PTS |
| `./_data/03_manual_game_csv/_conf/UConn vs Creighton Home.csv` | other | 189 | 35 | UsagePlayer / AssistPlayer / ReboundPlayer / StealPlayer / BlockPlayer / FGA / FGM / FTA / FTM / FGA3 / FGM3 / PTS |
| `./_data/03_manual_game_csv/_conf/UConn vs Depaul Away.csv` | other | 177 | 35 | UsagePlayer / AssistPlayer / ReboundPlayer / StealPlayer / BlockPlayer / FGA / FGM / FTA / FTM / FGA3 / FGM3 / PTS |
| `./_data/03_manual_game_csv/_conf/UConn vs Georgetown Away.csv` | other | 167 | 35 | UsagePlayer / AssistPlayer / ReboundPlayer / StealPlayer / BlockPlayer / FGA / FGM / FTA / FTM / FGA3 / FGM3 / PTS |
| `./_data/03_manual_game_csv/_conf/UConn vs Georgetown Home.csv` | other | 177 | 35 | UsagePlayer / AssistPlayer / ReboundPlayer / StealPlayer / BlockPlayer / FGA / FGM / FTA / FTM / FGA3 / FGM3 / PTS |
| `./_data/03_manual_game_csv/_conf/UConn vs Marquette Home.csv` | other | 187 | 35 | UsagePlayer / AssistPlayer / ReboundPlayer / StealPlayer / BlockPlayer / FGA / FGM / FTA / FTM / FGA3 / FGM3 / PTS |
| `./_data/03_manual_game_csv/_conf/UConn vs Providence Away.csv` | other | 209 | 35 | UsagePlayer / AssistPlayer / ReboundPlayer / StealPlayer / BlockPlayer / FGA / FGM / FTA / FTM / FGA3 / FGM3 / PTS |
| `./_data/03_manual_game_csv/_conf/UConn vs Providence Home.csv` | other | 199 | 35 | UsagePlayer / AssistPlayer / ReboundPlayer / StealPlayer / BlockPlayer / FGA / FGM / FTA / FTM / FGA3 / FGM3 / PTS |
| `./_data/03_manual_game_csv/_conf/UConn vs Seton Hall Away.csv` | other | 189 | 35 | UsagePlayer / AssistPlayer / ReboundPlayer / StealPlayer / BlockPlayer / FGA / FGM / FTA / FTM / FGA3 / FGM3 / PTS |
| `./_data/03_manual_game_csv/_conf/UConn vs Seton Hall Home.csv` | other | 167 | 35 | UsagePlayer / AssistPlayer / ReboundPlayer / StealPlayer / BlockPlayer / FGA / FGM / FTA / FTM / FGA3 / FGM3 / PTS |
| `./_data/03_manual_game_csv/_conf/UConn vs St Johns Away.csv` | other | 175 | 35 | UsagePlayer / AssistPlayer / ReboundPlayer / StealPlayer / BlockPlayer / FGA / FGM / FTA / FTM / FGA3 / FGM3 / PTS |
| `./_data/03_manual_game_csv/_conf/UConn vs St Johns Home.csv` | other | 156 | 35 | UsagePlayer / AssistPlayer / ReboundPlayer / StealPlayer / BlockPlayer / FGA / FGM / FTA / FTM / FGA3 / FGM3 / PTS |
| `./_data/03_manual_game_csv/_conf/UConn vs Villanova Away.csv` | other | 161 | 35 | UsagePlayer / AssistPlayer / ReboundPlayer / StealPlayer / BlockPlayer / FGA / FGM / FTA / FTM / FGA3 / FGM3 / PTS |
| `./_data/03_manual_game_csv/_conf/UConn vs Villanova Home.csv` | other | 184 | 35 | UsagePlayer / AssistPlayer / ReboundPlayer / StealPlayer / BlockPlayer / FGA / FGM / FTA / FTM / FGA3 / FGM3 / PTS |
| `./_data/03_manual_game_csv/_conf/UConn vs Xavier Away.csv` | other | 179 | 35 | UsagePlayer / AssistPlayer / ReboundPlayer / StealPlayer / BlockPlayer / FGA / FGM / FTA / FTM / FGA3 / FGM3 / PTS |
| `./_data/03_manual_game_csv/_conf/UConn vs Xavier Home.csv` | other | 181 | 35 | UsagePlayer / AssistPlayer / ReboundPlayer / StealPlayer / BlockPlayer / FGA / FGM / FTA / FTM / FGA3 / FGM3 / PTS |
| `./_data/03_manual_game_csv/_games/_espn_generation_summary.csv` | other | 28 | 4 | game_file / status / out_path / rows |
| `./_data/03_manual_game_csv/_nc/_espn_generation_summary.csv` | other | 2 | 4 | game_file / status / out_path / rows |
| `./_data/03_manual_game_csv/_nc/UConn vs Arizone Home.csv` | other | 168 | 35 | UsagePlayer / AssistPlayer / ReboundPlayer / StealPlayer / BlockPlayer / FGA / FGM / FTA / FTM / FGA3 / FGM3 / PTS |
| `./_data/03_manual_game_csv/_nc/UConn vs Bryant Home.csv` | other | 176 | 35 | UsagePlayer / AssistPlayer / ReboundPlayer / StealPlayer / BlockPlayer / FGA / FGM / FTA / FTM / FGA3 / FGM3 / PTS |
| `./_data/03_manual_game_csv/_nc/UConn vs Columbia Home.csv` | other | 177 | 35 | UsagePlayer / AssistPlayer / ReboundPlayer / StealPlayer / BlockPlayer / FGA / FGM / FTA / FTM / FGA3 / FGM3 / PTS |
| `./_data/03_manual_game_csv/_nc/UConn vs East Texas A&M Home.csv` | other | 164 | 35 | UsagePlayer / AssistPlayer / ReboundPlayer / StealPlayer / BlockPlayer / FGA / FGM / FTA / FTM / FGA3 / FGM3 / PTS |
| `./_data/03_manual_game_csv/_nc/UConn vs Florida Neutral.csv` | other | 181 | 35 | UsagePlayer / AssistPlayer / ReboundPlayer / StealPlayer / BlockPlayer / FGA / FGM / FTA / FTM / FGA3 / FGM3 / PTS |
| `./_data/03_manual_game_csv/_nc/UConn vs Illinois Neutral.csv` | other | 178 | 35 | UsagePlayer / AssistPlayer / ReboundPlayer / StealPlayer / BlockPlayer / FGA / FGM / FTA / FTM / FGA3 / FGM3 / PTS |
| `./_data/03_manual_game_csv/_nc/UConn vs Kansas Away.csv` | other | 160 | 35 | UsagePlayer / AssistPlayer / ReboundPlayer / StealPlayer / BlockPlayer / FGA / FGM / FTA / FTM / FGA3 / FGM3 / PTS |
| `./_data/03_manual_game_csv/_nc/UConn vs Texas Home.csv` | other | 174 | 35 | UsagePlayer / AssistPlayer / ReboundPlayer / StealPlayer / BlockPlayer / FGA / FGM / FTA / FTM / FGA3 / FGM3 / PTS |
| `./_data/03_manual_game_csv/_nc/UConn vs UMass Lowell Home.csv` | other | 201 | 35 | UsagePlayer / AssistPlayer / ReboundPlayer / StealPlayer / BlockPlayer / FGA / FGM / FTA / FTM / FGA3 / FGM3 / PTS |
| `./_data/04_templates/TEMPLATE FOR CSV GAMES.csv` | other | 158 | 27 | usage_player / assist_player / rebound_player / steal_player / block_player / fga / fgm / fta / ftm / fga / fgm / pts |
| `./_data/05_projects/scheme_matchup_project/breakdown_tags.csv` | other | 0 | 16 | breakdown_id / possession_id / event_id / team_id / phase / breakdown_type / breakdown_detail / primary_responsible_player_id / secondary_player_id / severity / confidence / teach_today_flag |
| `./_data/05_projects/scheme_matchup_project/clip_playlist.csv` | other | 0 | 13 | clip_id / game_id / possession_id / event_id / playlist / priority / title / start_ts / end_ts / assigned_staff / assigned_player_id / status |
| `./_data/05_projects/scheme_matchup_project/events.csv` | other | 0 | 33 | event_id / possession_id / game_id / timestamp / period / clock / event_index / team_id / event_type / event_subtype / player1_id / player2_id |
| `./_data/05_projects/scheme_matchup_project/player_dev_tags.csv` | other | 0 | 23 | pdev_tag_id / game_id / possession_id / event_id / player_id / team_id / phase / skill_domain / rep_type / decision_grade / technique_grade / result_grade |
| `./_data/05_projects/scheme_matchup_project/possessions.csv` | other | 0 | 28 | possession_id / game_id / period / clock_start / clock_end / offense_team_id / defense_team_id / points_scored / shot_value / turnover_flag / possession_result / transition_flag |
| `./_data/05_projects/scheme_matchup_project/scheme_tags.csv` | other | 0 | 17 | tag_id / possession_id / event_id / offense_action / defense_coverage / offense_action_family / offense_action_detail / defense_coverage_family / defense_coverage_detail / screen_location / handler_id / screener_id |
| `./_outputs/01_lineup_core/uconn_lineup_coach_view.csv` | other | 108 | 14 | lineup_pretty / possessions / minutes / games / raw_net_ppp / synergy_mean / synergy_p05 / synergy_p95 / decision_pred_pr_net_pos_raw / decision_pred_net_ppp_mean / decision_pred_pr_net_pos / decision_prob |
| `./_outputs/01_lineup_core/uconn_lineup_core_model_diagnostics.csv` | other | 4000 | 7 | accept_stat__ / stepsize__ / treedepth__ / n_leapfrog__ / divergent__ / energy__ / chain |
| `./_outputs/01_lineup_core/uconn_lineup_synergy_posterior.csv` | other | 108 | 8 | lineup_id / lineup / synergy_mean / synergy_p05 / synergy_p50 / synergy_p95 / pr_synergy_pos / lineup_pretty |
| `./_outputs/01_lineup_core/uconn_lineup_usage.csv` | other | 108 | 8 | uconn_lineup_canon / possessions / minutes / segments / games / raw_net_ppp / lineup_id / lineup_pretty |
| `./_outputs/02_defense_leaks/uconn_def_leak_lineups_by_game.csv` | other | 88 | 19 | game_file / game_date / uconn_is_home / lineup_key / lineup_pretty / poss_in_game / minutes_in_game / pts_allowed_in_game / u_def_mean / pr_leak / u_def_p05 / u_def_p50 |
| `./_outputs/02_defense_leaks/uconn_def_leak_primary_culprit_by_game.csv` | other | 28 | 19 | game_file / game_date / uconn_is_home / lineup_key / lineup_pretty / poss_in_game / minutes_in_game / pts_allowed_in_game / u_def_mean / pr_leak / u_def_p05 / u_def_p50 |
| `./_outputs/02_defense_leaks/uconn_def_leak_repeat_offenders.csv` | other | 1 | 7 | lineup_key / lineup_pretty / games_flagged / total_poss_flagged / mean_u_def / mean_pr_leak / definition |
| `./_outputs/02_defense_leaks/uconn_def_leaks_holdout_validation_by_bucket.csv` | other | 5 | 9 | risk_bucket / n_lineup_games / n_games / total_holdout_possessions / weighted_pr_leak / weighted_observed_leaky_rate / weighted_holdout_def_ppp / weighted_def_ppp_gap_vs_test_baseline / weighted_train_possessions |
| `./_outputs/02_defense_leaks/uconn_def_leaks_holdout_validation_fit_diagnostics.csv` | other | 1000 | 7 | accept_stat__ / stepsize__ / treedepth__ / n_leapfrog__ / divergent__ / energy__ / chain |
| `./_outputs/02_defense_leaks/uconn_def_leaks_holdout_validation_meta.csv` | other | 28 | 2 | metric / value |
| `./_outputs/02_defense_leaks/uconn_def_leaks_holdout_validation_rows.csv` | other | 82 | 23 | global_game_id / game_file / game_date / lineup / holdout_possessions / holdout_minutes / holdout_games / holdout_pts_against / holdout_def_ppp / train_possessions / train_minutes / train_games |
| `./_outputs/02_defense_leaks/uconn_game_level_def_leak_trend.csv` | other | 28 | 9 | game_file / game_date / poss / def_leak_mean / def_leak_p05 / def_leak_p95 / def_pts_40_mean / def_pts_40_p05 / def_pts_40_p95 |
| `./_outputs/02_defense_leaks/uconn_lineup_def_leaks_coach_table.csv` | other | 108 | 15 | lineup_pretty / possessions / minutes / games / raw_net_ppp / u_def_mean / u_def_p05 / u_def_p95 / pr_leak / expected_pts_allowed_per_40 / confidence / leak_flag |
| `./_outputs/02_defense_leaks/uconn_lineup_def_leaks_model_diagnostics.csv` | other | 4000 | 7 | accept_stat__ / stepsize__ / treedepth__ / n_leapfrog__ / divergent__ / energy__ / chain |
| `./_outputs/02_defense_leaks/uconn_lineup_def_leaks_posterior.csv` | other | 108 | 8 | lineup_id / lineup / u_def_mean / u_def_p05 / u_def_p50 / u_def_p95 / pr_leak / lineup_pretty |
| `./_outputs/03_players/uconn_player_net_posterior.csv` | other | 14 | 6 | player / net_mean / net_p05 / net_p50 / net_p95 / net_pr_pos |
| `./_outputs/03_players/uconn_player_net_ranking.csv` | other | 14 | 6 | player / net_mean / net_p05 / net_p50 / net_p95 / net_pr_pos |
| `./_outputs/03_players/uconn_player_rci_coach_table.csv` | other | 14 | 10 | player / total_possessions / unique_lineups / RCI / net_mean / net_p05 / net_p95 / net_pr_pos / role_type / recommendation |
| `./_outputs/03_players/uconn_player_rci_three_windows.csv` | other | 14 | 19 | player / phase1_range / phase2_range / phase3_range / current_phase / poss_phase1 / poss_phase2 / poss_phase3 / rci_phase1 / rci_phase2 / rci_phase3 / delta_rci_1_to_2 |
| `./_outputs/03_players/uconn_player_rci.csv` | other | 14 | 7 | player / total_possessions / unique_lineups / mean_poss_per_lineup / RCI / lineup_poss_sd / lineup_poss_cv |
| `./_outputs/05_decision_audit/uconn_availability_stress_test_report.csv` | other | 13 | 17 | generated_at_utc / scenario_player_out / top_n_requested / max_pr_leak / survivors_n / lost_n / section / rank / lineup_key / lineup_pretty / possessions / sample_tier |
| `./_outputs/05_decision_audit/uconn_decision_eligibility_by_stint.csv` | other | 750 | 21 | game_id / game_file / game_date / period / start_time / end_time / stint_index / uconn_lineup / lineup_key / poss_est / points_for / points_against |
| `./_outputs/05_decision_audit/uconn_lineup_decision_rolling_backtest_by_bucket.csv` | other | 6 | 21 | Decision / n_game_lineups / n_games / total_holdout_possessions / mean_prior_possessions / mean_pred_pr_net_pos / weighted_pred_pr_net_pos / observed_positive_rate / weighted_observed_positive_rate / net_prob_calibration_gap / weighted_net_prob_calibration_gap / mean_pred_net_ppp |
| `./_outputs/05_decision_audit/uconn_lineup_decision_rolling_backtest_by_game_bucket.csv` | other | 88 | 14 | holdout_game_id / holdout_game_file / holdout_game_date / Decision / n_lineups / holdout_possessions / weighted_pred_pr_net_pos / weighted_observed_positive_rate / weighted_pred_net_ppp / weighted_realized_net_ppp / weighted_net_prob_calibration_gap / net_value_gap_ppp |
| `./_outputs/05_decision_audit/uconn_lineup_decision_rolling_backtest_fit_diagnostics.csv` | other | 23 | 12 | holdout_game_id / holdout_game_file / holdout_game_date / prior_games / train_rows / train_players / train_lineups / holdout_rows / used_cache / divergences / max_treedepth / treedepth_hits_limit |
| `./_outputs/05_decision_audit/uconn_lineup_decision_rolling_backtest_rows.csv` | other | 324 | 39 | holdout_game_id / holdout_game_file / holdout_game_date / prior_games_available / lineup / lineup_pretty / decision_available / Decision / prior_possessions / prior_minutes / prior_games / prior_segments |
| `./_outputs/05_decision_audit/uconn_lineup_decision_rule_v2_thresholds.csv` | other | 26 | 2 | metric / value |
| `./_outputs/05_decision_audit/uconn_lineup_decision_table.csv` | other | 108 | 14 | lineup_pretty / possessions / minutes / games / raw_net_ppp / synergy_mean / synergy_p05 / synergy_p95 / decision_pred_pr_net_pos_raw / decision_pred_net_ppp_mean / decision_pred_pr_net_pos / decision_prob |
| `./_outputs/05_decision_audit/uconn_pred_pr_net_pos_calibration_deciles.csv` | other | 10 | 12 | decile / n_rows / n_games / total_holdout_possessions / pred_min / pred_max / mean_pred_pr_net_pos / weighted_pred_pr_net_pos / observed_positive_rate / weighted_observed_positive_rate / calibration_gap / weighted_calibration_gap |
| `./_outputs/05_decision_audit/uconn_pred_pr_net_pos_calibration_metrics.csv` | other | 20 | 2 | metric / value |
| `./_outputs/05_decision_audit/uconn_pred_pr_net_pos_calibration_model.csv` | other | 1 | 16 | mode / status / intercept / slope / fallback_shrink / tune_games_n / tune_rows_n / tune_weighted_n / train_weighted_ece_raw / train_weighted_ece_platt / train_max_decile_gap_raw / train_max_decile_gap_platt |
| `./_outputs/organize_manifest.csv` | other | 16 | 5 | file / from / bucket / to / ok |
| `./Notes/02 - Count models for low-score games/nhl_score_2022.csv` | other | 1400 | 11 | Date / Visitor / scoreVisitor / Home / scoreHome / overtime / Att. / LOG / Notes / atHome / playOff |
| `./Notes/02 - Count models for low-score games/nhl_score_2024.csv` | other | 1312 | 10 | Date / Time / Visitor / G / Home / G / / Att. / LOG / Notes |
| `./Notes/03 - Paired comparison models/mlb_score_2022.csv` | other | 2470 | 4 | Visitor / ScoreVisitor / Home / ScoreHome |
| `./Notes/04 - Regularizaton and Bayes/mlb_score_2022.csv` | other | 2429 | 161 | 20220407 / 0 / Thu / SDN / NL / 1 / ARI / NL / 1 / 2 / 4 / 51 |
| `./Notes/04 - Regularizaton and Bayes/mlb-team-abbrevs.csv` | other | 30 | 2 | Team.Name / Teams |
| `./Notes/04 - Regularizaton and Bayes/nhl_score_2022.csv` | other | 1400 | 11 | Date / Visitor / scoreVisitor / Home / scoreHome / overtime / Att. / LOG / Notes / atHome / playOff |
| `./Notes/04 - Regularizaton and Bayes/nhl_score_2024.csv` | other | 1312 | 10 | Date / Time / Visitor / G / Home / G / / Att. / LOG / Notes |
| `./Notes/05 - Multicompetitor Models/golf_2019_rankings.csv` | other | 34 | 2 | tournament id / 0 |
| `./Notes/05 - Multicompetitor Models/golf_2019.csv` | other | 271 | 4 | tournament id / player id / final position / Score |
| `./Notes/05 - Multicompetitor Models/golf-tournaments.csv` | other | 14041 | 64 | player / Height cm / Weight lbs / DOB / Age / player id / date / course / tournament name / tournament id / season / final position |
| `./Notes/05 - Multicompetitor Models/toy_example_data.csv` | other | 8 | 3 | Score / tournament.id / player.id |
| `./Notes/06 - Dynamic models/nba_games.csv` | other | 26651 | 21 | GAME_DATE_EST / GAME_ID / GAME_STATUS_TEXT / HOME_TEAM_ID / VISITOR_TEAM_ID / SEASON / TEAM_ID_home / PTS_home / FG_PCT_home / FT_PCT_home / FG3_PCT_home / AST_home |
| `./Notes/06 - Dynamic models/nba_startdate.csv` | other | 15 | 3 | season / start.date / end.date |
| `./Notes/06 - Dynamic models/nba_teams.csv` | other | 30 | 14 | LEAGUE_ID / TEAM_ID / MIN_YEAR / MAX_YEAR / ABBREVIATION / NICKNAME / YEARFOUNDED / CITY / ARENA / ARENACAPACITY / OWNER / GENERALMANAGER |
| `./Notes/07 - Rating systems/nfl2000-19/2000.csv` | other | 260 | 14 | Week / Day / Date / Time / Winner/tie / / Loser/tie / / PtsW / PtsL / YdsW / TOW |
| `./Notes/07 - Rating systems/nfl2000-19/2001.csv` | other | 261 | 14 | Week / Day / Date / Time / Winner/tie / / Loser/tie / / PtsW / PtsL / YdsW / TOW |
| `./Notes/07 - Rating systems/nfl2000-19/2002.csv` | other | 269 | 14 | Week / Day / Date / Time / Winner/tie / / Loser/tie / / PtsW / PtsL / YdsW / TOW |
| `./Notes/07 - Rating systems/nfl2000-19/2003.csv` | other | 269 | 14 | Week / Day / Date / Time / Winner/tie / / Loser/tie / / PtsW / PtsL / YdsW / TOW |
| `./Notes/07 - Rating systems/nfl2000-19/2004.csv` | other | 269 | 14 | Week / Day / Date / Time / Winner/tie / / Loser/tie / / PtsW / PtsL / YdsW / TOW |
| `./Notes/07 - Rating systems/nfl2000-19/2005.csv` | other | 269 | 14 | Week / Day / Date / Time / Winner/tie / / Loser/tie / / PtsW / PtsL / YdsW / TOW |
| `./Notes/07 - Rating systems/nfl2000-19/2006.csv` | other | 269 | 14 | Week / Day / Date / Time / Winner/tie / / Loser/tie / / PtsW / PtsL / YdsW / TOW |
| `./Notes/07 - Rating systems/nfl2000-19/2007.csv` | other | 269 | 14 | Week / Day / Date / Time / Winner/tie / / Loser/tie / / PtsW / PtsL / YdsW / TOW |
| `./Notes/07 - Rating systems/nfl2000-19/2008.csv` | other | 269 | 14 | Week / Day / Date / Time / Winner/tie / / Loser/tie / / PtsW / PtsL / YdsW / TOW |
| `./Notes/07 - Rating systems/nfl2000-19/2009.csv` | other | 268 | 14 | Week / Day / Date / Time / Winner/tie / / Loser/tie / / PtsW / PtsL / YdsW / TOW |
| `./Notes/07 - Rating systems/nfl2000-19/2010.csv` | other | 269 | 14 | Week / Day / Date / Time / Winner/tie / / Loser/tie / / PtsW / PtsL / YdsW / TOW |
| `./Notes/07 - Rating systems/nfl2000-19/2011.csv` | other | 268 | 14 | Week / Day / Date / Time / Winner/tie / / Loser/tie / / PtsW / PtsL / YdsW / TOW |
| `./Notes/07 - Rating systems/nfl2000-19/2012.csv` | other | 269 | 14 | Week / Day / Date / Time / Winner/tie / / Loser/tie / / PtsW / PtsL / YdsW / TOW |
| `./Notes/07 - Rating systems/nfl2000-19/2013.csv` | other | 269 | 14 | Week / Day / Date / Time / Winner/tie / / Loser/tie / / PtsW / PtsL / YdsW / TOW |
| `./Notes/07 - Rating systems/nfl2000-19/2014.csv` | other | 269 | 14 | Week / Day / Date / Time / Winner/tie / / Loser/tie / / PtsW / PtsL / YdsW / TOW |
| `./Notes/07 - Rating systems/nfl2000-19/2015.csv` | other | 269 | 14 | Week / Day / Date / Time / Winner/tie / / Loser/tie / / PtsW / PtsL / YdsW / TOW |
| `./Notes/07 - Rating systems/nfl2000-19/2016.csv` | other | 268 | 14 | Week / Day / Date / Time / Winner/tie / / Loser/tie / / PtsW / PtsL / YdsW / TOW |
| `./Notes/07 - Rating systems/nfl2000-19/2017.csv` | other | 268 | 14 | Week / Day / Date / Time / Winner/tie / / Loser/tie / / PtsW / PtsL / YdsW / TOW |
| `./Notes/07 - Rating systems/nfl2000-19/2018.csv` | other | 269 | 14 | Week / Day / Date / Time / Winner/tie / / Loser/tie / / PtsW / PtsL / YdsW / TOW |
| `./Notes/07 - Rating systems/nfl2000-19/2019.csv` | other | 268 | 14 | Week / Day / Date / Time / Winner/tie / / Loser/tie / / PtsW / PtsL / YdsW / TOW |
| `./Notes/08 - Simulating tournaments/nfl_divisions_abbr.csv` | other | 32 | 5 | name / division / division.id / conference / conference.id |
| `./Notes/08 - Simulating tournaments/nhl_score_2022.csv` | other | 1400 | 11 | Date / Visitor / scoreVisitor / Home / scoreHome / overtime / Att. / LOG / Notes / atHome / playOff |

### R Scripts

| Path | Kind | Lines | Purpose Snapshot | Input References | Output References |
|---|---|---:|---|---|---|
| `./_scripts/analysis/analyze_player_role_stability_by_phase.R` | other | 349 | Track player role stability over three season windows and pair it with / / a current-form impact check. / / Output: _outputs/03_players/uconn_player_rci_three_windows.csv | _data/01_core_inputs/uconn_stints_from_pbp.csv; uconn_player_rci_three_windows.csv |  |
| `./_scripts/analysis/attribute_defensive_leaks_by_game.R` | other | 304 | Break down poor defensive games to lineup-level contributors and flag / / repeat offenders across the sample. / / Helpers | uconn_stints_from_pbp.csv; uconn_games_meta.csv; uconn_lineup_def_leaks_posterior.csv; uconn_lineup_def_leaks_coach_table.csv; uconn_lineup_usage.csv | uconn_def_leak_lineups_by_game.csv; uconn_def_leak_primary_culprit_by_game.csv; uconn_def_leak_repeat_offenders.csv |
| `./_scripts/analysis/audit_lineup_decision_eligibility.R` | other | 216 | _scripts/analysis/audit_lineup_decision_eligibility.R / / Output: _outputs/05_decision_audit/uconn_decision_eligibility_by_stint.csv / / Label each stint decision as eligible/ineligible using only prior-game information. | _data/01_core_inputs/uconn_stints_from_pbp.csv; uconn_decision_eligibility_by_stint.csv; uconn_decision_validity_by_stint.csv |  |
| `./_scripts/analysis/build_defensive_leak_coach_table.R` | other | 203 | Merge defensive leak posterior + lineup context into a coach-facing / / watchlist table. / / UConn pace (possessions per minute) for per-40 conversions | uconn_lineup_def_leaks_posterior.csv; uconn_lineup_usage.csv | uconn_lineup_def_leaks_coach_table.csv |
| `./_scripts/analysis/build_game_level_defensive_trend.R` | other | 149 | Build a game-by-game defensive trend from lineup leak posteriors so / / staff can see whether issues are improving or compounding. / / Load inputs | uconn_stints_from_pbp.csv; uconn_lineup_def_leaks_posterior.csv; uconn_lineup_usage.csv; uconn_game_level_def_leak_trend.csv; uconn_game_level_def_leak_trend.png |  |
| `./_scripts/analysis/build_lineup_stability_baselines.R` | other | 124 | _scripts/analysis/build_lineup_stability_baselines.R / / Rebuilds: _data/02_derived_inputs/uconn_lineup_stabilizers.csv / / Inputs: _data/01_core_inputs/uconn_stints_from_pbp.csv | _data/01_core_inputs/uconn_stints_from_pbp.csv; _data/02_derived_inputs/uconn_lineup_stabilizers.csv |  |
| `./_scripts/analysis/build_player_role_concentration_table.R` | other | 155 | Build the main player role concentration table from stint usage + player / / posterior impact estimates. / / Config | uconn_stints_from_pbp.csv; uconn_player_net_posterior.csv; uconn_player_off_def_net_posterior.csv | uconn_player_rci.csv; uconn_player_rci_coach_table.csv |
| `./_scripts/analysis/evaluate_net_probability_calibration.R` | other | 234 | Probability reliability diagnostics for lineup holdout predictions. / / Uses rolling holdout rows and evaluates pred_pr_net_pos vs observed_net_positive. | uconn_lineup_decision_rolling_backtest_rows.csv; uconn_pred_pr_net_pos_calibration_deciles.csv; uconn_pred_pr_net_pos_calibration_metrics.csv; uconn_pred_pr_net_pos_calibration_curve.png |  |
| `./_scripts/analysis/generate_manual_game_csvs_from_espn.R` | other | 593 |  | _data/01_core_inputs/uconn_games_meta.csv; _espn_generation_summary.csv |  |
| `./_scripts/analysis/validate_defensive_leak_signal_holdout.R` | other | 582 | Time-split holdout validation for the defense leaks module. / / Fits the defense-only Stan model on early games, then evaluates whether the / / lineup leak signal (pr_leak / u_def_mean) separates late-game defensive outcomes. | uconn_stints_from_pbp.csv; uconn_games_meta.csv; opponent_controls.csv; uconn_lineup_gamelevel_defonly.stan; uconn_lineup_gamelevel_defonly_holdout_timesplit_fit.rds; uconn_def_leaks_holdout_validation_rows.csv; uconn_def_leaks_holdout_validation_by_bucket.csv; uconn_def_leaks_holdout_validation_meta.csv; uconn_def_leaks_holdout_validation_fit_diagnostics.csv |  |
| `./_scripts/models/fit_core_lineup_model.R` | other | 726 | Bayesian lineup model with opponent and game-state controls. | uconn_stints_from_pbp.csv; uconn_games_meta.csv; opponent_controls.csv; uconn_lineup_gamelevel_offdef.stan; uconn_lineup_gamelevel_offdef_fit.rds; uconn_lineup_decision_rule_v2_thresholds.csv; uconn_pred_pr_net_pos_calibration_model.csv; uconn_player_off_def_net_posterior.csv; uconn_player_offense_ranking.csv; uconn_player_defense_ranking.csv | uconn_lineup_core_model_diagnostics.csv; uconn_player_net_posterior.csv; uconn_player_net_ranking.csv; uconn_lineup_usage.csv; uconn_lineup_synergy_posterior.csv; uconn_lineup_coach_view.csv; uconn_lineup_decision_table.csv |
| `./_scripts/models/fit_lineup_defensive_leak_model.R` | other | 367 | Defense-only lineup model for leak-risk outputs. | uconn_stints_from_pbp.csv; uconn_games_meta.csv; opponent_controls.csv; uconn_lineup_gamelevel_defonly.stan; uconn_lineup_gamelevel_defonly_fit.rds | uconn_lineup_def_leaks_posterior.csv; uconn_lineup_def_leaks_model_diagnostics.csv |
| `./_scripts/models/fit_scheme_matchup_model.R` | other | 875 | Scheme matchup model for possession-level PPP. | possessions.csv; uconn_possessions.csv; scheme_tags.csv; uconn_scheme_tags.csv; events.csv; uconn_events.csv; games.csv; uconn_games.csv; teams.csv; uconn_teams.csv; lineups.csv; uconn_lineups.csv; uconn_scheme_matchup_ppp.stan; uconn_scheme_matchup_ppp_fit.rds | uconn_scheme_matchup_model_diagnostics.csv; uconn_scheme_matchup_cell_summary.csv; uconn_scheme_matchup_opponent_prep_table.csv; uconn_scheme_matchup_do_not_run.csv; uconn_scheme_matchup_lineup_recommendations.csv; uconn_scheme_matchup_calibration.csv; uconn_scheme_matchup_oos_lift.csv; uconn_scheme_matchup_model_meta.csv |
| `./_scripts/ops/cleanup_project.R` | other | 76 | !/usr/bin/env Rscript / / 1) Remove Finder metadata noise. / / 2) Detect archive duplicate-style filenames. | * 2.csv; _data/01_core_inputs/uconn_games_meta.csv |  |
| `./_scripts/pipeline/run_coaching_pipeline.R` | other | 611 | Runs the lineup, defense, and audit workflow. | organize_manifest.csv; uconn_stints_from_pbp.csv; uconn_games_meta.csv; opponent_controls.csv; _scripts/pipeline/run_rolling_lineup_decision_backtest.R; _scripts/analysis/evaluate_net_probability_calibration.R; _scripts/models/fit_core_lineup_model.R; _scripts/analysis/build_player_role_concentration_table.R; _scripts/analysis/analyze_player_role_stability_by_phase.R; _scripts/models/fit_lineup_defensive_leak_model.R; _scripts/analysis/build_defensive_leak_coach_table.R; _scripts/analysis/validate_defensive_leak_signal_holdout.R; _scripts/analysis/build_lineup_stability_baselines.R; _scripts/analysis/attribute_defensive_leaks_by_game.R; _scripts/analysis/build_game_level_defensive_trend.R; _scripts/pipeline/run_lineup_availability_stress_test.R; _scripts/analysis/audit_lineup_decision_eligibility.R; _outputs/01_lineup_core/uconn_lineup_coach_view.csv; _outputs/uconn_lineup_coach_view.csv; _outputs/05_decision_audit/uconn_lineup_decision_table.csv; _outputs/01_lineup_core/uconn_lineup_decision_table.csv; _outputs/uconn_lineup_decision_table.csv; _outputs/01_lineup_core/uconn_lineup_usage.csv; _outputs/uconn_lineup_usage.csv; _outputs/02_defense_leaks/uconn_lineup_def_leaks_posterior.csv; _outputs/uconn_lineup_def_leaks_posterior.csv; _outputs/03_players/uconn_player_rci_coach_table.csv; _outputs/uconn_player_rci_coach_table.csv; _outputs/05_decision_audit/uconn_lineup_decision_rolling_backtest_rows.csv; _outputs/uconn_lineup_decision_rolling_backtest_rows.csv; _outputs/05_decision_audit/uconn_pred_pr_net_pos_calibration_metrics.csv; _outputs/uconn_pred_pr_net_pos_calibration_metrics.csv; _outputs/05_decision_audit/uconn_decision_eligibility_by_stint.csv; _outputs/uconn_decision_eligibility_by_stint.csv |  |
| `./_scripts/pipeline/run_lineup_availability_stress_test.R` | other | 174 | Quick stress test: remove one player and surface the best remaining / / lineup options under current risk thresholds. / / Config (env overrides make this script reproducible in automation/CLI) | uconn_lineup_stabilizers.csv; uconn_lineup_def_leaks_posterior.csv; uconn_availability_stress_test_report.csv |  |
| `./_scripts/pipeline/run_rolling_lineup_decision_backtest.R` | other | 1447 | Rolling game-level holdout backtest for the lineup decision table. / / Trains on prior games only, scores the next game, and summarizes calibration / / plus realized outcomes by decision bucket. | uconn_stints_from_pbp.csv; uconn_games_meta.csv; opponent_controls.csv; uconn_lineup_gamelevel_offdef.stan; uconn_lineup_decision_rolling_backtest_rows.csv; uconn_lineup_decision_rolling_backtest_by_bucket.csv; uconn_lineup_decision_rolling_backtest_by_game_bucket.csv; uconn_lineup_decision_rolling_backtest_fit_diagnostics.csv; uconn_lineup_decision_rule_v2_thresholds.csv; uconn_pred_pr_net_pos_calibration_model.csv; uconn_lineup_bt_holdout_game_%02d_fit.rds |  |
| `./Notes/01 - Normal models for game outcomes/ability_normal_model-FINAL.R` | other | 207 | Regular season only: / / Fit linear model on 2024-25 season NFL games, / / considering only ability scores of teams. |  |  |
| `./Notes/02 - Count models for low-score games/ability_poisson_model-FINAL.R` | other | 277 | grab nhl_score_2024_raw.csv / / from https://www.hockey-reference.com/leagues/NHL_2025.html / / and select "schedule and results" | nhl_score_2024.csv |  |
| `./Notes/03 - Paired comparison models/ability_binary_model-FINAL.R` | other | 255 | Construct design matrix / / Construct W matrix to account for unidentifiability / / Total number of wins per team |  |  |
| `./Notes/04 - Regularizaton and Bayes/davidson-stan-FINAL.R` | other | 124 | Bayesian model on NHL data (with tie) / / Repeat from Poisson lecture notes R code / / grab nhl_score_2024_raw.csv | nhl_score_2024.csv; davidson_model_nohfa.stan; davidson_model_hfa.stan |  |
| `./Notes/04 - Regularizaton and Bayes/johnny-timmy.R` | other | 97 | the penalized log-likelihood as a function of the / / 2-element vector theta.vec / / control=list(fnscale=-1) searches for maximum of logLridge, |  |  |
| `./Notes/04 - Regularizaton and Bayes/regularization-FINAL.R` | other | 393 | https://mc-stan.org/cmdstanr/articles/cmdstanr.html / / Uncomment the next two lines to install cmdstanr / / install.packages("cmdstanr", | Install Cmdstan2.32.0 on MacOS Ventura.pdf; mlb_score_2022.csv; mlb-team-abbrevs.csv; nhl_score_2022.csv |  |
| `./Notes/04 - Regularizaton and Bayes/regularization-plus-stan-FINAL.R` | other | 352 | Fit Binary model on 2025 season MLB games, / / Bradley-Terry model / / Construct design matrix | bt_nohfa.stan; bt_hfa.stan |  |
| `./Notes/05 - Multicompetitor Models/ranking_golf_FINAL.R` | other | 128 | Billy Kilduff. (2021). PGA Tour Data Set - 2018 to 2021 [Data set]. / / https://doi.org/10.5281/zenodo.5235684 / / Multi-player game analysis on season 2019 golf tournament data | golf_2019.csv; golf-tournaments.csv; golf_2019_rankings.csv |  |
| `./Notes/05 - Multicompetitor Models/ranking_toy_example.R` | other | 42 | Construct design matrix / / Fit Gaussian model / / Players.ability["Normal.rankings"] = rank(Players.ability$Normal.ability.est) | toy_example_data.csv |  |
| `./Notes/06 - Dynamic models/dynamic_model-FINAL.R` | other | 100 | Fit Bradley-Terry model on 2003-2018 season NBA games, / / with exponential time-downweighting / / Remove playoff games, only consider games up to 2019 season due to pandemic | nba_games.csv; nba_startdate.csv; nba_teams.csv |  |
| `./Notes/07 - Rating systems/rating_system_nfl-FINAL.R` | other | 184 | Implement NFL team rating system using Elo and Glicko model / / Reading data / / Elo model |  |  |
| `./Notes/08 - Simulating tournaments/simulation-FINAL.R` | other | 384 | JAX vs LAC in 2022 NFL playoffs / / y ~ N(4.945, 11.73^2) / / Probability either team wins by more than 7 | nhl_score_2022.csv; nfl_divisions_abbr.csv |  |

### Stan Models

| Path | Kind | Lines | MD5 | Duplicate Status |
|---|---|---:|---|---|
| `./_models/uconn_lineup_gamelevel_defonly.stan` | other | 82 | `07487a52c14cca876046751a7a6bcdd3` | unique |
| `./_models/uconn_lineup_gamelevel_offdef.stan` | other | 103 | `f61b508838c6655a34d08ff20eb4b229` | unique |
| `./_models/uconn_scheme_matchup_ppp.stan` | other | 110 | `91dd1a60030de2652c60fdc2d6e05e9f` | unique |
| `./Notes/04 - Regularizaton and Bayes/bt_nohfa_basic_model.stan` | other | 21 | `c1d6385c2bec5fbd38667a8a27162a25` | unique |
| `./Notes/04 - Regularizaton and Bayes/davidson_model_hfa.stan` | other | 45 | `c534c256a6dec615d1b3d28fa1805258` | unique |


---


## System Logic and Thought Process

Rebuilt from active `.R` and `.stan` files on 2026-03-05 (historical snapshot section).

This is the practical reasoning behind the system design.

### 1) Problem Framing

The codebase treats lineup management as a repeated decision problem under uncertainty, not a highlight-reel argument.

The central thought process is:

1. descriptive stats are useful but not enough
2. predictions need uncertainty
3. predictions must be validated out-of-sample
4. decision labels should be calibrated to evidence quality

That is why the project invests heavily in backtesting, calibration, and eligibility gates before recommending action labels.

### 2) Why Bayesian Models Here

The code repeatedly applies partial pooling because lineup data is sparse and uneven.

The practical reason is simple:

1. many lineups have small samples
2. raw rates swing too hard in small samples
3. shrinkage keeps estimates usable until samples mature

The models preserve upside signal, but force low-sample caution.

### 3) Core Modeling Decisions

#### Core lineup model (`_scripts/models/fit_core_lineup_model.R` + `_models/uconn_lineup_gamelevel_offdef.stan`)

Main outcome:

- net points per possession at stint level (`net_ppp`), weighted by possessions

Structure:

1. player net effects (`alpha_net`)
2. lineup synergy residual (`u`)
3. opponent controls (`opp_adjO`, `opp_adjD`)
4. site effect (`home`)
5. game-state controls (score margin at stint start, elapsed game time)

Reasoning:

1. separate player value from lineup interaction value
2. keep context effects explicit so lineup effects are not confounded
3. use sum-to-zero constraints for identifiability and stable interpretation

#### Defense leak model (`_scripts/models/fit_lineup_defensive_leak_model.R` + `_models/uconn_lineup_gamelevel_defonly.stan`)

Main outcome:

- defensive points per possession (`points_against / poss_est`)

Signal of interest:

- lineup leak residual (`u_def`) and `pr_leak = P(u_def > 0)`

Reasoning:

1. isolate lineup defensive liability beyond player baseline and context
2. keep model aligned with coach language by turning posterior into risk probability

#### Scheme matchup model (`_scripts/models/fit_scheme_matchup_model.R` + `_models/uconn_scheme_matchup_ppp.stan`)

Status:

- separate from default coaching pipeline

Main outcome:

- possession PPP by action-coverage matchup with hierarchical pooling

Reasoning:

1. matchup exploitation needs action x coverage interaction estimates
2. sparse cells need pooling and minimum-support lumping
3. time split prevents leakage when generating prep recommendations

### 4) Why the Pipeline Runs in Its Current Order

The order in `run_coaching_pipeline.R` is intentional.

1. Run rolling backtest first.
2. Run calibration diagnostics second.
3. Fit core model after thresholds and calibration artifacts exist.
4. Build downstream role and defense layers after lineup baseline is stable.
5. Finish with eligibility and stress tests.

Practical logic:

1. decide reliability rules before producing decision language
2. avoid writing recommendations first and justifying them later

### 5) Reliability Doctrine in Code

#### Rolling backtest (`run_rolling_lineup_decision_backtest.R`)

The script uses prior-games-only training for each holdout game.

Key choices:

1. strict no-leakage time ordering
2. cached per-holdout fits with signature checks
3. posterior-predictive net-positive probability for holdout lineup outcomes
4. forward split for threshold tuning (tune early holdout, evaluate later holdout)

#### Calibration policy

Calibration is not accepted by default.

Platt calibration is only used if strict gates pass:

1. minimum rows and games
2. slope bounds
3. bounded ECE and decile gap
4. actual improvement over raw calibration

Fallback when gates fail:

- conservative shrink toward `0.5` (explicitly coded)

Thought process:

1. bad calibration is worse than no calibration
2. if evidence is weak, the model should become less extreme

### 6) Guardrail Layer (Small-Sample Discipline)

#### Stabilizers (`build_lineup_stability_baselines.R`)

Creates sample tiers and trust baseline shrinkage.

Core idea:

1. blend lineup signal with team baseline using possession-weighted prior
2. mark collapse risk under low-sample extremes or clearly negative medium/high-sample lineups

#### Eligibility audit (`audit_lineup_decision_eligibility.R`)

Labels each stint using only prior-game information.

Core idea:

1. no future leakage in eligibility
2. lineup must be games-qualified and not flagged collapse-risk in prior data

#### Availability stress test (`run_lineup_availability_stress_test.R`)

Simulates player removal and reports surviving stop-a-run options under risk filters.

Core idea:

1. pre-wire contingency decisions
2. avoid ad hoc panic substitutions under absence shocks

### 7) Defense Interpretation Layer

#### Coach table (`build_defensive_leak_coach_table.R`)

Transforms posterior estimates into operational labels:

1. confidence bands by effective sample and interval width
2. leak and plus labels by probability thresholds
3. explicit caution tags (`dont_overreact`)

Reasoning:

1. give staff a triage board, not only raw posterior columns
2. preserve uncertainty language so weak evidence is not over-sold

#### Holdout validation (`validate_defensive_leak_signal_holdout.R`)

Time-split validation checks whether risk buckets separate actual holdout defense outcomes.

Reasoning:

1. defensive labels should earn trust on unseen games
2. bucket-level separation and weighted Brier metrics provide that test

#### Attribution (`attribute_defensive_leaks_by_game.R`)

Converts leak posterior + stint usage into game-level culprit and repeat-offender files.

Reasoning:

1. move from global lineup risk to concrete game accountability
2. identify repeat patterns, not one-off blame

### 8) Player Role Stability Thought Process

Two scripts split this layer:

1. `build_player_role_concentration_table.R` for baseline RCI + coach table
2. `analyze_player_role_stability_by_phase.R` for early/mid/recent phase drift

Design logic:

1. role stability is concentration of usage, not just total minutes
2. recent-phase shift is prioritized over early-season shift
3. impact confirmation is secondary and sample-gated

### 9) Data Quality Thought Process

The orchestrator explicitly fails runs for stale joins, malformed dates, duplicate keys, weak sample size, and stale outputs.

This reflects a core value in the code:

1. no silent degradation
2. fail loud when evidence quality drops

### 10) Notes Layer

#### `Notes/`

The notes folder is a methods foundation, not production pipeline code.

It documents where model patterns came from:

1. normal/Poisson outcome models
2. paired comparison and regularization
3. dynamic models
4. rating systems
5. simulation workflows

### 11) One-Line Summary of the System

The project is designed to make lineup decisions harder to fake, easier to audit, and safer under uncertainty.


---


---


## Active Script Runbook

This runbook covers every active production script and active Stan model in `_scripts/` and `_models/`.


### Pipeline Entrypoint

#### `run_coaching_pipeline.sh`

1. switches to repo root
2. runs `_scripts/pipeline/run_coaching_pipeline.R`

#### `_scripts/pipeline/run_coaching_pipeline.R`

Purpose:

1. execute full coaching workflow in fixed order
2. enforce pre-QC and post-QC gates
3. organize outputs into bucket folders
4. write run logs

Pre-QC themes:

1. required files and columns
2. non-exhibition sample sufficiency
3. join integrity for opponent controls
4. date and clock parse quality

Post-QC themes:

1. output schema integrity
2. probability range checks
3. decision-label whitelist
4. backtest/calibration thresholds
5. stale-output guard by `game_id`

### Decision Backbone

#### `_scripts/pipeline/run_rolling_lineup_decision_backtest.R`

Purpose:

1. train on prior games only
2. score next game holdouts
3. evaluate bucket performance and calibration
4. tune decision thresholds using forward split

Key logic:

1. no leakage train/test ordering
2. per-holdout cached fits with signature checks
3. strict calibration gate for Platt usage
4. fallback shrink-to-0.5 when strict calibration fails
5. threshold grid search with feasibility rules

Outputs:

1. holdout rows
2. bucket and game-bucket summaries
3. fit diagnostics
4. tuned threshold file
5. calibration model file

#### `_scripts/analysis/evaluate_net_probability_calibration.R`

Purpose:

1. measure reliability of `pred_pr_net_pos`
2. write decile diagnostics and scalar metrics
3. generate calibration curve plot

Key logic:

1. possession-weighted calibration metrics
2. Brier, log-loss, ECE, and max decile gap
3. decile bins based on ordered predictions

### Core Lineup Model Layer

#### `_scripts/models/fit_core_lineup_model.R`

Purpose:

1. fit main Bayesian lineup model
2. produce player net posterior outputs
3. produce lineup synergy and decision tables

Key logic:

1. non-exhibition filtering
2. tiny-possession clamping
3. canonical lineup normalization
4. game-state reconstruction from stint timing
5. threshold precedence: env > tuned file > defaults
6. decision probability calibration using backtest artifact

Outputs:

1. lineup usage
2. lineup synergy posterior
3. coach view and decision table
4. core model diagnostics
5. player net posterior and ranking

#### `_models/uconn_lineup_gamelevel_offdef.stan`

Model role:

1. net PPP hierarchical model
2. player net effects plus lineup synergy
3. opponent and game-state controls
4. possession-weighted likelihood

### Player Role Layer

#### `_scripts/analysis/build_player_role_concentration_table.R`

Purpose:

1. compute baseline RCI diagnostics by player
2. merge with player net posterior
3. produce coach-facing role recommendation table

Key logic:

1. lineup-level possession concentration
2. role_type from RCI threshold
3. impact_type from posterior positivity thresholds
4. recommendation text from role x impact combinations

#### `_scripts/analysis/analyze_player_role_stability_by_phase.R`

Purpose:

1. track RCI drift across season phases
2. pair role drift with current-form impact check

Key logic:

1. phase windows based on `game_id`
2. phase 3 auto-extends to latest game
3. role signal prioritizes recent phase delta when sample supports it
4. impact labels are sample-gated

Output:

1. `uconn_player_rci_three_windows.csv`

### Defense Model Layer

#### `_scripts/models/fit_lineup_defensive_leak_model.R`

Purpose:

1. fit defense-only Bayesian lineup model
2. export lineup leak posterior table

Key logic:

1. target `points_against / poss_est`
2. canonical lineup mapping
3. opponent AdjO and game-state controls
4. cached fit validation via signature

Outputs:

1. defense leak posterior
2. defense model diagnostics

#### `_models/uconn_lineup_gamelevel_defonly.stan`

Model role:

1. hierarchical defense PPP model
2. lineup leak residual `u_def`
3. leak probability signal via posterior draw direction

#### `_scripts/analysis/build_defensive_leak_coach_table.R`

Purpose:

1. convert defense posterior into coach watchlist language

Key logic:

1. confidence bands use effective sample and interval width
2. risk/plus labels use probability thresholds
3. caution tags prevent overreaction under weak evidence

#### `_scripts/analysis/validate_defensive_leak_signal_holdout.R`

Purpose:

1. time-split holdout validation for defense leak signal

Key logic:

1. train/test split by game date sequence
2. train-only scaling to avoid leakage
3. risk bucket assignment on holdout rows
4. weighted calibration and Brier summaries

Outputs:

1. holdout validation rows
2. validation-by-bucket summary
3. validation meta and diagnostics

#### `_scripts/analysis/attribute_defensive_leaks_by_game.R`

Purpose:

1. attribute poor defense games to lineup contributors
2. detect repeat offenders

Key logic:

1. game x lineup aggregation
2. strict leak flags plus bad-game top-k flags
3. repeat-offender definitions with game-count filters

Outputs:

1. lineup attribution by game
2. primary culprit by game
3. repeat offenders

#### `_scripts/analysis/build_game_level_defensive_trend.R`

Purpose:

1. build game-by-game defense trend from lineup leak posterior

Key logic:

1. possession-weighted aggregation by game
2. pace-scaled conversion to per-40 framing
3. trend plot with uncertainty band

Outputs:

1. game-level defense trend CSV
2. game-level defense trend plot

### Guardrail Layer

#### `_scripts/analysis/build_lineup_stability_baselines.R`

Purpose:

1. rebuild lineup stabilizer table used by eligibility and stress logic

Key logic:

1. sample tiers by possession
2. games-played qualification
3. trust baseline shrinkage toward team average
4. collapse-risk rules

#### `_scripts/analysis/audit_lineup_decision_eligibility.R`

Purpose:

1. label every stint as eligible/ineligible using only prior-game information

Key logic:

1. leakage-safe prior-only stabilizer rebuild per game
2. eligibility based on games-qualified and non-collapse-risk status
3. explicit ineligibility reason field

#### `_scripts/pipeline/run_lineup_availability_stress_test.R`

Purpose:

1. simulate player-out scenario and rank surviving options

Key logic:

1. filters by sample tier and collapse-risk
2. leak-risk threshold gating
3. reports top survivors and lost options

### Data Engineering Utility Layer

#### `_scripts/analysis/generate_manual_game_csvs_from_espn.R`

Purpose:

1. generate manual game CSVs from ESPN play-by-play APIs

Key logic:

1. event-id lookup with date offsets and fuzzy opponent matching
2. play parsing for usage/assist/rebound/steal/block attribution
3. substitution-aware on-court lineup reconstruction
4. summary report of written/skipped/missing games

Output organization now lives inside `_scripts/pipeline/run_coaching_pipeline.R` so there is one active organizer path instead of a second standalone copy that can drift.

#### `_scripts/ops/cleanup_project.R`

Purpose:

1. clean Finder metadata noise
2. check metadata consistency and archive duplicate naming
3. report local environment directories

### Separate Scheme Model Layer

#### `_scripts/models/fit_scheme_matchup_model.R`

Purpose:

1. fit action-coverage matchup model for PPP
2. produce opponent prep and lineup recommendation tables

Key logic:

1. time split by date
2. sparse-level lumping by train support thresholds
3. hierarchical action, coverage, interaction, team, and lineup effects
4. out-of-sample calibration and lift reporting

Outputs:

1. cell summary
2. opponent prep table
3. do-not-run table
4. lineup recommendations
5. calibration and OOS lift tables
6. model meta and diagnostics

#### `_models/uconn_scheme_matchup_ppp.stan`

Model role:

1. hierarchical PPP model for action x coverage effects
2. team and lineup random effects
3. transition and shot-quality covariates


---


## Scripts Standard

Last updated: 2026-04-08

This is how scripts are organized and how I run them without breaking dependencies.

### 1) Folder Layout

- `pipeline/`: orchestration scripts and multi-stage runs
- `models/`: model fit scripts
- `analysis/`: post-fit tables, validation, calibration, attribution
- `ops/`: cleanup and output organization utilities
- `dashboard/`: optional local dashboard tooling (not part of default run order)

### 2) Main Entrypoint

Run full workflow from repo root:

```bash
bash run_coaching_pipeline.sh
```

That shell script runs:

- `_scripts/pipeline/run_coaching_pipeline.R`

### 3) Actual Pipeline Execution Order

This is the exact order in `run_coaching_pipeline.R`.

1. `_scripts/models/fit_lineup_defensive_leak_model.R`
2. `_scripts/analysis/build_uconn_lineup_shot_diet.R`
3. `_scripts/analysis/build_uconn_player_creation_profile.R`
4. `_scripts/pipeline/run_rolling_lineup_decision_backtest.R`
5. `_scripts/analysis/evaluate_net_probability_calibration.R`
6. `_scripts/models/fit_core_lineup_model.R`
7. `_scripts/analysis/build_player_role_concentration_table.R`
8. `_scripts/analysis/analyze_player_role_stability_by_phase.R`
9. `_scripts/analysis/build_defensive_leak_coach_table.R`
10. `_scripts/analysis/validate_defensive_leak_signal_holdout.R`
11. `_scripts/analysis/build_lineup_stability_baselines.R`
12. `_scripts/analysis/attribute_defensive_leaks_by_game.R`
13. `_scripts/analysis/build_game_level_defensive_trend.R`
14. `_scripts/pipeline/run_lineup_availability_stress_test.R`
15. `_scripts/analysis/audit_lineup_decision_eligibility.R`

The scheme matchup model is intentionally separate and not in this list.

### 4) Separate Script (Optional Project)

- `_scripts/models/fit_scheme_matchup_model.R`

Inputs live in:

- `_data/05_projects/scheme_matchup_project/`

### 5) Reliability Contract for Every Script

Each script should:

- resolve project root from its own path
- run from any cwd without hidden assumptions
- fail clearly on missing required files/columns
- write outputs to expected bucket folders
- avoid silent partial success

### 6) Notes-to-Scripts Logic Map

I use Notes modules as the logic source, then scripts as implementation.

- `Notes/01 - Normal models for game outcomes`: normal outcome framing
- `Notes/02 - Count models for low-score games`: count/rate framing for defense leakage style signals
- `Notes/03 - Paired comparison models`: paired-comparison probability framing
- `Notes/04 - Regularizaton and Bayes`: regularization and Bayesian uncertainty discipline
- `Notes/06 - Dynamic models`: time-aware validation and rolling splits
- `Notes/08 - Simulating tournaments`: scenario/stress-test thinking

### 7) Run + Cleanup Commands

Full pipeline:

```bash
bash run_coaching_pipeline.sh
```

Run everything (required + optional manifest entrypoints):

```bash
bash run_everything.sh
```

Cleanup only:

```bash
Rscript --vanilla _scripts/ops/cleanup_project.R
```

### 8) Practical Do/Do-Not

Do:

- rerun pipeline after logic edits
- check backtest and calibration outputs before changing thresholds
- keep script paths canonical to numbered `_data` folders

Do not:

- patch outputs manually to make tables "look right"
- reorder pipeline stages casually
- mix experimental scripts into main run order
