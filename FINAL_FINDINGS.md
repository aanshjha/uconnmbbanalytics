# Final Findings Report

Source run: `20260408_154948`
Audience: Head coach and evaluators
Scope: Completed, QC-passed findings only

## Executive Summary

The latest completed workflow passed postflight QC with `result=PASS`. The completed output set covers 126 lineup rows, 14 player rows, 32 game-level defensive trend rows, 415 validation rows, and 35 canonical scout report entries covering 32 unique game keys.

The strongest completed findings are:

- The lineup layer has 14 lineups with full sample status. Most of the 126 lineup rows remain low sample, so broad lineup-level claims are limited to the stable subset.
- The most-used stable lineup was `Ball Solo|Demary Jr.,Silas|Karaban Alex|Mullins Braylon|Reed Jr.,Tarris`, with 560 possessions, 289.8 minutes, 23 games, and a raw net PPP of `0.180`.
- The top stable raw net PPP row was `Demary Jr.,Silas|Karaban Alex|Mullins Braylon|Reed Jr.,Tarris|Stewart Jaylin`, with 66 possessions, 33.0 minutes, 11 games, and raw net PPP of `0.534`.
- Defensive leak outputs found 4 `LEAK RISK (LEAN)` rows, 19 `WATCH (LEAK SIGNAL)` rows, 18 `WATCH (DEF PLUS SIGNAL)` rows, and 85 inconclusive rows.
- Player outputs show `Demary Jr.,Silas` with the highest player net mean (`0.044`) and highest positive net probability (`0.868`) among the 14-player table.
- Manual scout outputs produced 35 canonical report entries, 32 unique game keys, 19 opponents, and report summaries for every manifest row.

## Completed Work

Completed artifacts included the core coaching pipeline, manual opponent scouts, and ESPN/manual game CSV generation. The latest manifest run marked each entry point as `OK` with exit code `0`.

Completed row counts:

- Lineup coach view: 126 rows.
- Stable lineup shot diet: 13 rows.
- Defensive leak table: 126 rows.
- Defensive repeat table: 4 rows.
- Game-level defensive trend: 32 rows.
- Player RCI table: 14 rows.
- Player creation table: 14 rows.
- Validation/backtest rows: 415 rows.
- Manual scout manifest: 35 rows.

Source files:

- `_outputs/_run_logs/20260408_154948_qc_postflight.txt`
- `_outputs/_run_logs/runnable_entrypoints_20260408_154948_summary.csv`
- `_outputs/01_lineup_core/uconn_lineup_coach_view.csv`
- `_outputs/02_defense_leaks/uconn_lineup_def_leaks_coach_table.csv`
- `_outputs/03_players/uconn_player_rci_coach_table.csv`
- `_outputs/07_opps/manual_game_scouts/manual_game_scout_manifest.csv`

## Lineup Findings

Only 14 of 126 lineup rows carried full `ok` sample status. The remaining 112 rows were marked `low_sample`, so they are not treated as final high-confidence lineup findings.

Within the stable subset:

- Highest volume: `Ball Solo|Demary Jr.,Silas|Karaban Alex|Mullins Braylon|Reed Jr.,Tarris` logged 560 possessions across 23 games, with raw net PPP `0.180` and model synergy mean `0.004`.
- Highest raw net PPP: `Demary Jr.,Silas|Karaban Alex|Mullins Braylon|Reed Jr.,Tarris|Stewart Jaylin` logged raw net PPP `0.534` over 66 possessions.
- Highest model synergy mean: the same `Demary Jr.,Silas|Karaban Alex|Mullins Braylon|Reed Jr.,Tarris|Stewart Jaylin` row had synergy mean `0.012`, with interval `-0.032` to `0.080`.
- Stable shot diet output contained 13 lineups. The highest rim-plus-three share was `0.780` for `Alex Karaban | Braylon Mullins | Eric Reibe | Jayden Ross | Malachi Smith`, over 50 FGA.

Source files:

- `_outputs/01_lineup_core/uconn_lineup_coach_view.csv`
- `_outputs/01_lineup_core/uconn_lineup_shot_diet_stable.csv`

## Pairing Findings

The two-player defensive pairing table adds a smaller, more specific view than five-man lineups. It tracks defensive events and stops by player pair.

Stronger stop-rate signals:

- `Dwayne Koroma | Jaylin Stewart`: 81 defensive events, 21 stops, `25.9%` raw stop rate, `+0.062` above baseline. Medium sample.
- `Dwayne Koroma | Jayden Ross`: 107 defensive events, 26 stops, `24.3%` raw stop rate, `+0.062` above baseline. Medium sample.
- `Jayden Ross | Jaylin Stewart`: 271 defensive events, 50 stops, `18.5%` raw stop rate, `+0.038` above baseline. High sample.
- `Jayden Ross | Malachi Smith`: 587 defensive events, 99 stops, `16.9%` raw stop rate, `+0.029` above baseline. High sample.

Lower stop-rate signals among high-sample pairs:

- `Jayden Ross | Tarris Reed Jr.`: 518 defensive events, 62 stops, `12.0%` raw stop rate, `-0.014` below baseline.
- `Tarris Reed Jr. | Malachi Smith`: 409 defensive events, 49 stops, `12.0%` raw stop rate, `-0.013` below baseline.
- `Alex Karaban | Solo Ball`: 1,688 defensive events, 207 stops, `12.3%` raw stop rate, `-0.012` below baseline.

Source file:

- `_outputs/01_lineup_core/uconn_defensive_two_man_pairs.csv`

## Defensive Findings

The defensive leak table contains 126 lineup rows. Confidence remains limited across most rows: 1 high-confidence row, 9 medium-confidence rows, and 116 insufficient-evidence rows.

Key defensive patterns:

- The highest leak probability row was `Ball Solo|Demary Jr.,Silas|Karaban Alex|Reed Jr.,Tarris|Ross Jayden`, with 96 possessions, `pr_leak=0.826`, and expected points allowed per 40 of `5.6`. This row was also marked insufficient evidence.
- The highest-volume leak-risk row was `Ball Solo|Demary Jr.,Silas|Karaban Alex|Mullins Braylon|Reed Jr.,Tarris`, with 560 possessions, `pr_leak=0.609`, and high confidence.
- Game-level defensive leak trend covered games from `2025-11-07` through `2026-03-14`.
- The highest game-level defensive leak mean was Seton Hall Home on `2026-02-28`, with `def_leak_mean=0.0227`.
- The lowest game-level defensive leak mean was DePaul Away on `2025-12-21`, with `def_leak_mean=-0.0070`.

Source files:

- `_outputs/02_defense_leaks/uconn_lineup_def_leaks_coach_table.csv`
- `_outputs/02_defense_leaks/uconn_def_leak_repeat_offenders.csv`
- `_outputs/02_defense_leaks/uconn_game_level_def_leak_trend.csv`

## Game-by-Game Defensive Stress Findings

The game-by-game defensive table shows where specific lineup stretches were most connected to defensive stress. It covered 32 games, with 7 games marked as poor defensive games in this table.

Highest points allowed per possession in a flagged lineup stretch:

- St. Johns Away on `2026-02-05`: `Ball Solo | Silas Demary Jr. | Alex Karaban | Braylon Mullins | Tarris Reed Jr.` allowed 41 points over 34 possessions, or `1.141` points allowed per possession.
- Providence Away on `2026-01-07`: the same group allowed 37 points over 34 possessions, or `1.140` points allowed per possession.
- Butler Away on `2026-02-11`: the same group allowed 25 points over 24 possessions, or `1.138` points allowed per possession.

Largest estimated defensive stress contribution:

- Seton Hall Home on `2026-02-28`: `Ball Solo | Silas Demary Jr. | Alex Karaban | Tarris Reed Jr. | Jayden Ross` had the largest estimated stress contribution in the file.
- St. Johns Away BET Final on `2026-03-14`, Providence Home on `2026-01-27`, Villanova Home on `2026-01-24`, and Marquette Away on `2026-03-07` all showed the same five-man group near the top of the game-level stress table.

Source files:

- `_outputs/02_defense_leaks/uconn_def_leak_primary_culprit_by_game.csv`
- `_outputs/02_defense_leaks/uconn_def_leak_lineups_by_game.csv`

## Player Findings

The player layer contains 14 players. Player creation rows include 11 `ok` sample rows and 3 small-sample rows.

Player net findings:

- `Demary Jr.,Silas` had the highest net mean (`0.044`) and highest positive net probability (`0.868`) across 1,748 possessions.
- `Ross Jayden` had the second-highest net mean (`0.034`) across 998 possessions.
- `Reed Jr.,Tarris` had the highest RCI value (`0.1910`) across 1,382 possessions and 36 unique lineups.

Creation findings:

- `Silas Demary Jr` led created scoring actions with 303, along with 209 recorded assists, 87 turnovers, and a `2.40` assist-to-turnover ratio.
- `Malachi Smith` had 120 created scoring actions, 96 recorded assists, 31 turnovers, and a `3.10` assist-to-turnover ratio.
- `Solo Ball` led player points in the creation profile with 462 points and had the highest UConn FGA share (`0.183`) among the top output rows.
- `Tarris Reed Jr` posted the highest FG% among the major-volume scoring rows shown, at `0.611` on 280 FGA.

Source files:

- `_outputs/03_players/uconn_player_rci_coach_table.csv`
- `_outputs/03_players/uconn_player_creation_profile.csv`

## Player Role Movement Findings

The three-window player table splits the season into early, middle, and late phases. It adds context for whether a player's role became more settled or more variable as the season moved forward.

Role movement patterns:

- 5 players showed a more settled recent role pattern.
- 2 players were flat recently.
- 2 players were more variable recently.
- 5 players did not have enough sample for a clean role-movement read.

Players with the clearest late-season role settling:

- `Braylon Mullins`: role consistency rose from `0.097` in the middle phase to `0.205` in the late phase, with 299 current-phase possessions.
- `Solo Ball`: role consistency rose from `0.097` to `0.185`, with 235 current-phase possessions.
- `Silas Demary Jr.`: role consistency rose from `0.091` to `0.170`, with 283 current-phase possessions.
- `Alex Karaban`: role consistency rose from `0.066` to `0.144`, with 323 current-phase possessions.
- `Tarris Reed Jr.`: role consistency rose from `0.145` to `0.219`, with 252 current-phase possessions.

Current-phase net scoring-margin leaders in that file:

- `Malachi Smith`: `+0.086` net points per possession over 163 current-phase possessions.
- `Solo Ball`: `+0.085` over 235 current-phase possessions.
- `Tarris Reed Jr.`: `+0.063` over 252 current-phase possessions.
- `Jayden Ross`: `+0.063` over 238 current-phase possessions.

Source file:

- `_outputs/03_players/uconn_player_rci_three_windows.csv`

## Validation Findings

Validation outputs indicate the completed workflow was checked rather than only generated.

Model reliability findings:

- The probability validation file contains 415 rows across 27 games and 2,073 holdout possessions.
- No rows were dropped in the calibration file.
- Weighted observed positive rate was `0.565`, compared with weighted mean predicted probability `0.394`.
- Weighted Brier score was `0.266`; weighted log loss was `0.726`.
- Defensive leak holdout validation covered 25 training games, 7 test games, 626 training rows, and 156 test rows.
- Defensive leak holdout weighted observed leaky rate was `0.405`, with weighted predicted leak mean `0.494`.

These validation results support inclusion as completed findings, while also showing that probability estimates were imperfect and require cautious interpretation.

Source files:

- `_outputs/05_decision_audit/uconn_pred_pr_net_pos_calibration_metrics.csv`
- `_outputs/05_decision_audit/uconn_lineup_decision_rolling_backtest_rows.csv`
- `_outputs/02_defense_leaks/uconn_def_leaks_holdout_validation_meta.csv`
- `_outputs/02_defense_leaks/uconn_def_leaks_holdout_validation_by_bucket.csv`

## Manual Scout Findings

Manual scout coverage completed with 35 canonical manifest rows, 32 unique game keys, 19 opponents, and report summaries present for every manifest row.

Coverage:

- Conference rows: 22.
- Non-conference rows: 13.
- Date range: `2025-11-07` through `2026-03-14`.

Notable tracked-game findings from unique game keys:

- Florida neutral on `2025-12-09` had the highest opponent points total in the scout manifest: 146 points on 118 FGA.
- Marquette away on `2026-03-07` had the highest opponent turnover total: 26 turnovers, including 14 live-ball turnovers.
- Illinois neutral on `2025-11-28` had the highest opponent 3PA total: 58 attempts.
- UConn’s highest tracked points against an opponent defense in the manifest came against Florida neutral on `2025-12-09`: 154 points on 116 FGA.

Source file:

- `_outputs/07_opps/manual_game_scouts/manual_game_scout_manifest.csv`

## Excluded From Final Findings

The following were not included as final findings:

- Scheme matchup outputs, because the model produced skip-status files with `0` modeled possessions after filtering.
- Unselfish offense manual files, because that folder contains a template/manual workflow rather than completed findings.
- Duplicate scout files with ` 2` in the filename.
- Notes, caches, `.DS_Store`, RStudio state, virtual environments, and git metadata.

## Limitations

- Lineup evidence is sample-limited: 112 of 126 lineup rows are low sample.
- Defensive leak evidence is confidence-limited: 116 of 126 defensive rows are insufficient evidence.
- Manual scout outputs are built from manually tracked game CSVs, not full possession-level play-by-play.
- Validation metrics show meaningful calibration gaps, so probability values are best read as model-estimated signals rather than exact truth.
