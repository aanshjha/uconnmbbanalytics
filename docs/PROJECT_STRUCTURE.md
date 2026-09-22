# Project Structure and Naming

[Project home](../README.md) / [Documentation](README.md) / Project structure

This guide separates the current source-backed workflow from retained model research. Existing directory names, script entrypoints, and data field names remain stable so saved commands and dependent scripts continue to work.

## Repository Map

| Location | Purpose | Status |
|---|---|---|
| `README.md` | Project entrypoint and quick start | Current |
| `docs/` | Guides, methods, and release limits | Current |
| `docs/archive/` | Superseded findings and runbooks | Historical |
| `_data/00_source/espn/` | Cached source JSON and provenance | Current, private local files |
| `_data/01_core_inputs/` | Legacy game, stint, and player inputs | Preserved for audit; lineup attribution unresolved |
| `_data/02_derived_inputs/` | Canonical reconciled tables plus retained older derived tables | Use the `reconciled_*` files for the current workflow |
| `_data/03_manual_game_csv/` | Original manual event files used to identify covered games and audit duplicates | Preserved inputs |
| `_data/04_templates/` | Input templates | Supporting material |
| `_data/05_projects/` | Separate research project inputs | Research |
| `_scripts/ops/` | Source reconciliation and maintenance | Reconciliation is current; older utilities are retained |
| `_scripts/analysis/` | Forecast evaluation, staff review, and older analyses | Current Python workflow plus historical R work |
| `_scripts/dashboard/` | Website export and legacy Shiny implementation | See the [dashboard guide](STAFF_DASHBOARD.md) |
| `_scripts/models/`, `_models/` | R/Stan model code and local cached fits | Historical/research; lineup recommendations withheld |
| `_scripts/pipeline/` | Earlier R pipeline and backtest orchestration | Historical entrypoints are guarded |
| `_scripts/utils/` | Shared R input and model helpers | Retained and regression checked |
| `tests/`, `_scripts/tests/` | Python tests and R input-guard checks | Current verification |
| `_outputs/00_qc/` | Reconciliation reports and exclusions | Current checks |
| `_outputs/08_reconciled_evaluation/` | Matched pregame forecasts and errors | Current evaluation |
| `_outputs/08_staff_pilot/` | Evidence packets, assignment, and actual trial ledger | Current pilot |
| `_outputs/01_*` through `_outputs/07_*` | Previous lineup, player, defense, and scouting outputs | Historical, marked locally with `HISTORICAL_UNVALIDATED.txt`; not current validation evidence |
| `staff-dashboard/` | Separate staff website checkout | Local presentation project; see its guide |
| `Notes/` | Course notes and methods background | Reference material |

## Supported Workflow

Run commands from the repository root:

```bash
bash run_coaching_pipeline.sh
```

The sequence is:

1. `_scripts/ops/reconcile_source_data.py` rebuilds canonical events and checks game totals.
2. `_scripts/analysis/evaluate_pregame_defense.py` tests the same held-out games against two baselines.
3. `_scripts/analysis/postgame_defensive_review.py build --game-id 401812793` builds the Florida demonstration packet.
4. `_scripts/analysis/postgame_defensive_review.py summary` summarizes actual recorded human sessions.

`run_everything.sh` is a compatibility alias for this same workflow. Pass `--fetch` to retrieve missing source snapshots or `--refresh` to replace cached snapshots explicitly. Neither command starts a human trial. Direct legacy R analysis, modeling, pipeline, and batch-run entrypoints are guarded in the shared bootstrap; the source CSV generator, maintenance tools, and read-only historical Shiny view retain their existing paths.

## Data Names and Display Labels

Machine-readable contracts use their existing field names. Use the plain-language labels below in reports. Source game and play IDs must stay strings, including when they contain only digits.

| Field or value | Display label | Meaning |
|---|---|---|
| `game_id` | Source game ID | ESPN game identifier |
| `game_file` | Original game file | Name associated with the manual source input |
| `game_date` | Game date | Date in America/New_York, in `YYYY-MM-DD` format |
| `tipoff_utc` | Scheduled tipoff (UTC) | Source timestamp; it does not certify game completion time |
| `opponent` | Opponent | Source team display name |
| `site_type` | Game location | `home`, `away`, or `neutral`, from UConn's perspective |
| `uconn_points`, `opponent_points` | UConn final points; opponent final points | Final game totals |
| `score_reconciled` | Final scores verified | Final source scores agree with scoring-event totals |
| `event_stats_reconciled` | Team statistics verified | Event totals match the box-score comparisons |
| `score_timeline_reconciled` | Intermediate scoreboard verified | No intermediate scoreboard mismatches were detected; separate from final-score verification |
| `play_id` | Source play ID | Exact source event identifier |
| `event_order` | Event order | Canonical order by period, clock, then source sequence |
| `sequence_number` | Source sequence | Original source entry sequence; delayed entry can make it differ from game order |
| `period_number`, `clock_display_value` | Period; game clock | Source event locator |
| `team_id` | Event team ID | Team associated with the event; blank when the source has no team |
| `is_uconn_offense` | Event belongs to UConn | Historical field name for event ownership; a rebound or foul is not necessarily an offensive possession |
| `source_url`, `source_sha256` | Source URL; source content hash | Provenance of the cached response |
| `retrieved_at_utc` | Source retrieved (UTC) | Retrieval time, not proof of a historical pregame data vintage |
| `expanding_mean` | Prior-game average | Baseline using all eligible earlier games |
| `last_three_mean` | Last-three-game average | Baseline using the three most recent eligible earlier games |
| `pregame_ridge` | Pregame ridge model | Fixed regularized model using location and earlier scoring/conceding averages |
| `mae_points` | Mean absolute error (points) | Average absolute forecast error; lower is better |
| `rmse_points` | Root mean squared error (points) | Error metric that gives more weight to large misses; lower is better |
| `mean_error_points` | Mean forecast error (points) | Predicted opponent points minus actual opponent points |

## Basketball Statistics

These abbreviations are existing source/data contracts. Game-table columns use the `uconn_` or `opponent_` prefix and lowercase statistic names; event-table statistics retain uppercase names.

| Field | Label |
|---|---|
| `PTS` | Points |
| `FGA`, `FGM` | Field goals attempted; field goals made |
| `FTA`, `FTM` | Free throws attempted; free throws made |
| `FGA3`, `FGM3` | Three-pointers attempted; three-pointers made |
| `OREB`, `DREB` | Offensive rebounds; defensive rebounds |
| `TOV` | Turnovers |

The staff pilot uses `threes` for opponent three-point attempts, `close_shots` for source-labeled layups/dunks/tips, and `turnovers` for opponent turnovers. These categories do not establish shot location, coverage, defensive blame, or film confirmation. See [the pilot guide](STAFF_PILOT.md) for the reviewer assessment fields.

## Naming Rules for Changes

- Use descriptive `snake_case` for new Python functions and variables; add units to quantities such as `elapsed_seconds` or `mae_points`.
- Use `UPPER_SNAKE_CASE` for Python constants and preserve conventional basketball abbreviations in existing data contracts.
- Use readable titles and section headings in reports; expand abbreviations on first use.
- Keep a stable machine key separate from its display label. A wording improvement should not silently rename a CSV column, JSON key, CLI option, or source ID.
- Mark superseded reports as historical and link to the current guide. Keep original data and audit evidence intact.

## Verification

```bash
bash run_checks.sh
```

The runner parses shell, Python, R, Stan, and website JavaScript code; runs the Python and R regression suites; and rebuilds the current workflow in a temporary directory when private inputs are available. Use `--require-real-data` to fail on missing inputs or `--fixtures-only` to skip the rebuild. See [the check guide](../tests/README.md). The staff trial uses a separate actual-results ledger; tests must not add human results to it.
