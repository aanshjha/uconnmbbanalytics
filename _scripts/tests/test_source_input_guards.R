#!/usr/bin/env Rscript
# Regression checks for source identity, conflicting copies, and fabricated denominators.
suppressPackageStartupMessages({ library(dplyr); library(readr); library(stringr) })
source("_scripts/utils/manual_game_data.R")
source("_scripts/utils/core_input_repair.R")
source("_scripts/utils/bootstrap_project.R")

expect_error <- function(expr, pattern) {
  error <- tryCatch({ force(expr); NULL }, error = identity)
  stopifnot(inherits(error, "error"), grepl(pattern, conditionMessage(error)))
}

# Every archived modeling/recommendation entrypoint must fail before writing.
retired_paths <- c(
  "_scripts/models/fit_core_lineup_model.R",
  "_scripts/models/fit_lineup_defensive_leak_model.R",
  "_scripts/models/fit_scheme_matchup_model.R",
  "_scripts/analysis/build_manual_game_opponent_scouts.R",
  "_scripts/analysis/analyze_player_role_stability_by_phase.R",
  "_scripts/pipeline/run_lineup_availability_stress_test.R",
  "_scripts/ops/run_runnable_entrypoints.R"
)
for (path in retired_paths) expect_error(bootstrap_project(path), "Historical analysis is retired")
stopifnot(dir.exists(bootstrap_project("_scripts/dashboard/run_dashboard.R")))
stopifnot(dir.exists(bootstrap_project("_scripts/analysis/generate_manual_game_csvs_from_espn.R")))

# Load generator functions without executing its network/write entrypoint.
for (expr in parse("_scripts/analysis/generate_manual_game_csvs_from_espn.R")) {
  if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
      is.call(expr[[3]]) && identical(expr[[3]][[1]], as.name("function"))) eval(expr)
}

fixture_root <- tempfile("source-input-guards-")
dir.create(fixture_root)
fixtures <- tibble(game_id = "g1", play_id = c("401812793120250296", "401812793120250297"), PTS = c(2L, 3L))
events <- bind_rows(fixtures, fixtures[1, ])
clean <- deduplicate_source_events(events)
stopifnot(nrow(clean) == 2L, is.character(clean$play_id), attr(clean, "duplicate_rows_removed") == 1L)
events$PTS[3] <- 1L
expect_error(deduplicate_source_events(events), "conflicting duplicate")
expect_error(deduplicate_source_events(mutate(fixtures, play_id = as.numeric(play_id))), "unsafe numeric")
expect_error(deduplicate_source_events(mutate(fixtures, play_id = NA_character_)), "missing play_id")
stopifnot(infer_competition_bucket(tibble(matchup_header = "Florida (5-4,0-0 SEC) -vs- UConn (9-1,0-0 Big East)",
  uconn_is_home = FALSE)) == "non_conference")
stopifnot(is.na(infer_competition_bucket(tibble(matchup_header = "UConn (27-3,15-2 Big East) -vs- Marquette",
  uconn_is_home = FALSE))))

# Real Florida copies differ in folder metadata, not the event payload.
ensure_manual_csv_dirs(fixture_root)
florida_file <- "UConn vs Florida Neutral.csv"
florida_paths <- file.path("_data", "03_manual_game_csv", c("_conf", "_nc"), florida_file)
if (all(file.exists(florida_paths))) {
  for (bucket in c("_conf", "_nc")) {
    file.copy(file.path("_data", "03_manual_game_csv", bucket, florida_file), file.path(fixture_root, bucket, florida_file))
  }
  florida <- load_manual_games(fixture_root)
  stopifnot(nrow(florida) == 181L, attr(florida, "duplicate_rows_removed") == 181L,
    all(florida$source_copy_count == 2L), all(florida$source_bucket_conflict),
    all(is.na(florida$competition_bucket)), sum(florida$PTS) == 150,
    !anyDuplicated(florida[c("game_id", "play_id")]))
} else {
  message("Local Florida input files unavailable; skipping optional real-data regression.")
}

# No invalid denominator may be inferred from points, even during rewrite.
stints_path <- file.path(fixture_root, "stints.csv")
report_dir <- file.path(fixture_root, "reports")
dir.create(report_dir)
stints <- tibble(game_file = "fixture.pdf", period = "1", stint_index = 1:3,
  start_time = c("20:00", "19:00", "18:00"), end_time = c("19:00", "18:00", "17:00"),
  uconn_lineup = "E|D|C|B|A", points_for = c(8, 0, 3), points_against = c(2, 0, 0),
  poss_est = c(0, NA_real_, 3), lineup_size = 5, net_pts = c(6, 0, 3), net_ppp = c(Inf, NA, 1))
write_csv(stints, stints_path)
result <- repair_uconn_stints_core_input(stints_path, file.path(fixture_root, "backup"), report_dir, rewrite = TRUE)
repaired <- read_csv(stints_path, show_col_types = FALSE)
stopifnot(result$quarantined_rows == 2L, file.exists(result$backup_path),
  all(is.na(repaired$poss_est[1:2])), all(is.na(repaired$net_ppp[1:2])),
  identical(repaired$analysis_eligible, c(FALSE, FALSE, TRUE)), repaired$poss_est[3] == 3)
expect_error(repair_uconn_stints_core_input(stints_path, report_dir = report_dir, rewrite = TRUE, backup = FALSE), "requires backup")

# Invalid stored rates must not disappear from the invariant mismatch count.
rate_fixture <- stints[rep(3, 4), ]
rate_fixture$net_ppp <- c(Inf, NA_real_, 99, 1)
rate_path <- file.path(fixture_root, "invalid_rates.csv")
write_csv(rate_fixture, rate_path)
stopifnot(validate_uconn_stints_invariants(rate_path)$net_ppp_mismatch == 3L)

# Exercise the role-analysis preparation without running its report entrypoint.
for (expr in parse("_scripts/analysis/analyze_player_role_stability_by_phase.R")) {
  if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
      identical(expr[[2]], as.name("prepare_role_stints"))) eval(expr)
}
role_fixture <- tibble(points_for = c(3, 0), points_against = c(1, 2),
                       poss_est = c(2, 0.5), net_ppp = 999)
stopifnot(identical(prepare_role_stints(role_fixture)$net_ppp, c(1, -4)))
expect_error(prepare_role_stints(select(role_fixture, -poss_est)), "cannot be inferred")
for (bad_possessions in c(0, -1, NA_real_, Inf, NaN)) {
  invalid_role <- role_fixture
  invalid_role$poss_est[1] <- bad_possessions
  expect_error(prepare_role_stints(invalid_role), "Invalid source stint field")
}
expect_error(prepare_role_stints(mutate(role_fixture, analysis_eligible = FALSE)), "source repair")
expect_error(prepare_role_stints(mutate(role_fixture, source_repair_required = TRUE)), "source repair")

# An old positive denominator created from points is also quarantined.
legacy <- stints[3, c("game_file", "period", "stint_index", "start_time", "end_time")]
legacy$new_poss_est <- 3
legacy$reason_codes <- "poss_est_invalid_repaired_from_points"
write_csv(legacy, file.path(report_dir, "uconn_stints_core_input_repair_report_legacy.csv"))
legacy_result <- repair_uconn_stints_core_input(stints_path, report_dir = report_dir)
stopifnot(legacy_result$historical_imputed_rows == 1L, legacy_result$quarantined_rows == 3L)

# Full generation: duplicate source rows cannot inflate totals; made free throws
# cannot receive rebounds; steals must belong to the opposing team.
pbp <- tibble(game_id = "g1", id = paste0("40181279312025029", 1:8),
  sequence_number = 1:8, period_number = 1L,
  type_text = c("JumpShot", "Defensive Rebound", "MadeFreeThrow", "Defensive Rebound", "Turnover", "Steal", "Steal", "JumpShot"),
  text = c("A misses Jumper.", "B Defensive Rebound.", "B makes Free Throw.", "A Defensive Rebound.", "A turnover.", "A Steal.", "B Steal.", "B makes Three Point Jumper."),
  team_id = c("1", "2", "2", "1", "1", "1", "2", "2"),
  shooting_play = c(TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE),
  scoring_play = c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE),
  points_attempted = c(2, NA, 1, NA, NA, NA, NA, 3), score_value = c(0, 0, 1, 0, 0, 0, 0, 3),
  clock_display_value = c("18:00", "18:00", "17:00", "17:00", "16:00", "16:00", "16:00", "15:00"),
  home_team_id = "1", away_team_id = "2", home_team_full_name = "UConn", away_team_full_name = "Opponent",
  home_score = 0, away_score = c(0, 0, 1, 1, 1, 1, 1, 4),
  game_date = "2026-01-01", season_type = 2L, type_id = "1", athlete_id_1 = "1", athlete_id_2 = NA_character_,
  short_description = "", period_display_value = "1st Half", coordinate_x_raw = 0, coordinate_y_raw = 0,
  coordinate_x = 30, coordinate_y = 0)
generated <- build_manual_table(bind_rows(pbp, pbp[1, ]), tibble())
stopifnot(nrow(generated) == 4L, sum(generated$PTS) == 4L, sum(generated$DREB) == 1L,
  sum(generated$STL) == 1L, sum(generated$FTA) == 1L, sum(generated$FGA) == 2L,
  all(is.na(generated$OffenseOnCourt)), all(!generated$lineup_source_verified))
complete_roster <- tibble(team_id = rep(c("1", "2"), each = 5),
  full_name = c("A", "C", "D", "E", "F", "B", "G", "H", "I", "J"),
  starter = TRUE, did_not_play = FALSE)
provisional <- build_manual_table(pbp, complete_roster)
stopifnot(all(!is.na(provisional$OffenseOnCourt)),
  all(!provisional$lineup_source_verified),
  all(provisional$lineup_source_status == "provisional_starters_and_substitutions_unverified"))
pbp_conflict <- bind_rows(pbp, mutate(pbp[1, ], score_value = 2))
expect_error(build_manual_table(pbp_conflict, tibble()), "conflicting duplicate")

roster <- tibble(team_id = "1", full_name = LETTERS[1:6], starter = c(rep(TRUE, 4), FALSE, FALSE), did_not_play = FALSE)
stopifnot(length(init_team_lineup(roster, "1")) == 0L)
roster$starter[5] <- TRUE
stopifnot(length(init_team_lineup(roster, "1")) == 5L)
unlink(fixture_root, recursive = TRUE)
cat("Source input guards passed: exact IDs, cross-bucket dedupe/conflicts, backups/quarantine, event counts, lineup uncertainty.\n")
