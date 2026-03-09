#!/usr/bin/env Rscript

.local_script_path <- local({
  script_arg <- grep("^--file=", commandArgs(), value = TRUE)
  if (length(script_arg) == 0) {
    stop("Could not determine script path. Run this file with Rscript.", call. = FALSE)
  }
  normalizePath(sub("^--file=", "", script_arg[[1]]), winslash = "/", mustWork = TRUE)
})
source(normalizePath(file.path(dirname(.local_script_path), "..", "utils", "bootstrap_project.R"), winslash = "/", mustWork = TRUE))
bootstrap_project(.local_script_path)
rm(.local_script_path)

# Runs the lineup, defense, and audit workflow.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(lubridate)
})

stopf <- function(fmt, ...) {
  stop(sprintf(fmt, ...), call. = FALSE)
}

assert_file <- function(path) {
  if (!file.exists(path)) stopf("Missing required file: %s", path)
}

assert_columns <- function(df, cols, label) {
  missing <- setdiff(cols, names(df))
  if (length(missing) > 0) {
    stopf("%s is missing required columns: %s", label, paste(missing, collapse = ", "))
  }
}

as_num <- function(x) suppressWarnings(as.numeric(x))
as_int <- function(x) suppressWarnings(as.integer(x))

parse_any_date <- function(x) {
  x <- str_trim(as.character(x))
  suppressWarnings(as.Date(parse_date_time(x, orders = c("m/d/y", "m/d/Y", "Y-m-d", "Y/m/d"))))
}

pick_existing <- function(label, candidates) {
  hit <- candidates[file.exists(candidates)]
  if (length(hit) == 0) {
    stopf(
      "Missing expected output for %s. Checked: %s",
      label,
      paste(candidates, collapse = ", ")
    )
  }
  hit[[1]]
}

pick_bucket <- function(base) {
  if (str_detect(base, "scheme_matchup|scheme_matchups|scheme_tags|matchup_exploitation")) return("06_scheme_matchups")

  if (str_detect(base, "decision_|eligibility_by_stint|rolling_backtest|threshold|pred_pr_net_pos_calibration|synergy_prob_calibration|availability_stress")) {
    return("05_decision_audit")
  }

  if (str_detect(base, "^uconn_player_.*\\.csv$")) return("03_players")

  if (str_detect(base, "^uconn_lineup_(usage|synergy_posterior|coach_view|decision_table)\\.csv$")) return("01_lineup_core")
  if (str_detect(base, "^uconn_lineup_core_model_diagnostics\\.csv$")) return("01_lineup_core")

  if (str_detect(base, "def_leak|def_leaks|defonly|defense_leaks")) return("02_defense_leaks")
  if (str_detect(base, "model_diagnostics|param_diagnostics|diagnostics")) return("02_defense_leaks")

  if (str_detect(base, "game_level|trend|bad_games|by_game|game_attribution")) return("04_games_trends")
  if (str_detect(base, "trend.*\\.png$|game_level.*\\.png$|by_game.*\\.png$")) return("04_games_trends")

  "01_lineup_core"
}

summarize_examples <- function(x, n = 3) {
  if (length(x) == 0) return("")
  paste(utils::head(x, n), collapse = "; ")
}

scan_manual_scout_release_issues <- function(root_dir) {
  if (!dir.exists(root_dir)) {
    return(list(
      checked = FALSE,
      absolute_path_refs = character(),
      duplicate_artifacts = character(),
      internal_chatter_refs = character()
    ))
  }

  md_files <- list.files(root_dir, pattern = "\\.md$", recursive = TRUE, full.names = TRUE)
  absolute_path_refs <- character()
  internal_chatter_refs <- character()

  for (path in md_files) {
    lines <- readLines(path, warn = FALSE)

    abs_hits <- grep("!\\[[^]]*\\]\\((/|file://|[A-Za-z]:[/\\\\])", lines, perl = TRUE)
    if (length(abs_hits) > 0) {
      absolute_path_refs <- c(
        absolute_path_refs,
        sprintf("%s:%d", path, abs_hits)
      )
    }

    chatter_hits <- grep("(?i)(duplicate .*ignored|ignored .*duplicate)", lines, perl = TRUE)
    if (length(chatter_hits) > 0) {
      internal_chatter_refs <- c(
        internal_chatter_refs,
        sprintf("%s:%d", path, chatter_hits)
      )
    }
  }

  duplicate_artifacts <- list.files(
    root_dir,
    pattern = " 2\\.(csv|md|png)$",
    recursive = TRUE,
    full.names = TRUE
  )

  list(
    checked = TRUE,
    absolute_path_refs = absolute_path_refs,
    duplicate_artifacts = duplicate_artifacts,
    internal_chatter_refs = internal_chatter_refs
  )
}

organize_top_level_outputs <- function(output_dir) {
  # We only move files from top-level _outputs into buckets.
  # Existing bucket contents are left intact on purpose.
  buckets <- c(
    "01_lineup_core",
    "02_defense_leaks",
    "03_players",
    "04_games_trends",
    "05_decision_audit",
    "06_scheme_matchups"
  )
  for (b in buckets) dir.create(file.path(output_dir, b), recursive = TRUE, showWarnings = FALSE)

  files <- list.files(output_dir, full.names = TRUE, recursive = FALSE)
  files <- files[file.info(files)$isdir == FALSE]
  files <- files[!basename(files) %in% c("organize_manifest.csv", ".DS_Store")]

  if (length(files) == 0) {
    return(data.frame(
      file = character(),
      from = character(),
      bucket = character(),
      to = character(),
      ok = logical(),
      stringsAsFactors = FALSE
    ))
  }

  manifest <- data.frame(
    file = basename(files),
    from = files,
    bucket = vapply(basename(files), pick_bucket, character(1)),
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      to = file.path(output_dir, bucket, file),
      ok = FALSE
    )

  for (i in seq_len(nrow(manifest))) {
    src <- manifest$from[[i]]
    dst <- manifest$to[[i]]
    if (file.exists(dst)) file.remove(dst)

    ok <- file.rename(src, dst)
    if (!ok) {
      ok_copy <- file.copy(src, dst, overwrite = TRUE)
      if (ok_copy) file.remove(src)
      ok <- isTRUE(ok_copy)
    }
    manifest$ok[[i]] <- ok
  }

  manifest
}

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
out_dir <- "_outputs"
log_dir <- file.path(out_dir, "_run_logs")
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

summary_path <- file.path(log_dir, sprintf("%s_summary.txt", timestamp))
pre_qc_path <- file.path(log_dir, sprintf("%s_qc_preflight.txt", timestamp))
post_qc_path <- file.path(log_dir, sprintf("%s_qc_postflight.txt", timestamp))

# Pre-run checks
message("=== PRE-QC: validating inputs ===")

stints_path <- file.path("_data", "01_core_inputs", "uconn_stints_from_pbp.csv")
games_path <- file.path("_data", "01_core_inputs", "uconn_games_meta.csv")
opp_path <- file.path("_data", "01_core_inputs", "opponent_controls.csv")

assert_file(stints_path)
assert_file(games_path)
assert_file(opp_path)

stints <- read_csv(
  stints_path,
  show_col_types = FALSE,
  col_types = cols(
    start_time = col_character(),
    end_time = col_character()
  )
)
games <- read_csv(games_path, show_col_types = FALSE)
opp <- read_csv(opp_path, show_col_types = FALSE)

assert_columns(
  stints,
  c(
    "game_file", "period", "stint_index", "start_time", "end_time", "uconn_is_home",
    "uconn_lineup", "points_for", "points_against", "poss_est", "lineup_size",
    "game_date", "game_id"
  ),
  "uconn_stints_from_pbp.csv"
)
assert_columns(games, c("game_file", "game_date", "opponent", "uconn_is_home"), "uconn_games_meta.csv")
assert_columns(opp, c("game_date", "opponent", "opp_adjO", "opp_adjD"), "opponent_controls.csv")

stints_qc <- stints %>%
  mutate(
    poss_est = as_num(poss_est),
    points_for = as_num(points_for),
    points_against = as_num(points_against),
    lineup_size = as_num(lineup_size),
    game_id = as_int(game_id),
    game_date_parsed = parse_any_date(game_date)
  )

games_qc <- games %>%
  mutate(
    game_date_norm = format(parse_any_date(game_date), "%m/%d/%y"),
    opponent_norm = str_trim(opponent)
  )

opp_qc <- opp %>%
  mutate(
    game_date_norm = format(parse_any_date(game_date), "%m/%d/%y"),
    opponent_norm = str_trim(opponent),
    opp_adjO = as_num(opp_adjO),
    opp_adjD = as_num(opp_adjD)
  )

n_stints <- nrow(stints_qc)
n_games <- nrow(games_qc)
n_opp <- nrow(opp_qc)

if (n_stints < 200) stopf("Pre-QC failed: stints rows too low (%d).", n_stints)
if (n_games < 10) stopf("Pre-QC failed: games rows too low (%d).", n_games)
if (n_opp < 10) stopf("Pre-QC failed: opponent controls rows too low (%d).", n_opp)

non_ex_stints <- stints_qc %>%
  filter(!str_detect(game_file, regex("Exhibition", ignore_case = TRUE)))
non_ex_games <- games_qc %>%
  filter(!str_detect(game_file, regex("Exhibition", ignore_case = TRUE)))

if (nrow(non_ex_stints) < 100) {
  stopf("Pre-QC failed: non-exhibition stint rows too low (%d).", nrow(non_ex_stints))
}
if (nrow(non_ex_games) < 10) {
  stopf("Pre-QC failed: non-exhibition games rows too low (%d).", nrow(non_ex_games))
}

model_eligible_rows <- non_ex_stints %>%
  filter(
    lineup_size == 5,
    is.finite(poss_est), poss_est > 0,
    is.finite(points_for),
    is.finite(points_against)
  ) %>%
  nrow()

if (model_eligible_rows < 100) {
  stopf(
    "Pre-QC failed: too few model-eligible non-exhibition stint rows (%d).",
    model_eligible_rows
  )
}

start_sec <- suppressWarnings(period_to_seconds(ms(non_ex_stints$start_time)))
time_parse_rate <- mean(is.finite(start_sec), na.rm = TRUE)
if (!is.finite(time_parse_rate) || time_parse_rate < 0.90) {
  stopf("Pre-QC failed: start_time parse rate too low (%.1f%%).", 100 * time_parse_rate)
}

if (any(is.na(non_ex_games$game_date_norm))) {
  stop("Pre-QC failed: non-exhibition games contain unparseable game_date values.", call. = FALSE)
}
if (any(is.na(opp_qc$game_date_norm))) {
  stop("Pre-QC failed: opponent_controls contains unparseable game_date values.", call. = FALSE)
}
if (any(!is.finite(opp_qc$opp_adjO)) || any(!is.finite(opp_qc$opp_adjD))) {
  stop("Pre-QC failed: opponent_controls has non-numeric opp_adjO/opp_adjD.", call. = FALSE)
}

dup_opp <- opp_qc %>%
  count(game_date_norm, opponent_norm, name = "n") %>%
  filter(n > 1)
if (nrow(dup_opp) > 0) {
  sample_dup <- dup_opp %>%
    head(5) %>%
    transmute(key = paste0(game_date_norm, " | ", opponent_norm, " (n=", n, ")")) %>%
    pull(key)
  stopf(
    "Pre-QC failed: duplicate opponent_controls keys found. Examples: %s",
    paste(sample_dup, collapse = "; ")
  )
}

missing_controls <- non_ex_games %>%
  left_join(
    opp_qc %>% select(game_date_norm, opponent_norm, opp_adjO, opp_adjD),
    by = c("game_date_norm", "opponent_norm")
  ) %>%
  filter(is.na(opp_adjO) | is.na(opp_adjD))
if (nrow(missing_controls) > 0) {
  sample_missing <- missing_controls %>%
    head(5) %>%
    transmute(key = paste0(game_date_norm, " | ", opponent_norm, " | ", game_file)) %>%
    pull(key)
  stopf(
    "Pre-QC failed: missing opponent_controls join for %d non-exhibition games. Examples: %s",
    nrow(missing_controls),
    paste(sample_missing, collapse = "; ")
  )
}

unknown_game_files <- non_ex_stints %>%
  distinct(game_file) %>%
  anti_join(non_ex_games %>% distinct(game_file), by = "game_file")
if (nrow(unknown_game_files) > 0) {
  sample_unknown <- unknown_game_files %>% head(5) %>% pull(game_file)
  stopf(
    "Pre-QC failed: stints contain game_file values missing from games_meta. Examples: %s",
    paste(sample_unknown, collapse = "; ")
  )
}

max_game_id <- max(stints_qc$game_id, na.rm = TRUE)
if (!is.finite(max_game_id) || max_game_id <= 0) {
  stop("Pre-QC failed: invalid game_id values in stints.", call. = FALSE)
}

latest_stint_date <- max(non_ex_stints$game_date_parsed, na.rm = TRUE)
latest_game_date <- max(parse_any_date(non_ex_games$game_date), na.rm = TRUE)
if (is.finite(as.numeric(latest_stint_date)) &&
    is.finite(as.numeric(latest_game_date)) &&
    latest_stint_date > latest_game_date) {
  stopf(
    "Pre-QC failed: stints latest non-ex date (%s) is newer than games_meta latest non-ex date (%s).",
    as.character(latest_stint_date),
    as.character(latest_game_date)
  )
}

pre_qc_lines <- c(
  sprintf("timestamp=%s", timestamp),
  sprintf("stints_rows=%d", n_stints),
  sprintf("games_rows=%d", n_games),
  sprintf("opponent_controls_rows=%d", n_opp),
  sprintf("non_exhibition_stints=%d", nrow(non_ex_stints)),
  sprintf("non_exhibition_games=%d", nrow(non_ex_games)),
  sprintf("model_eligible_non_ex_stints=%d", model_eligible_rows),
  sprintf("start_time_parse_rate=%.4f", time_parse_rate),
  sprintf("latest_non_ex_stint_date=%s", as.character(latest_stint_date)),
  sprintf("latest_non_ex_game_meta_date=%s", as.character(latest_game_date)),
  sprintf("max_game_id=%d", as.integer(max_game_id)),
  "result=PASS"
)
writeLines(pre_qc_lines, pre_qc_path)

message("Pre-QC passed.")

# Run scripts in a fixed order so downstream outputs are available when needed.
scripts <- c(
  "_scripts/pipeline/run_rolling_lineup_decision_backtest.R",
  "_scripts/analysis/evaluate_net_probability_calibration.R",
  "_scripts/models/fit_core_lineup_model.R",
  "_scripts/analysis/build_player_role_stability_table.R",
  "_scripts/analysis/analyze_player_role_stability_by_phase.R",
  "_scripts/models/fit_lineup_defensive_leak_model.R",
  "_scripts/analysis/build_defensive_leak_coach_table.R",
  "_scripts/analysis/validate_defensive_leak_signal_holdout.R",
  "_scripts/analysis/build_lineup_stability_baselines.R",
  "_scripts/analysis/attribute_defensive_leaks_by_game.R",
  "_scripts/analysis/build_game_level_defensive_trend.R",
  "_scripts/pipeline/run_lineup_availability_stress_test.R",
  "_scripts/analysis/audit_lineup_decision_eligibility.R"
)

for (s in scripts) assert_file(s)

run_one <- function(script_path) {
  script_name <- tools::file_path_sans_ext(basename(script_path))
  log_path <- file.path(log_dir, sprintf("%s_%s.log", timestamp, script_name))
  start <- Sys.time()
  exit_code <- system2(
    "Rscript",
    c("--vanilla", script_path),
    stdout = log_path,
    stderr = log_path
  )
  elapsed <- as.integer(round(difftime(Sys.time(), start, units = "secs")))
  list(
    script = script_path,
    log_path = log_path,
    exit_code = as.integer(exit_code),
    elapsed = elapsed,
    ok = identical(as.integer(exit_code), 0L)
  )
}

message("=== RUN: executing pipeline scripts (scheme excluded) ===")

run_results <- list()
for (s in scripts) {
  message("Running: ", s)
  res <- run_one(s)
  run_results[[length(run_results) + 1]] <- res
  if (!res$ok) break
}

summary_lines <- c(
  sprintf("OK\tPRE_QC\t0s\t%s", pre_qc_path),
  vapply(run_results, function(res) {
    if (isTRUE(res$ok)) {
      sprintf("OK\t%s\t%ss\t%s", res$script, res$elapsed, res$log_path)
    } else {
      sprintf("FAIL\t%s\texit=%s\t%ss\t%s", res$script, res$exit_code, res$elapsed, res$log_path)
    }
  }, character(1))
)
writeLines(summary_lines, summary_path)

last_res <- run_results[[length(run_results)]]
if (!isTRUE(last_res$ok)) {
  stopf(
    "Pipeline failed at %s (exit=%d). See log: %s",
    last_res$script,
    last_res$exit_code,
    last_res$log_path
  )
}

message("=== ORGANIZE: moving top-level _outputs files into buckets ===")
organize_manifest <- organize_top_level_outputs(out_dir)
organize_manifest_path <- file.path(out_dir, "organize_manifest.csv")
write_csv(organize_manifest, organize_manifest_path)

if (nrow(organize_manifest) > 0 && any(!organize_manifest$ok)) {
  failed <- organize_manifest %>% filter(!ok)
  fail_names <- failed %>% head(5) %>% pull(file)
  stopf(
    "Organize step failed for %d file(s). Examples: %s",
    nrow(failed),
    paste(fail_names, collapse = ", ")
  )
}

cat(sprintf("OK\tORGANIZE_OUTPUTS\t0s\t%s\n", organize_manifest_path), file = summary_path, append = TRUE)

# Check the coach-facing outputs before marking the run complete.
message("=== POST-QC: validating outputs ===")

coach_view_path <- pick_existing(
  "coach_view",
  c("_outputs/01_lineup_core/uconn_lineup_coach_view.csv", "_outputs/uconn_lineup_coach_view.csv")
)
decision_table_path <- pick_existing(
  "decision_table",
  c(
    "_outputs/05_decision_audit/uconn_lineup_decision_table.csv",
    "_outputs/01_lineup_core/uconn_lineup_decision_table.csv",
    "_outputs/uconn_lineup_decision_table.csv"
  )
)
usage_path <- pick_existing(
  "lineup_usage",
  c("_outputs/01_lineup_core/uconn_lineup_usage.csv", "_outputs/uconn_lineup_usage.csv")
)
leak_post_path <- pick_existing(
  "defense_leak_posterior",
  c("_outputs/02_defense_leaks/uconn_lineup_def_leaks_posterior.csv", "_outputs/uconn_lineup_def_leaks_posterior.csv")
)
rsi_coach_path <- pick_existing(
  "rsi_coach_table",
  c("_outputs/03_players/uconn_player_rsi_coach_table.csv", "_outputs/uconn_player_rsi_coach_table.csv")
)
bt_rows_path <- pick_existing(
  "decision_backtest_rows",
  c("_outputs/05_decision_audit/uconn_lineup_decision_rolling_backtest_rows.csv", "_outputs/uconn_lineup_decision_rolling_backtest_rows.csv")
)
cal_metrics_path <- pick_existing(
  "calibration_metrics",
  c("_outputs/05_decision_audit/uconn_pred_pr_net_pos_calibration_metrics.csv", "_outputs/uconn_pred_pr_net_pos_calibration_metrics.csv")
)
eligibility_path <- pick_existing(
  "decision_eligibility",
  c("_outputs/05_decision_audit/uconn_decision_eligibility_by_stint.csv", "_outputs/uconn_decision_eligibility_by_stint.csv")
)

coach <- read_csv(coach_view_path, show_col_types = FALSE)
decision <- read_csv(decision_table_path, show_col_types = FALSE)
usage <- read_csv(usage_path, show_col_types = FALSE)
leaks <- read_csv(leak_post_path, show_col_types = FALSE)
rsi <- read_csv(rsi_coach_path, show_col_types = FALSE)
bt_rows <- read_csv(bt_rows_path, show_col_types = FALSE)
cal_metrics <- read_csv(cal_metrics_path, show_col_types = FALSE)
elig <- read_csv(eligibility_path, show_col_types = FALSE)

assert_columns(
  coach,
  c("lineup_pretty", "possessions", "decision_pred_pr_net_pos", "decision_prob", "Decision"),
  "coach_view"
)
assert_columns(
  decision,
  c("lineup_pretty", "decision_pred_pr_net_pos", "decision_prob", "Decision"),
  "decision_table"
)
assert_columns(usage, c("uconn_lineup_canon", "possessions", "minutes"), "lineup_usage")
assert_columns(leaks, c("lineup", "u_def_mean", "pr_leak"), "defense_leak_posterior")
assert_columns(rsi, c("player", "RSI", "net_pr_pos", "recommendation"), "rsi_coach_table")
assert_columns(bt_rows, c("pred_pr_net_pos", "holdout_possessions", "observed_net_positive"), "decision_backtest_rows")
assert_columns(cal_metrics, c("metric", "value"), "calibration_metrics")
assert_columns(elig, c("game_id", "decision_eligible_prior"), "decision_eligibility")

if (nrow(coach) < 10) stopf("Post-QC failed: coach_view has too few rows (%d).", nrow(coach))
if (nrow(leaks) < 10) stopf("Post-QC failed: defense_leak_posterior has too few rows (%d).", nrow(leaks))
if (nrow(rsi) < 5) stopf("Post-QC failed: rsi_coach_table has too few rows (%d).", nrow(rsi))
if (nrow(bt_rows) < 100) stopf("Post-QC failed: backtest_rows has too few rows (%d).", nrow(bt_rows))

coach_prob <- as_num(coach$decision_prob)
coach_pr_net <- as_num(coach$decision_pred_pr_net_pos)
if (any(is.finite(coach_prob) & (coach_prob < 0 | coach_prob > 1))) {
  stop("Post-QC failed: coach_view decision_prob contains values outside [0,1].", call. = FALSE)
}
if (any(is.finite(coach_pr_net) & (coach_pr_net < 0 | coach_pr_net > 1))) {
  stop("Post-QC failed: coach_view decision_pred_pr_net_pos contains values outside [0,1].", call. = FALSE)
}

leak_prob <- as_num(leaks$pr_leak)
if (any(is.finite(leak_prob) & (leak_prob < 0 | leak_prob > 1))) {
  stop("Post-QC failed: defense leak pr_leak contains values outside [0,1].", call. = FALSE)
}

rsi_prob <- as_num(rsi$net_pr_pos)
if (any(is.finite(rsi_prob) & (rsi_prob < 0 | rsi_prob > 1))) {
  stop("Post-QC failed: RSI net_pr_pos contains values outside [0,1].", call. = FALSE)
}

if (anyDuplicated(coach$lineup_pretty) > 0) {
  stop("Post-QC failed: duplicate lineup_pretty values in coach_view.", call. = FALSE)
}

allowed_decisions <- c("PLAY MORE", "LEAN IN", "NEUTRAL", "LIMIT / WATCH", "TOO SMALL")
if (any(!decision$Decision %in% allowed_decisions)) {
  bad <- unique(decision$Decision[!decision$Decision %in% allowed_decisions])
  stopf("Post-QC failed: unexpected Decision labels: %s", paste(bad, collapse = ", "))
}

non_ex_games_n <- nrow(non_ex_games)
if (non_ex_games_n >= 5 && all(decision$Decision == "TOO SMALL")) {
  stop("Post-QC failed: all decision labels are TOO SMALL despite >=5 non-exhibition games.", call. = FALSE)
}

metric_value <- function(name) {
  out <- cal_metrics %>% filter(metric == name) %>% pull(value)
  if (length(out) == 0) return(NA_real_)
  as_num(out[[1]])
}

n_rows_used <- metric_value("n_rows_used")
sum_holdout_possessions <- metric_value("sum_holdout_possessions")
weighted_ece <- metric_value("weighted_ece_decile")

# Tunable gates: can be tightened/relaxed via environment variables.
qc_min_bt_rows <- as_num(Sys.getenv("QC_MIN_BACKTEST_ROWS", "150"))
qc_min_holdout_poss <- as_num(Sys.getenv("QC_MIN_HOLDOUT_POSSESSIONS", "500"))
qc_max_weighted_ece <- as_num(Sys.getenv("QC_MAX_WEIGHTED_ECE", "0.25"))
qc_min_lineups_30 <- as_num(Sys.getenv("QC_MIN_LINEUPS_30_POSS", "1"))

if (is.finite(n_rows_used) && is.finite(qc_min_bt_rows) && n_rows_used < qc_min_bt_rows) {
  stopf(
    "Post-QC failed: n_rows_used in calibration metrics (%.0f) is below threshold (%.0f).",
    n_rows_used, qc_min_bt_rows
  )
}
if (is.finite(sum_holdout_possessions) && is.finite(qc_min_holdout_poss) && sum_holdout_possessions < qc_min_holdout_poss) {
  stopf(
    "Post-QC failed: holdout possessions (%.2f) below threshold (%.2f).",
    sum_holdout_possessions, qc_min_holdout_poss
  )
}
if (is.finite(weighted_ece) && is.finite(qc_max_weighted_ece) && weighted_ece > qc_max_weighted_ece) {
  stopf(
    "Post-QC failed: weighted ECE (%.4f) exceeds threshold (%.4f).",
    weighted_ece, qc_max_weighted_ece
  )
}

lineups_30 <- sum(as_num(coach$possessions) >= 30, na.rm = TRUE)
if (is.finite(qc_min_lineups_30) && lineups_30 < qc_min_lineups_30) {
  stopf(
    "Post-QC failed: lineups with >=30 possessions (%d) below threshold (%.0f).",
    lineups_30, qc_min_lineups_30
  )
}

max_elig_game_id <- max(as_int(elig$game_id), na.rm = TRUE)
if (!is.finite(max_elig_game_id) || max_elig_game_id < max_game_id) {
  stopf(
    "Post-QC failed: decision eligibility output appears stale (max game_id=%d, expected >=%d).",
    as.integer(max_elig_game_id), as.integer(max_game_id)
  )
}

manual_scout_release <- scan_manual_scout_release_issues(
  file.path(out_dir, "07_opps", "manual_game_scouts")
)
manual_scout_release_status <- "SKIP"
if (isTRUE(manual_scout_release$checked)) {
  manual_scout_release_status <- "PASS"
  manual_scout_errors <- character()

  if (length(manual_scout_release$absolute_path_refs) > 0) {
    manual_scout_errors <- c(
      manual_scout_errors,
      sprintf(
        "markdown contains absolute local paths (%s)",
        summarize_examples(manual_scout_release$absolute_path_refs)
      )
    )
  }

  if (length(manual_scout_release$duplicate_artifacts) > 0) {
    manual_scout_errors <- c(
      manual_scout_errors,
      sprintf(
        "duplicate release artifacts found (%s)",
        summarize_examples(manual_scout_release$duplicate_artifacts)
      )
    )
  }

  if (length(manual_scout_release$internal_chatter_refs) > 0) {
    manual_scout_errors <- c(
      manual_scout_errors,
      sprintf(
        "staff-facing markdown still contains internal cleanup chatter (%s)",
        summarize_examples(manual_scout_release$internal_chatter_refs)
      )
    )
  }

  if (length(manual_scout_errors) > 0) {
    stop(
      paste(
        c(
          "Post-QC failed: manual scout release bundle issues detected.",
          paste0("- ", manual_scout_errors)
        ),
        collapse = "\n"
      ),
      call. = FALSE
    )
  }
}

post_qc_lines <- c(
  sprintf("timestamp=%s", timestamp),
  sprintf("coach_view_rows=%d", nrow(coach)),
  sprintf("decision_table_rows=%d", nrow(decision)),
  sprintf("lineup_usage_rows=%d", nrow(usage)),
  sprintf("defense_leak_rows=%d", nrow(leaks)),
  sprintf("rsi_rows=%d", nrow(rsi)),
  sprintf("backtest_rows=%d", nrow(bt_rows)),
  sprintf("lineups_with_30_plus_possessions=%d", lineups_30),
  sprintf("cal_n_rows_used=%.0f", n_rows_used),
  sprintf("cal_sum_holdout_possessions=%.2f", sum_holdout_possessions),
  sprintf("cal_weighted_ece_decile=%.6f", weighted_ece),
  sprintf("max_input_game_id=%d", as.integer(max_game_id)),
  sprintf("max_eligibility_game_id=%d", as.integer(max_elig_game_id)),
  sprintf("manual_scout_release_qc=%s", manual_scout_release_status),
  sprintf("coach_view_path=%s", coach_view_path),
  sprintf("decision_table_path=%s", decision_table_path),
  sprintf("leak_posterior_path=%s", leak_post_path),
  "result=PASS"
)
writeLines(post_qc_lines, post_qc_path)

cat(sprintf("OK\tPOST_QC\t0s\t%s\n", post_qc_path), file = summary_path, append = TRUE)

message("Post-QC passed.")
message("Pipeline complete.")
message("Summary: ", summary_path)
