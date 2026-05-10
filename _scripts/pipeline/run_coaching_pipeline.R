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

source("_scripts/utils/output_cleanup.R")

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

normalize_player_token_local <- function(x) {
  x <- as.character(x)
  vapply(x, function(tok) {
    tok <- str_squish(tok)
    if (grepl(",", tok, fixed = TRUE)) {
      parts <- str_split(tok, ",", simplify = TRUE)
      if (ncol(parts) >= 2) {
        lhs <- str_trim(parts[1])
        rhs <- str_trim(parts[2])
        if (nzchar(lhs) && nzchar(rhs)) tok <- paste(rhs, lhs)
      }
    }
    tok <- tolower(gsub("[^[:alnum:] ]", " ", tok, perl = TRUE))
    pieces <- str_split(tok, "\\s+")[[1]]
    pieces <- pieces[nzchar(pieces)]
    if (length(pieces) == 0) return("")
    paste(sort(pieces), collapse = "")
  }, character(1))
}

lineup_key_norm_local <- function(x) {
  x <- as.character(x)
  vapply(x, function(lineup) {
    toks <- str_split(lineup, "\\|")[[1]]
    toks <- str_trim(toks)
    toks <- toks[nzchar(toks)]
    if (length(toks) == 0) return(NA_character_)
    norm <- normalize_player_token_local(toks)
    norm <- sort(unique(norm[nzchar(norm)]))
    if (length(norm) == 0) return(NA_character_)
    paste(norm, collapse = "|")
  }, character(1))
}

pick_existing <- function(label, candidates, required = TRUE) {
  hit <- candidates[file.exists(candidates)]
  if (length(hit) == 0) {
    if (!isTRUE(required)) return(NULL)
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
  if (str_detect(base, "^uconn_lineup_decision_board\\.csv$")) return("01_lineup_core")

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
archetype_path <- file.path("_data", "01_core_inputs", "player_archetypes.csv")
manual_csv_root <- file.path("_data", "03_manual_game_csv")

assert_file(stints_path)
assert_file(games_path)
assert_file(opp_path)
assert_file(archetype_path)

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

arche <- read_csv(archetype_path, show_col_types = FALSE)
assert_columns(arche, c("player", "archetype"), "player_archetypes.csv")
arche <- arche %>%
  mutate(
    player = str_trim(as.character(player)),
    archetype = toupper(str_trim(as.character(archetype)))
  )
if (nrow(arche) == 0) {
  stop("Pre-QC failed: player_archetypes.csv is empty.", call. = FALSE)
}
bad_arche <- arche %>% filter(!(archetype %in% c("GUARD", "WING", "BIG")))
if (nrow(bad_arche) > 0) {
  stopf(
    "Pre-QC failed: invalid archetype values found. Examples: %s",
    paste(head(unique(bad_arche$archetype), 5), collapse = ", ")
  )
}

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

active_players <- non_ex_stints %>%
  mutate(player_vec = str_split(as.character(uconn_lineup), "\\|")) %>%
  select(player_vec) %>%
  tidyr::unnest(player_vec) %>%
  transmute(player = str_trim(player_vec)) %>%
  filter(!is.na(player), nzchar(player)) %>%
  distinct()
missing_arche <- active_players %>%
  anti_join(arche %>% select(player), by = "player")
if (nrow(missing_arche) > 0) {
  stopf(
    "Pre-QC failed: player_archetypes.csv missing active players: %s",
    paste(missing_arche$player, collapse = ", ")
  )
}

manual_dirs <- file.path(manual_csv_root, c("_conf", "_nc", "_bet", "_ncaatourn"))
manual_files <- unlist(lapply(manual_dirs[file.exists(manual_dirs)], function(d) {
  list.files(d, pattern = "\\.csv$", full.names = TRUE, recursive = FALSE)
}), use.names = FALSE)
manual_files <- manual_files[!grepl("_espn_generation_summary\\.csv$", manual_files)]
if (length(manual_files) == 0) {
  stop("Pre-QC failed: no manual game CSV files found for V3 pair signal.", call. = FALSE)
}

manual_req <- c("game_file", "game_play_number", "is_uconn_offense", "score_value", "defense_lineup_key")
manual_cover_games <- character()
manual_off_cover_games <- character()
for (mf in manual_files) {
  x <- read_csv(mf, show_col_types = FALSE)
  missing_manual_cols <- setdiff(manual_req, names(x))
  if (length(missing_manual_cols) > 0) {
    stopf(
      "Pre-QC failed: manual file %s missing columns: %s",
      basename(mf),
      paste(missing_manual_cols, collapse = ", ")
    )
  }
  x2 <- x %>%
    mutate(
      game_file = as.character(game_file),
      game_play_number = as.integer(game_play_number),
      is_uconn_offense = as.logical(is_uconn_offense),
      score_value = as_num(score_value)
    ) %>%
    filter(!is.na(game_file), nzchar(game_file), !is.na(game_play_number), is_uconn_offense == FALSE)
  if (nrow(x2) > 0) {
    manual_cover_games <- c(manual_cover_games, unique(x2$game_file))
  }

  x3 <- x %>%
    mutate(
      game_file = as.character(game_file),
      game_play_number = as.integer(game_play_number),
      is_uconn_offense = as.logical(is_uconn_offense)
    ) %>%
    filter(!is.na(game_file), nzchar(game_file), !is.na(game_play_number), is_uconn_offense == TRUE)
  if (nrow(x3) > 0) {
    manual_off_cover_games <- c(manual_off_cover_games, unique(x3$game_file))
  }
}

manual_cover_games <- unique(manual_cover_games)
manual_non_ex_coverage <- length(intersect(manual_cover_games, non_ex_games$game_file)) / nrow(non_ex_games)
if (!is.finite(manual_non_ex_coverage) || manual_non_ex_coverage < 0.95) {
  stopf(
    "Pre-QC failed: manual defensive-play coverage %.3f is below 0.95.",
    manual_non_ex_coverage
  )
}

manual_off_cover_games <- unique(manual_off_cover_games)
manual_off_non_ex_coverage <- length(intersect(manual_off_cover_games, non_ex_games$game_file)) / nrow(non_ex_games)
if (!is.finite(manual_off_non_ex_coverage) || manual_off_non_ex_coverage < 0.95) {
  stopf(
    "Pre-QC failed: manual offensive-play coverage %.3f is below 0.95.",
    manual_off_non_ex_coverage
  )
}

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
  sprintf("manual_non_ex_coverage=%.4f", manual_non_ex_coverage),
  sprintf("manual_off_non_ex_coverage=%.4f", manual_off_non_ex_coverage),
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
  "_scripts/models/fit_lineup_defensive_leak_model.R",
  "_scripts/analysis/build_uconn_lineup_shot_diet.R",
  "_scripts/analysis/build_uconn_player_creation_profile.R",
  "_scripts/pipeline/run_rolling_lineup_decision_backtest.R",
  "_scripts/analysis/evaluate_net_probability_calibration.R",
  "_scripts/models/fit_core_lineup_model.R",
  "_scripts/analysis/build_player_role_concentration_table.R",
  "_scripts/analysis/analyze_player_role_stability_by_phase.R",
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
decision_board_path <- pick_existing(
  "decision_board",
  c(
    "_outputs/01_lineup_core/uconn_lineup_decision_board.csv",
    "_outputs/05_decision_audit/uconn_lineup_decision_board.csv",
    "_outputs/uconn_lineup_decision_board.csv"
  )
)
decision_table_path <- pick_existing(
  "decision_table",
  c(
    "_outputs/05_decision_audit/uconn_lineup_decision_table.csv",
    "_outputs/01_lineup_core/uconn_lineup_decision_table.csv",
    "_outputs/uconn_lineup_decision_table.csv"
  ),
  required = FALSE
)
if (is.null(decision_table_path)) decision_table_path <- coach_view_path
usage_path <- pick_existing(
  "lineup_usage",
  c("_outputs/01_lineup_core/uconn_lineup_usage.csv", "_outputs/uconn_lineup_usage.csv")
)
leak_post_path <- pick_existing(
  "defense_leak_posterior",
  c("_outputs/02_defense_leaks/uconn_lineup_def_leaks_posterior.csv", "_outputs/uconn_lineup_def_leaks_posterior.csv")
)
rci_coach_path <- pick_existing(
  "rci_coach_table",
  c(
    "_outputs/03_players/uconn_player_rci_coach_table.csv",
    "_outputs/uconn_player_rci_coach_table.csv"
  )
)
shot_diet_path <- pick_existing(
  "lineup_shot_diet",
  c("_outputs/01_lineup_core/uconn_lineup_shot_diet.csv", "_outputs/uconn_lineup_shot_diet.csv")
)
player_creation_path <- pick_existing(
  "player_creation",
  c(
    "_outputs/03_players/uconn_player_creation_profile.csv",
    "_outputs/uconn_player_creation_profile.csv"
  )
)
bt_rows_path <- pick_existing(
  "decision_backtest_rows",
  c("_outputs/05_decision_audit/uconn_lineup_decision_rolling_backtest_rows.csv", "_outputs/uconn_lineup_decision_rolling_backtest_rows.csv")
)
bt_bucket_path <- pick_existing(
  "decision_backtest_bucket",
  c("_outputs/05_decision_audit/uconn_lineup_decision_rolling_backtest_by_bucket.csv", "_outputs/uconn_lineup_decision_rolling_backtest_by_bucket.csv")
)
thresholds_qc_path <- pick_existing(
  "decision_thresholds",
  c("_outputs/05_decision_audit/uconn_lineup_decision_rule_v2_thresholds.csv", "_outputs/uconn_lineup_decision_rule_v2_thresholds.csv")
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
decision_board <- read_csv(decision_board_path, show_col_types = FALSE)
decision <- if (identical(decision_table_path, coach_view_path)) {
  coach
} else {
  read_csv(decision_table_path, show_col_types = FALSE)
}
usage <- read_csv(usage_path, show_col_types = FALSE)
leaks <- read_csv(leak_post_path, show_col_types = FALSE)
rci <- read_csv(rci_coach_path, show_col_types = FALSE)
shot_diet <- read_csv(shot_diet_path, show_col_types = FALSE)
player_creation <- read_csv(player_creation_path, show_col_types = FALSE)
bt_rows <- read_csv(bt_rows_path, show_col_types = FALSE)
bt_bucket <- read_csv(bt_bucket_path, show_col_types = FALSE)
thresholds_qc <- read_csv(thresholds_qc_path, show_col_types = FALSE)
cal_metrics <- read_csv(cal_metrics_path, show_col_types = FALSE)
elig <- read_csv(eligibility_path, show_col_types = FALSE)

assert_columns(
  coach,
  c("lineup_pretty", "lineup_key_norm", "possessions", "decision_survive_score_raw", "decision_survive_score_robust", "defense_score", "offense_upside_score", "shot_diet_score", "creation_score", "composite_score", "decision_def_ppp_pred", "prior_pair_events", "prior_trio_possessions", "opp_fragile_flag", "decision_label", "Decision"),
  "coach_view"
)
assert_columns(
  decision_board,
  c("lineup_pretty", "lineup_key_norm", "decision_label", "defense_floor_pass", "defense_score", "offense_upside_score", "shot_diet_score", "creation_score", "composite_score", "decision_def_ppp_pred", "opp_fragile_flag", "sample_flag"),
  "decision_board"
)
assert_columns(
  decision,
  c("lineup_pretty", "decision_survive_score_raw", "decision_survive_score_robust", "decision_def_ppp_pred", "Decision"),
  "decision_table"
)
assert_columns(usage, c("uconn_lineup_canon", "possessions", "minutes"), "lineup_usage")
assert_columns(leaks, c("lineup", "u_def_mean", "pr_leak"), "defense_leak_posterior")
if (!("RCI" %in% names(rci))) {
  stop("rci_coach_table is missing required concentration column: expected `RCI`.", call. = FALSE)
}
assert_columns(rci, c("player", "net_pr_pos", "recommendation"), "rci_coach_table")
assert_columns(shot_diet, c("lineup_pretty", "lineup_key_norm", "fga", "sample_flag"), "lineup_shot_diet")
assert_columns(player_creation, c("player", "player_key_norm", "created_scoring_actions", "sample_flag"), "player_creation")
assert_columns(
  bt_rows,
  c(
    "holdout_game_id", "lineup", "holdout_possessions", "Decision",
    "decision_survive_score_robust", "decision_def_ppp_pred", "composite_score",
    "observed_survive4", "observed_leaky",
    "observed_def_ppp_gap_vs_game_baseline",
    "regret_vs_actual_ppp", "regret_vs_best_feasible_ppp"
  ),
  "decision_backtest_rows"
)
assert_columns(
  bt_bucket,
  c(
    "Decision", "total_holdout_possessions",
    "weighted_observed_survive4_rate", "weighted_observed_leaky_rate",
    "weighted_opp_fragile_rate"
  ),
  "decision_backtest_bucket"
)
assert_columns(thresholds_qc, c("metric", "value"), "decision_thresholds")
assert_columns(cal_metrics, c("metric", "value"), "calibration_metrics")
assert_columns(elig, c("game_id", "decision_eligible_prior"), "decision_eligibility")

if (nrow(coach) < 10) stopf("Post-QC failed: coach_view has too few rows (%d).", nrow(coach))
if (nrow(decision_board) < 10) stopf("Post-QC failed: decision_board has too few rows (%d).", nrow(decision_board))
if (nrow(leaks) < 10) stopf("Post-QC failed: defense_leak_posterior has too few rows (%d).", nrow(leaks))
if (nrow(rci) < 5) stopf("Post-QC failed: rci_coach_table has too few rows (%d).", nrow(rci))
if (nrow(bt_rows) < 100) stopf("Post-QC failed: backtest_rows has too few rows (%d).", nrow(bt_rows))

coach_score <- as_num(coach$decision_survive_score_robust)
if (any(is.finite(coach_score) & (coach_score < 0 | coach_score > 1))) {
  stop("Post-QC failed: coach_view decision_survive_score_robust contains values outside [0,1].", call. = FALSE)
}

leak_prob <- as_num(leaks$pr_leak)
if (any(is.finite(leak_prob) & (leak_prob < 0 | leak_prob > 1))) {
  stop("Post-QC failed: defense leak pr_leak contains values outside [0,1].", call. = FALSE)
}

rci_prob <- as_num(rci$net_pr_pos)
if (any(is.finite(rci_prob) & (rci_prob < 0 | rci_prob > 1))) {
  stop("Post-QC failed: RCI net_pr_pos contains values outside [0,1].", call. = FALSE)
}

if (anyDuplicated(coach$lineup_pretty) > 0) {
  stop("Post-QC failed: duplicate lineup_pretty values in coach_view.", call. = FALSE)
}
if (anyDuplicated(coach$lineup_key_norm) > 0) {
  stop("Post-QC failed: duplicate lineup_key_norm values in coach_view.", call. = FALSE)
}

allowed_decisions <- c(
  "DEF_FLOOR_FAIL",
  "DEF_FLOOR_PASS_UPSIDE_HIGH",
  "DEF_FLOOR_PASS_UPSIDE_MED",
  "DEF_FLOOR_PASS_UPSIDE_LOW",
  "LOW_SAMPLE",
  "UNSEEN"
)
if (any(!decision$Decision %in% allowed_decisions)) {
  bad <- unique(decision$Decision[!decision$Decision %in% allowed_decisions])
  stopf("Post-QC failed: unexpected Decision labels: %s", paste(bad, collapse = ", "))
}

non_ex_games_n <- nrow(non_ex_games)
if (non_ex_games_n >= 5 && all(decision$Decision %in% c("LOW_SAMPLE", "UNSEEN"))) {
  stop("Post-QC failed: all decision labels are LOW_SAMPLE/UNSEEN despite >=5 non-exhibition games.", call. = FALSE)
}

manual_mtime <- suppressWarnings(max(file.info(manual_files)$mtime, na.rm = TRUE))
shot_mtime <- file.info(shot_diet_path)$mtime
creation_mtime <- file.info(player_creation_path)$mtime
if (is.finite(as.numeric(manual_mtime)) && is.finite(as.numeric(shot_mtime)) && shot_mtime < manual_mtime) {
  stop("Post-QC failed: lineup shot diet output appears stale vs manual game CSV source.", call. = FALSE)
}
if (is.finite(as.numeric(manual_mtime)) && is.finite(as.numeric(creation_mtime)) && creation_mtime < manual_mtime) {
  stop("Post-QC failed: player creation output appears stale vs manual game CSV source.", call. = FALSE)
}

coach_key_tbl <- coach %>%
  transmute(
    lineup_key_norm = lineup_key_norm_local(lineup_key_norm),
    possessions = as_num(possessions)
  ) %>%
  filter(!is.na(lineup_key_norm), nzchar(lineup_key_norm), is.finite(possessions), possessions > 0)
coach_keys <- unique(coach_key_tbl$lineup_key_norm)
shot_keys <- unique(lineup_key_norm_local(shot_diet$lineup_key_norm))
shot_keys <- shot_keys[!is.na(shot_keys) & nzchar(shot_keys)]

LINEUP_OVERLAP_MIN_POSSESSIONS <- 5
coach_keys_overlap <- unique(coach_key_tbl$lineup_key_norm[coach_key_tbl$possessions >= LINEUP_OVERLAP_MIN_POSSESSIONS])
if (length(coach_keys_overlap) == 0) coach_keys_overlap <- coach_keys

lineup_join_overlap <- if (length(coach_keys_overlap) > 0) {
  length(intersect(coach_keys_overlap, shot_keys)) / length(coach_keys_overlap)
} else {
  NA_real_
}

coach_poss_tbl <- coach_key_tbl
shot_key_set <- unique(shot_keys)
poss_covered <- coach_poss_tbl %>%
  mutate(in_shot = lineup_key_norm %in% shot_key_set)
possession_join_overlap <- if (nrow(poss_covered) > 0) {
  sum(poss_covered$possessions[poss_covered$in_shot], na.rm = TRUE) / sum(poss_covered$possessions, na.rm = TRUE)
} else {
  NA_real_
}
if (!is.finite(lineup_join_overlap) || lineup_join_overlap < 0.93) {
  stopf(
    "Post-QC failed: coach-to-shot lineup key overlap %.3f is below 0.93 (coach possessions >= %d).",
    lineup_join_overlap,
    as.integer(LINEUP_OVERLAP_MIN_POSSESSIONS)
  )
}
if (!is.finite(possession_join_overlap) || possession_join_overlap < 0.98) {
  stopf("Post-QC failed: coach-to-shot possession overlap %.3f is below 0.98.", possession_join_overlap)
}

threshold_metric <- function(name) {
  out <- thresholds_qc %>% filter(metric == name) %>% pull(value)
  if (length(out) == 0) return(NA_real_)
  as_num(out[[1]])
}

threshold_metric_chr <- function(name) {
  out <- thresholds_qc %>% filter(metric == name) %>% pull(value)
  if (length(out) == 0) return(NA_character_)
  as.character(out[[1]])
}

tuning_grid_feasible <- threshold_metric("TUNING_GRID_FEASIBLE")
tuning_status <- threshold_metric_chr("TUNING_STATUS")
v4_safe_fallback <- identical(tuning_status, "fallback_defense_first_baseline_no_feasible_grid")
forward_games_n <- as_int(threshold_metric("FORWARD_GAMES_N"))
if (!is.finite(forward_games_n) || forward_games_n < 0) forward_games_n <- 0L

holdout_ids_sorted <- sort(unique(as_int(bt_rows$holdout_game_id)))
forward_ids <- if (forward_games_n > 0 && length(holdout_ids_sorted) >= forward_games_n) {
  tail(holdout_ids_sorted, forward_games_n)
} else {
  integer()
}
forward_holdout_possessions <- if (length(forward_ids) > 0) {
  sum(as_num(bt_rows$holdout_possessions[as_int(bt_rows$holdout_game_id) %in% forward_ids]), na.rm = TRUE)
} else {
  NA_real_
}

weighted_mean_safe_local <- function(x, w) {
  x <- as_num(x)
  w <- as_num(w)
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok], na.rm = TRUE) / sum(w[ok], na.rm = TRUE)
}

forward_bucket <- bt_rows %>%
  transmute(
    holdout_game_id = as_int(holdout_game_id),
    Decision = as.character(Decision),
    holdout_weight = as_num(holdout_possessions),
    observed_survive4 = as_num(observed_survive4),
    opp_fragile_flag = if ("opp_fragile_flag" %in% names(bt_rows)) as_num(opp_fragile_flag) else NA_real_
  ) %>%
  filter(
    holdout_game_id %in% forward_ids,
    !is.na(Decision),
    is.finite(holdout_weight),
    holdout_weight > 0
  ) %>%
  group_by(Decision) %>%
  summarise(
    weighted_observed_survive4_rate = weighted_mean_safe_local(observed_survive4, holdout_weight),
    weighted_opp_fragile_rate = weighted_mean_safe_local(opp_fragile_flag, holdout_weight),
    .groups = "drop"
  )

bucket_rate <- function(df, lbl, col) {
  row <- df %>% filter(Decision == lbl) %>% pull(.data[[col]])
  if (length(row) == 0) NA_real_ else as_num(row[[1]])
}
survive_guard <- bucket_rate(forward_bucket, "DEF_FLOOR_PASS_UPSIDE_HIGH", "weighted_observed_survive4_rate")
survive_survive4 <- bucket_rate(forward_bucket, "DEF_FLOOR_PASS_UPSIDE_MED", "weighted_observed_survive4_rate")
survive_high <- bucket_rate(forward_bucket, "DEF_FLOOR_FAIL", "weighted_observed_survive4_rate")
separation <- survive_guard - survive_high
opp_fragile_guard <- bucket_rate(forward_bucket, "DEF_FLOOR_PASS_UPSIDE_HIGH", "weighted_opp_fragile_rate")
regret_mean <- bt_rows %>%
  transmute(holdout_game_id = as_int(holdout_game_id), regret_vs_actual_ppp = as_num(regret_vs_actual_ppp)) %>%
  distinct(holdout_game_id, regret_vs_actual_ppp) %>%
  summarise(v = mean(regret_vs_actual_ppp, na.rm = TRUE)) %>%
  pull(v)

V4_FORWARD_HOLDOUT_POSSESSIONS_BAR <- 0
volume_gate_failed <- FALSE
forward_holdout_possession_shortfall <- 0

delta_def_ppp <- threshold_metric("DELTA_WEIGHTED_OBSERVED_DEF_PPP")
delta_survive <- threshold_metric("DELTA_WEIGHTED_OBSERVED_SURVIVE4_RATE")
delta_net_ppp <- threshold_metric("DELTA_WEIGHTED_HOLDOUT_RAW_NET_PPP")

non_volume_fail_reasons <- character()
if (!is.finite(forward_games_n) || forward_games_n < 2) {
  non_volume_fail_reasons <- c(non_volume_fail_reasons, "FORWARD_GAMES_N must be >= 2")
}
if (!isTRUE(v4_safe_fallback)) {
  if (!is.finite(tuning_grid_feasible) || tuning_grid_feasible <= 0) {
    non_volume_fail_reasons <- c(non_volume_fail_reasons, "TUNING_GRID_FEASIBLE must be > 0")
  }
  if (!is.finite(delta_def_ppp) || delta_def_ppp > 0.005) {
    non_volume_fail_reasons <- c(non_volume_fail_reasons, "DELTA_WEIGHTED_OBSERVED_DEF_PPP must be <= 0.005")
  }
  if (!is.finite(delta_survive) || delta_survive < -0.010) {
    non_volume_fail_reasons <- c(non_volume_fail_reasons, "DELTA_WEIGHTED_OBSERVED_SURVIVE4_RATE must be >= -0.010")
  }
  if (!is.finite(delta_net_ppp) || delta_net_ppp < 0.010) {
    non_volume_fail_reasons <- c(non_volume_fail_reasons, "DELTA_WEIGHTED_HOLDOUT_RAW_NET_PPP must be >= 0.010")
  }
}

lineups_30 <- sum(as_num(coach$possessions) >= 30, na.rm = TRUE)
non_volume_gate_fail_count <- length(non_volume_fail_reasons)
v3_gate_reasons <- non_volume_fail_reasons
v3_qc_result <- if (non_volume_gate_fail_count > 0) "v3_gate_fail" else "PASS"

max_elig_game_id <- max(as_int(elig$game_id), na.rm = TRUE)
if (!is.finite(max_elig_game_id) || max_elig_game_id < max_game_id) {
  stopf(
    "Post-QC failed: decision eligibility output appears stale (max game_id=%d, expected >=%d).",
    as.integer(max_elig_game_id), as.integer(max_game_id)
  )
}

manual_scout_root <- file.path(out_dir, "07_opps", "manual_game_scouts")
manual_scout_cleanup <- remove_duplicate_suffix_artifacts(manual_scout_root)
manual_scout_cleanup_removed <- sum(manual_scout_cleanup$removed, na.rm = TRUE)
if (manual_scout_cleanup_removed > 0) {
  message("Removed ", manual_scout_cleanup_removed, " duplicate manual-scout artifact(s) before QC.")
}

manual_scout_release <- scan_manual_scout_release_issues(
  manual_scout_root
)
manual_scout_release_status <- "SKIP"
if (isTRUE(manual_scout_release$checked)) {
  strict_manual_scout_release_qc <- tolower(Sys.getenv("STRICT_MANUAL_SCOUT_RELEASE_QC", "false")) %in% c("1", "true", "t", "yes", "y")
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
    manual_scout_release_status <- if (strict_manual_scout_release_qc) "FAIL" else "WARN"
    msg <- paste(
      c(
        "Manual scout release bundle issues detected.",
        paste0("- ", manual_scout_errors)
      ),
      collapse = "\n"
    )
    if (strict_manual_scout_release_qc) {
      stop(paste("Post-QC failed:", msg), call. = FALSE)
    } else {
      warning(msg, call. = FALSE)
    }
  }
}

post_qc_lines <- c(
  sprintf("timestamp=%s", timestamp),
  sprintf("coach_view_rows=%d", nrow(coach)),
  sprintf("decision_board_rows=%d", nrow(decision_board)),
  sprintf("decision_table_rows=%d", nrow(decision)),
  sprintf("lineup_usage_rows=%d", nrow(usage)),
  sprintf("defense_leak_rows=%d", nrow(leaks)),
  sprintf("rci_rows=%d", nrow(rci)),
  sprintf("backtest_rows=%d", nrow(bt_rows)),
  sprintf("backtest_bucket_rows=%d", nrow(bt_bucket)),
  sprintf("lineups_with_30_plus_possessions=%d", lineups_30),
  sprintf("result=%s", v3_qc_result),
  sprintf("tuning_status=%s", as.character(tuning_status)),
  sprintf("v4_safe_fallback=%s", as.character(v4_safe_fallback)),
  sprintf("tuning_grid_feasible=%s", as.character(tuning_grid_feasible)),
  sprintf("forward_games_n=%s", as.character(forward_games_n)),
  sprintf("forward_holdout_possessions=%.2f", as_num(forward_holdout_possessions)),
  sprintf("forward_holdout_possessions_bar=%d", as.integer(V4_FORWARD_HOLDOUT_POSSESSIONS_BAR)),
  sprintf("forward_holdout_possession_shortfall=%.2f", as_num(forward_holdout_possession_shortfall)),
  sprintf("non_volume_gate_fail_count=%d", as.integer(non_volume_gate_fail_count)),
  sprintf("survive_guardable=%s", as.character(survive_guard)),
  sprintf("survive_survive4=%s", as.character(survive_survive4)),
  sprintf("survive_high_risk=%s", as.character(survive_high)),
  sprintf("separation_guardable_vs_high=%s", as.character(separation)),
  sprintf("opp_fragile_guardable=%s", as.character(opp_fragile_guard)),
  sprintf("mean_regret_vs_actual_ppp=%s", as.character(regret_mean)),
  sprintf("max_input_game_id=%d", as.integer(max_game_id)),
  sprintf("max_eligibility_game_id=%d", as.integer(max_elig_game_id)),
  sprintf("manual_scout_release_qc=%s", manual_scout_release_status),
  sprintf("manual_scout_duplicate_cleanup_removed=%d", manual_scout_cleanup_removed),
  sprintf("lineup_join_overlap=%.4f", as_num(lineup_join_overlap)),
  sprintf("possession_join_overlap=%.4f", as_num(possession_join_overlap)),
  sprintf("coach_view_path=%s", coach_view_path),
  sprintf("decision_board_path=%s", decision_board_path),
  sprintf("decision_table_path=%s", decision_table_path),
  sprintf("shot_diet_path=%s", shot_diet_path),
  sprintf("player_creation_path=%s", player_creation_path),
  sprintf("thresholds_path=%s", thresholds_qc_path),
  sprintf("leak_posterior_path=%s", leak_post_path)
)
if (length(v3_gate_reasons) > 0) {
  post_qc_lines <- c(post_qc_lines, paste0("reasons=", paste(v3_gate_reasons, collapse = " | ")))
}
writeLines(post_qc_lines, post_qc_path)

if (identical(v3_qc_result, "PASS")) {
  cat(sprintf("OK\tPOST_QC\t0s\t%s\n", post_qc_path), file = summary_path, append = TRUE)
  message("Post-QC passed.")
  message("Pipeline complete.")
} else {
  cat(sprintf("FAIL\tPOST_QC\t0s\t%s\n", post_qc_path), file = summary_path, append = TRUE)
  stopf("Post-QC failed (v3_gate_fail): %s", paste(v3_gate_reasons, collapse = " | "))
}
message("Summary: ", summary_path)
