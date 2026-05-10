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

# Scheme matchup model for possession-level PPP.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(tidyr)
  library(rstan)
})

source("_scripts/utils/project_paths.R")

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

resolve_first_path <- function(candidates, required = TRUE, search_roots = c(".", "_data", "_models", "_outputs")) {
  resolve_project_first_path(candidates, required = required, search_roots = search_roots)
}

parse_any_date <- function(x) {
  x <- as.character(x)
  x <- str_trim(x)
  suppressWarnings(as.Date(parse_date_time(x, orders = c("Y-m-d", "m/d/Y", "m/d/y", "Y/m/d"))))
}

first_existing_col <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) return(NULL)
  hit[[1]]
}

as_binary <- function(x) {
  if (is.logical(x)) return(as.integer(ifelse(is.na(x), FALSE, x)))

  x_chr <- tolower(str_trim(as.character(x)))
  is_true <- x_chr %in% c("1", "true", "t", "yes", "y")
  as.integer(ifelse(is.na(x_chr), FALSE, is_true))
}

mode_value <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

fit_has_draws <- function(fit, par = "alpha") {
  if (is.null(fit)) return(FALSE)

  draws_n <- tryCatch(nrow(as.data.frame(fit)), error = function(e) 0)
  if (is.na(draws_n) || draws_n == 0) return(FALSE)

  arr <- tryCatch(rstan::extract(fit, pars = par)[[par]], error = function(e) NULL)
  if (is.null(arr)) return(FALSE)

  TRUE
}

lump_by_train <- function(train_raw, test_raw, min_n = 20, other_label = "OTHER") {
  train_raw <- as.character(train_raw)
  test_raw <- as.character(test_raw)

  train_raw[is.na(train_raw) | train_raw == ""] <- other_label
  test_raw[is.na(test_raw) | test_raw == ""] <- other_label

  counts <- table(train_raw)
  keep <- names(counts[counts >= min_n])

  if (length(keep) == 0 && length(counts) > 0) {
    keep <- names(which.max(counts))
  }

  train_new <- ifelse(train_raw %in% keep, train_raw, other_label)
  test_new <- ifelse(test_raw %in% keep, test_raw, other_label)

  list(
    train = train_new,
    test = test_new,
    keep = keep,
    counts = counts
  )
}

draw_col <- function(draw_mat, idx) {
  if (is.null(dim(draw_mat))) {
    return(as.numeric(draw_mat))
  }
  as.numeric(draw_mat[, idx])
}

FORCE_REFIT <- tolower(Sys.getenv("FORCE_REFIT", "false")) == "true"

MIN_ACTION_N <- as.integer(Sys.getenv("MIN_ACTION_N", "30"))
MIN_COVERAGE_N <- as.integer(Sys.getenv("MIN_COVERAGE_N", "30"))
MIN_LINEUP_N <- as.integer(Sys.getenv("MIN_LINEUP_N", "20"))
MIN_CELL_N <- as.integer(Sys.getenv("MIN_CELL_N", "25"))

TOP_N_ACTIONS <- as.integer(Sys.getenv("TOP_N_ACTIONS", "3"))
TOP_N_LINEUPS <- as.integer(Sys.getenv("TOP_N_LINEUPS", "10"))

STAN_CHAINS <- as.integer(Sys.getenv("SCHEME_STAN_CHAINS", "4"))
STAN_ITER <- as.integer(Sys.getenv("SCHEME_STAN_ITER", "1500"))
STAN_WARMUP <- as.integer(Sys.getenv("SCHEME_STAN_WARMUP", as.character(max(500, STAN_ITER %/% 2))))
STAN_SEED <- as.integer(Sys.getenv("SCHEME_STAN_SEED", "20260219"))

TARGET_DEF_TEAM_ID <- Sys.getenv("TARGET_DEF_TEAM_ID", "")
TARGET_DEF_TEAM_NAME <- Sys.getenv("TARGET_DEF_TEAM_NAME", "")
SCHEME_PROJECT_DATA_DIR <- file.path("_data", "05_projects", "scheme_matchup_project")
SCHEME_DATA_SEARCH_ROOTS <- c(
  SCHEME_PROJECT_DATA_DIR,
  file.path("_data", "01_core_inputs"),
  file.path("_data", "02_derived_inputs"),
  file.path("_data", "03_manual_game_csv"),
  file.path("_data", "04_templates"),
  file.path("_data", "05_projects"),
  "_data",
  ".",
  "_models",
  "_outputs"
)

poss_path <- resolve_first_path(
  c("possessions.csv", "uconn_possessions.csv"),
  required = FALSE,
  search_roots = SCHEME_DATA_SEARCH_ROOTS
)
tags_path <- resolve_first_path(
  c("scheme_tags.csv", "uconn_scheme_tags.csv"),
  required = FALSE,
  search_roots = SCHEME_DATA_SEARCH_ROOTS
)
events_path <- resolve_first_path(
  c("events.csv", "uconn_events.csv"),
  required = FALSE,
  search_roots = SCHEME_DATA_SEARCH_ROOTS
)
games_path <- resolve_first_path(
  c("games.csv", "uconn_games.csv"),
  required = FALSE,
  search_roots = SCHEME_DATA_SEARCH_ROOTS
)
teams_path <- resolve_first_path(
  c("teams.csv", "uconn_teams.csv"),
  required = FALSE,
  search_roots = SCHEME_DATA_SEARCH_ROOTS
)
lineups_path <- resolve_first_path(
  c("lineups.csv", "uconn_lineups.csv"),
  required = FALSE,
  search_roots = SCHEME_DATA_SEARCH_ROOTS
)

stan_path <- resolve_first_path(c("uconn_scheme_matchup_ppp.stan"), required = TRUE)

models_dir <- if (dir.exists("_models")) "_models" else "."
out_dir <- if (dir.exists("_outputs")) file.path("_outputs", "06_scheme_matchups") else "06_scheme_matchups"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

fit_path <- file.path(models_dir, "uconn_scheme_matchup_ppp_fit.rds")

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) return(y)
  x
}

scheme_output_paths <- list(
  cell_summary = file.path(out_dir, "uconn_scheme_matchup_cell_summary.csv"),
  prep_table = file.path(out_dir, "uconn_scheme_matchup_opponent_prep_table.csv"),
  do_not_run = file.path(out_dir, "uconn_scheme_matchup_do_not_run.csv"),
  lineup_recommendations = file.path(out_dir, "uconn_scheme_matchup_lineup_recommendations.csv"),
  calibration = file.path(out_dir, "uconn_scheme_matchup_calibration.csv"),
  oos_lift = file.path(out_dir, "uconn_scheme_matchup_oos_lift.csv"),
  meta = file.path(out_dir, "uconn_scheme_matchup_model_meta.csv"),
  diagnostics = file.path(out_dir, "uconn_scheme_matchup_model_diagnostics.csv")
)

write_scheme_skip_outputs <- function(status, reason, extra_meta = list()) {
  empty_cell <- tibble::tibble(
    coverage = character(),
    action = character(),
    train_possessions = integer(),
    ppp_mean = numeric(),
    ppp_p05 = numeric(),
    ppp_p50 = numeric(),
    ppp_p95 = numeric(),
    delta_vs_cov_mean = numeric(),
    delta_vs_cov_p05 = numeric(),
    delta_vs_cov_p95 = numeric(),
    pr_above_cov_baseline = numeric(),
    rank_within_coverage = integer()
  )
  empty_lineup <- tibble::tibble(
    lineup_display = character(),
    lineup = character(),
    coverage = character(),
    recommended_action = character(),
    expected_ppp_mean = numeric(),
    expected_ppp_p05 = numeric(),
    expected_ppp_p95 = numeric(),
    gain_vs_cov_mean = numeric(),
    gain_vs_cov_p05 = numeric(),
    gain_vs_cov_p95 = numeric(),
    pr_gain_positive = numeric(),
    train_lineup_possessions = integer()
  )
  empty_cal <- tibble::tibble(
    coverage = character(),
    action = character(),
    n = integer(),
    predicted_ppp = numeric(),
    actual_ppp = numeric(),
    calibration_gap = numeric(),
    bucket_type = character(),
    bucket_id = character()
  )
  empty_lift <- tibble::tibble(
    coverage = character(),
    recommended_action = character(),
    baseline_n = integer(),
    recommended_n = integer(),
    baseline_actual_ppp = numeric(),
    recommended_actual_ppp = numeric(),
    lift_ppp = numeric(),
    lift_positive = logical()
  )
  empty_diag <- tibble::tibble(
    accept_stat__ = numeric(),
    stepsize__ = numeric(),
    treedepth__ = integer(),
    n_leapfrog__ = integer(),
    divergent__ = integer(),
    energy__ = numeric(),
    chain = integer()
  )

  meta <- tibble::tibble(
    train_n = as.integer(extra_meta$train_n %||% NA_integer_),
    test_n = as.integer(extra_meta$test_n %||% NA_integer_),
    split_cut_date = as.character(extra_meta$split_cut_date %||% NA_character_),
    actions_modeled = as.integer(extra_meta$actions_modeled %||% NA_integer_),
    coverages_modeled = as.integer(extra_meta$coverages_modeled %||% NA_integer_),
    lineups_modeled = as.integer(extra_meta$lineups_modeled %||% NA_integer_),
    off_teams_modeled = as.integer(extra_meta$off_teams_modeled %||% NA_integer_),
    def_teams_modeled = as.integer(extra_meta$def_teams_modeled %||% NA_integer_),
    min_action_n = MIN_ACTION_N,
    min_coverage_n = MIN_COVERAGE_N,
    min_lineup_n = MIN_LINEUP_N,
    target_def_team_id = as.character(extra_meta$target_def_team_id %||% TARGET_DEF_TEAM_ID),
    target_def_team_name = as.character(extra_meta$target_def_team_name %||% TARGET_DEF_TEAM_NAME),
    reference_lineup = as.character(extra_meta$reference_lineup %||% NA_character_),
    stan_chains = STAN_CHAINS,
    stan_iter = STAN_ITER,
    stan_warmup = STAN_WARMUP,
    model_cache_path = fit_path,
    status = as.character(status),
    reason = as.character(reason),
    raw_possessions_rows = as.integer(extra_meta$raw_possessions_rows %||% NA_integer_),
    raw_scheme_tags_rows = as.integer(extra_meta$raw_scheme_tags_rows %||% NA_integer_),
    modeled_rows_after_filtering = as.integer(extra_meta$modeled_rows_after_filtering %||% NA_integer_),
    min_modeled_rows_required = 200L,
    min_train_rows_required = 100L,
    min_test_rows_required = 50L,
    generated_at_utc = format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")
  )

  write_csv(empty_cell, scheme_output_paths$cell_summary)
  write_csv(empty_cell, scheme_output_paths$prep_table)
  write_csv(empty_cell, scheme_output_paths$do_not_run)
  write_csv(empty_lineup, scheme_output_paths$lineup_recommendations)
  write_csv(empty_cal, scheme_output_paths$calibration)
  write_csv(empty_lift, scheme_output_paths$oos_lift)
  write_csv(empty_diag, scheme_output_paths$diagnostics)
  write_csv(meta, scheme_output_paths$meta)
}

soft_skip_scheme <- function(status, reason, extra_meta = list()) {
  write_scheme_skip_outputs(status = status, reason = reason, extra_meta = extra_meta)
  message("Scheme matchup soft-skip: ", reason)
  message("Wrote skip-status outputs to: ", normalizePath(out_dir))
  quit(save = "no", status = 0)
}

safe_read_csv <- function(path, label) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) return(NULL)
  tryCatch(
    read_csv(path, show_col_types = FALSE),
    error = function(e) {
      soft_skip_scheme(
        status = "skipped_input_read_error",
        reason = paste0("Failed reading ", label, ": ", conditionMessage(e)),
        extra_meta = list(
          raw_possessions_rows = NA_integer_,
          raw_scheme_tags_rows = NA_integer_
        )
      )
      NULL
    }
  )
}

if (is.null(poss_path) || !nzchar(poss_path)) {
  soft_skip_scheme(
    status = "skipped_missing_input_file",
    reason = "Missing possessions.csv (or uconn_possessions.csv) in scheme data search roots."
  )
}
if (is.null(tags_path) || !nzchar(tags_path)) {
  soft_skip_scheme(
    status = "skipped_missing_input_file",
    reason = "Missing scheme_tags.csv (or uconn_scheme_tags.csv) in scheme data search roots."
  )
}

possessions <- safe_read_csv(poss_path, "possessions")
scheme_tags <- safe_read_csv(tags_path, "scheme_tags")
events <- safe_read_csv(events_path, "events")
games <- safe_read_csv(games_path, "games")
teams <- safe_read_csv(teams_path, "teams")
lineups <- safe_read_csv(lineups_path, "lineups")

required_poss <- c("possession_id", "game_id", "offense_team_id", "defense_team_id", "points_scored")
missing_poss <- setdiff(required_poss, names(possessions))
if (length(missing_poss) > 0) {
  soft_skip_scheme(
    status = "skipped_missing_input_columns",
    reason = paste0("possessions file missing required columns: ", paste(missing_poss, collapse = ", ")),
    extra_meta = list(
      raw_possessions_rows = nrow(possessions),
      raw_scheme_tags_rows = nrow(scheme_tags)
    )
  )
}

required_tags <- c("offense_action", "defense_coverage")
missing_tags <- setdiff(required_tags, names(scheme_tags))
if (length(missing_tags) > 0) {
  soft_skip_scheme(
    status = "skipped_missing_input_columns",
    reason = paste0("scheme_tags file missing required columns: ", paste(missing_tags, collapse = ", ")),
    extra_meta = list(
      raw_possessions_rows = nrow(possessions),
      raw_scheme_tags_rows = nrow(scheme_tags)
    )
  )
}

if (!("possession_id" %in% names(scheme_tags))) {
  if (("event_id" %in% names(scheme_tags)) && !is.null(events) && ("event_id" %in% names(events)) && ("possession_id" %in% names(events))) {
    scheme_tags <- scheme_tags %>%
      left_join(events %>% select(event_id, possession_id), by = "event_id")
  } else {
    soft_skip_scheme(
      status = "skipped_missing_input_columns",
      reason = "scheme_tags must include possession_id (or event_id with events mapping).",
      extra_meta = list(
        raw_possessions_rows = nrow(possessions),
        raw_scheme_tags_rows = nrow(scheme_tags)
      )
    )
  }
}

# Build shot_quality source
if (!("shot_quality" %in% names(possessions))) {
  if (!is.null(events) && ("possession_id" %in% names(events))) {
    sq_col <- first_existing_col(events, c("shot_quality", "xPPP", "xppp", "xFG", "xfg"))

    if (!is.null(sq_col)) {
      events_sq <- events %>%
        mutate(possession_id = as.character(possession_id), shot_quality = as.numeric(.data[[sq_col]])) %>%
        group_by(possession_id) %>%
        summarise(shot_quality = mean(shot_quality, na.rm = TRUE), .groups = "drop")

      possessions <- possessions %>%
        mutate(possession_id = as.character(possession_id)) %>%
        left_join(events_sq, by = "possession_id")
    } else {
      possessions$shot_quality <- NA_real_
    }
  } else {
    possessions$shot_quality <- NA_real_
  }
}

# Build game date map for time split
game_date_df <- NULL

if (!is.null(games) && ("game_id" %in% names(games))) {
  date_col <- first_existing_col(games, c("date", "game_date"))
  if (!is.null(date_col)) {
    game_date_df <- games %>%
      transmute(
        game_id = as.character(game_id),
        game_date = parse_any_date(.data[[date_col]])
      )
  }
}

if (is.null(game_date_df)) {
  poss_date_col <- first_existing_col(possessions, c("date", "game_date"))
  if (!is.null(poss_date_col)) {
    game_date_df <- possessions %>%
      transmute(
        game_id = as.character(game_id),
        game_date = parse_any_date(.data[[poss_date_col]])
      ) %>%
      group_by(game_id) %>%
      summarise(game_date = suppressWarnings(max(game_date, na.rm = TRUE)), .groups = "drop")
  }
}

if (is.null(game_date_df)) {
  soft_skip_scheme(
    status = "skipped_unusable_time_split",
    reason = "Cannot time-split by date. Provide games.csv with date/game_date or possessions date column.",
    extra_meta = list(
      raw_possessions_rows = nrow(possessions),
      raw_scheme_tags_rows = nrow(scheme_tags)
    )
  )
}

# Clean and dedupe scheme tags
if (!("tag_confidence" %in% names(scheme_tags))) scheme_tags$tag_confidence <- 1
if (!("review_date" %in% names(scheme_tags))) scheme_tags$review_date <- NA

tags_best <- scheme_tags %>%
  mutate(
    possession_id = as.character(possession_id),
    offense_action = str_squish(as.character(offense_action)),
    defense_coverage = str_squish(as.character(defense_coverage)),
    tag_confidence = as.numeric(tag_confidence),
    tag_confidence = if_else(is.na(tag_confidence), 1, tag_confidence),
    review_date = parse_any_date(review_date)
  ) %>%
  filter(
    !is.na(possession_id),
    !is.na(offense_action), offense_action != "",
    !is.na(defense_coverage), defense_coverage != ""
  ) %>%
  group_by(possession_id) %>%
  arrange(desc(tag_confidence), desc(review_date), .by_group = TRUE) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  select(possession_id, offense_action, defense_coverage, tag_confidence)

# Build modeling frame
if (!("transition_flag" %in% names(possessions))) possessions$transition_flag <- 0
if (!("garbage_time_flag" %in% names(possessions))) possessions$garbage_time_flag <- 0
if (!("off_lineup_id" %in% names(possessions))) possessions$off_lineup_id <- "UNKNOWN_LINEUP"

model_df <- possessions %>%
  transmute(
    possession_id = as.character(possession_id),
    game_id = as.character(game_id),
    offense_team_id = as.character(offense_team_id),
    defense_team_id = as.character(defense_team_id),
    off_lineup_id = as.character(off_lineup_id),
    points_scored = as.numeric(points_scored),
    transition_flag = as_binary(transition_flag),
    garbage_time_flag = as_binary(garbage_time_flag),
    shot_quality = as.numeric(shot_quality)
  ) %>%
  inner_join(tags_best, by = "possession_id") %>%
  left_join(game_date_df, by = "game_id") %>%
  filter(
    garbage_time_flag == 0,
    !is.na(game_date),
    !is.na(points_scored),
    points_scored >= 0,
    points_scored <= 4,
    !is.na(offense_team_id), offense_team_id != "",
    !is.na(defense_team_id), defense_team_id != ""
  )

if (nrow(model_df) < 200) {
  soft_skip_scheme(
    status = "skipped_insufficient_modeled_rows",
    reason = paste0("Too few modeled possessions after filtering (N = ", nrow(model_df), ")."),
    extra_meta = list(
      raw_possessions_rows = nrow(possessions),
      raw_scheme_tags_rows = nrow(scheme_tags),
      modeled_rows_after_filtering = nrow(model_df)
    )
  )
}

# Date split (no leakage)
all_dates <- sort(unique(model_df$game_date))
if (length(all_dates) < 2) {
  soft_skip_scheme(
    status = "skipped_unusable_time_split",
    reason = "Need at least 2 distinct dates to perform time split.",
    extra_meta = list(
      raw_possessions_rows = nrow(possessions),
      raw_scheme_tags_rows = nrow(scheme_tags),
      modeled_rows_after_filtering = nrow(model_df)
    )
  )
}

split_idx <- floor(length(all_dates) * 0.8)
split_idx <- max(1, min(split_idx, length(all_dates) - 1))
cut_date <- all_dates[split_idx]

train <- model_df %>% filter(game_date <= cut_date)
test <- model_df %>% filter(game_date > cut_date)

if (nrow(train) < 100 || nrow(test) < 50) {
  soft_skip_scheme(
    status = "skipped_unusable_time_split",
    reason = paste0(
      "Time split produced too-small train/test sets. Train N=",
      nrow(train),
      ", Test N=",
      nrow(test),
      "."
    ),
    extra_meta = list(
      raw_possessions_rows = nrow(possessions),
      raw_scheme_tags_rows = nrow(scheme_tags),
      modeled_rows_after_filtering = nrow(model_df),
      train_n = nrow(train),
      test_n = nrow(test),
      split_cut_date = as.character(cut_date)
    )
  )
}

# NCAA-friendly level pooling
act_map <- lump_by_train(train$offense_action, test$offense_action, MIN_ACTION_N, "OTHER_ACTION")
cov_map <- lump_by_train(train$defense_coverage, test$defense_coverage, MIN_COVERAGE_N, "OTHER_COVERAGE")
lin_map <- lump_by_train(train$off_lineup_id, test$off_lineup_id, MIN_LINEUP_N, "OTHER_LINEUP")

train$action <- act_map$train
test$action <- act_map$test
train$coverage <- cov_map$train
test$coverage <- cov_map$test
train$lineup <- lin_map$train
test$lineup <- lin_map$test

train$off_team <- as.character(train$offense_team_id)
train$def_team <- as.character(train$defense_team_id)

off_mode <- mode_value(train$off_team)
def_mode <- mode_value(train$def_team)

test$off_team <- ifelse(test$offense_team_id %in% unique(train$off_team), as.character(test$offense_team_id), off_mode)
test$def_team <- ifelse(test$defense_team_id %in% unique(train$def_team), as.character(test$defense_team_id), def_mode)

# Shot quality standardization from train only
sq_mean <- mean(train$shot_quality, na.rm = TRUE)
sq_sd <- sd(train$shot_quality, na.rm = TRUE)
if (!is.finite(sq_mean)) sq_mean <- 0
if (!is.finite(sq_sd) || sq_sd <= 0) sq_sd <- 1

train$shot_quality_z <- (train$shot_quality - sq_mean) / sq_sd
test$shot_quality_z <- (test$shot_quality - sq_mean) / sq_sd

train$shot_quality_z[!is.finite(train$shot_quality_z)] <- 0
test$shot_quality_z[!is.finite(test$shot_quality_z)] <- 0

# Factor levels from train
action_levels <- sort(unique(train$action))
coverage_levels <- sort(unique(train$coverage))
action_coverage_levels <- as.vector(outer(action_levels, coverage_levels, paste, sep = "__"))
off_team_levels <- sort(unique(train$off_team))
def_team_levels <- sort(unique(train$def_team))
lineup_levels <- sort(unique(train$lineup))

# Force test into train levels only
test$action <- ifelse(test$action %in% action_levels, test$action, action_levels[[1]])
test$coverage <- ifelse(test$coverage %in% coverage_levels, test$coverage, coverage_levels[[1]])
test$off_team <- ifelse(test$off_team %in% off_team_levels, test$off_team, off_team_levels[[1]])
test$def_team <- ifelse(test$def_team %in% def_team_levels, test$def_team, def_team_levels[[1]])
test$lineup <- ifelse(test$lineup %in% lineup_levels, test$lineup, lineup_levels[[1]])

train$action_coverage <- paste(train$action, train$coverage, sep = "__")
test$action_coverage <- paste(test$action, test$coverage, sep = "__")

# IDs
train <- train %>%
  mutate(
    action_id = match(action, action_levels),
    coverage_id = match(coverage, coverage_levels),
    action_coverage_id = match(action_coverage, action_coverage_levels),
    off_team_id = match(off_team, off_team_levels),
    def_team_id = match(def_team, def_team_levels),
    lineup_id = match(lineup, lineup_levels),
    y = points_scored
  )

test <- test %>%
  mutate(
    action_id = match(action, action_levels),
    coverage_id = match(coverage, coverage_levels),
    action_coverage_id = match(action_coverage, action_coverage_levels),
    off_team_id = match(off_team, off_team_levels),
    def_team_id = match(def_team, def_team_levels),
    lineup_id = match(lineup, lineup_levels),
    y = points_scored
  )

if (any(!is.finite(train$y))) stop("Non-finite y in train set.")
if (any(is.na(train$action_coverage_id))) stop("Missing action_coverage_id in train set.")

# Build Stan data
stan_data <- list(
  N = nrow(train),
  A = length(action_levels),
  C = length(coverage_levels),
  AC = length(action_coverage_levels),
  T_off = length(off_team_levels),
  T_def = length(def_team_levels),
  L = length(lineup_levels),

  action_id = train$action_id,
  coverage_id = train$coverage_id,
  action_coverage_id = train$action_coverage_id,
  off_team_id = train$off_team_id,
  def_team_id = train$def_team_id,
  lineup_id = train$lineup_id,

  transition_flag = as.numeric(train$transition_flag),
  shot_quality_z = as.numeric(train$shot_quality_z),
  y = as.numeric(train$y)
)

# Fit or load cached model
model_signature <- paste(
  nrow(train),
  length(action_levels),
  length(coverage_levels),
  length(lineup_levels),
  as.character(min(train$game_date)),
  as.character(max(train$game_date)),
  sep = "|"
)

fit <- NULL
if (!FORCE_REFIT && file.exists(fit_path)) {
  message("Cache hit: loading fit from ", fit_path)
  cached <- readRDS(fit_path)

  cached_sig <- attr(cached, "model_signature")

  if (is.null(cached_sig) || cached_sig != model_signature) {
    message("Cached model signature mismatch. Refitting.")
    fit <- NULL
  } else if (!fit_has_draws(cached, "alpha")) {
    message("Cached fit has no usable draws. Refitting.")
    fit <- NULL
  } else {
    fit <- cached
  }
}

if (is.null(fit)) {
  message("Cache miss (or FORCE_REFIT=TRUE): fitting scheme matchup model...")

  init_fun <- function() list(
    alpha = mean(stan_data$y),
    tau_action = 0.05,
    tau_coverage = 0.05,
    tau_action_coverage = 0.03,
    tau_off_team = 0.05,
    tau_def_team = 0.05,
    tau_lineup = 0.04,
    b_transition = 0,
    b_shot_quality = 0,
    sigma = max(sd(stan_data$y), 0.20)
  )

  fit <- rstan::stan(
    file = stan_path,
    data = stan_data,
    chains = STAN_CHAINS,
    iter = STAN_ITER,
    warmup = STAN_WARMUP,
    seed = STAN_SEED,
    init = init_fun,
    refresh = 50,
    control = list(adapt_delta = 0.995, max_treedepth = 14)
  )

  if (!fit_has_draws(fit, "alpha")) {
    stop("Scheme matchup fit produced no usable samples.")
  }

  attr(fit, "model_signature") <- model_signature
  saveRDS(fit, fit_path)
  message("Model cache saved: ", fit_path)
}

print(
  fit,
  pars = c(
    "alpha",
    "tau_action",
    "tau_coverage",
    "tau_action_coverage",
    "tau_off_team",
    "tau_def_team",
    "tau_lineup",
    "b_transition",
    "b_shot_quality",
    "sigma"
  )
)

# Posterior extraction
post <- rstan::extract(fit)

n_draws <- length(post$alpha)
if (!is.finite(n_draws) || n_draws < 100) stop("Insufficient posterior draws.")

alpha_mean <- mean(post$alpha)
beta_action_mean <- colMeans(post$beta_action)
gamma_coverage_mean <- colMeans(post$gamma_coverage)
delta_action_coverage_mean <- colMeans(post$delta_action_coverage)
u_off_mean <- colMeans(post$u_off)
v_def_mean <- colMeans(post$v_def)
w_lineup_mean <- colMeans(post$w_lineup)
b_transition_mean <- mean(post$b_transition)
b_shot_quality_mean <- mean(post$b_shot_quality)

predict_mu_mean <- function(df) {
  alpha_mean +
    beta_action_mean[df$action_id] +
    gamma_coverage_mean[df$coverage_id] +
    delta_action_coverage_mean[df$action_coverage_id] +
    u_off_mean[df$off_team_id] +
    v_def_mean[df$def_team_id] +
    w_lineup_mean[df$lineup_id] +
    b_transition_mean * as.numeric(df$transition_flag) +
    b_shot_quality_mean * as.numeric(df$shot_quality_z)
}

# Validation: calibration + OOS lift
test_eval <- test %>%
  mutate(pred_ppp = predict_mu_mean(test))

cal_action_coverage <- test_eval %>%
  group_by(coverage, action) %>%
  summarise(
    n = n(),
    predicted_ppp = mean(pred_ppp),
    actual_ppp = mean(y),
    calibration_gap = actual_ppp - predicted_ppp,
    .groups = "drop"
  ) %>%
  mutate(bucket_type = "action_coverage", bucket_id = paste(coverage, action, sep = " | "))

cal_deciles <- test_eval %>%
  mutate(pred_decile = ntile(pred_ppp, 10)) %>%
  group_by(pred_decile) %>%
  summarise(
    n = n(),
    predicted_ppp = mean(pred_ppp),
    actual_ppp = mean(y),
    calibration_gap = actual_ppp - predicted_ppp,
    .groups = "drop"
  ) %>%
  mutate(
    coverage = "ALL",
    action = "ALL",
    bucket_type = "pred_decile",
    bucket_id = as.character(pred_decile)
  ) %>%
  select(coverage, action, n, predicted_ppp, actual_ppp, calibration_gap, bucket_type, bucket_id)

calibration_out <- bind_rows(cal_action_coverage, cal_deciles)

# Recommendation engine setup
cell_support <- train %>%
  count(coverage, action, name = "train_possessions")

off_ref <- mode_value(train$off_team)
lineup_ref <- mode_value(train$lineup)

if (TARGET_DEF_TEAM_ID != "") {
  def_ref <- TARGET_DEF_TEAM_ID
} else if (TARGET_DEF_TEAM_NAME != "" && !is.null(teams) && all(c("team_id", "team_name") %in% names(teams))) {
  matches <- teams %>%
    mutate(team_name_l = tolower(str_squish(team_name))) %>%
    filter(team_name_l == tolower(str_squish(TARGET_DEF_TEAM_NAME)))
  def_ref <- if (nrow(matches) > 0) as.character(matches$team_id[[1]]) else mode_value(train$def_team)
} else {
  def_ref <- mode_value(train$def_team)
}

off_ref <- ifelse(is.na(off_ref), off_team_levels[[1]], off_ref)
def_ref <- ifelse(is.na(def_ref), def_team_levels[[1]], def_ref)
lineup_ref <- ifelse(is.na(lineup_ref), lineup_levels[[1]], lineup_ref)

off_ref_id <- match(off_ref, off_team_levels)
def_ref_id <- match(def_ref, def_team_levels)
lineup_ref_id <- match(lineup_ref, lineup_levels)

if (is.na(off_ref_id)) off_ref_id <- 1
if (is.na(def_ref_id)) def_ref_id <- 1
if (is.na(lineup_ref_id)) lineup_ref_id <- 1

# Per-action/coverage posterior summaries at reference context
cell_rows <- list()

for (c_idx in seq_along(coverage_levels)) {
  coverage_name <- coverage_levels[[c_idx]]

  mu_mat <- matrix(NA_real_, nrow = n_draws, ncol = length(action_levels))

  for (a_idx in seq_along(action_levels)) {
    action_name <- action_levels[[a_idx]]
    ac_name <- paste(action_name, coverage_name, sep = "__")
    ac_idx <- match(ac_name, action_coverage_levels)

    mu_draws <-
      as.numeric(post$alpha) +
      draw_col(post$beta_action, a_idx) +
      draw_col(post$gamma_coverage, c_idx) +
      draw_col(post$delta_action_coverage, ac_idx) +
      draw_col(post$u_off, off_ref_id) +
      draw_col(post$v_def, def_ref_id) +
      draw_col(post$w_lineup, lineup_ref_id)

    mu_mat[, a_idx] <- mu_draws
  }

  cov_baseline_draws <- rowMeans(mu_mat)

  for (a_idx in seq_along(action_levels)) {
    action_name <- action_levels[[a_idx]]
    mu_draws <- mu_mat[, a_idx]
    delta_draws <- mu_draws - cov_baseline_draws

    support_n <- cell_support %>%
      filter(coverage == coverage_name, action == action_name) %>%
      pull(train_possessions)

    if (length(support_n) == 0) support_n <- 0

    cell_rows[[length(cell_rows) + 1]] <- tibble(
      coverage = coverage_name,
      action = action_name,
      train_possessions = as.integer(support_n),
      ppp_mean = mean(mu_draws),
      ppp_p05 = as.numeric(quantile(mu_draws, 0.05)),
      ppp_p50 = as.numeric(quantile(mu_draws, 0.50)),
      ppp_p95 = as.numeric(quantile(mu_draws, 0.95)),
      delta_vs_cov_mean = mean(delta_draws),
      delta_vs_cov_p05 = as.numeric(quantile(delta_draws, 0.05)),
      delta_vs_cov_p95 = as.numeric(quantile(delta_draws, 0.95)),
      pr_above_cov_baseline = mean(delta_draws > 0)
    )
  }
}

cell_summary <- bind_rows(cell_rows) %>%
  group_by(coverage) %>%
  arrange(desc(ppp_mean), .by_group = TRUE) %>%
  mutate(rank_within_coverage = row_number()) %>%
  ungroup()

prep_table <- cell_summary %>%
  filter(rank_within_coverage <= TOP_N_ACTIONS) %>%
  arrange(coverage, rank_within_coverage)

do_not_run <- cell_summary %>%
  filter(
    train_possessions >= MIN_CELL_N,
    pr_above_cov_baseline <= 0.20,
    delta_vs_cov_p95 < -0.01
  ) %>%
  arrange(delta_vs_cov_mean)

# Lineup-specific recommendations
lineup_counts <- train %>% count(lineup, name = "train_lineup_possessions", sort = TRUE)
lineup_candidates <- lineup_counts %>%
  filter(lineup != "OTHER_LINEUP") %>%
  slice_head(n = TOP_N_LINEUPS)

if (nrow(lineup_candidates) == 0) {
  lineup_candidates <- lineup_counts %>% slice_head(n = min(TOP_N_LINEUPS, n()))
}

lineup_rows <- list()
for (l_idx in seq_len(nrow(lineup_candidates))) {
  lineup_name <- lineup_candidates$lineup[[l_idx]]
  lineup_id_idx <- match(lineup_name, lineup_levels)

  for (c_idx in seq_along(coverage_levels)) {
    coverage_name <- coverage_levels[[c_idx]]

    mu_mat <- matrix(NA_real_, nrow = n_draws, ncol = length(action_levels))
    mu_mean <- rep(NA_real_, length(action_levels))

    for (a_idx in seq_along(action_levels)) {
      action_name <- action_levels[[a_idx]]
      ac_name <- paste(action_name, coverage_name, sep = "__")
      ac_idx <- match(ac_name, action_coverage_levels)

      mu_draws <-
        as.numeric(post$alpha) +
        draw_col(post$beta_action, a_idx) +
        draw_col(post$gamma_coverage, c_idx) +
        draw_col(post$delta_action_coverage, ac_idx) +
        draw_col(post$u_off, off_ref_id) +
        draw_col(post$v_def, def_ref_id) +
        draw_col(post$w_lineup, lineup_id_idx)

      mu_mat[, a_idx] <- mu_draws
      mu_mean[a_idx] <- mean(mu_draws)
    }

    best_a <- which.max(mu_mean)
    best_draws <- mu_mat[, best_a]
    cov_baseline_draws <- rowMeans(mu_mat)
    gain_draws <- best_draws - cov_baseline_draws

    lineup_rows[[length(lineup_rows) + 1]] <- tibble(
      lineup = lineup_name,
      coverage = coverage_name,
      recommended_action = action_levels[[best_a]],
      expected_ppp_mean = mean(best_draws),
      expected_ppp_p05 = as.numeric(quantile(best_draws, 0.05)),
      expected_ppp_p95 = as.numeric(quantile(best_draws, 0.95)),
      gain_vs_cov_mean = mean(gain_draws),
      gain_vs_cov_p05 = as.numeric(quantile(gain_draws, 0.05)),
      gain_vs_cov_p95 = as.numeric(quantile(gain_draws, 0.95)),
      pr_gain_positive = mean(gain_draws > 0),
      train_lineup_possessions = lineup_candidates$train_lineup_possessions[[l_idx]]
    )
  }
}

lineup_recommendations <- bind_rows(lineup_rows) %>%
  arrange(desc(train_lineup_possessions), coverage)

# Optional lineup labeling
if (!is.null(lineups) && all(c("lineup_id", "p1", "p2", "p3", "p4", "p5") %in% names(lineups))) {
  lineup_labels <- lineups %>%
    transmute(
      lineup = as.character(lineup_id),
      lineup_label = paste(p1, p2, p3, p4, p5, sep = " | ")
    )

  lineup_recommendations <- lineup_recommendations %>%
    left_join(lineup_labels, by = "lineup") %>%
    mutate(lineup_display = if_else(!is.na(lineup_label), lineup_label, lineup)) %>%
    select(lineup_display, everything(), -lineup_label)
}

# OOS lift: recommended vs baseline
coverage_reco <- prep_table %>%
  filter(rank_within_coverage == 1) %>%
  select(coverage, recommended_action = action, recommended_predicted_ppp = ppp_mean)

test_with_reco <- test_eval %>%
  left_join(coverage_reco, by = "coverage")

lift_by_coverage <- test_with_reco %>%
  group_by(coverage, recommended_action) %>%
  summarise(
    baseline_n = n(),
    recommended_n = sum(action == recommended_action, na.rm = TRUE),
    baseline_actual_ppp = mean(y, na.rm = TRUE),
    recommended_actual_ppp = ifelse(
      sum(action == recommended_action, na.rm = TRUE) > 0,
      mean(y[action == recommended_action], na.rm = TRUE),
      NA_real_
    ),
    .groups = "drop"
  ) %>%
  mutate(
    lift_ppp = recommended_actual_ppp - baseline_actual_ppp,
    lift_positive = lift_ppp > 0
  )

overall_baseline <- mean(test_with_reco$y, na.rm = TRUE)
overall_reco_mask <- test_with_reco$action == test_with_reco$recommended_action
overall_recommended <- if (sum(overall_reco_mask, na.rm = TRUE) > 0) {
  mean(test_with_reco$y[overall_reco_mask], na.rm = TRUE)
} else {
  NA_real_
}

lift_overall <- tibble(
  coverage = "OVERALL",
  recommended_action = "Coverage-specific top action",
  baseline_n = nrow(test_with_reco),
  recommended_n = sum(test_with_reco$action == test_with_reco$recommended_action, na.rm = TRUE),
  baseline_actual_ppp = overall_baseline,
  recommended_actual_ppp = overall_recommended,
  lift_ppp = overall_recommended - overall_baseline,
  lift_positive = (overall_recommended - overall_baseline) > 0
)

oos_lift <- bind_rows(lift_by_coverage, lift_overall)

# Model diagnostics + metadata
sampler_params <- tryCatch(rstan::get_sampler_params(fit, inc_warmup = FALSE), error = function(e) NULL)
diag_df <- tibble::tibble(
  accept_stat__ = numeric(),
  stepsize__ = numeric(),
  treedepth__ = integer(),
  n_leapfrog__ = integer(),
  divergent__ = integer(),
  energy__ = numeric(),
  chain = integer()
)
if (!is.null(sampler_params)) {
  diag_df <- bind_rows(lapply(seq_along(sampler_params), function(i) {
    as_tibble(sampler_params[[i]]) %>% mutate(chain = i)
  }))
}

team_name_ref <- def_ref
if (!is.null(teams) && all(c("team_id", "team_name") %in% names(teams))) {
  team_lookup <- setNames(as.character(teams$team_name), as.character(teams$team_id))
  if (!is.na(team_lookup[def_ref])) team_name_ref <- as.character(team_lookup[def_ref])
}

meta <- tibble(
  train_n = nrow(train),
  test_n = nrow(test),
  split_cut_date = as.character(cut_date),
  actions_modeled = length(action_levels),
  coverages_modeled = length(coverage_levels),
  lineups_modeled = length(lineup_levels),
  off_teams_modeled = length(off_team_levels),
  def_teams_modeled = length(def_team_levels),
  min_action_n = MIN_ACTION_N,
  min_coverage_n = MIN_COVERAGE_N,
  min_lineup_n = MIN_LINEUP_N,
  target_def_team_id = def_ref,
  target_def_team_name = team_name_ref,
  reference_lineup = lineup_ref,
  stan_chains = STAN_CHAINS,
  stan_iter = STAN_ITER,
  stan_warmup = STAN_WARMUP,
  model_cache_path = fit_path,
  status = "ok",
  reason = "",
  raw_possessions_rows = nrow(possessions),
  raw_scheme_tags_rows = nrow(scheme_tags),
  modeled_rows_after_filtering = nrow(model_df),
  min_modeled_rows_required = 200L,
  min_train_rows_required = 100L,
  min_test_rows_required = 50L,
  generated_at_utc = format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")
)

# Write outputs
write_csv(cell_summary, scheme_output_paths$cell_summary)
write_csv(prep_table, scheme_output_paths$prep_table)
write_csv(do_not_run, scheme_output_paths$do_not_run)
write_csv(lineup_recommendations, scheme_output_paths$lineup_recommendations)
write_csv(calibration_out, scheme_output_paths$calibration)
write_csv(oos_lift, scheme_output_paths$oos_lift)
write_csv(meta, scheme_output_paths$meta)
write_csv(diag_df, scheme_output_paths$diagnostics)

message("Done. Wrote scheme matchup outputs to: ", normalizePath(out_dir))
message(" - uconn_scheme_matchup_cell_summary.csv")
message(" - uconn_scheme_matchup_opponent_prep_table.csv")
message(" - uconn_scheme_matchup_do_not_run.csv")
message(" - uconn_scheme_matchup_lineup_recommendations.csv")
message(" - uconn_scheme_matchup_calibration.csv")
message(" - uconn_scheme_matchup_oos_lift.csv")
message(" - uconn_scheme_matchup_model_meta.csv")
message(" - uconn_scheme_matchup_model_diagnostics.csv")
message("Model cache: ", normalizePath(fit_path))
