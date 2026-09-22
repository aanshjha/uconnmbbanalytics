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

# The historical posterior-sign score and outcome-relative-to-test-mean were
# different targets; unreconciled stints also invalidate lineup-level claims.
stop(
  "Legacy defensive lineup validation retired: target mismatch and unreconciled stint inputs. ",
  "Run python3 _scripts/analysis/evaluate_pregame_defense.py after source reconciliation. ",
  "Do not interpret historical leak scores as calibrated future-game probabilities.",
  call. = FALSE
)

# Time-split holdout validation for the defense leaks module.
# Fits the defense-only Stan model on early games, then evaluates whether the
# lineup leak signal (pr_leak / u_def_mean) separates late-game defensive outcomes.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(rstan)
  library(tidyr)
})

source("_scripts/utils/project_paths.R")
source("_scripts/utils/lineup_model_utils.R")

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


resolve_path <- function(fname) {
  resolve_project_path(fname)
}

weighted_brier <- function(p, y, w) {
  p <- as.numeric(p)
  y <- as.numeric(y)
  w <- as.numeric(w)
  ok <- is.finite(p) & is.finite(y) & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_)
  sum(w[ok] * (p[ok] - y[ok])^2) / sum(w[ok])
}

# ---------- Config ----------
DEF_HOLDOUT_TRAIN_FRAC <- suppressWarnings(as.numeric(Sys.getenv("DEF_HOLDOUT_TRAIN_FRAC", "0.80")))
DEF_HOLDOUT_STAN_CHAINS <- suppressWarnings(as.integer(Sys.getenv("DEF_HOLDOUT_STAN_CHAINS", "2")))
DEF_HOLDOUT_STAN_ITER <- suppressWarnings(as.integer(Sys.getenv("DEF_HOLDOUT_STAN_ITER", "1000")))
DEF_HOLDOUT_STAN_WARMUP <- suppressWarnings(as.integer(Sys.getenv(
  "DEF_HOLDOUT_STAN_WARMUP",
  as.character(max(500L, DEF_HOLDOUT_STAN_ITER %/% 2L))
)))
DEF_HOLDOUT_STAN_SEED <- suppressWarnings(as.integer(Sys.getenv("DEF_HOLDOUT_STAN_SEED", "20260224")))
DEF_HOLDOUT_ADAPT_DELTA <- suppressWarnings(as.numeric(Sys.getenv("DEF_HOLDOUT_ADAPT_DELTA", "0.99")))
DEF_HOLDOUT_MAX_TREEDEPTH <- suppressWarnings(as.integer(Sys.getenv("DEF_HOLDOUT_MAX_TREEDEPTH", "15")))
DEF_HOLDOUT_FORCE_REFIT <- tolower(Sys.getenv("DEF_HOLDOUT_FORCE_REFIT", "false")) %in% c("1", "true", "t", "yes", "y")

if (!is.finite(DEF_HOLDOUT_TRAIN_FRAC) || DEF_HOLDOUT_TRAIN_FRAC <= 0.5 || DEF_HOLDOUT_TRAIN_FRAC >= 0.95) {
  DEF_HOLDOUT_TRAIN_FRAC <- 0.80
}
if (!is.finite(DEF_HOLDOUT_STAN_CHAINS) || DEF_HOLDOUT_STAN_CHAINS < 1) DEF_HOLDOUT_STAN_CHAINS <- 2L
if (!is.finite(DEF_HOLDOUT_STAN_ITER) || DEF_HOLDOUT_STAN_ITER < 400) DEF_HOLDOUT_STAN_ITER <- 1000L
if (!is.finite(DEF_HOLDOUT_STAN_WARMUP) || DEF_HOLDOUT_STAN_WARMUP < 200 || DEF_HOLDOUT_STAN_WARMUP >= DEF_HOLDOUT_STAN_ITER) {
  DEF_HOLDOUT_STAN_WARMUP <- max(200L, min(DEF_HOLDOUT_STAN_ITER - 200L, DEF_HOLDOUT_STAN_ITER %/% 2L))
}
if (!is.finite(DEF_HOLDOUT_ADAPT_DELTA) || DEF_HOLDOUT_ADAPT_DELTA <= 0 || DEF_HOLDOUT_ADAPT_DELTA >= 1) DEF_HOLDOUT_ADAPT_DELTA <- 0.99
if (!is.finite(DEF_HOLDOUT_MAX_TREEDEPTH) || DEF_HOLDOUT_MAX_TREEDEPTH < 8) DEF_HOLDOUT_MAX_TREEDEPTH <- 15L

DEF_LEAK_STRONG_PR <- suppressWarnings(as.numeric(Sys.getenv("DEF_LEAK_STRONG_PR", "0.70")))
DEF_LEAK_LEAN_PR   <- suppressWarnings(as.numeric(Sys.getenv("DEF_LEAK_LEAN_PR", "0.60")))
DEF_PLUS_STRONG_PR <- suppressWarnings(as.numeric(Sys.getenv("DEF_PLUS_STRONG_PR", "0.30")))
DEF_PLUS_LEAN_PR   <- suppressWarnings(as.numeric(Sys.getenv("DEF_PLUS_LEAN_PR", "0.40")))
DEF_EFFECTIVE_SAMPLE_HIGH <- suppressWarnings(as.numeric(Sys.getenv("DEF_EFFECTIVE_SAMPLE_HIGH", "120")))
DEF_EFFECTIVE_SAMPLE_MED  <- suppressWarnings(as.numeric(Sys.getenv("DEF_EFFECTIVE_SAMPLE_MED", "60")))
if (!is.finite(DEF_EFFECTIVE_SAMPLE_HIGH) || DEF_EFFECTIVE_SAMPLE_HIGH <= 0) DEF_EFFECTIVE_SAMPLE_HIGH <- 120
if (!is.finite(DEF_EFFECTIVE_SAMPLE_MED) || DEF_EFFECTIVE_SAMPLE_MED <= 0) DEF_EFFECTIVE_SAMPLE_MED <- 60
if (DEF_EFFECTIVE_SAMPLE_MED > DEF_EFFECTIVE_SAMPLE_HIGH) {
  tmp <- DEF_EFFECTIVE_SAMPLE_MED
  DEF_EFFECTIVE_SAMPLE_MED <- DEF_EFFECTIVE_SAMPLE_HIGH
  DEF_EFFECTIVE_SAMPLE_HIGH <- tmp
}

# ---------- Paths ----------
stints_path <- resolve_path("uconn_stints_from_pbp.csv")
games_path  <- resolve_path("uconn_games_meta.csv")
opp_path    <- resolve_path("opponent_controls.csv")
stan_path   <- resolve_path("uconn_lineup_gamelevel_defonly.stan")

out_dir <- if (dir.exists("_outputs")) file.path("_outputs", "02_defense_leaks") else "02_defense_leaks"
models_dir <- if (dir.exists("_models")) "_models" else "."
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

fit_path <- file.path(models_dir, "uconn_lineup_gamelevel_defonly_holdout_timesplit_fit.rds")
rows_out_path <- file.path(out_dir, "uconn_def_leaks_holdout_validation_rows.csv")
bucket_out_path <- file.path(out_dir, "uconn_def_leaks_holdout_validation_by_bucket.csv")
meta_out_path <- file.path(out_dir, "uconn_def_leaks_holdout_validation_meta.csv")
diag_out_path <- file.path(out_dir, "uconn_def_leaks_holdout_validation_fit_diagnostics.csv")

# ---------- Load / preprocess ----------
model_inputs <- load_common_lineup_model_inputs(
  stints_path = stints_path,
  games_path = games_path,
  opp_path = opp_path,
  add_global_game_id = TRUE
)
stints <- model_inputs$stints
games <- model_inputs$games
games2 <- model_inputs$games_joined

missing <- games2 %>%
  filter(is.na(opp_adjO)) %>%
  select(game_date, opponent, game_file)

if (nrow(missing) > 0) {
  print(missing)
  stop("Some games did not match opponent_controls.csv (opp_adjO missing).")
}

stints2 <- stints %>%
  filter(
    lineup_size == 5,
    !is.na(poss_est), poss_est > 0,
    !is.na(points_against),
    !is.na(score_margin_start),
    !is.na(elapsed_game_sec)
  ) %>%
  mutate(
    uconn_lineup_canon = canonicalize_lineup(uconn_lineup),
    y_def = points_against / poss_est,
    w = poss_est
  ) %>%
  left_join(
    games2 %>% transmute(game_file, global_game_id, game_date_model = game_date, game_date_parsed, opp_adjO, site_home),
    by = "game_file"
  ) %>%
  filter(!is.na(global_game_id), !is.na(opp_adjO))

if (nrow(stints2) < 100) stop("Too few defense rows after preprocessing.")

games_used <- games2 %>%
  semi_join(stints2 %>% distinct(global_game_id), by = "global_game_id") %>%
  arrange(global_game_id)

if (nrow(games_used) < 8) stop("Need at least 8 games with eligible defense rows for time-split validation.")

split_idx <- floor(nrow(games_used) * DEF_HOLDOUT_TRAIN_FRAC)
split_idx <- max(4L, min(split_idx, nrow(games_used) - 2L))

train_games <- games_used %>% slice(seq_len(split_idx))
test_games <- games_used %>% slice((split_idx + 1L):n())

message(
  "Defense holdout validation | train games=", nrow(train_games),
  " test games=", nrow(test_games),
  " | train through ", as.character(max(train_games$game_date_parsed, na.rm = TRUE)),
  " | test from ", as.character(min(test_games$game_date_parsed, na.rm = TRUE))
)

train_rows <- stints2 %>%
  filter(global_game_id %in% train_games$global_game_id)

test_rows <- stints2 %>%
  filter(global_game_id %in% test_games$global_game_id)

if (nrow(train_rows) < 100 || nrow(test_rows) < 50) {
  stop("Time split produced too-small train/test sets. Train rows=", nrow(train_rows), " Test rows=", nrow(test_rows))
}

# Train-only scaling (no leakage in standardization)
oppO_scaler <- fit_scaler(train_games$opp_adjO)
score_scaler <- fit_scaler(train_rows$score_margin_start)
elapsed_scaler <- fit_scaler(train_rows$elapsed_game_sec)

train_games <- train_games %>%
  mutate(
    game_id_compact = row_number(),
    opp_adjO_z_split = apply_scaler(opp_adjO, oppO_scaler)
  )

train_rows <- train_rows %>%
  left_join(train_games %>% select(global_game_id, game_id_compact), by = "global_game_id") %>%
  mutate(
    score_margin_start_z = apply_scaler(score_margin_start, score_scaler),
    elapsed_game_sec_z = apply_scaler(elapsed_game_sec, elapsed_scaler)
  )

players <- sort(unique(unlist(str_split(train_rows$uconn_lineup_canon, "\\|"))))
player_id <- setNames(seq_along(players), players)

lineups <- sort(unique(train_rows$uconn_lineup_canon))
lineup_id <- setNames(seq_along(lineups), lineups)

uconn5_mat <- t(sapply(
  str_split(train_rows$uconn_lineup_canon, "\\|"),
  function(x) player_id[x]
))

data_list <- list(
  N = nrow(train_rows),
  P = length(players),
  L = length(lineups),
  G = nrow(train_games),
  uconn5 = uconn5_mat,
  lineup_id = as.integer(lineup_id[train_rows$uconn_lineup_canon]),
  game_id = train_rows$game_id_compact,
  y_def = train_rows$y_def,
  w = train_rows$w,
  opp_adjO_z = train_games$opp_adjO_z_split,
  site_home = train_games$site_home,
  score_margin_start_z = train_rows$score_margin_start_z,
  elapsed_game_sec_z = train_rows$elapsed_game_sec_z
)

current_cache_signature <- paste(
  "def_holdout_timesplit_v1",
  "train_frac", format(DEF_HOLDOUT_TRAIN_FRAC, digits = 6),
  "stints", safe_md5(stints_path),
  "games", safe_md5(games_path),
  "opp", safe_md5(opp_path),
  "stan", safe_md5(stan_path),
  "train_games", paste(train_games$global_game_id, collapse = "-"),
  "test_games", paste(test_games$global_game_id, collapse = "-"),
  "N", nrow(train_rows),
  "P", length(players),
  "L", length(lineups),
  sep = "|"
)

# ---------- Fit or load cached validation fit ----------
fit <- NULL

if (!DEF_HOLDOUT_FORCE_REFIT && file.exists(fit_path)) {
  message("Cache hit: loading defense holdout fit from ", fit_path)
  cached <- readRDS(fit_path)
  cached_sig <- attr(cached, "cache_signature")
  if (is.null(cached_sig) || !identical(cached_sig, current_cache_signature)) {
    message("Cached defense holdout fit invalid (signature mismatch). Refitting.")
  } else if (!fit_has_draws(cached, "alpha_def")) {
    message("Cached defense holdout fit missing draws. Refitting.")
  } else {
    fit <- cached
  }
}

if (is.null(fit)) {
  message("Cache miss (or DEF_HOLDOUT_FORCE_REFIT=TRUE): fitting defense holdout model...")

  init_fun <- function() list(
    intercept_def = mean(data_list$y_def),
    sigma = 0.8,
    tau_def = 0.06,
    tau_u_def = 0.06,
    b_oppO = 0.0,
    b_home = 0.0,
    b_score_margin = 0.0,
    b_elapsed_game = 0.0
  )

  fit <- rstan::stan(
    file = stan_path,
    data = data_list,
    chains = DEF_HOLDOUT_STAN_CHAINS,
    iter = DEF_HOLDOUT_STAN_ITER,
    warmup = DEF_HOLDOUT_STAN_WARMUP,
    seed = DEF_HOLDOUT_STAN_SEED,
    init = init_fun,
    refresh = 50,
    control = list(adapt_delta = DEF_HOLDOUT_ADAPT_DELTA, max_treedepth = DEF_HOLDOUT_MAX_TREEDEPTH)
  )

  if (!fit_has_draws(fit, "alpha_def")) {
    stop("Defense holdout validation fit produced no usable samples.")
  }

  attr(fit, "cache_signature") <- current_cache_signature
  saveRDS(fit, fit_path)
  message("Defense holdout validation cache saved: ", fit_path)
}

post <- rstan::extract(fit)
u_def_draws <- post$u_def
if (is.null(u_def_draws) || length(dim(u_def_draws)) < 2) stop("u_def missing/invalid in holdout fit posterior.")

u_def_q <- apply(u_def_draws, 2, quantile, probs = c(0.05, 0.5, 0.95))

lineup_scores <- tibble(
  lineup = lineups,
  u_def_mean = colMeans(u_def_draws),
  u_def_p05 = u_def_q[1, ],
  u_def_p50 = u_def_q[2, ],
  u_def_p95 = u_def_q[3, ],
  pr_leak = colMeans(u_def_draws > 0)
) %>%
  mutate(lineup_pretty = lineup)

train_usage <- train_rows %>%
  group_by(lineup = uconn_lineup_canon) %>%
  summarise(
    train_possessions = sum(w, na.rm = TRUE),
    train_minutes = sum(dur_min, na.rm = TRUE),
    train_games = n_distinct(game_file),
    .groups = "drop"
  )

test_team_def_ppp <- sum(test_rows$points_against, na.rm = TRUE) / sum(test_rows$w, na.rm = TRUE)

holdout_rows <- test_rows %>%
  group_by(global_game_id, game_file, game_date = game_date_model, lineup = uconn_lineup_canon) %>%
  summarise(
    holdout_possessions = sum(w, na.rm = TRUE),
    holdout_minutes = sum(dur_min, na.rm = TRUE),
    holdout_games = n_distinct(game_file),
    holdout_pts_against = sum(points_against, na.rm = TRUE),
    holdout_def_ppp = sum(points_against, na.rm = TRUE) / sum(w, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(train_usage, by = "lineup") %>%
  left_join(lineup_scores, by = "lineup") %>%
  mutate(
    seen_in_train = is.finite(pr_leak),
    observed_leaky_vs_test_baseline = holdout_def_ppp > test_team_def_ppp,
    observed_leaky_vs_test_baseline_num = as.numeric(observed_leaky_vs_test_baseline),
    def_ppp_gap_vs_test_baseline = holdout_def_ppp - test_team_def_ppp,
    risk_bucket = case_when(
      !seen_in_train ~ "UNSEEN",
      train_possessions >= DEF_EFFECTIVE_SAMPLE_HIGH & pr_leak >= DEF_LEAK_STRONG_PR & u_def_p50 > 0 ~ "LEAK RISK (HIGH CONF)",
      train_possessions >= DEF_EFFECTIVE_SAMPLE_MED  & pr_leak >= DEF_LEAK_LEAN_PR & u_def_mean > 0 ~ "LEAK RISK (LEAN)",
      train_possessions >= DEF_EFFECTIVE_SAMPLE_HIGH & pr_leak <= DEF_PLUS_STRONG_PR & u_def_p50 < 0 ~ "DEF PLUS (HIGH CONF)",
      train_possessions >= DEF_EFFECTIVE_SAMPLE_HIGH & pr_leak <= DEF_PLUS_LEAN_PR & u_def_p50 < 0 ~ "DEF PLUS (LEAN)",
      pr_leak >= 0.55 & u_def_mean > 0 ~ "WATCH (LEAK SIGNAL)",
      pr_leak <= 0.45 & u_def_mean < 0 ~ "WATCH (DEF PLUS SIGNAL)",
      TRUE ~ "INCONCLUSIVE"
    )
  ) %>%
  arrange(global_game_id, desc(holdout_possessions))

seen_eval <- holdout_rows %>%
  filter(seen_in_train, is.finite(pr_leak), is.finite(holdout_possessions), holdout_possessions > 0)

bucket_summary <- holdout_rows %>%
  group_by(risk_bucket) %>%
  summarise(
    n_lineup_games = n(),
    n_games = n_distinct(global_game_id),
    total_holdout_possessions = sum(holdout_possessions, na.rm = TRUE),
    weighted_pr_leak = weighted_mean_safe(pr_leak, holdout_possessions),
    weighted_observed_leaky_rate = weighted_mean_safe(observed_leaky_vs_test_baseline_num, holdout_possessions),
    weighted_holdout_def_ppp = weighted_mean_safe(holdout_def_ppp, holdout_possessions),
    weighted_def_ppp_gap_vs_test_baseline = weighted_mean_safe(def_ppp_gap_vs_test_baseline, holdout_possessions),
    weighted_train_possessions = weighted_mean_safe(train_possessions, holdout_possessions),
    .groups = "drop"
  ) %>%
  arrange(
    factor(
      risk_bucket,
      levels = c(
        "LEAK RISK (HIGH CONF)",
        "LEAK RISK (LEAN)",
        "WATCH (LEAK SIGNAL)",
        "INCONCLUSIVE",
        "WATCH (DEF PLUS SIGNAL)",
        "DEF PLUS (LEAN)",
        "DEF PLUS (HIGH CONF)",
        "UNSEEN"
      )
    )
  )

overall_seen_summary <- tibble(
  metric = c(
    "test_team_def_ppp_baseline",
    "seen_lineup_games",
    "seen_holdout_possessions",
    "weighted_pr_leak_mean",
    "weighted_observed_leaky_rate",
    "weighted_calibration_gap",
    "weighted_brier",
    "weighted_sign_accuracy_at_0.5"
  ),
  value = c(
    test_team_def_ppp,
    nrow(seen_eval),
    sum(seen_eval$holdout_possessions, na.rm = TRUE),
    weighted_mean_safe(seen_eval$pr_leak, seen_eval$holdout_possessions),
    weighted_mean_safe(seen_eval$observed_leaky_vs_test_baseline_num, seen_eval$holdout_possessions),
    weighted_mean_safe(seen_eval$observed_leaky_vs_test_baseline_num, seen_eval$holdout_possessions) -
      weighted_mean_safe(seen_eval$pr_leak, seen_eval$holdout_possessions),
    weighted_brier(seen_eval$pr_leak, seen_eval$observed_leaky_vs_test_baseline_num, seen_eval$holdout_possessions),
    weighted_mean_safe(as.numeric((seen_eval$pr_leak >= 0.5) == seen_eval$observed_leaky_vs_test_baseline), seen_eval$holdout_possessions)
  )
)

meta <- tibble(
  train_frac = DEF_HOLDOUT_TRAIN_FRAC,
  train_games_n = nrow(train_games),
  test_games_n = nrow(test_games),
  train_game_id_min = min(train_games$global_game_id),
  train_game_id_max = max(train_games$global_game_id),
  test_game_id_min = min(test_games$global_game_id),
  test_game_id_max = max(test_games$global_game_id),
  train_date_start = as.character(min(train_games$game_date_parsed, na.rm = TRUE)),
  train_date_end = as.character(max(train_games$game_date_parsed, na.rm = TRUE)),
  test_date_start = as.character(min(test_games$game_date_parsed, na.rm = TRUE)),
  test_date_end = as.character(max(test_games$game_date_parsed, na.rm = TRUE)),
  train_rows = nrow(train_rows),
  test_rows = nrow(test_rows),
  train_lineups = length(lineups),
  train_players = length(players),
  def_effective_sample_high = DEF_EFFECTIVE_SAMPLE_HIGH,
  def_effective_sample_med = DEF_EFFECTIVE_SAMPLE_MED,
  stan_chains = DEF_HOLDOUT_STAN_CHAINS,
  stan_iter = DEF_HOLDOUT_STAN_ITER,
  stan_warmup = DEF_HOLDOUT_STAN_WARMUP
)

sampler_params <- tryCatch(rstan::get_sampler_params(fit, inc_warmup = FALSE), error = function(e) NULL)
diag_df <- tibble()
if (!is.null(sampler_params)) {
  diag_df <- bind_rows(lapply(seq_along(sampler_params), function(i) {
    as_tibble(sampler_params[[i]]) %>% mutate(chain = i)
  }))
}

meta_kv <- tibble(
  metric = names(meta),
  value = as.character(unlist(meta[1, ], use.names = FALSE))
)

write_csv(holdout_rows, rows_out_path)
write_csv(bucket_summary, bucket_out_path)
write_csv(bind_rows(meta_kv, overall_seen_summary %>% mutate(value = as.character(value))), meta_out_path)
if (nrow(diag_df) > 0) write_csv(diag_df, diag_out_path)

message("Done. Wrote defense holdout validation outputs to: ", normalizePath(out_dir))
message(" - ", basename(rows_out_path))
message(" - ", basename(bucket_out_path))
message(" - ", basename(meta_out_path))
if (nrow(diag_df) > 0) message(" - ", basename(diag_out_path))
