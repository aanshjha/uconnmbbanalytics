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

# Rolling game-level holdout backtest for the lineup decision table.
# Trains on prior games only, scores the next game, and summarizes calibration
# plus realized outcomes by decision bucket.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(tidyr)
  library(rstan)
})

source("_scripts/utils/project_paths.R")
source("_scripts/utils/lineup_model_utils.R")

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


resolve_path <- function(fname) {
  resolve_project_path(
    fname,
    extra_dirs = c(file.path("_outputs", "01_lineup_core"))
  )
}

as_bool_env <- function(x) {
  tolower(Sys.getenv(x, "false")) %in% c("1", "true", "t", "yes", "y")
}

fit_platt_calibration <- function(df, prob_col, outcome_col, weight_col) {
  prob <- as.numeric(df[[prob_col]])
  y <- as.numeric(df[[outcome_col]])
  w <- as.numeric(df[[weight_col]])

  ok <- is.finite(prob) & is.finite(y) & is.finite(w) & w > 0
  if (sum(ok) < 20 || length(unique(y[ok])) < 2) {
    return(tibble(
      status = "insufficient_data",
      model = "platt_logit",
      source_prob_col = prob_col,
      source_outcome_col = outcome_col,
      n_rows = sum(ok),
      weighted_n = sum(w[ok], na.rm = TRUE),
      intercept = NA_real_,
      slope = NA_real_
    ))
  }

  z <- qlogis(clamp_prob(prob[ok]))
  fit <- tryCatch(
    suppressWarnings(glm(y[ok] ~ z, family = binomial(), weights = w[ok])),
    error = function(e) NULL
  )

  if (is.null(fit)) {
    return(tibble(
      status = "fit_failed",
      model = "platt_logit",
      source_prob_col = prob_col,
      source_outcome_col = outcome_col,
      n_rows = sum(ok),
      weighted_n = sum(w[ok], na.rm = TRUE),
      intercept = NA_real_,
      slope = NA_real_
    ))
  }

  cf <- coef(fit)
  slope_hat <- unname(cf[[2]])
  status <- if (is.finite(slope_hat) && slope_hat > 0) "ok" else "non_monotone_fit"

  tibble(
    status = status,
    model = "platt_logit",
    source_prob_col = prob_col,
    source_outcome_col = outcome_col,
    n_rows = sum(ok),
    weighted_n = sum(w[ok], na.rm = TRUE),
    intercept = unname(cf[[1]]),
    slope = slope_hat
  )
}

apply_platt_calibration <- function(p, intercept, slope) {
  p <- as.numeric(p)
  if (!is.finite(intercept) || !is.finite(slope)) return(rep(NA_real_, length(p)))
  plogis(intercept + slope * qlogis(clamp_prob(p)))
}

calc_prob_metrics <- function(prob, outcome, weight, n_bins = 10L) {
  prob <- as.numeric(prob)
  outcome <- as.numeric(outcome)
  weight <- as.numeric(weight)
  ok <- is.finite(prob) & is.finite(outcome) & is.finite(weight) & weight > 0
  if (sum(ok) < 10) {
    return(tibble(
      n_rows = sum(ok),
      weighted_n = sum(weight[ok], na.rm = TRUE),
      weighted_brier = NA_real_,
      weighted_log_loss = NA_real_,
      weighted_ece_decile = NA_real_,
      max_abs_weighted_decile_gap = NA_real_
    ))
  }

  p <- clamp_prob(prob[ok], eps = 1e-6)
  y <- outcome[ok]
  w <- weight[ok]

  brier <- weighted_mean_safe((y - p)^2, w)
  log_loss <- weighted_mean_safe(-(y * log(p) + (1 - y) * log(1 - p)), w)

  n_bins <- max(2L, min(as.integer(n_bins), length(p)))
  dec <- tibble(p = p, y = y, w = w) %>%
    arrange(p) %>%
    mutate(bin = dplyr::ntile(p, n_bins)) %>%
    group_by(bin) %>%
    summarise(
      pred = weighted_mean_safe(p, w),
      obs = weighted_mean_safe(y, w),
      wt = sum(w, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(gap = obs - pred)

  ece <- if (nrow(dec) > 0 && is.finite(sum(dec$wt, na.rm = TRUE)) && sum(dec$wt, na.rm = TRUE) > 0) {
    sum(abs(dec$gap) * dec$wt, na.rm = TRUE) / sum(dec$wt, na.rm = TRUE)
  } else {
    NA_real_
  }
  max_gap <- if (nrow(dec) > 0) max(abs(dec$gap), na.rm = TRUE) else NA_real_

  tibble(
    n_rows = sum(ok),
    weighted_n = sum(w, na.rm = TRUE),
    weighted_brier = brier,
    weighted_log_loss = log_loss,
    weighted_ece_decile = ece,
    max_abs_weighted_decile_gap = max_gap
  )
}

classify_decision <- function(pr, net, prior_possessions, thresholds, decision_available = NULL) {
  pr <- as.numeric(pr)
  net <- as.numeric(net)
  prior_possessions <- as.numeric(prior_possessions)
  if (is.null(decision_available)) {
    decision_available <- is.finite(pr) & is.finite(net) & is.finite(prior_possessions)
  }

  out <- rep("UNSEEN", length(pr))
  if (length(decision_available) != length(pr)) {
    idx <- rep(TRUE, length(pr))
  } else {
    idx <- as.logical(decision_available)
  }
  idx[is.na(idx)] <- FALSE
  idx <- idx & is.finite(pr) & is.finite(net) & is.finite(prior_possessions)

  out[idx] <- dplyr::case_when(
    prior_possessions[idx] >= 70 &
      pr[idx] >= thresholds$play_pr_min &
      net[idx] >= thresholds$play_net_min ~ "PLAY MORE",
    prior_possessions[idx] >= 45 &
      pr[idx] >= thresholds$lean_pr_min &
      net[idx] >= thresholds$lean_net_min ~ "LEAN IN",
    prior_possessions[idx] >= 30 &
      (
        pr[idx] <= thresholds$limit_pr_max |
          net[idx] <= thresholds$limit_net_max
      ) ~ "LIMIT / WATCH",
    prior_possessions[idx] < 30 ~ "TOO SMALL",
    TRUE ~ "NEUTRAL"
  )
  out
}

summarise_decision_buckets <- function(df, decision_col = "Decision") {
  if (!(decision_col %in% names(df))) stop("Decision column not found: ", decision_col)
  df %>%
    group_by(decision_bucket = .data[[decision_col]]) %>%
    summarise(
      total_holdout_possessions = sum(holdout_weight, na.rm = TRUE),
      weighted_pred_pr_net_pos = weighted_mean_safe(pred_pr_net_pos, holdout_weight),
      weighted_observed_positive_rate = weighted_mean_safe(observed_net_positive_num, holdout_weight),
      weighted_net_prob_calibration_gap = weighted_observed_positive_rate - weighted_pred_pr_net_pos,
      weighted_pred_net_ppp = weighted_mean_safe(pred_net_ppp_mean, holdout_weight),
      weighted_realized_net_ppp = weighted_mean_safe(holdout_raw_net_ppp, holdout_weight),
      .groups = "drop"
    ) %>%
    rename(Decision = decision_bucket)
}

tune_rule_v2_forward <- function(tune_df, default_thresholds) {
  required <- c(
    "pred_pr_net_pos",
    "pred_net_ppp_mean",
    "prior_possessions",
    "holdout_weight",
    "observed_net_positive_num",
    "holdout_raw_net_ppp"
  )
  if (length(setdiff(required, names(tune_df))) > 0 || nrow(tune_df) < 30) {
    return(list(
      thresholds = default_thresholds,
      status = "fallback_default_insufficient_tune_data",
      explored = 0L,
      feasible = 0L
    ))
  }

  prob_vals <- tune_df$pred_pr_net_pos
  net_vals <- tune_df$pred_net_ppp_mean

  play_pr_vals <- sort(unique(c(
    default_thresholds$play_pr_min,
    round(quantile(prob_vals, probs = c(0.60, 0.65, 0.70, 0.75), na.rm = TRUE), 3)
  )))
  lean_pr_vals <- sort(unique(c(
    default_thresholds$lean_pr_min,
    round(quantile(prob_vals, probs = c(0.50, 0.55, 0.60, 0.65), na.rm = TRUE), 3)
  )))
  limit_pr_vals <- sort(unique(c(
    default_thresholds$limit_pr_max,
    round(quantile(prob_vals, probs = c(0.35, 0.40, 0.45, 0.50, 0.55), na.rm = TRUE), 3)
  )))
  play_net_vals <- sort(unique(c(
    default_thresholds$play_net_min,
    round(quantile(net_vals, probs = c(0.55, 0.60, 0.65, 0.70), na.rm = TRUE), 3)
  )))
  lean_net_vals <- sort(unique(c(
    default_thresholds$lean_net_min,
    round(quantile(net_vals, probs = c(0.40, 0.45, 0.50, 0.55), na.rm = TRUE), 3)
  )))
  limit_net_vals <- sort(unique(c(
    default_thresholds$limit_net_max,
    round(quantile(net_vals, probs = c(0.25, 0.30, 0.35, 0.40), na.rm = TRUE), 3)
  )))

  explored <- 0L
  feasible <- 0L
  best <- NULL
  best_score <- -Inf

  for (play_pr in play_pr_vals) {
    for (lean_pr in lean_pr_vals) {
      if (!is.finite(play_pr) || !is.finite(lean_pr) || play_pr < lean_pr) next
      for (limit_pr in limit_pr_vals) {
        if (!is.finite(limit_pr) || limit_pr > lean_pr) next
        for (play_net in play_net_vals) {
          for (lean_net in lean_net_vals) {
            if (!is.finite(play_net) || !is.finite(lean_net) || play_net < lean_net) next
            for (limit_net in limit_net_vals) {
              if (!is.finite(limit_net) || limit_net > lean_net) next
              explored <- explored + 1L

              thr <- list(
                play_pr_min = play_pr,
                play_net_min = play_net,
                lean_pr_min = lean_pr,
                lean_net_min = lean_net,
                limit_pr_max = limit_pr,
                limit_net_max = limit_net
              )

              dec <- classify_decision(
                pr = tune_df$pred_pr_net_pos,
                net = tune_df$pred_net_ppp_mean,
                prior_possessions = tune_df$prior_possessions,
                thresholds = thr,
                decision_available = tune_df$decision_available
              )
              tmp <- tune_df %>% mutate(Decision_tuned = dec)
              b <- summarise_decision_buckets(tmp, "Decision_tuned")

              get_metric <- function(label, col, default = NA_real_) {
                v <- b %>% filter(Decision == label) %>% pull(.data[[col]])
                if (length(v) == 0) return(default)
                as.numeric(v[[1]])
              }

              play_pos <- get_metric("PLAY MORE", "total_holdout_possessions", 0)
              lean_pos <- get_metric("LEAN IN", "total_holdout_possessions", 0)
              limit_pos <- get_metric("LIMIT / WATCH", "total_holdout_possessions", 0)

              if (play_pos < 100 || lean_pos < 80 || limit_pos < 50) next

              play_obs <- get_metric("PLAY MORE", "weighted_observed_positive_rate")
              lean_obs <- get_metric("LEAN IN", "weighted_observed_positive_rate")
              limit_obs <- get_metric("LIMIT / WATCH", "weighted_observed_positive_rate")
              play_gap <- get_metric("PLAY MORE", "weighted_net_prob_calibration_gap")
              lean_gap <- get_metric("LEAN IN", "weighted_net_prob_calibration_gap")
              limit_gap <- get_metric("LIMIT / WATCH", "weighted_net_prob_calibration_gap")
              play_real <- get_metric("PLAY MORE", "weighted_realized_net_ppp")
              lean_real <- get_metric("LEAN IN", "weighted_realized_net_ppp")
              limit_real <- get_metric("LIMIT / WATCH", "weighted_realized_net_ppp")

              hard_ok <- is.finite(play_obs) && is.finite(lean_obs) && is.finite(limit_obs) &&
                is.finite(play_gap) && is.finite(lean_gap) && is.finite(limit_gap) &&
                is.finite(play_real) && is.finite(lean_real) && is.finite(limit_real) &&
                play_obs >= 0.62 && lean_obs >= 0.54 && limit_obs <= 0.44 &&
                play_real >= 0.08 && lean_real >= 0.00 && limit_real <= 0.00 &&
                abs(play_gap) <= 0.14 && abs(lean_gap) <= 0.16 && abs(limit_gap) <= 0.16

              if (!hard_ok) next
              feasible <- feasible + 1L

              actionable <- play_pos + lean_pos + limit_pos
              separation <- (play_obs - lean_obs) + (lean_obs - limit_obs)
              realized <- play_real + 0.75 * lean_real - pmax(limit_real, 0)
              calibration_penalty <- abs(play_gap) + abs(lean_gap) + abs(limit_gap)
              score <- actionable + 120 * separation + 180 * realized - 80 * calibration_penalty

              if (is.finite(score) && score > best_score) {
                best_score <- score
                best <- list(
                  thresholds = thr,
                  status = "ok",
                  explored = explored,
                  feasible = feasible,
                  tune_play_obs = play_obs,
                  tune_lean_obs = lean_obs,
                  tune_limit_obs = limit_obs,
                  tune_play_realized = play_real,
                  tune_lean_realized = lean_real,
                  tune_limit_realized = limit_real,
                  tune_score = score
                )
              }
            }
          }
        }
      }
    }
  }

  if (is.null(best)) {
    return(list(
      thresholds = default_thresholds,
      status = "fallback_default_no_feasible_grid_solution",
      explored = explored,
      feasible = feasible
    ))
  }
  best
}

# ---------- Config ----------
BT_MIN_PRIOR_GAMES <- as.integer(Sys.getenv("BT_MIN_PRIOR_GAMES", "5"))
BT_STAN_CHAINS <- as.integer(Sys.getenv("BT_STAN_CHAINS", "2"))
BT_STAN_ITER <- as.integer(Sys.getenv("BT_STAN_ITER", "1000"))
BT_STAN_WARMUP <- as.integer(Sys.getenv("BT_STAN_WARMUP", as.character(max(500, BT_STAN_ITER %/% 2))))
BT_STAN_SEED <- as.integer(Sys.getenv("BT_STAN_SEED", "20260222"))
BT_ADAPT_DELTA <- as.numeric(Sys.getenv("BT_ADAPT_DELTA", "0.99"))
BT_MAX_TREEDEPTH <- as.integer(Sys.getenv("BT_MAX_TREEDEPTH", "15"))
BT_FORCE_REFIT <- as_bool_env("BT_FORCE_REFIT")
BT_MAX_HOLDOUT_GAMES <- as.integer(Sys.getenv("BT_MAX_HOLDOUT_GAMES", "0")) # 0 = no limit

# Decision rule v2 seeds (full-net posterior predictive, baseline context).
# Final thresholds are tuned with a clean forward split in this script.
DECISION_RULE_PROB_POSSESSIONS <- as.numeric(Sys.getenv("DECISION_RULE_PROB_POSSESSIONS", "40"))
DECISION_PLAY_MORE_PR_NET_MIN <- as.numeric(Sys.getenv("DECISION_PLAY_MORE_PR_NET_MIN", "0.553"))
DECISION_PLAY_MORE_NET_PPP_MIN <- as.numeric(Sys.getenv("DECISION_PLAY_MORE_NET_PPP_MIN", "-0.015"))
DECISION_LEAN_IN_PR_NET_MIN <- as.numeric(Sys.getenv("DECISION_LEAN_IN_PR_NET_MIN", "0.528"))
DECISION_LEAN_IN_NET_PPP_MIN <- as.numeric(Sys.getenv("DECISION_LEAN_IN_NET_PPP_MIN", "-0.015"))
DECISION_LIMIT_WATCH_PR_NET_MAX <- as.numeric(Sys.getenv("DECISION_LIMIT_WATCH_PR_NET_MAX", "0.657"))
DECISION_LIMIT_WATCH_NET_PPP_MAX <- as.numeric(Sys.getenv("DECISION_LIMIT_WATCH_NET_PPP_MAX", "-0.06"))

# Forward split for nested-like threshold tuning.
BT_TUNE_FRACTION <- as.numeric(Sys.getenv("BT_TUNE_FRACTION", "0.70"))
BT_TUNE_MIN_GAMES <- as.integer(Sys.getenv("BT_TUNE_MIN_GAMES", "8"))

# Strict calibration gates.
BT_CALIB_MIN_ROWS <- as.integer(Sys.getenv("BT_CALIB_MIN_ROWS", "80"))
BT_CALIB_MIN_GAMES <- as.integer(Sys.getenv("BT_CALIB_MIN_GAMES", "8"))
BT_CALIB_MAX_ECE <- as.numeric(Sys.getenv("BT_CALIB_MAX_ECE", "0.08"))
BT_CALIB_MAX_DECILE_GAP <- as.numeric(Sys.getenv("BT_CALIB_MAX_DECILE_GAP", "0.18"))
BT_CALIB_SLOPE_MIN <- as.numeric(Sys.getenv("BT_CALIB_SLOPE_MIN", "0.70"))
BT_CALIB_SLOPE_MAX <- as.numeric(Sys.getenv("BT_CALIB_SLOPE_MAX", "1.30"))
BT_CALIB_FALLBACK_SHRINK <- as.numeric(Sys.getenv("BT_CALIB_FALLBACK_SHRINK", "0.85"))

if (!is.finite(BT_MIN_PRIOR_GAMES) || BT_MIN_PRIOR_GAMES < 1) BT_MIN_PRIOR_GAMES <- 5L
if (!is.finite(BT_STAN_CHAINS) || BT_STAN_CHAINS < 1) BT_STAN_CHAINS <- 2L
if (!is.finite(BT_STAN_ITER) || BT_STAN_ITER < 200) BT_STAN_ITER <- 1000L
if (!is.finite(BT_STAN_WARMUP) || BT_STAN_WARMUP < 100 || BT_STAN_WARMUP >= BT_STAN_ITER) {
  BT_STAN_WARMUP <- max(100L, min(BT_STAN_ITER - 100L, BT_STAN_ITER %/% 2L))
}
if (!is.finite(BT_TUNE_FRACTION) || BT_TUNE_FRACTION <= 0.50 || BT_TUNE_FRACTION >= 0.95) BT_TUNE_FRACTION <- 0.70
if (!is.finite(BT_TUNE_MIN_GAMES) || BT_TUNE_MIN_GAMES < 4) BT_TUNE_MIN_GAMES <- 8L
if (!is.finite(BT_CALIB_MIN_ROWS) || BT_CALIB_MIN_ROWS < 30) BT_CALIB_MIN_ROWS <- 80L
if (!is.finite(BT_CALIB_MIN_GAMES) || BT_CALIB_MIN_GAMES < 4) BT_CALIB_MIN_GAMES <- 8L
if (!is.finite(BT_CALIB_MAX_ECE) || BT_CALIB_MAX_ECE <= 0 || BT_CALIB_MAX_ECE >= 0.5) BT_CALIB_MAX_ECE <- 0.08
if (!is.finite(BT_CALIB_MAX_DECILE_GAP) || BT_CALIB_MAX_DECILE_GAP <= 0 || BT_CALIB_MAX_DECILE_GAP >= 0.5) BT_CALIB_MAX_DECILE_GAP <- 0.18
if (!is.finite(BT_CALIB_SLOPE_MIN) || BT_CALIB_SLOPE_MIN <= 0 || BT_CALIB_SLOPE_MIN >= 1) BT_CALIB_SLOPE_MIN <- 0.70
if (!is.finite(BT_CALIB_SLOPE_MAX) || BT_CALIB_SLOPE_MAX <= 1 || BT_CALIB_SLOPE_MAX > 3) BT_CALIB_SLOPE_MAX <- 1.30
if (!is.finite(BT_CALIB_FALLBACK_SHRINK) || BT_CALIB_FALLBACK_SHRINK <= 0 || BT_CALIB_FALLBACK_SHRINK >= 1) {
  BT_CALIB_FALLBACK_SHRINK <- 0.85
}

message(
  "Backtest config | min_prior_games=", BT_MIN_PRIOR_GAMES,
  " chains=", BT_STAN_CHAINS,
  " iter=", BT_STAN_ITER,
  " warmup=", BT_STAN_WARMUP,
  " force_refit=", BT_FORCE_REFIT
)
message(
  "Decision rule v2 | prob_possessions=", DECISION_RULE_PROB_POSSESSIONS,
  " | PLAY_MORE: pr>=", DECISION_PLAY_MORE_PR_NET_MIN, " & net_ppp>=", DECISION_PLAY_MORE_NET_PPP_MIN,
  " | LEAN_IN: pr>=", DECISION_LEAN_IN_PR_NET_MIN, " & net_ppp>=", DECISION_LEAN_IN_NET_PPP_MIN,
  " | LIMIT/WATCH if pr<=", DECISION_LIMIT_WATCH_PR_NET_MAX, " or net_ppp<=", DECISION_LIMIT_WATCH_NET_PPP_MAX
)
message(
  "Forward tuning config | tune_fraction=", BT_TUNE_FRACTION,
  " min_tune_games=", BT_TUNE_MIN_GAMES,
  " | strict calibration rows>=", BT_CALIB_MIN_ROWS,
  " games>=", BT_CALIB_MIN_GAMES,
  " ece<=", BT_CALIB_MAX_ECE,
  " max_gap<=", BT_CALIB_MAX_DECILE_GAP,
  " slope in [", BT_CALIB_SLOPE_MIN, ", ", BT_CALIB_SLOPE_MAX, "]",
  " | fallback_shrink=", BT_CALIB_FALLBACK_SHRINK
)

# ---------- Paths ----------
stints_path <- resolve_path("uconn_stints_from_pbp.csv")
games_path  <- resolve_path("uconn_games_meta.csv")
opp_path    <- resolve_path("opponent_controls.csv")
stan_path   <- resolve_path("uconn_lineup_gamelevel_offdef.stan")

models_bt_dir <- file.path("_models", "backtests")
dir.create(models_bt_dir, recursive = TRUE, showWarnings = FALSE)

out_dir <- file.path("_outputs", "05_decision_audit")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

rows_out_path <- file.path(out_dir, "uconn_lineup_decision_rolling_backtest_rows.csv")
bucket_out_path <- file.path(out_dir, "uconn_lineup_decision_rolling_backtest_by_bucket.csv")
game_bucket_out_path <- file.path(out_dir, "uconn_lineup_decision_rolling_backtest_by_game_bucket.csv")
diag_out_path <- file.path(out_dir, "uconn_lineup_decision_rolling_backtest_fit_diagnostics.csv")
rule_v2_thresholds_out_path <- file.path(out_dir, "uconn_lineup_decision_rule_v2_thresholds.csv")
calibration_model_out_path <- file.path(out_dir, "uconn_pred_pr_net_pos_calibration_model.csv")

# ---------- Load + preprocess (matches core model pipeline) ----------
model_inputs <- load_common_lineup_model_inputs(
  stints_path = stints_path,
  games_path = games_path,
  opp_path = opp_path,
  add_global_game_id = TRUE
)
stints <- model_inputs$stints
games <- model_inputs$games
games2 <- model_inputs$games_joined

tiny_poss_n <- nrow(model_inputs$tiny_poss)
if (tiny_poss_n > 0) {
  message("Found ", tiny_poss_n, " rows with 0 < poss_est < 1 (clamped to 1).")
}

missing_controls <- games2 %>%
  filter(is.na(opp_adjO) | is.na(opp_adjD)) %>%
  select(game_file, game_date, opponent)

if (nrow(missing_controls) > 0) {
  print(missing_controls)
  stop("Some games are missing opponent controls.")
}

stints2 <- stints %>%
  filter(
    lineup_size == 5,
    !is.na(poss_est), poss_est > 0,
    !is.na(net_ppp),
    !is.na(score_margin_start),
    !is.na(elapsed_game_sec)
  ) %>%
  mutate(
    game_id = as.integer(factor(game_file, levels = games2$game_file)),
    uconn_lineup_canon = canonicalize_lineup(uconn_lineup),
    y = net_ppp,
    w = poss_est,
    net_pts_stint = y * w
  )

if (nrow(stints2) < 50) stop("Too few modeling rows after preprocessing.")

eligible_games <- sort(unique(stints2$game_id))
holdout_games <- eligible_games[eligible_games > BT_MIN_PRIOR_GAMES]
if (length(holdout_games) == 0) {
  stop("No holdout games available after BT_MIN_PRIOR_GAMES=", BT_MIN_PRIOR_GAMES)
}

if (BT_MAX_HOLDOUT_GAMES > 0) {
  holdout_games <- tail(holdout_games, BT_MAX_HOLDOUT_GAMES)
}

message(
  "Rolling backtest holdout games: ", length(holdout_games),
  " (game_id ", min(holdout_games), " to ", max(holdout_games), ")"
)

# ---------- Rolling backtest ----------
row_results <- list()
fit_diags <- list()

for (idx in seq_along(holdout_games)) {
  g <- holdout_games[[idx]]

  holdout_game_file <- games2$game_file[games2$global_game_id == g][[1]]
  holdout_game_date <- games2$game_date[games2$global_game_id == g][[1]]

  message(
    "\n[", idx, "/", length(holdout_games), "] Holdout game_id=", g,
    " | ", holdout_game_file
  )

  train_rows <- stints2 %>% filter(game_id < g)
  test_rows  <- stints2 %>% filter(game_id == g)

  if (nrow(test_rows) == 0) {
    message("Skipping holdout game with no eligible rows after filters.")
    next
  }
  if (nrow(train_rows) < 10) {
    message("Skipping holdout game due to too-small train set.")
    next
  }

  train_game_ids <- sort(unique(train_rows$game_id))
  games_train <- games2 %>%
    filter(global_game_id %in% train_game_ids) %>%
    arrange(global_game_id)

  oppO_scaler <- fit_scaler(games_train$opp_adjO)
  oppD_scaler <- fit_scaler(games_train$opp_adjD)
  score_margin_scaler <- fit_scaler(train_rows$score_margin_start)
  elapsed_game_scaler <- fit_scaler(train_rows$elapsed_game_sec)

  # Build training indices and model matrices
  players <- sort(unique(unlist(str_split(train_rows$uconn_lineup_canon, "\\|"))))
  player_id <- setNames(seq_along(players), players)

  lineups <- sort(unique(train_rows$uconn_lineup_canon))
  lineup_id <- setNames(seq_along(lineups), lineups)

  train_uconn5 <- t(sapply(
    str_split(train_rows$uconn_lineup_canon, "\\|"),
    function(x) player_id[x]
  ))

  train_game_id_compact <- match(train_rows$game_id, train_game_ids)
  if (any(is.na(train_game_id_compact))) stop("Failed to map train game IDs.")

  opp_adjO_z_train <- apply_scaler(games_train$opp_adjO, oppO_scaler)
  opp_adjD_z_train <- apply_scaler(games_train$opp_adjD, oppD_scaler)
  train_score_margin_start_z <- apply_scaler(train_rows$score_margin_start, score_margin_scaler)
  train_elapsed_game_sec_z <- apply_scaler(train_rows$elapsed_game_sec, elapsed_game_scaler)

  stan_data <- list(
    N = nrow(train_rows),
    P = length(players),
    L = length(lineups),
    uconn5 = train_uconn5,
    lineup_id = as.integer(lineup_id[train_rows$uconn_lineup_canon]),
    game_id = as.integer(train_game_id_compact),
    G = length(train_game_ids),
    y = as.numeric(train_rows$y),
    w = as.numeric(train_rows$w),
    opp_adjO_z = opp_adjO_z_train,
    opp_adjD_z = opp_adjD_z_train,
    site_home = as.numeric(games_train$site_home),
    score_margin_start_z = as.numeric(train_score_margin_start_z),
    elapsed_game_sec_z = as.numeric(train_elapsed_game_sec_z)
  )

  fit_path <- file.path(models_bt_dir, sprintf("uconn_lineup_bt_holdout_game_%02d_fit.rds", g))
  fit <- NULL
  used_cache <- FALSE

  model_signature <- paste(
    "schema_v2",
    "holdout", g,
    "stints", safe_md5(stints_path),
    "games", safe_md5(games_path),
    "opp", safe_md5(opp_path),
    "stan", safe_md5(stan_path),
    "N", stan_data$N,
    "P", stan_data$P,
    "L", stan_data$L,
    "G", stan_data$G,
    "train_games", paste(train_game_ids, collapse = ","),
    "iter", BT_STAN_ITER,
    "warmup", BT_STAN_WARMUP,
    "chains", BT_STAN_CHAINS,
    sep = "|"
  )

  if (!BT_FORCE_REFIT && file.exists(fit_path)) {
    cached <- readRDS(fit_path)
    cached_sig <- attr(cached, "model_signature")
    if (!is.null(cached_sig) && identical(cached_sig, model_signature) && fit_has_draws(cached, "u")) {
      fit <- cached
      used_cache <- TRUE
      message("Cache hit: ", basename(fit_path))
    } else {
      message("Cache invalid/mismatch for holdout game ", g, "; refitting.")
    }
  }

  if (is.null(fit)) {
    init_fun <- function() list(
      intercept = 0.0,
      sigma = 0.8,
      tau_net = 0.08,
      tau_u = 0.05,
      b_oppO = 0.0,
      b_oppD = 0.0,
      b_home = 0.0,
      b_score_margin = 0.0,
      b_elapsed_game = 0.0
    )

    fit <- rstan::stan(
      file = stan_path,
      data = stan_data,
      chains = BT_STAN_CHAINS,
      iter = BT_STAN_ITER,
      warmup = BT_STAN_WARMUP,
      seed = BT_STAN_SEED + g,
      init = init_fun,
      refresh = 50,
      control = list(adapt_delta = BT_ADAPT_DELTA, max_treedepth = BT_MAX_TREEDEPTH)
    )

    if (!fit_has_draws(fit, "u")) {
      stop("Rolling backtest fit produced no usable samples for holdout game_id=", g)
    }

    attr(fit, "model_signature") <- model_signature
    saveRDS(fit, fit_path)
  }

  # Fold diagnostics
  sampler_params <- tryCatch(rstan::get_sampler_params(fit, inc_warmup = FALSE), error = function(e) NULL)
  fold_diag <- tibble(
    holdout_game_id = g,
    holdout_game_file = holdout_game_file,
    holdout_game_date = holdout_game_date,
    prior_games = length(train_game_ids),
    train_rows = nrow(train_rows),
    train_players = length(players),
    train_lineups = length(lineups),
    holdout_rows = nrow(test_rows),
    used_cache = used_cache,
    divergences = NA_integer_,
    max_treedepth = NA_integer_,
    treedepth_hits_limit = NA_integer_
  )
  if (!is.null(sampler_params)) {
    sp_df <- bind_rows(lapply(seq_along(sampler_params), function(i) as_tibble(sampler_params[[i]]) %>% mutate(chain = i)))
    fold_diag <- fold_diag %>%
      mutate(
        divergences = sum(sp_df$divergent__, na.rm = TRUE),
        max_treedepth = suppressWarnings(max(sp_df$treedepth__, na.rm = TRUE)),
        treedepth_hits_limit = sum(sp_df$treedepth__ >= BT_MAX_TREEDEPTH, na.rm = TRUE)
      )
  }
  fit_diags[[length(fit_diags) + 1L]] <- fold_diag

  # Training decision table (same logic as core script)
  post <- rstan::extract(fit)
  intercept_draws <- post$intercept
  alpha_net_draws <- post$alpha_net
  u_draws <- post$u
  sigma_draws <- post$sigma
  b_oppO_draws <- post$b_oppO
  b_oppD_draws <- post$b_oppD
  b_home_draws <- post$b_home
  b_score_margin_draws <- post$b_score_margin
  b_elapsed_game_draws <- post$b_elapsed_game
  if (is.null(u_draws) || length(dim(u_draws)) < 2) stop("u missing/invalid in posterior for holdout game ", g)
  if (is.null(alpha_net_draws) || length(dim(alpha_net_draws)) < 2) stop("alpha_net missing/invalid in posterior for holdout game ", g)
  if (is.null(sigma_draws) || !is.numeric(sigma_draws) || length(sigma_draws) == 0) stop("sigma missing/invalid in posterior for holdout game ", g)

  holdout_game_meta <- games2 %>% filter(global_game_id == g) %>% slice(1)
  holdout_opp_adjO_z <- apply_scaler(holdout_game_meta$opp_adjO, oppO_scaler)[1]
  holdout_opp_adjD_z <- apply_scaler(holdout_game_meta$opp_adjD, oppD_scaler)[1]
  holdout_site_home <- as.numeric(holdout_game_meta$site_home[[1]])

  train_usage <- train_rows %>%
    group_by(uconn_lineup_canon) %>%
    summarise(
      possessions = sum(w, na.rm = TRUE),
      minutes = sum(dur_min, na.rm = TRUE),
      segments = n(),
      games = n_distinct(game_file),
      raw_net_ppp = sum(net_pts_stint, na.rm = TRUE) / sum(w, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      lineup_id = as.integer(lineup_id[uconn_lineup_canon]),
      lineup_pretty = uconn_lineup_canon
    )

  pace_train <- sum(train_usage$possessions, na.rm = TRUE) / sum(train_usage$minutes, na.rm = TRUE)
  if (!is.finite(pace_train) || pace_train <= 0) pace_train <- NA_real_
  pace_train_scalar <- pace_train

  u_mean <- colMeans(u_draws)
  u_q <- apply(u_draws, 2, quantile, probs = c(0.05, 0.5, 0.95))

  lineup_bayes <- tibble(
    lineup_id = seq_along(lineups),
    lineup = lineups,
    synergy_mean = u_mean,
    synergy_p05 = u_q[1, ],
    synergy_p50 = u_q[2, ],
    synergy_p95 = u_q[3, ],
    pr_synergy_pos = colMeans(u_draws > 0)
  ) %>%
    mutate(lineup_pretty = lineup)

  # Baseline-context full-net predictions for the decision table (context z=0, away_or_neutral baseline).
  train_decision_pred_tbl <- bind_rows(lapply(seq_along(lineups), function(lid) {
    lineup_str <- lineups[[lid]]
    pids <- unname(player_id[str_split(lineup_str, "\\|")[[1]]])
    if (any(is.na(pids))) {
      return(tibble(
        lineup = lineup_str,
        decision_pred_net_ppp_mean = NA_real_,
        decision_pred_pr_net_pos = NA_real_
      ))
    }

    alpha_sum_draws <- rowSums(alpha_net_draws[, pids, drop = FALSE])
    mu_draws <- intercept_draws + alpha_sum_draws + u_draws[, lid]
    pred_sd_draws <- sigma_draws / sqrt(DECISION_RULE_PROB_POSSESSIONS)
    ok_draws <- is.finite(mu_draws) & is.finite(pred_sd_draws) & pred_sd_draws > 0
    pr_pos <- if (any(ok_draws)) mean(pnorm(mu_draws[ok_draws] / pred_sd_draws[ok_draws]), na.rm = TRUE) else NA_real_

    tibble(
      lineup = lineup_str,
      decision_pred_net_ppp_mean = mean(mu_draws, na.rm = TRUE),
      decision_pred_pr_net_pos = pr_pos
    )
  }))

  coach_pred <- lineup_bayes %>%
    left_join(train_decision_pred_tbl, by = "lineup") %>%
    left_join(
      train_usage %>% select(lineup_id, possessions, minutes, raw_net_ppp, games, segments),
      by = "lineup_id"
    ) %>%
    mutate(
      expected_points_per_40 = if (is.finite(pace_train_scalar)) synergy_mean * 40 * pace_train_scalar else NA_real_,
      minutes = round(minutes, 1),
      possessions = round(possessions, 0),
      raw_net_ppp = round(raw_net_ppp, 3),
      synergy_mean = round(synergy_mean, 3),
      synergy_p05 = round(synergy_p05, 3),
      synergy_p95 = round(synergy_p95, 3),
      pr_synergy_pos = round(pr_synergy_pos, 3),
      decision_pred_net_ppp_mean = round(decision_pred_net_ppp_mean, 3),
      decision_pred_pr_net_pos = round(decision_pred_pr_net_pos, 3),
      expected_points_per_40 = round(expected_points_per_40, 1),
      Decision = case_when(
        possessions >= 70 &
          decision_pred_pr_net_pos >= DECISION_PLAY_MORE_PR_NET_MIN &
          decision_pred_net_ppp_mean >= DECISION_PLAY_MORE_NET_PPP_MIN ~ "PLAY MORE",
        possessions >= 45 &
          decision_pred_pr_net_pos >= DECISION_LEAN_IN_PR_NET_MIN &
          decision_pred_net_ppp_mean >= DECISION_LEAN_IN_NET_PPP_MIN ~ "LEAN IN",
        possessions >= 30 &
          (
            decision_pred_pr_net_pos <= DECISION_LIMIT_WATCH_PR_NET_MAX |
            decision_pred_net_ppp_mean <= DECISION_LIMIT_WATCH_NET_PPP_MAX
          ) ~ "LIMIT / WATCH",
        possessions < 30 ~ "TOO SMALL",
        TRUE ~ "NEUTRAL"
      )
    ) %>%
    select(
      lineup,
      lineup_pretty,
      prior_possessions = possessions,
      prior_minutes = minutes,
      prior_games = games,
      prior_segments = segments,
      prior_raw_net_ppp = raw_net_ppp,
      synergy_mean,
      synergy_p05,
      synergy_p95,
      pr_synergy_pos,
      decision_pred_net_ppp_mean,
      decision_pred_pr_net_pos,
      pred_points_per_40 = expected_points_per_40,
      Decision
    )

  # Holdout realized lineup outcomes (game-level)
  holdout_lineups <- test_rows %>%
  group_by(uconn_lineup_canon) %>%
  summarise(
    holdout_possessions = sum(w, na.rm = TRUE),
    holdout_minutes = sum(dur_min, na.rm = TRUE),
    holdout_segments = n(),
    holdout_net_pts = sum(net_pts_stint, na.rm = TRUE),
    holdout_raw_net_ppp = sum(net_pts_stint, na.rm = TRUE) / sum(w, na.rm = TRUE),
    holdout_score_margin_start = weighted_mean_safe(score_margin_start, w),
    holdout_elapsed_game_sec = weighted_mean_safe(elapsed_game_sec, w),
    .groups = "drop"
  ) %>%
  mutate(
    holdout_score_margin_start_z = apply_scaler(holdout_score_margin_start, score_margin_scaler),
    holdout_elapsed_game_sec_z = apply_scaler(holdout_elapsed_game_sec, elapsed_game_scaler),
    lineup_pretty_holdout = uconn_lineup_canon
  )

  seen_holdout_lineups <- holdout_lineups %>%
    semi_join(coach_pred, by = c("uconn_lineup_canon" = "lineup"))

  net_pred_tbl <- tibble(
    lineup = character(),
    pred_net_ppp_mean = numeric(),
    pred_pr_net_pos = numeric()
  )

  if (nrow(seen_holdout_lineups) > 0) {
    net_pred_tbl <- bind_rows(lapply(seq_len(nrow(seen_holdout_lineups)), function(i) {
      lineup_str <- seen_holdout_lineups$uconn_lineup_canon[[i]]
      lid <- unname(lineup_id[[lineup_str]])
      pids <- unname(player_id[str_split(lineup_str, "\\|")[[1]]])

      if (is.na(lid) || any(is.na(pids))) {
        return(tibble(
          lineup = lineup_str,
          pred_net_ppp_mean = NA_real_,
          pred_pr_net_pos = NA_real_
        ))
      }

      alpha_sum_draws <- rowSums(alpha_net_draws[, pids, drop = FALSE])
      mu_draws <- intercept_draws +
        alpha_sum_draws +
        u_draws[, lid] +
        b_oppO_draws * holdout_opp_adjO_z +
        b_oppD_draws * holdout_opp_adjD_z +
        b_home_draws * holdout_site_home +
        b_score_margin_draws * seen_holdout_lineups$holdout_score_margin_start_z[[i]] +
        b_elapsed_game_draws * seen_holdout_lineups$holdout_elapsed_game_sec_z[[i]]

      holdout_w <- as.numeric(seen_holdout_lineups$holdout_possessions[[i]])
      holdout_w <- ifelse(is.finite(holdout_w) && holdout_w > 0, holdout_w, NA_real_)
      ppd_pr_net_pos <- NA_real_
      if (is.finite(holdout_w)) {
        pred_sd_draws <- sigma_draws / sqrt(holdout_w)
        ok_draws <- is.finite(mu_draws) & is.finite(pred_sd_draws) & pred_sd_draws > 0
        if (any(ok_draws)) {
          # Strict posterior-predictive probability for the aggregated holdout lineup PPP.
          ppd_pr_net_pos <- mean(pnorm(mu_draws[ok_draws] / pred_sd_draws[ok_draws]), na.rm = TRUE)
        }
      }

      tibble(
        lineup = lineup_str,
        pred_net_ppp_mean = mean(mu_draws, na.rm = TRUE),
        pred_pr_net_pos = ppd_pr_net_pos
      )
    }))
  }

  fold_rows <- holdout_lineups %>%
    left_join(coach_pred, by = c("uconn_lineup_canon" = "lineup")) %>%
    left_join(net_pred_tbl, by = c("uconn_lineup_canon" = "lineup")) %>%
    mutate(
      holdout_game_id = g,
      holdout_game_file = holdout_game_file,
      holdout_game_date = holdout_game_date,
      prior_games_available = length(train_game_ids),
      pace_train = pace_train_scalar,
      holdout_points_per_40 = if (is.finite(pace_train_scalar)) holdout_raw_net_ppp * 40 * pace_train_scalar else NA_real_,
      observed_net_positive = holdout_raw_net_ppp > 0,
      decision_available = !is.na(Decision),
      Decision = if_else(is.na(Decision), "UNSEEN", Decision),
      lineup_pretty = coalesce(lineup_pretty, lineup_pretty_holdout)
    ) %>%
    select(
      holdout_game_id,
      holdout_game_file,
      holdout_game_date,
      prior_games_available,
      lineup = uconn_lineup_canon,
      lineup_pretty,
      decision_available,
      Decision,
      prior_possessions,
      prior_minutes,
      prior_games,
      prior_segments,
      prior_raw_net_ppp,
      synergy_mean,
      synergy_p05,
      synergy_p95,
      pr_synergy_pos,
      decision_pred_net_ppp_mean,
      decision_pred_pr_net_pos,
      pred_net_ppp_mean,
      pred_pr_net_pos,
      pred_points_per_40,
      holdout_possessions,
      holdout_minutes,
      holdout_segments,
      holdout_net_pts,
      holdout_raw_net_ppp,
      holdout_points_per_40,
      holdout_score_margin_start,
      holdout_elapsed_game_sec,
      observed_net_positive
    )

  row_results[[length(row_results) + 1L]] <- fold_rows
}

rows_df <- bind_rows(row_results)
if (nrow(rows_df) == 0) stop("Backtest produced no evaluation rows.")

diag_df <- bind_rows(fit_diags)

# ---------- Summaries ----------
eval_df <- rows_df %>%
  mutate(
    observed_net_positive_num = as.numeric(observed_net_positive),
    holdout_weight = holdout_possessions,
    has_prob_pred = is.finite(pred_pr_net_pos),
    has_value_pred = is.finite(pred_net_ppp_mean)
  )

# Clean forward split (tune early holdout games, evaluate on later holdout games).
decision_eval <- eval_df %>%
  filter(
    decision_available,
    has_prob_pred,
    has_value_pred,
    is.finite(prior_possessions),
    is.finite(holdout_weight),
    holdout_weight > 0
  )

split_games <- sort(unique(decision_eval$holdout_game_id))
tune_games <- split_games
forward_games <- integer()
if (length(split_games) >= 6) {
  tune_n <- floor(length(split_games) * BT_TUNE_FRACTION)
  tune_n <- max(BT_TUNE_MIN_GAMES, tune_n)
  tune_n <- min(tune_n, length(split_games) - 2L)
  if (is.finite(tune_n) && tune_n >= 1 && tune_n < length(split_games)) {
    tune_games <- split_games[seq_len(tune_n)]
    forward_games <- split_games[(tune_n + 1L):length(split_games)]
  }
}

message(
  "Forward split | tune_games=", length(tune_games),
  " (", if (length(tune_games) > 0) min(tune_games) else NA_integer_, "-",
  if (length(tune_games) > 0) max(tune_games) else NA_integer_, ")",
  " | forward_games=", length(forward_games),
  " (", if (length(forward_games) > 0) min(forward_games) else NA_integer_, "-",
  if (length(forward_games) > 0) max(forward_games) else NA_integer_, ")"
)

# Strict calibration handling for decision probabilities.
calib_train <- decision_eval %>%
  filter(holdout_game_id %in% tune_games)

calib_fit <- fit_platt_calibration(
  calib_train,
  prob_col = "pred_pr_net_pos",
  outcome_col = "observed_net_positive_num",
  weight_col = "holdout_weight"
)
calib_raw_metrics <- calc_prob_metrics(
  calib_train$pred_pr_net_pos,
  calib_train$observed_net_positive_num,
  calib_train$holdout_weight
)
platt_train_prob <- apply_platt_calibration(
  calib_train$pred_pr_net_pos,
  intercept = calib_fit$intercept[[1]],
  slope = calib_fit$slope[[1]]
)
calib_platt_metrics <- calc_prob_metrics(
  platt_train_prob,
  calib_train$observed_net_positive_num,
  calib_train$holdout_weight
)

calibration_mode <- "shrunk_raw"
calibration_status <- "fallback_shrink_strict_gate_fail"
calibration_intercept <- NA_real_
calibration_slope <- NA_real_

platt_strict_ok <- (
  nrow(calib_train) >= BT_CALIB_MIN_ROWS &&
    n_distinct(calib_train$holdout_game_id) >= BT_CALIB_MIN_GAMES &&
    identical(calib_fit$status[[1]], "ok") &&
    is.finite(calib_fit$slope[[1]]) &&
    calib_fit$slope[[1]] >= BT_CALIB_SLOPE_MIN &&
    calib_fit$slope[[1]] <= BT_CALIB_SLOPE_MAX &&
    is.finite(calib_platt_metrics$weighted_ece_decile[[1]]) &&
    calib_platt_metrics$weighted_ece_decile[[1]] <= BT_CALIB_MAX_ECE &&
    is.finite(calib_platt_metrics$max_abs_weighted_decile_gap[[1]]) &&
    calib_platt_metrics$max_abs_weighted_decile_gap[[1]] <= BT_CALIB_MAX_DECILE_GAP &&
    is.finite(calib_raw_metrics$weighted_ece_decile[[1]]) &&
    calib_platt_metrics$weighted_ece_decile[[1]] <= (calib_raw_metrics$weighted_ece_decile[[1]] - 0.005)
)

if (platt_strict_ok) {
  calibration_mode <- "platt"
  calibration_status <- "ok_strict"
  calibration_intercept <- calib_fit$intercept[[1]]
  calibration_slope <- calib_fit$slope[[1]]
}

raw_prob_all <- as.numeric(eval_df$pred_pr_net_pos)
if (identical(calibration_mode, "platt")) {
  calibrated_prob_all <- apply_platt_calibration(
    raw_prob_all,
    intercept = calibration_intercept,
    slope = calibration_slope
  )
} else {
  calibrated_prob_all <- 0.5 + BT_CALIB_FALLBACK_SHRINK * (clamp_prob(raw_prob_all) - 0.5)
}
calibrated_prob_all <- clamp_prob(calibrated_prob_all, eps = 0.01)
calibrated_prob_all[!is.finite(raw_prob_all)] <- NA_real_

eval_df <- eval_df %>%
  mutate(
    pred_pr_net_pos_raw = raw_prob_all,
    pred_pr_net_pos_calibrated = calibrated_prob_all,
    pred_pr_net_pos_for_decision = pred_pr_net_pos_calibrated,
    pred_pr_net_pos = pred_pr_net_pos_for_decision,
    calibration_mode = calibration_mode
  )

message(
  "Calibration mode: ", calibration_mode,
  " | status=", calibration_status,
  " | raw_ece=", round(calib_raw_metrics$weighted_ece_decile[[1]], 4),
  " | used_ece=", round(
    if (identical(calibration_mode, "platt")) calib_platt_metrics$weighted_ece_decile[[1]] else
      calc_prob_metrics(
        eval_df$pred_pr_net_pos,
        eval_df$observed_net_positive_num,
        eval_df$holdout_weight
      )$weighted_ece_decile[[1]],
    4
  )
)

default_thresholds <- list(
  play_pr_min = DECISION_PLAY_MORE_PR_NET_MIN,
  play_net_min = DECISION_PLAY_MORE_NET_PPP_MIN,
  lean_pr_min = DECISION_LEAN_IN_PR_NET_MIN,
  lean_net_min = DECISION_LEAN_IN_NET_PPP_MIN,
  limit_pr_max = DECISION_LIMIT_WATCH_PR_NET_MAX,
  limit_net_max = DECISION_LIMIT_WATCH_NET_PPP_MAX
)

tune_threshold_df <- eval_df %>%
  filter(
    holdout_game_id %in% tune_games,
    decision_available,
    is.finite(pred_pr_net_pos),
    is.finite(pred_net_ppp_mean),
    is.finite(prior_possessions),
    is.finite(holdout_weight),
    holdout_weight > 0
  )

tune_result <- tune_rule_v2_forward(tune_threshold_df, default_thresholds)
tuned_thresholds <- tune_result$thresholds

eval_df <- eval_df %>%
  mutate(
    Decision = classify_decision(
      pr = pred_pr_net_pos,
      net = pred_net_ppp_mean,
      prior_possessions = prior_possessions,
      thresholds = tuned_thresholds,
      decision_available = decision_available
    )
  )

forward_eval_df <- eval_df %>% filter(holdout_game_id %in% forward_games)
forward_bucket <- summarise_decision_buckets(forward_eval_df, "Decision")
forward_play_obs <- forward_bucket %>% filter(Decision == "PLAY MORE") %>% pull(weighted_observed_positive_rate)
forward_lean_obs <- forward_bucket %>% filter(Decision == "LEAN IN") %>% pull(weighted_observed_positive_rate)
forward_limit_obs <- forward_bucket %>% filter(Decision == "LIMIT / WATCH") %>% pull(weighted_observed_positive_rate)
forward_play_real <- forward_bucket %>% filter(Decision == "PLAY MORE") %>% pull(weighted_realized_net_ppp)
forward_lean_real <- forward_bucket %>% filter(Decision == "LEAN IN") %>% pull(weighted_realized_net_ppp)
forward_limit_real <- forward_bucket %>% filter(Decision == "LIMIT / WATCH") %>% pull(weighted_realized_net_ppp)

rule_v2_thresholds <- tibble(
  metric = c(
    "DECISION_RULE_PROB_POSSESSIONS",
    "DECISION_PLAY_MORE_PR_NET_MIN",
    "DECISION_PLAY_MORE_NET_PPP_MIN",
    "DECISION_LEAN_IN_PR_NET_MIN",
    "DECISION_LEAN_IN_NET_PPP_MIN",
    "DECISION_LIMIT_WATCH_PR_NET_MAX",
    "DECISION_LIMIT_WATCH_NET_PPP_MAX",
    "TUNING_STATUS",
    "TUNING_GRID_EXPLORED",
    "TUNING_GRID_FEASIBLE",
    "TUNE_GAMES_N",
    "FORWARD_GAMES_N",
    "CALIBRATION_MODE",
    "CALIBRATION_STATUS",
    "CALIBRATION_INTERCEPT",
    "CALIBRATION_SLOPE",
    "CALIBRATION_TRAIN_WEIGHTED_ECE_RAW",
    "CALIBRATION_TRAIN_WEIGHTED_ECE_PLATT",
    "CALIBRATION_TRAIN_MAX_DECILE_GAP_RAW",
    "CALIBRATION_TRAIN_MAX_DECILE_GAP_PLATT",
    "FORWARD_PLAY_MORE_WEIGHTED_OBS_POS_RATE",
    "FORWARD_LEAN_IN_WEIGHTED_OBS_POS_RATE",
    "FORWARD_LIMIT_WATCH_WEIGHTED_OBS_POS_RATE",
    "FORWARD_PLAY_MORE_WEIGHTED_REALIZED_NET_PPP",
    "FORWARD_LEAN_IN_WEIGHTED_REALIZED_NET_PPP",
    "FORWARD_LIMIT_WATCH_WEIGHTED_REALIZED_NET_PPP"
  ),
  value = as.character(c(
    DECISION_RULE_PROB_POSSESSIONS,
    tuned_thresholds$play_pr_min,
    tuned_thresholds$play_net_min,
    tuned_thresholds$lean_pr_min,
    tuned_thresholds$lean_net_min,
    tuned_thresholds$limit_pr_max,
    tuned_thresholds$limit_net_max,
    tune_result$status,
    tune_result$explored,
    tune_result$feasible,
    length(tune_games),
    length(forward_games),
    calibration_mode,
    calibration_status,
    calibration_intercept,
    calibration_slope,
    calib_raw_metrics$weighted_ece_decile[[1]],
    calib_platt_metrics$weighted_ece_decile[[1]],
    calib_raw_metrics$max_abs_weighted_decile_gap[[1]],
    calib_platt_metrics$max_abs_weighted_decile_gap[[1]],
    ifelse(length(forward_play_obs) > 0, forward_play_obs[[1]], NA_real_),
    ifelse(length(forward_lean_obs) > 0, forward_lean_obs[[1]], NA_real_),
    ifelse(length(forward_limit_obs) > 0, forward_limit_obs[[1]], NA_real_),
    ifelse(length(forward_play_real) > 0, forward_play_real[[1]], NA_real_),
    ifelse(length(forward_lean_real) > 0, forward_lean_real[[1]], NA_real_),
    ifelse(length(forward_limit_real) > 0, forward_limit_real[[1]], NA_real_)
  ))
)

calibration_model <- tibble(
  mode = calibration_mode,
  status = calibration_status,
  intercept = calibration_intercept,
  slope = calibration_slope,
  fallback_shrink = BT_CALIB_FALLBACK_SHRINK,
  tune_games_n = length(tune_games),
  tune_rows_n = nrow(calib_train),
  tune_weighted_n = sum(calib_train$holdout_weight, na.rm = TRUE),
  train_weighted_ece_raw = calib_raw_metrics$weighted_ece_decile[[1]],
  train_weighted_ece_platt = calib_platt_metrics$weighted_ece_decile[[1]],
  train_max_decile_gap_raw = calib_raw_metrics$max_abs_weighted_decile_gap[[1]],
  train_max_decile_gap_platt = calib_platt_metrics$max_abs_weighted_decile_gap[[1]],
  slope_gate_min = BT_CALIB_SLOPE_MIN,
  slope_gate_max = BT_CALIB_SLOPE_MAX,
  ece_gate_max = BT_CALIB_MAX_ECE,
  decile_gap_gate_max = BT_CALIB_MAX_DECILE_GAP
)

rows_df <- eval_df

bucket_summary <- eval_df %>%
  group_by(Decision) %>%
  summarise(
    n_game_lineups = n(),
    n_games = n_distinct(holdout_game_id),
    total_holdout_possessions = sum(holdout_weight, na.rm = TRUE),
    mean_prior_possessions = mean(prior_possessions, na.rm = TRUE),

    mean_pred_pr_net_pos = mean(pred_pr_net_pos, na.rm = TRUE),
    weighted_pred_pr_net_pos = weighted_mean_safe(pred_pr_net_pos, holdout_weight),
    observed_positive_rate = mean(observed_net_positive_num, na.rm = TRUE),
    weighted_observed_positive_rate = weighted_mean_safe(observed_net_positive_num, holdout_weight),
    net_prob_calibration_gap = observed_positive_rate - mean_pred_pr_net_pos,
    weighted_net_prob_calibration_gap = weighted_observed_positive_rate - weighted_pred_pr_net_pos,

    mean_pred_net_ppp = mean(pred_net_ppp_mean, na.rm = TRUE),
    weighted_pred_net_ppp = weighted_mean_safe(pred_net_ppp_mean, holdout_weight),
    mean_realized_net_ppp = mean(holdout_raw_net_ppp, na.rm = TRUE),
    weighted_realized_net_ppp = weighted_mean_safe(holdout_raw_net_ppp, holdout_weight),
    net_value_gap_ppp = weighted_realized_net_ppp - weighted_pred_net_ppp,

    mean_pred_pr_synergy_pos = mean(pr_synergy_pos, na.rm = TRUE),
    weighted_pred_pr_synergy_pos = weighted_mean_safe(pr_synergy_pos, holdout_weight),
    mean_pred_synergy_ppp = mean(synergy_mean, na.rm = TRUE),
    weighted_pred_synergy_ppp = weighted_mean_safe(synergy_mean, holdout_weight),

    weighted_realized_pts_per_40 = weighted_mean_safe(holdout_points_per_40, holdout_weight),
    .groups = "drop"
  ) %>%
  arrange(factor(Decision, levels = c("PLAY MORE", "LEAN IN", "NEUTRAL", "LIMIT / WATCH", "TOO SMALL", "UNSEEN")))

game_bucket_summary <- eval_df %>%
  group_by(holdout_game_id, holdout_game_file, holdout_game_date, Decision) %>%
  summarise(
    n_lineups = n(),
    holdout_possessions = sum(holdout_weight, na.rm = TRUE),
    weighted_pred_pr_net_pos = weighted_mean_safe(pred_pr_net_pos, holdout_weight),
    weighted_observed_positive_rate = weighted_mean_safe(observed_net_positive_num, holdout_weight),
    weighted_pred_net_ppp = weighted_mean_safe(pred_net_ppp_mean, holdout_weight),
    weighted_realized_net_ppp = weighted_mean_safe(holdout_raw_net_ppp, holdout_weight),
    weighted_net_prob_calibration_gap = weighted_observed_positive_rate - weighted_pred_pr_net_pos,
    net_value_gap_ppp = weighted_realized_net_ppp - weighted_pred_net_ppp,
    weighted_pred_pr_synergy_pos = weighted_mean_safe(pr_synergy_pos, holdout_weight),
    weighted_pred_synergy_ppp = weighted_mean_safe(synergy_mean, holdout_weight),
    .groups = "drop"
  ) %>%
  arrange(holdout_game_id, factor(Decision, levels = c("PLAY MORE", "LEAN IN", "NEUTRAL", "LIMIT / WATCH", "TOO SMALL", "UNSEEN")))

# ---------- Write outputs ----------
write_csv(rows_df, rows_out_path)
write_csv(bucket_summary, bucket_out_path)
write_csv(game_bucket_summary, game_bucket_out_path)
if (nrow(diag_df) > 0) write_csv(diag_df, diag_out_path)
write_csv(rule_v2_thresholds, rule_v2_thresholds_out_path)
write_csv(calibration_model, calibration_model_out_path)

message("\nRolling lineup decision backtest complete.")
message("Wrote:")
message(" - ", normalizePath(rows_out_path))
message(" - ", normalizePath(bucket_out_path))
message(" - ", normalizePath(game_bucket_out_path))
if (nrow(diag_df) > 0) message(" - ", normalizePath(diag_out_path))
message(" - ", normalizePath(rule_v2_thresholds_out_path))
message(" - ", normalizePath(calibration_model_out_path))

message("\nDecision bucket summary (weighted net calibration + realized outcomes):")
print(bucket_summary %>%
        select(
          Decision,
          n_game_lineups,
          n_games,
          total_holdout_possessions,
          weighted_pred_pr_net_pos,
          weighted_observed_positive_rate,
          weighted_net_prob_calibration_gap,
          weighted_pred_net_ppp,
          weighted_realized_net_ppp,
          net_value_gap_ppp
        ))
