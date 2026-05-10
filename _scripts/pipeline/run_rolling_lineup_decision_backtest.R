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

rescale01 <- function(x, default = 0.5) {
  x <- as.numeric(x)
  out <- rep(default, length(x))
  ok <- is.finite(x)
  if (!any(ok)) return(out)
  lo <- min(x[ok], na.rm = TRUE)
  hi <- max(x[ok], na.rm = TRUE)
  if (!is.finite(lo) || !is.finite(hi) || hi <= lo) {
    out[ok] <- default
    return(out)
  }
  out[ok] <- (x[ok] - lo) / (hi - lo)
  out
}

safe_quantile <- function(x, prob, default = NA_real_) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x) == 0) return(default)
  as.numeric(stats::quantile(x, probs = prob, na.rm = TRUE))
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
  prob_col <- if ("pred_pr_net_pos_for_decision" %in% names(df)) {
    "pred_pr_net_pos_for_decision"
  } else {
    "pred_pr_net_pos"
  }
  net_col <- if ("pred_net_ppp_mean_for_decision" %in% names(df)) {
    "pred_net_ppp_mean_for_decision"
  } else {
    "pred_net_ppp_mean"
  }
  df %>%
    group_by(decision_bucket = .data[[decision_col]]) %>%
    summarise(
      total_holdout_possessions = sum(holdout_weight, na.rm = TRUE),
      weighted_pred_pr_net_pos = weighted_mean_safe(.data[[prob_col]], holdout_weight),
      weighted_observed_positive_rate = weighted_mean_safe(observed_net_positive_num, holdout_weight),
      weighted_net_prob_calibration_gap = weighted_observed_positive_rate - weighted_pred_pr_net_pos,
      weighted_pred_net_ppp = weighted_mean_safe(.data[[net_col]], holdout_weight),
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

# Decision rule legacy seeds (kept for compatibility columns only).
DECISION_RULE_PROB_POSSESSIONS <- as.numeric(Sys.getenv("DECISION_RULE_PROB_POSSESSIONS", "40"))
DECISION_PLAY_MORE_PR_NET_MIN <- as.numeric(Sys.getenv("DECISION_PLAY_MORE_PR_NET_MIN", "0.553"))
DECISION_PLAY_MORE_NET_PPP_MIN <- as.numeric(Sys.getenv("DECISION_PLAY_MORE_NET_PPP_MIN", "-0.015"))
DECISION_LEAN_IN_PR_NET_MIN <- as.numeric(Sys.getenv("DECISION_LEAN_IN_PR_NET_MIN", "0.528"))
DECISION_LEAN_IN_NET_PPP_MIN <- as.numeric(Sys.getenv("DECISION_LEAN_IN_NET_PPP_MIN", "-0.015"))
DECISION_LIMIT_WATCH_PR_NET_MAX <- as.numeric(Sys.getenv("DECISION_LIMIT_WATCH_PR_NET_MAX", "0.657"))
DECISION_LIMIT_WATCH_NET_PPP_MAX <- as.numeric(Sys.getenv("DECISION_LIMIT_WATCH_NET_PPP_MAX", "-0.06"))

# Forward split for threshold/weight tuning.
BT_TUNE_FRACTION <- as.numeric(Sys.getenv("BT_TUNE_FRACTION", "0.70"))
BT_TUNE_MIN_GAMES <- as.integer(Sys.getenv("BT_TUNE_MIN_GAMES", "8"))
DECISION_V3_MIN_BUCKET_POS <- as.numeric(Sys.getenv("DECISION_V3_MIN_BUCKET_POS", "10"))

V4_W_DEF_GRID <- c(0.55, 0.60, 0.65, 0.70)
V4_W_OFF_GRID <- c(0.20, 0.25, 0.30, 0.35)
V4_ALPHA_OPP_GRID <- c(0.05, 0.10, 0.15)
V4_DEF_FLOOR_Q_GRID <- c(0.30, 0.35, 0.40)
V4_W_STYLE_MIN <- 0.05
V4_W_STYLE_MAX <- 0.20
V4_DEF_PPP_DELTA_MAX <- 0.005
V4_SURVIVE_DELTA_MIN <- -0.010
V4_NET_PPP_DELTA_MIN <- 0.010

FLOOR_RIM_PLUS_THREE_SHARE <- 0.6190
FLOOR_FTA_PER_FGA <- 0.2308
CEILING_TOV_PER_FGA <- 0.2000
CEILING_NON_RIM_PAINT_SHARE <- 0.2596

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
if (!is.finite(DECISION_V3_MIN_BUCKET_POS) || DECISION_V3_MIN_BUCKET_POS < 1) DECISION_V3_MIN_BUCKET_POS <- 10
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
  "Decision engine v4 | defense floor + offensive upside + fixed shot style constraints",
  " | labels = DEF_FLOOR_FAIL/DEF_FLOOR_PASS_UPSIDE_HIGH/MED/LOW/LOW_SAMPLE/UNSEEN"
)
message(
  "Forward tuning config | tune_fraction=", BT_TUNE_FRACTION,
  " min_tune_games=", BT_TUNE_MIN_GAMES,
  " | guardrails: d_def_ppp<=", V4_DEF_PPP_DELTA_MAX,
  " d_survive>=", V4_SURVIVE_DELTA_MIN,
  " d_net_ppp>=", V4_NET_PPP_DELTA_MIN
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

  # Holdout-context predictions are useful diagnostics, but the decision audit
  # below must calibrate and tune against the same baseline-context signal the
  # live coach table uses.
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
      decision_available = is.finite(decision_pred_pr_net_pos) & is.finite(decision_pred_net_ppp_mean),
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
    has_value_pred = is.finite(pred_net_ppp_mean),
    has_decision_prob_pred = is.finite(decision_pred_pr_net_pos),
    has_decision_value_pred = is.finite(decision_pred_net_ppp_mean)
  )

# Clean forward split (tune early holdout games, evaluate on later holdout games).
decision_eval <- eval_df %>%
  filter(
    decision_available,
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

classify_v3 <- function(score, prior_5man_poss, prior_pair_events, prior_trio_poss, t_guardable, t_high_risk, decision_available = NULL) {
  if (is.null(decision_available)) decision_available <- rep(TRUE, length(score))
  score <- as.numeric(score)
  prior_5man_poss <- as.numeric(prior_5man_poss)
  prior_pair_events <- as.numeric(prior_pair_events)
  prior_trio_poss <- as.numeric(prior_trio_poss)
  decision_available <- as.logical(decision_available)
  decision_available[is.na(decision_available)] <- FALSE

  out <- rep("UNSEEN", length(score))
  ok <- decision_available & is.finite(score) & is.finite(prior_5man_poss) & is.finite(prior_pair_events) & is.finite(prior_trio_poss)
  out[ok] <- dplyr::case_when(
    prior_5man_poss[ok] == 0 & prior_pair_events[ok] < 20 ~ "UNSEEN",
    prior_5man_poss[ok] < 30 | prior_pair_events[ok] < 80 | prior_trio_poss[ok] < 40 ~ "LOW_SAMPLE",
    score[ok] <= t_high_risk ~ "HIGH_RISK",
    score[ok] >= t_guardable ~ "GUARDABLE",
    TRUE ~ "SURVIVE_4"
  )
  out
}

downgrade_v3_label <- function(x) {
  dplyr::case_when(
    x == "GUARDABLE" ~ "SURVIVE_4",
    x == "SURVIVE_4" ~ "HIGH_RISK",
    TRUE ~ x
  )
}

tune_v3_thresholds <- function(tune_df) {
  q_guardable_vals <- c(0.20, 0.25, 0.30, 0.35)
  q_high_risk_vals <- c(0.15, 0.20, 0.25, 0.30)
  explored <- 0L
  feasible <- 0L
  best <- NULL
  best_score <- -Inf

  if (nrow(tune_df) < 40) {
    return(list(
      t_guardable = suppressWarnings(stats::quantile(tune_df$decision_survive_score_raw, probs = 0.75, na.rm = TRUE)),
      t_high_risk = suppressWarnings(stats::quantile(tune_df$decision_survive_score_raw, probs = 0.20, na.rm = TRUE)),
      q_guardable = 0.25,
      q_high_risk = 0.20,
      status = "fallback_insufficient_tune_rows",
      explored = 0L,
      feasible = 0L
    ))
  }

  for (qg in q_guardable_vals) {
    for (qh in q_high_risk_vals) {
      explored <- explored + 1L
      t_guardable <- as.numeric(stats::quantile(tune_df$decision_survive_score_raw, probs = 1 - qg, na.rm = TRUE))
      t_high_risk <- as.numeric(stats::quantile(tune_df$decision_survive_score_raw, probs = qh, na.rm = TRUE))
      if (!is.finite(t_guardable) || !is.finite(t_high_risk) || t_guardable <= t_high_risk) next

      tmp <- tune_df %>%
        mutate(
          Decision_tmp = classify_v3(
            score = decision_survive_score_raw,
            prior_5man_poss = prior_possessions,
            prior_pair_events = prior_pair_events,
            prior_trio_poss = prior_trio_possessions,
            t_guardable = t_guardable,
            t_high_risk = t_high_risk,
            decision_available = decision_available
          )
        )

      guard_pos <- sum(tmp$holdout_weight[tmp$Decision_tmp == "GUARDABLE"], na.rm = TRUE)
      high_pos <- sum(tmp$holdout_weight[tmp$Decision_tmp == "HIGH_RISK"], na.rm = TRUE)
      if (guard_pos < DECISION_V3_MIN_BUCKET_POS || high_pos < DECISION_V3_MIN_BUCKET_POS) next

      guard_survive <- weighted_mean_safe(tmp$observed_survive4[tmp$Decision_tmp == "GUARDABLE"], tmp$holdout_weight[tmp$Decision_tmp == "GUARDABLE"])
      high_survive <- weighted_mean_safe(tmp$observed_survive4[tmp$Decision_tmp == "HIGH_RISK"], tmp$holdout_weight[tmp$Decision_tmp == "HIGH_RISK"])
      if (!is.finite(guard_survive) || !is.finite(high_survive)) next
      separation <- guard_survive - high_survive
      if (!is.finite(separation)) next

      rec <- tmp %>%
        filter(Decision_tmp %in% c("GUARDABLE", "SURVIVE_4")) %>%
        group_by(holdout_game_id) %>%
        arrange(desc(decision_survive_score_raw), desc(prior_possessions), .by_group = TRUE) %>%
        slice(1) %>%
        ungroup() %>%
        select(holdout_game_id, rec_survive = observed_survive4, rec_w = holdout_weight)

      base <- tmp %>%
        filter(decision_available) %>%
        group_by(holdout_game_id) %>%
        arrange(desc(prior_possessions), desc(holdout_weight), .by_group = TRUE) %>%
        slice(1) %>%
        ungroup() %>%
        select(holdout_game_id, base_survive = observed_survive4)

      lift_df <- rec %>% left_join(base, by = "holdout_game_id") %>% mutate(lift = rec_survive - base_survive)
      lift <- weighted_mean_safe(lift_df$lift, lift_df$rec_w)
      if (!is.finite(lift)) lift <- 0

      feasible <- feasible + 1L
      score <- 120 * separation + 80 * lift + (guard_pos + high_pos) / 100

      if (is.finite(score) && score > best_score) {
        best_score <- score
        best <- list(
          t_guardable = t_guardable,
          t_high_risk = t_high_risk,
          q_guardable = qg,
          q_high_risk = qh,
          status = "ok",
          explored = explored,
          feasible = feasible,
          tune_weighted_separation = separation,
          tune_weighted_lift_vs_baseline = lift
        )
      }
    }
  }

  if (is.null(best)) {
    return(list(
      t_guardable = as.numeric(stats::quantile(tune_df$decision_survive_score_raw, probs = 0.75, na.rm = TRUE)),
      t_high_risk = as.numeric(stats::quantile(tune_df$decision_survive_score_raw, probs = 0.20, na.rm = TRUE)),
      q_guardable = 0.25,
      q_high_risk = 0.20,
      status = "fallback_no_feasible_grid_solution",
      explored = explored,
      feasible = feasible,
      tune_weighted_separation = NA_real_,
      tune_weighted_lift_vs_baseline = NA_real_
    ))
  }
  best
}

# Required V3 inputs.
archetype_path <- file.path("_data", "01_core_inputs", "player_archetypes.csv")
active_players <- sort(unique(unlist(stringr::str_split(stints2$uconn_lineup_canon, "\\|"))))
arche_map <- load_player_archetypes(archetype_path, active_players = active_players)

manual_events <- load_manual_defensive_events(
  manual_root = file.path("_data", "03_manual_game_csv"),
  games_joined = games2
)
if (nrow(manual_events) == 0) {
  stop("No manual defensive events loaded from _data/03_manual_game_csv. V3 requires this source.")
}

non_ex_game_ids <- if ("ex_game" %in% names(games2)) {
  games2 %>%
    filter(!dplyr::coalesce(ex_game, FALSE)) %>%
    pull(global_game_id) %>%
    unique()
} else {
  unique(games2$global_game_id)
}
manual_games_covered <- manual_events %>%
  filter(!is.na(global_game_id), global_game_id %in% non_ex_game_ids) %>%
  summarise(n = dplyr::n_distinct(game_file), .groups = "drop") %>%
  pull(n)
manual_coverage_rate <- manual_games_covered / max(1, dplyr::n_distinct(games2$game_file[games2$global_game_id %in% non_ex_game_ids]))
if (!is.finite(manual_coverage_rate) || manual_coverage_rate < 0.95) {
  stop(sprintf(
    "Manual defensive-play coverage is %.3f (< 0.95).",
    manual_coverage_rate
  ))
}

leak_candidates <- c(
  file.path("_outputs", "02_defense_leaks", "uconn_lineup_def_leaks_posterior.csv"),
  file.path("_outputs", "uconn_lineup_def_leaks_posterior.csv"),
  file.path("_outputs", "01_lineup_core", "uconn_lineup_def_leaks_posterior.csv")
)
leak_path <- leak_candidates[file.exists(leak_candidates)][1]
if (length(leak_path) == 0 || !nzchar(leak_path)) {
  stop("Missing required defensive leak posterior for V3 scoring.")
}
leak_tbl <- read_csv(leak_path, show_col_types = FALSE)
if (!("lineup" %in% names(leak_tbl)) && ("lineup_key" %in% names(leak_tbl))) {
  leak_tbl <- leak_tbl %>% rename(lineup = lineup_key)
}
if (!all(c("lineup", "pr_leak") %in% names(leak_tbl))) {
  stop("Defensive leak posterior missing lineup/pr_leak columns.")
}
leak_tbl <- leak_tbl %>%
  mutate(
    lineup = canonicalize_lineup(lineup),
    pr_leak = as.numeric(pr_leak)
  ) %>%
  select(lineup, pr_leak)

shot_candidates <- c(
  file.path("_outputs", "01_lineup_core", "uconn_lineup_shot_diet.csv"),
  file.path("_outputs", "uconn_lineup_shot_diet.csv")
)
shot_path <- shot_candidates[file.exists(shot_candidates)][1]
if (length(shot_path) == 0 || !nzchar(shot_path)) {
  stop("Missing required lineup shot diet profile for V4 backtest.")
}
shot_tbl <- read_csv(shot_path, show_col_types = FALSE)
if (!("lineup_key_norm" %in% names(shot_tbl))) {
  if (!("lineup_pretty" %in% names(shot_tbl))) {
    stop("Shot diet profile requires lineup_pretty or lineup_key_norm.", call. = FALSE)
  }
  shot_tbl <- shot_tbl %>%
    mutate(lineup_key_norm = canonicalize_lineup_norm(lineup_pretty))
}
shot_tbl <- shot_tbl %>%
  mutate(
    lineup_key_norm = canonicalize_lineup_norm(lineup_key_norm),
    fga = as.numeric(fga),
    rim_share = as.numeric(rim_share),
    paint_share = as.numeric(paint_share),
    corner_3_share = as.numeric(corner_3_share),
    above_break_3_share = as.numeric(above_break_3_share),
    fta_per_fga = as.numeric(fta_per_fga),
    tov_per_fga = as.numeric(tov_per_fga),
    rim_plus_three_share = as.numeric(if ("rim_plus_three_share" %in% names(shot_tbl)) rim_plus_three_share else NA_real_),
    non_rim_paint_share = as.numeric(if ("non_rim_paint_share" %in% names(shot_tbl)) non_rim_paint_share else NA_real_)
  ) %>%
  mutate(
    rim_plus_three_share = if_else(
      is.finite(rim_plus_three_share),
      rim_plus_three_share,
      coalesce(rim_share, 0) + coalesce(corner_3_share, 0) + coalesce(above_break_3_share, 0)
    ),
    non_rim_paint_share = if_else(
      is.finite(non_rim_paint_share),
      non_rim_paint_share,
      coalesce(paint_share, 0) - coalesce(rim_share, 0)
    )
  ) %>%
  filter(!is.na(lineup_key_norm), nzchar(lineup_key_norm)) %>%
  group_by(lineup_key_norm) %>%
  summarise(
    fta_per_fga_lineup = weighted_mean_safe(fta_per_fga, pmax(fga, 1)),
    tov_per_fga_lineup = weighted_mean_safe(tov_per_fga, pmax(fga, 1)),
    rim_plus_three_share = weighted_mean_safe(rim_plus_three_share, pmax(fga, 1)),
    non_rim_paint_share = weighted_mean_safe(non_rim_paint_share, pmax(fga, 1)),
    shot_sample_flag = if_else(any(tolower(coalesce(sample_flag, "")) == "ok"), "ok", "small_sample"),
    .groups = "drop"
  )
if (nrow(shot_tbl) == 0) {
  stop("Shot diet profile has no usable lineup rows after normalization.", call. = FALSE)
}

player_creation_candidates <- c(
  file.path("_outputs", "03_players", "uconn_player_creation_profile.csv"),
  file.path("_outputs", "uconn_player_creation_profile.csv")
)
player_creation_path <- player_creation_candidates[file.exists(player_creation_candidates)][1]
if (length(player_creation_path) == 0 || !nzchar(player_creation_path)) {
  stop("Missing required player creation profile for V4 backtest.")
}
player_creation_tbl <- read_csv(player_creation_path, show_col_types = FALSE)
if (!("player_key_norm" %in% names(player_creation_tbl))) {
  if (!("player" %in% names(player_creation_tbl))) {
    stop("Player creation profile requires player or player_key_norm.", call. = FALSE)
  }
  player_creation_tbl <- player_creation_tbl %>%
    mutate(player_key_norm = normalize_player_key(player))
}
if (!("created_scoring_actions" %in% names(player_creation_tbl))) {
  stop("Player creation profile missing created_scoring_actions.", call. = FALSE)
}
if (!("ast_to_tov" %in% names(player_creation_tbl))) player_creation_tbl$ast_to_tov <- NA_real_
if (!("self_created_make_rate" %in% names(player_creation_tbl))) player_creation_tbl$self_created_make_rate <- NA_real_
if (!("tracked_event_rows" %in% names(player_creation_tbl))) player_creation_tbl$tracked_event_rows <- 1

player_creation_tbl <- player_creation_tbl %>%
  mutate(
    player_key_norm = normalize_player_key(player_key_norm),
    created_scoring_actions = as.numeric(created_scoring_actions),
    ast_to_tov = as.numeric(ast_to_tov),
    self_created_make_rate = as.numeric(self_created_make_rate),
    tracked_event_rows = as.numeric(tracked_event_rows)
  ) %>%
  filter(!is.na(player_key_norm), nzchar(player_key_norm)) %>%
  group_by(player_key_norm) %>%
  summarise(
    created_scoring_actions = weighted_mean_safe(created_scoring_actions, pmax(tracked_event_rows, 1)),
    ast_to_tov = weighted_mean_safe(ast_to_tov, pmax(tracked_event_rows, 1)),
    self_created_make_rate = weighted_mean_safe(self_created_make_rate, pmax(tracked_event_rows, 1)),
    .groups = "drop"
  ) %>%
  mutate(
    creation_raw = 0.60 * zscore_safe(created_scoring_actions) +
      0.25 * zscore_safe(ast_to_tov) +
      0.15 * zscore_safe(self_created_make_rate),
    creation_score_player = rescale01(creation_raw, default = 0.5)
  )
player_creation_map <- setNames(player_creation_tbl$creation_score_player, player_creation_tbl$player_key_norm)

# Holdout defensive outcomes.
holdout_lineup_def <- stints2 %>%
  group_by(holdout_game_id = game_id, lineup = uconn_lineup_canon) %>%
  summarise(
    holdout_pts_against = sum(points_against, na.rm = TRUE),
    holdout_possessions_def = sum(w, na.rm = TRUE),
    holdout_def_ppp = if_else(holdout_possessions_def > 0, holdout_pts_against / holdout_possessions_def, NA_real_),
    .groups = "drop"
  )
holdout_game_def <- stints2 %>%
  group_by(holdout_game_id = game_id) %>%
  summarise(
    holdout_game_team_def_ppp = sum(points_against, na.rm = TRUE) / sum(w, na.rm = TRUE),
    .groups = "drop"
  )
opp_context <- games2 %>%
  transmute(
    holdout_game_id = global_game_id,
    opp_adjO_z_ctx = zscore_safe(opp_adjO),
    opp_adjD_z_ctx = zscore_safe(opp_adjD)
  )

eval_df <- eval_df %>%
  left_join(holdout_lineup_def, by = c("holdout_game_id", "lineup")) %>%
  left_join(holdout_game_def, by = "holdout_game_id") %>%
  left_join(opp_context, by = "holdout_game_id") %>%
  mutate(
    holdout_def_ppp = if_else(is.finite(holdout_def_ppp), holdout_def_ppp, NA_real_),
    observed_def_ppp_gap_vs_game_baseline = holdout_def_ppp - holdout_game_team_def_ppp,
    observed_survive4 = as.numeric(observed_def_ppp_gap_vs_game_baseline <= 0.03),
    observed_leaky = as.numeric(observed_def_ppp_gap_vs_game_baseline >= 0.08)
  )

# Precompute pair/trio priors by holdout game.
pair_models <- list()
pair_baseline <- list()
trio_models <- list()
trio_baseline <- list()
for (g in split_games) {
  pair_train <- manual_events %>%
    filter(!is.na(global_game_id), global_game_id < g)

  p_base <- weighted_mean_safe(pair_train$stop_event, rep(1, nrow(pair_train)))
  if (!is.finite(p_base)) p_base <- 0.5
  pair_baseline[[as.character(g)]] <- p_base

  if (nrow(pair_train) > 0) {
    pair_tbl <- pair_train %>%
      mutate(pair_key = lapply(defense_lineup_key, lineup_pair_keys_norm)) %>%
      select(stop_event, pair_key) %>%
      tidyr::unnest_longer(pair_key, values_to = "pair_key") %>%
      filter(!is.na(pair_key), nzchar(pair_key)) %>%
      group_by(pair_key) %>%
      summarise(
        events = n(),
        stops = sum(stop_event, na.rm = TRUE),
        pair_survive = (stops + 80 * p_base) / (events + 80),
        .groups = "drop"
      )
  } else {
    pair_tbl <- tibble(pair_key = character(), events = numeric(), stops = numeric(), pair_survive = numeric())
  }
  pair_models[[as.character(g)]] <- pair_tbl

  trio_train <- stints2 %>% filter(game_id < g, is.finite(poss_est), poss_est > 0, is.finite(points_against))
  if (nrow(trio_train) > 0) {
    train_def_ppp <- sum(trio_train$points_against, na.rm = TRUE) / sum(trio_train$poss_est, na.rm = TRUE)
    trio_train <- trio_train %>%
      mutate(
        trio_stop = as.numeric((points_against / poss_est) <= train_def_ppp),
        trio_key = lapply(uconn_lineup_canon, lineup_trio_keys),
        event_w = poss_est,
        stop_w = trio_stop * poss_est
      )
    t_base <- weighted_mean_safe(trio_train$trio_stop, trio_train$event_w)
    if (!is.finite(t_base)) t_base <- 0.5
    trio_tbl <- trio_train %>%
      select(trio_key, event_w, stop_w) %>%
      tidyr::unnest_longer(trio_key, values_to = "trio_key") %>%
      filter(!is.na(trio_key), nzchar(trio_key)) %>%
      group_by(trio_key) %>%
      summarise(
        events = sum(event_w, na.rm = TRUE),
        stops = sum(stop_w, na.rm = TRUE),
        trio_survive = (stops + 120 * t_base) / (events + 120),
        .groups = "drop"
      )
  } else {
    t_base <- 0.5
    trio_tbl <- tibble(trio_key = character(), events = numeric(), stops = numeric(), trio_survive = numeric())
  }
  trio_baseline[[as.character(g)]] <- t_base
  trio_models[[as.character(g)]] <- trio_tbl
}

lineup_unique <- sort(unique(eval_df$lineup))
pair_keys_map <- setNames(lapply(lineup_unique, lineup_pair_keys_norm), lineup_unique)
trio_keys_map <- setNames(lapply(lineup_unique, lineup_trio_keys), lineup_unique)
lineup_key_norm_map <- setNames(canonicalize_lineup_norm(lineup_unique), lineup_unique)
archetype_balance_map <- setNames(vapply(lineup_unique, compute_lineup_archetype_balance, numeric(1), arche_map = arche_map), lineup_unique)
leak_map <- setNames(leak_tbl$pr_leak, leak_tbl$lineup)

n_eval <- nrow(eval_df)
pair_survive <- rep(NA_real_, n_eval)
prior_pair_events <- rep(NA_real_, n_eval)
trio_survive <- rep(NA_real_, n_eval)
prior_trio_possessions <- rep(NA_real_, n_eval)
pr_leak_use <- rep(NA_real_, n_eval)
archetype_balance <- rep(NA_real_, n_eval)
lineup_key_norm_use <- rep(NA_character_, n_eval)
creation_score <- rep(NA_real_, n_eval)

for (i in seq_len(n_eval)) {
  g <- as.character(eval_df$holdout_game_id[[i]])
  l <- as.character(eval_df$lineup[[i]])
  pk <- pair_keys_map[[l]]
  tk <- trio_keys_map[[l]]

  p_tbl <- pair_models[[g]]
  p_base <- pair_baseline[[g]]
  if (!is.finite(p_base)) p_base <- 0.5
  if (length(pk) == 0) {
    pair_survive[[i]] <- p_base
    prior_pair_events[[i]] <- 0
  } else {
    m <- match(pk, p_tbl$pair_key)
    ev <- p_tbl$events[m]
    ps <- p_tbl$pair_survive[m]
    ev[!is.finite(ev)] <- 0
    ps[!is.finite(ps)] <- p_base
    total_ev <- sum(ev, na.rm = TRUE)
    pair_survive[[i]] <- if (is.finite(total_ev) && total_ev > 0) {
      weighted_mean_safe(ps, ev)
    } else {
      mean(ps, na.rm = TRUE)
    }
    prior_pair_events[[i]] <- total_ev
  }

  t_tbl <- trio_models[[g]]
  t_base <- trio_baseline[[g]]
  if (!is.finite(t_base)) t_base <- 0.5
  if (length(tk) == 0) {
    trio_survive[[i]] <- t_base
    prior_trio_possessions[[i]] <- 0
  } else {
    m2 <- match(tk, t_tbl$trio_key)
    ev2 <- t_tbl$events[m2]
    ts <- t_tbl$trio_survive[m2]
    ev2[!is.finite(ev2)] <- 0
    ts[!is.finite(ts)] <- t_base
    total_ev2 <- sum(ev2, na.rm = TRUE)
    trio_survive[[i]] <- if (is.finite(total_ev2) && total_ev2 > 0) {
      weighted_mean_safe(ts, ev2)
    } else {
      mean(ts, na.rm = TRUE)
    }
    prior_trio_possessions[[i]] <- total_ev2
  }

  # Some holdout lineups have no leak posterior; fall back to the neutral default.
  pr <- unname(leak_map[l])[[1]]
  if (!is.finite(pr)) pr <- 0.5
  pr_leak_use[[i]] <- pr
  archetype_balance[[i]] <- archetype_balance_map[[l]]
  lineup_key_norm_use[[i]] <- lineup_key_norm_map[[l]]

  toks <- split_lineup_players_norm(l)
  vals <- as.numeric(player_creation_map[toks])
  creation_score[[i]] <- if (length(vals) == 0 || all(!is.finite(vals))) 0.5 else mean(vals[is.finite(vals)], na.rm = TRUE)
}

opp_component <- function(oppO_z, oppD_z) {
  plogis(-0.6 * oppO_z + 0.2 * oppD_z)
}

shot_default_rim_plus_three <- safe_quantile(shot_tbl$rim_plus_three_share, 0.50, default = FLOOR_RIM_PLUS_THREE_SHARE)
shot_default_non_rim_paint <- safe_quantile(shot_tbl$non_rim_paint_share, 0.50, default = CEILING_NON_RIM_PAINT_SHARE)
shot_default_fta <- safe_quantile(shot_tbl$fta_per_fga_lineup, 0.50, default = FLOOR_FTA_PER_FGA)
shot_default_tov <- safe_quantile(shot_tbl$tov_per_fga_lineup, 0.50, default = CEILING_TOV_PER_FGA)

eval_df <- eval_df %>%
  mutate(
    prior_5man_possessions = prior_possessions,
    prior_pair_events = prior_pair_events,
    prior_trio_possessions = prior_trio_possessions,
    pair_survive = pair_survive,
    trio_survive = trio_survive,
    pr_leak = pr_leak_use,
    archetype_balance = archetype_balance,
    lineup_key_norm = lineup_key_norm_use,
    creation_score = creation_score
  ) %>%
  left_join(shot_tbl, by = "lineup_key_norm") %>%
  mutate(
    rim_plus_three_share = if_else(is.finite(rim_plus_three_share), rim_plus_three_share, shot_default_rim_plus_three),
    non_rim_paint_share = if_else(is.finite(non_rim_paint_share), non_rim_paint_share, shot_default_non_rim_paint),
    fta_per_fga_lineup = if_else(is.finite(fta_per_fga_lineup), fta_per_fga_lineup, shot_default_fta),
    tov_per_fga_lineup = if_else(is.finite(tov_per_fga_lineup), tov_per_fga_lineup, shot_default_tov),
    p1 = pmax(0, FLOOR_RIM_PLUS_THREE_SHARE - rim_plus_three_share) / FLOOR_RIM_PLUS_THREE_SHARE,
    p2 = pmax(0, FLOOR_FTA_PER_FGA - fta_per_fga_lineup) / FLOOR_FTA_PER_FGA,
    p3 = pmax(0, tov_per_fga_lineup - CEILING_TOV_PER_FGA) / CEILING_TOV_PER_FGA,
    p4 = pmax(0, non_rim_paint_share - CEILING_NON_RIM_PAINT_SHARE) / CEILING_NON_RIM_PAINT_SHARE,
    shot_diet_score = pmin(pmax(1 - (0.35 * p1 + 0.20 * p2 + 0.25 * p3 + 0.20 * p4), 0), 1),
    defense_score_neutral = 0.55 * (1 - pr_leak) + 0.30 * pair_survive + 0.10 * trio_survive + 0.05 * archetype_balance,
    defense_score_neutral = pmin(pmax(defense_score_neutral, 0), 1),
    opp_adjO_z_ctx = if_else(is.finite(opp_adjO_z_ctx), opp_adjO_z_ctx, 0),
    opp_adjD_z_ctx = if_else(is.finite(opp_adjD_z_ctx), opp_adjD_z_ctx, 0),
    defense_score_context = 0.9 * defense_score_neutral + 0.1 * opp_component(opp_adjO_z_ctx, opp_adjD_z_ctx)
  )

tune_threshold_df <- eval_df %>%
  filter(
    holdout_game_id %in% tune_games,
    decision_available,
    is.finite(defense_score_neutral),
    is.finite(observed_survive4),
    is.finite(holdout_weight),
    holdout_weight > 0
  )

label_v4 <- function(df, def_floor_t) {
  df <- df %>%
    mutate(
      is_unseen = !(decision_available %in% TRUE) |
        !is.finite(prior_5man_possessions) |
        !is.finite(prior_pair_events) |
        !is.finite(prior_trio_possessions) |
        (prior_5man_possessions == 0 & prior_pair_events < 20),
      is_low_sample = !is_unseen & (
        prior_5man_possessions < 30 |
          prior_pair_events < 80 |
          prior_trio_possessions < 40
      ),
      defense_floor_pass = !is_unseen & !is_low_sample & is.finite(decision_survive_score_robust) & decision_survive_score_robust >= def_floor_t
    )

  pass_scores <- df$composite_score[df$defense_floor_pass & is.finite(df$composite_score)]
  q_low <- safe_quantile(pass_scores, 1 / 3, default = NA_real_)
  q_high <- safe_quantile(pass_scores, 2 / 3, default = NA_real_)
  if (!is.finite(q_low) || !is.finite(q_high) || q_high < q_low) {
    q_low <- 0.33
    q_high <- 0.66
  }

  df %>%
    mutate(
      decision_label = case_when(
        is_unseen ~ "UNSEEN",
        is_low_sample ~ "LOW_SAMPLE",
        !defense_floor_pass ~ "DEF_FLOOR_FAIL",
        composite_score >= q_high ~ "DEF_FLOOR_PASS_UPSIDE_HIGH",
        composite_score < q_low ~ "DEF_FLOOR_PASS_UPSIDE_LOW",
        TRUE ~ "DEF_FLOOR_PASS_UPSIDE_MED"
      ),
      Decision = decision_label
    )
}

score_v4 <- function(df, w_def, w_off, w_style, alpha_opp, def_floor_q) {
  out <- df %>%
    mutate(
      decision_survive_score_raw = (1 - alpha_opp) * defense_score_neutral + alpha_opp * defense_score_context,
      decision_survive_score_raw = pmin(pmax(decision_survive_score_raw, 0), 1),
      offense_upside_raw = 0.60 * zscore_safe(pred_points_per_40) +
        0.25 * zscore_safe(creation_score) +
        0.15 * zscore_safe(fta_per_fga_lineup),
      offense_upside_score = rescale01(offense_upside_raw, default = 0.5),
      composite_score = w_def * decision_survive_score_raw + w_off * offense_upside_score + w_style * shot_diet_score,
      composite_score = pmin(pmax(composite_score, 0), 1)
    )

  grid_vals <- c(-0.5, 0, 0.5)
  robust <- out$decision_survive_score_raw
  fragile <- rep(FALSE, nrow(out))
  for (i in seq_len(nrow(out))) {
    if (!isTRUE(out$decision_available[[i]])) next
    o <- out$opp_adjO_z_ctx[[i]]
    d <- out$opp_adjD_z_ctx[[i]]
    if (!is.finite(o)) o <- 0
    if (!is.finite(d)) d <- 0
    pert <- c()
    for (do in grid_vals) {
      for (dd in grid_vals) {
        s_context <- 0.9 * out$defense_score_neutral[[i]] + 0.1 * opp_component(o + do, d + dd)
        s <- (1 - alpha_opp) * out$defense_score_neutral[[i]] + alpha_opp * s_context
        pert <- c(pert, min(max(s, 0), 1))
      }
    }
    if (length(pert) > 0) {
      robust[[i]] <- min(c(out$decision_survive_score_raw[[i]], pert), na.rm = TRUE)
    }
  }
  out <- out %>%
    mutate(decision_survive_score_robust = pmin(pmax(robust, 0), 1))
  def_floor_t <- safe_quantile(
    out$decision_survive_score_robust[out$decision_available %in% TRUE],
    def_floor_q,
    default = 0.50
  )
  out <- label_v4(out, def_floor_t)
  out$opp_fragile_flag <- rep(FALSE, nrow(out))

  for (i in seq_len(nrow(out))) {
    if (!isTRUE(out$decision_available[[i]])) next
    o <- out$opp_adjO_z_ctx[[i]]
    d <- out$opp_adjD_z_ctx[[i]]
    if (!is.finite(o)) o <- 0
    if (!is.finite(d)) d <- 0
    pert_pass <- c()
    for (do in c(-0.5, 0, 0.5)) {
      for (dd in c(-0.5, 0, 0.5)) {
        s_context <- 0.9 * out$defense_score_neutral[[i]] + 0.1 * opp_component(o + do, d + dd)
        s <- (1 - alpha_opp) * out$defense_score_neutral[[i]] + alpha_opp * s_context
        pert_pass <- c(pert_pass, s >= def_floor_t)
      }
    }
    base_pass <- isTRUE(out$defense_floor_pass[[i]])
    out$opp_fragile_flag[[i]] <- if (length(pert_pass) > 0) any(pert_pass != base_pass) else FALSE
  }

  out <- out %>%
    mutate(
      decision_def_ppp_pred = holdout_game_team_def_ppp + (0.5 - decision_survive_score_robust) * 0.30
    )

  list(df = out, def_floor_t = def_floor_t)
}

summarize_recommended_metrics <- function(df) {
  keep <- df$decision_label %in% c(
    "DEF_FLOOR_PASS_UPSIDE_HIGH",
    "DEF_FLOOR_PASS_UPSIDE_MED",
    "DEF_FLOOR_PASS_UPSIDE_LOW"
  )
  if (!any(keep, na.rm = TRUE)) {
    keep <- df$decision_available %in% TRUE
  }
  tibble(
    poss = sum(df$holdout_weight[keep], na.rm = TRUE),
    weighted_observed_def_ppp = weighted_mean_safe(df$holdout_def_ppp[keep], df$holdout_weight[keep]),
    weighted_observed_survive4_rate = weighted_mean_safe(df$observed_survive4[keep], df$holdout_weight[keep]),
    weighted_holdout_raw_net_ppp = weighted_mean_safe(df$holdout_raw_net_ppp[keep], df$holdout_weight[keep])
  )
}

forward_eval_games <- if (length(forward_games) > 0) forward_games else split_games
baseline_scored <- score_v4(
  eval_df,
  w_def = 1.0,
  w_off = 0.0,
  w_style = 0.0,
  alpha_opp = 0.10,
  def_floor_q = 0.35
)
baseline_forward <- summarize_recommended_metrics(
  baseline_scored$df %>% filter(holdout_game_id %in% forward_eval_games)
)

explored <- 0L
feasible <- 0L
best <- NULL
best_score <- -Inf

for (w_def in V4_W_DEF_GRID) {
  for (w_off in V4_W_OFF_GRID) {
    w_style <- 1 - w_def - w_off
    if (!is.finite(w_style) || w_style < V4_W_STYLE_MIN || w_style > V4_W_STYLE_MAX) next
    for (alpha_opp in V4_ALPHA_OPP_GRID) {
      for (def_floor_q in V4_DEF_FLOOR_Q_GRID) {
        explored <- explored + 1L
        scored <- score_v4(
          eval_df,
          w_def = w_def,
          w_off = w_off,
          w_style = w_style,
          alpha_opp = alpha_opp,
          def_floor_q = def_floor_q
        )
        cand_forward_df <- scored$df %>% filter(holdout_game_id %in% forward_eval_games)
        cand <- summarize_recommended_metrics(cand_forward_df)
        if (!is.finite(cand$poss[[1]]) || cand$poss[[1]] < 100) next

        d_def <- as.numeric(cand$weighted_observed_def_ppp[[1]] - baseline_forward$weighted_observed_def_ppp[[1]])
        d_survive <- as.numeric(cand$weighted_observed_survive4_rate[[1]] - baseline_forward$weighted_observed_survive4_rate[[1]])
        d_net <- as.numeric(cand$weighted_holdout_raw_net_ppp[[1]] - baseline_forward$weighted_holdout_raw_net_ppp[[1]])
        if (!is.finite(d_def) || !is.finite(d_survive) || !is.finite(d_net)) next
        if (d_def > V4_DEF_PPP_DELTA_MAX || d_survive < V4_SURVIVE_DELTA_MIN || d_net < V4_NET_PPP_DELTA_MIN) next

        feasible <- feasible + 1L
        score <- d_net - d_def + 0.25 * d_survive
        if (is.finite(score) && score > best_score) {
          best_score <- score
          best <- list(
            w_def = w_def,
            w_off = w_off,
            w_style = w_style,
            alpha_opp = alpha_opp,
            def_floor_q = def_floor_q,
            def_floor_t = scored$def_floor_t,
            d_def = d_def,
            d_survive = d_survive,
            d_net = d_net,
            score = score
          )
        }
      }
    }
  }
}

if (is.null(best)) {
  message("No feasible V4 parameter set passed guardrails. Falling back to defense-first baseline parameters.")
  final_scored <- baseline_scored
  eval_df <- final_scored$df
  def_floor_t <- final_scored$def_floor_t
  tune_result <- list(
    status = "fallback_defense_first_baseline_no_feasible_grid",
    explored = explored,
    feasible = feasible,
    w_def = 1.0,
    w_off = 0.0,
    w_style = 0.0,
    alpha_opp = 0.10,
    def_floor_q = 0.35,
    def_floor_t = def_floor_t,
    delta_weighted_observed_def_ppp = 0.0,
    delta_weighted_observed_survive4_rate = 0.0,
    delta_weighted_holdout_raw_net_ppp = 0.0
  )
} else {
  final_scored <- score_v4(
    eval_df,
    w_def = best$w_def,
    w_off = best$w_off,
    w_style = best$w_style,
    alpha_opp = best$alpha_opp,
    def_floor_q = best$def_floor_q
  )
  eval_df <- final_scored$df
  def_floor_t <- final_scored$def_floor_t
  tune_result <- list(
    status = "ok",
    explored = explored,
    feasible = feasible,
    w_def = best$w_def,
    w_off = best$w_off,
    w_style = best$w_style,
    alpha_opp = best$alpha_opp,
    def_floor_q = best$def_floor_q,
    def_floor_t = def_floor_t,
    delta_weighted_observed_def_ppp = best$d_def,
    delta_weighted_observed_survive4_rate = best$d_survive,
    delta_weighted_holdout_raw_net_ppp = best$d_net
  )
}

# Regret tracking (game-level recommendation vs actual and best feasible).
recommended_by_game <- eval_df %>%
  filter(
    decision_available,
    decision_label %in% c("DEF_FLOOR_PASS_UPSIDE_HIGH", "DEF_FLOOR_PASS_UPSIDE_MED", "DEF_FLOOR_PASS_UPSIDE_LOW")
  ) %>%
  group_by(holdout_game_id) %>%
  arrange(desc(composite_score), desc(prior_5man_possessions), .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  transmute(
    holdout_game_id,
    recommended_lineup = lineup,
    recommended_pred_def_ppp = decision_def_ppp_pred
  )

actual_by_game <- eval_df %>%
  group_by(holdout_game_id) %>%
  arrange(desc(holdout_possessions), desc(prior_5man_possessions), .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  transmute(
    holdout_game_id,
    actual_lineup = lineup,
    actual_pred_def_ppp = decision_def_ppp_pred
  )

best_feasible_by_game <- eval_df %>%
  filter(
    decision_available,
    decision_label %in% c("DEF_FLOOR_PASS_UPSIDE_HIGH", "DEF_FLOOR_PASS_UPSIDE_MED", "DEF_FLOOR_PASS_UPSIDE_LOW"),
    prior_5man_possessions >= 30,
    prior_pair_events >= 80
  ) %>%
  group_by(holdout_game_id) %>%
  summarise(
    best_feasible_pred_def_ppp = {
      v <- decision_def_ppp_pred[is.finite(decision_def_ppp_pred)]
      if (length(v) == 0) NA_real_ else min(v)
    },
    .groups = "drop"
  )

game_regret <- recommended_by_game %>%
  left_join(actual_by_game, by = "holdout_game_id") %>%
  left_join(best_feasible_by_game, by = "holdout_game_id") %>%
  mutate(
    regret_vs_actual_ppp = actual_pred_def_ppp - recommended_pred_def_ppp,
    regret_vs_best_feasible_ppp = pmax(recommended_pred_def_ppp - best_feasible_pred_def_ppp, 0)
  ) %>%
  select(holdout_game_id, regret_vs_actual_ppp, regret_vs_best_feasible_ppp)

eval_df <- eval_df %>%
  left_join(game_regret, by = "holdout_game_id")

forward_eval_df <- eval_df %>% filter(holdout_game_id %in% forward_games)
forward_bucket <- forward_eval_df %>%
  group_by(Decision) %>%
  summarise(
    weighted_composite_score = weighted_mean_safe(composite_score, holdout_weight),
    weighted_observed_survive4_rate = weighted_mean_safe(observed_survive4, holdout_weight),
    weighted_observed_leaky_rate = weighted_mean_safe(observed_leaky, holdout_weight),
    weighted_observed_def_ppp = weighted_mean_safe(holdout_def_ppp, holdout_weight),
    .groups = "drop"
  )

forward_bucket_val <- function(label, col) {
  v <- forward_bucket %>% filter(Decision == label) %>% pull(.data[[col]])
  if (length(v) == 0) return(NA_real_)
  as.numeric(v[[1]])
}

rule_v2_thresholds <- tibble(
  metric = c(
    "W_DEF",
    "W_OFF",
    "W_STYLE",
    "ALPHA_OPP",
    "DEF_FLOOR_QUANTILE",
    "DEF_FLOOR_T",
    "FLOOR_RIM_PLUS_THREE_SHARE",
    "FLOOR_FTA_PER_FGA",
    "CEILING_TOV_PER_FGA",
    "CEILING_NON_RIM_PAINT_SHARE",
    "DELTA_WEIGHTED_OBSERVED_DEF_PPP",
    "DELTA_WEIGHTED_OBSERVED_SURVIVE4_RATE",
    "DELTA_WEIGHTED_HOLDOUT_RAW_NET_PPP",
    "TUNING_STATUS",
    "TUNING_GRID_EXPLORED",
    "TUNING_GRID_FEASIBLE",
    "TUNE_GAMES_N",
    "FORWARD_GAMES_N",
    "MANUAL_DEF_COVERAGE_RATE",
    "MANUAL_DEF_GAMES_COVERED",
    "FORWARD_PASS_HIGH_WEIGHTED_SURVIVE4",
    "FORWARD_DEF_FLOOR_FAIL_WEIGHTED_SURVIVE4",
    "FORWARD_PASS_HIGH_WEIGHTED_DEF_PPP",
    "FORWARD_DEF_FLOOR_FAIL_WEIGHTED_DEF_PPP"
  ),
  value = as.character(c(
    tune_result$w_def,
    tune_result$w_off,
    tune_result$w_style,
    tune_result$alpha_opp,
    tune_result$def_floor_q,
    tune_result$def_floor_t,
    FLOOR_RIM_PLUS_THREE_SHARE,
    FLOOR_FTA_PER_FGA,
    CEILING_TOV_PER_FGA,
    CEILING_NON_RIM_PAINT_SHARE,
    tune_result$delta_weighted_observed_def_ppp,
    tune_result$delta_weighted_observed_survive4_rate,
    tune_result$delta_weighted_holdout_raw_net_ppp,
    tune_result$status,
    tune_result$explored,
    tune_result$feasible,
    length(tune_games),
    length(forward_games),
    manual_coverage_rate,
    manual_games_covered,
    forward_bucket_val("DEF_FLOOR_PASS_UPSIDE_HIGH", "weighted_observed_survive4_rate"),
    forward_bucket_val("DEF_FLOOR_FAIL", "weighted_observed_survive4_rate"),
    forward_bucket_val("DEF_FLOOR_PASS_UPSIDE_HIGH", "weighted_observed_def_ppp"),
    forward_bucket_val("DEF_FLOOR_FAIL", "weighted_observed_def_ppp")
  ))
)

calibration_model <- tibble(
  mode = "v4_hybrid_score",
  status = "not_applicable",
  intercept = NA_real_,
  slope = NA_real_,
  fallback_shrink = NA_real_,
  tune_games_n = length(tune_games),
  tune_rows_n = nrow(tune_threshold_df),
  tune_weighted_n = sum(tune_threshold_df$holdout_weight, na.rm = TRUE),
  train_weighted_ece_raw = NA_real_,
  train_weighted_ece_platt = NA_real_,
  train_max_decile_gap_raw = NA_real_,
  train_max_decile_gap_platt = NA_real_,
  slope_gate_min = NA_real_,
  slope_gate_max = NA_real_,
  ece_gate_max = NA_real_,
  decile_gap_gate_max = NA_real_
)

rows_df <- eval_df %>%
  select(
    -any_of(c(
      "observed_net_positive",
      "observed_net_positive_num",
      "pred_pr_net_pos",
      "pred_net_ppp_mean",
      "decision_pred_pr_net_pos",
      "decision_pred_net_ppp_mean"
    ))
  )

bucket_summary <- eval_df %>%
  group_by(Decision) %>%
  summarise(
    n_game_lineups = n(),
    n_games = n_distinct(holdout_game_id),
    total_holdout_possessions = sum(holdout_weight, na.rm = TRUE),
    mean_prior_5man_possessions = mean(prior_5man_possessions, na.rm = TRUE),
    mean_prior_pair_events = mean(prior_pair_events, na.rm = TRUE),
    mean_prior_trio_possessions = mean(prior_trio_possessions, na.rm = TRUE),
    weighted_survive_score_raw = weighted_mean_safe(decision_survive_score_raw, holdout_weight),
    weighted_survive_score_robust = weighted_mean_safe(decision_survive_score_robust, holdout_weight),
    weighted_pred_def_ppp = weighted_mean_safe(decision_def_ppp_pred, holdout_weight),
    weighted_observed_def_ppp = weighted_mean_safe(holdout_def_ppp, holdout_weight),
    weighted_observed_survive4_rate = weighted_mean_safe(observed_survive4, holdout_weight),
    weighted_observed_leaky_rate = weighted_mean_safe(observed_leaky, holdout_weight),
    weighted_def_ppp_gap_vs_game_baseline = weighted_mean_safe(observed_def_ppp_gap_vs_game_baseline, holdout_weight),
    weighted_opp_fragile_rate = weighted_mean_safe(as.numeric(opp_fragile_flag), holdout_weight),
    mean_regret_vs_actual_ppp = mean(regret_vs_actual_ppp, na.rm = TRUE),
    mean_regret_vs_best_feasible_ppp = mean(regret_vs_best_feasible_ppp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(factor(
    Decision,
    levels = c(
      "DEF_FLOOR_PASS_UPSIDE_HIGH",
      "DEF_FLOOR_PASS_UPSIDE_MED",
      "DEF_FLOOR_PASS_UPSIDE_LOW",
      "DEF_FLOOR_FAIL",
      "LOW_SAMPLE",
      "UNSEEN"
    )
  ))

game_bucket_summary <- eval_df %>%
  group_by(holdout_game_id, holdout_game_file, holdout_game_date, Decision) %>%
  summarise(
    n_lineups = n(),
    holdout_possessions = sum(holdout_weight, na.rm = TRUE),
    weighted_survive_score_raw = weighted_mean_safe(decision_survive_score_raw, holdout_weight),
    weighted_survive_score_robust = weighted_mean_safe(decision_survive_score_robust, holdout_weight),
    weighted_pred_def_ppp = weighted_mean_safe(decision_def_ppp_pred, holdout_weight),
    weighted_observed_def_ppp = weighted_mean_safe(holdout_def_ppp, holdout_weight),
    weighted_observed_survive4_rate = weighted_mean_safe(observed_survive4, holdout_weight),
    weighted_observed_leaky_rate = weighted_mean_safe(observed_leaky, holdout_weight),
    weighted_def_ppp_gap_vs_game_baseline = weighted_mean_safe(observed_def_ppp_gap_vs_game_baseline, holdout_weight),
    weighted_opp_fragile_rate = weighted_mean_safe(as.numeric(opp_fragile_flag), holdout_weight),
    .groups = "drop"
  ) %>%
  arrange(holdout_game_id, factor(
    Decision,
    levels = c(
      "DEF_FLOOR_PASS_UPSIDE_HIGH",
      "DEF_FLOOR_PASS_UPSIDE_MED",
      "DEF_FLOOR_PASS_UPSIDE_LOW",
      "DEF_FLOOR_FAIL",
      "LOW_SAMPLE",
      "UNSEEN"
    )
  ))

# ---------- Write outputs ----------
write_csv(rows_df, rows_out_path)
write_csv(bucket_summary, bucket_out_path)
write_csv(game_bucket_summary, game_bucket_out_path)
if (nrow(diag_df) > 0) write_csv(diag_df, diag_out_path)
write_csv(rule_v2_thresholds, rule_v2_thresholds_out_path)
write_csv(calibration_model, calibration_model_out_path)

message("\nRolling lineup decision backtest complete (V4 hybrid).")
message("Wrote:")
message(" - ", normalizePath(rows_out_path))
message(" - ", normalizePath(bucket_out_path))
message(" - ", normalizePath(game_bucket_out_path))
if (nrow(diag_df) > 0) message(" - ", normalizePath(diag_out_path))
message(" - ", normalizePath(rule_v2_thresholds_out_path))
message(" - ", normalizePath(calibration_model_out_path))

message("\nDecision bucket summary (V4 hybrid):")
print(bucket_summary %>%
        select(
          Decision,
          n_game_lineups,
          n_games,
          total_holdout_possessions,
          weighted_survive_score_robust,
          weighted_pred_def_ppp,
          weighted_observed_def_ppp,
          weighted_observed_survive4_rate,
          weighted_observed_leaky_rate,
          weighted_def_ppp_gap_vs_game_baseline
        ))
