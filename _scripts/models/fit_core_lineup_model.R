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

# Bayesian lineup model with opponent and game-state controls.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(rstan)
})

source("_scripts/utils/project_paths.R")

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

resolve_path <- function(fname) {
  resolve_project_path(fname)
}

ms_to_seconds <- function(x) {
  x <- as.character(x)
  x <- stringr::str_trim(x)
  suppressWarnings(lubridate::period_to_seconds(lubridate::ms(x)))
}

fit_has_draws <- function(fit) {
  # Returns TRUE if stanfit contains usable samples for alpha_net
  if (is.null(fit)) return(FALSE)

  # Quick check: can we coerce to draws?
  draws_n <- tryCatch(nrow(as.data.frame(fit)), error = function(e) 0)
  if (is.na(draws_n) || draws_n == 0) return(FALSE)

  # Stronger check: alpha_net exists and is 2D
  an <- tryCatch(rstan::extract(fit, pars = "alpha_net")$alpha_net,
                 error = function(e) NULL)
  if (is.null(an)) return(FALSE)
  d <- dim(an)
  if (is.null(d) || length(d) < 2) return(FALSE)

  TRUE
}

safe_md5 <- function(path) {
  x <- tryCatch(unname(tools::md5sum(path)), error = function(e) NA_character_)
  as.character(x[[1]])
}

zscore_safe <- function(x) {
  x <- as.numeric(x)
  m <- mean(x, na.rm = TRUE)
  s <- sd(x, na.rm = TRUE)
  if (!is.finite(m)) m <- 0
  if (!is.finite(s) || s <= 0) return(rep(0, length(x)))
  out <- (x - m) / s
  out[!is.finite(out)] <- 0
  out
}

as_num_env <- function(name, default) {
  x <- suppressWarnings(as.numeric(Sys.getenv(name, as.character(default))))
  if (!is.finite(x)) as.numeric(default) else x
}

read_metric_value <- function(metric_tbl, key, default) {
  if (is.null(metric_tbl) || !all(c("metric", "value") %in% names(metric_tbl))) {
    return(as.numeric(default))
  }
  v <- metric_tbl %>% filter(metric == key) %>% pull(value)
  if (length(v) == 0) return(as.numeric(default))
  x <- suppressWarnings(as.numeric(v[[1]]))
  if (!is.finite(x)) as.numeric(default) else x
}

load_calibration_model <- function(path) {
  if (!file.exists(path)) return(NULL)
  tbl <- tryCatch(read_csv(path, show_col_types = FALSE), error = function(e) NULL)
  if (is.null(tbl) || nrow(tbl) == 0) return(NULL)
  row <- tbl %>% slice(1)
  list(
    mode = as.character(row$mode[[1]]),
    status = as.character(row$status[[1]]),
    intercept = suppressWarnings(as.numeric(row$intercept[[1]])),
    slope = suppressWarnings(as.numeric(row$slope[[1]])),
    fallback_shrink = suppressWarnings(as.numeric(row$fallback_shrink[[1]]))
  )
}

apply_decision_calibration <- function(p_raw, calib_model, fallback_shrink = 0.85) {
  p_raw <- as.numeric(p_raw)
  p_base <- pmin(pmax(p_raw, 1e-4), 1 - 1e-4)
  out <- rep(NA_real_, length(p_raw))

  use_platt <- !is.null(calib_model) &&
    identical(calib_model$mode, "platt") &&
    identical(calib_model$status, "ok_strict") &&
    is.finite(calib_model$intercept) &&
    is.finite(calib_model$slope) &&
    calib_model$slope > 0

  if (use_platt) {
    out <- plogis(calib_model$intercept + calib_model$slope * qlogis(p_base))
  } else {
    shrink <- fallback_shrink
    if (!is.null(calib_model) && is.finite(calib_model$fallback_shrink)) {
      shrink <- calib_model$fallback_shrink
    }
    if (!is.finite(shrink) || shrink <= 0 || shrink >= 1) shrink <- fallback_shrink
    out <- 0.5 + shrink * (p_base - 0.5)
  }

  out[!is.finite(p_raw)] <- NA_real_
  pmin(pmax(out, 0.01), 0.99)
}

parse_period_index <- function(period) {
  p <- stringr::str_trim(as.character(period))
  out <- suppressWarnings(as.integer(p))

  idx1 <- is.na(out) & stringr::str_detect(p, regex("^1st\\s+Half", ignore_case = TRUE))
  idx2 <- is.na(out) & stringr::str_detect(p, regex("^2nd\\s+Half", ignore_case = TRUE))
  out[idx1] <- 1L
  out[idx2] <- 2L

  ot_match <- stringr::str_match(p, regex("^OT\\s*(\\d+)", ignore_case = TRUE))[, 2]
  ot_num <- suppressWarnings(as.integer(ot_match))
  idx_ot <- is.na(out) & !is.na(ot_num)
  out[idx_ot] <- 2L + ot_num[idx_ot]

  out
}

stints_path <- resolve_path("uconn_stints_from_pbp.csv")
games_path  <- resolve_path("uconn_games_meta.csv")
opp_path    <- resolve_path("opponent_controls.csv")
stan_path   <- resolve_path("uconn_lineup_gamelevel_offdef.stan")

out_dir     <- if (dir.exists("_outputs")) "_outputs" else "."
models_dir  <- if (dir.exists("_models")) "_models" else "."

fit_path    <- file.path(models_dir, "uconn_lineup_gamelevel_offdef_fit.rds")

# Thresholds come from env vars first, then the backtest output, then built-in defaults.
thresholds_path <- file.path("_outputs", "05_decision_audit", "uconn_lineup_decision_rule_v2_thresholds.csv")
thresholds_tbl <- if (file.exists(thresholds_path)) {
  tryCatch(read_csv(thresholds_path, show_col_types = FALSE), error = function(e) NULL)
} else {
  NULL
}

DECISION_RULE_PROB_POSSESSIONS <- {
  env <- suppressWarnings(as.numeric(Sys.getenv("DECISION_RULE_PROB_POSSESSIONS", "")))
  if (is.finite(env)) env else read_metric_value(thresholds_tbl, "DECISION_RULE_PROB_POSSESSIONS", 40)
}
DECISION_PLAY_MORE_PR_NET_MIN <- {
  env <- suppressWarnings(as.numeric(Sys.getenv("DECISION_PLAY_MORE_PR_NET_MIN", "")))
  if (is.finite(env)) env else read_metric_value(thresholds_tbl, "DECISION_PLAY_MORE_PR_NET_MIN", 0.553)
}
DECISION_PLAY_MORE_NET_PPP_MIN <- {
  env <- suppressWarnings(as.numeric(Sys.getenv("DECISION_PLAY_MORE_NET_PPP_MIN", "")))
  if (is.finite(env)) env else read_metric_value(thresholds_tbl, "DECISION_PLAY_MORE_NET_PPP_MIN", -0.015)
}
DECISION_LEAN_IN_PR_NET_MIN <- {
  env <- suppressWarnings(as.numeric(Sys.getenv("DECISION_LEAN_IN_PR_NET_MIN", "")))
  if (is.finite(env)) env else read_metric_value(thresholds_tbl, "DECISION_LEAN_IN_PR_NET_MIN", 0.528)
}
DECISION_LEAN_IN_NET_PPP_MIN <- {
  env <- suppressWarnings(as.numeric(Sys.getenv("DECISION_LEAN_IN_NET_PPP_MIN", "")))
  if (is.finite(env)) env else read_metric_value(thresholds_tbl, "DECISION_LEAN_IN_NET_PPP_MIN", -0.015)
}
DECISION_LIMIT_WATCH_PR_NET_MAX <- {
  env <- suppressWarnings(as.numeric(Sys.getenv("DECISION_LIMIT_WATCH_PR_NET_MAX", "")))
  if (is.finite(env)) env else read_metric_value(thresholds_tbl, "DECISION_LIMIT_WATCH_PR_NET_MAX", 0.657)
}
DECISION_LIMIT_WATCH_NET_PPP_MAX <- {
  env <- suppressWarnings(as.numeric(Sys.getenv("DECISION_LIMIT_WATCH_NET_PPP_MAX", "")))
  if (is.finite(env)) env else read_metric_value(thresholds_tbl, "DECISION_LIMIT_WATCH_NET_PPP_MAX", -0.06)
}

# Load the backtest calibration model when it exists.
calibration_model_path <- file.path("_outputs", "05_decision_audit", "uconn_pred_pr_net_pos_calibration_model.csv")
calibration_model <- load_calibration_model(calibration_model_path)

message(
  "Decision rule v2 (core) | prob_possessions=", DECISION_RULE_PROB_POSSESSIONS,
  " | PLAY MORE: pr_net>=", DECISION_PLAY_MORE_PR_NET_MIN, ", net_ppp>=", DECISION_PLAY_MORE_NET_PPP_MIN,
  " | LEAN IN: pr_net>=", DECISION_LEAN_IN_PR_NET_MIN, ", net_ppp>=", DECISION_LEAN_IN_NET_PPP_MIN,
  " | LIMIT/WATCH if pr_net<=", DECISION_LIMIT_WATCH_PR_NET_MAX, " or net_ppp<=", DECISION_LIMIT_WATCH_NET_PPP_MAX,
  " | thresholds_file=", file.exists(thresholds_path),
  " | calibration_mode=", if (!is.null(calibration_model)) calibration_model$mode else "fallback_shrink"
)

stints <- readr::read_csv(
  stints_path,
  show_col_types = FALSE,
  col_types = cols(
    start_time = col_character(),
    end_time   = col_character()
  )
)

games <- readr::read_csv(games_path, show_col_types = FALSE)
if (!("site_type" %in% names(games))) games$site_type <- NA_character_

games <- games %>%
  filter(!str_detect(game_file, "Exhibition"))

stints <- stints %>%
  filter(!str_detect(game_file, "Exhibition"))

# Clamp tiny possession counts before fitting.
stints <- stints %>%
  mutate(
    poss_est = as.numeric(poss_est),
    points_for = as.numeric(points_for),
    points_against = as.numeric(points_against),
    net_pts  = points_for - points_against
  )

tiny_poss <- stints %>%
  filter(!is.na(poss_est), poss_est > 0, poss_est < 1) %>%
  select(game_file, period, stint_index, start_time, end_time, poss_est, net_pts)

if (nrow(tiny_poss) > 0) {
  message("Found poss_est < 1 (will clamp to 1 to prevent Stan failure). Rows:")
  print(tiny_poss)
}

stints <- stints %>%
  mutate(
    poss_est = if_else(!is.na(poss_est) & poss_est > 0 & poss_est < 1, 1, poss_est),
    net_ppp  = if_else(!is.na(poss_est) & poss_est > 0, net_pts / poss_est, NA_real_)
  )

# Rebuild stint-level score and time context from the play-by-play sequence.
stints <- stints %>%
  mutate(
    period_num = parse_period_index(period),
    start_clock_sec = ms_to_seconds(start_time),
    elapsed_game_sec = case_when(
      is.na(period_num) | is.na(start_clock_sec) ~ NA_real_,
      period_num <= 1 ~ pmax(0, 1200 - start_clock_sec),
      period_num == 2 ~ 1200 + pmax(0, 1200 - start_clock_sec),
      TRUE ~ 2400 + pmax(0, period_num - 3) * 300 + pmax(0, 300 - pmin(start_clock_sec, 300))
    ),
    .row_id_restore = row_number()
  ) %>%
  group_by(game_file) %>%
  arrange(period_num, desc(start_clock_sec), stint_index, .by_group = TRUE) %>%
  mutate(
    score_margin_start = lag(cumsum(coalesce(net_pts, 0)), default = 0)
  ) %>%
  ungroup() %>%
  arrange(.row_id_restore) %>%
  select(-.row_id_restore)

opponent_controls <- read_csv(opp_path, show_col_types = FALSE) %>%
  mutate(
    game_date = format(mdy(game_date), "%m/%d/%y"),
    opponent  = str_trim(opponent)
  )

games2 <- games %>%
  mutate(
    game_date = format(mdy(game_date), "%m/%d/%y"),
    opponent  = str_trim(opponent),
    site_home = if_else(uconn_is_home, 1.0, 0.0)
  ) %>%
  left_join(opponent_controls, by = c("game_date", "opponent"))

missing <- games2 %>%
  filter(is.na(opp_adjO) | is.na(opp_adjD)) %>%
  select(game_date, opponent, game_file)

if (nrow(missing) > 0) {
  print(missing)
  stop("Some games did not match opponent_controls.csv. Fix the rows above.")
}

# Sort player names inside each lineup key so the same unit is not duplicated.
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
    uconn_lineup_canon = sapply(
      str_split(uconn_lineup, "\\|"),
      function(x) paste(sort(str_trim(x)), collapse = "|")
    ),
    y = net_ppp,
    w = poss_est
  ) %>%
  mutate(
    score_margin_start_z = zscore_safe(score_margin_start),
    elapsed_game_sec_z = zscore_safe(elapsed_game_sec)
  )

# Make sure the latest game actually feeds the fit.
eligible_by_game <- stints2 %>%
  count(game_file, name = "eligible_rows") %>%
  arrange(desc(eligible_rows))

message("Eligible modeling rows by game (top 10):")
print(head(eligible_by_game, 10))

if (nrow(stints2) < 10) {
  stop(
    "Model input too small after filtering (N = ", nrow(stints2), ").\n",
    "This usually means the new game has poss_est missing/0 or net_ppp missing.\n",
    "Fix those columns, then rerun."
  )
}

players <- sort(unique(unlist(str_split(stints2$uconn_lineup_canon, "\\|"))))
player_id <- setNames(seq_along(players), players)

lineups <- sort(unique(stints2$uconn_lineup_canon))
lineup_id <- setNames(seq_along(lineups), lineups)

uconn5_mat <- t(sapply(
  str_split(stints2$uconn_lineup_canon, "\\|"),
  function(x) player_id[x]
))

opp_adjO_z <- as.numeric(scale(games2$opp_adjO))
opp_adjD_z <- as.numeric(scale(games2$opp_adjD))
opp_adjO_z[!is.finite(opp_adjO_z)] <- 0
opp_adjD_z[!is.finite(opp_adjD_z)] <- 0

data_list <- list(
  N = nrow(stints2),
  P = length(players),
  L = length(lineups),
  uconn5 = uconn5_mat,
  lineup_id = as.integer(lineup_id[stints2$uconn_lineup_canon]),
  game_id = stints2$game_id,
  G = nrow(games2),
  y = stints2$y,
  w = stints2$w,
  opp_adjO_z = opp_adjO_z,
  opp_adjD_z = opp_adjD_z,
  site_home = games2$site_home,
  score_margin_start_z = stints2$score_margin_start_z,
  elapsed_game_sec_z = stints2$elapsed_game_sec_z
)

current_cache_signature <- paste(
  "schema_v2",
  "stints", safe_md5(stints_path),
  "games", safe_md5(games_path),
  "opp", safe_md5(opp_path),
  "stan", safe_md5(stan_path),
  "N", nrow(stints2),
  "P", length(players),
  "L", length(lineups),
  "G", nrow(games2),
  sep = "|"
)

# Reuse a cached fit when the inputs still match.
FORCE_REFIT <- tolower(Sys.getenv("FORCE_REFIT", "false")) == "true"

fit <- NULL

if (!FORCE_REFIT && file.exists(fit_path)) {
  message("Cache hit: loading fitted model from ", fit_path)
  cached <- readRDS(fit_path)

  cached_sig <- attr(cached, "cache_signature")

  if (is.null(cached_sig) || !identical(cached_sig, current_cache_signature)) {
    message(
      "Cached fit invalid (cache signature mismatch). Refitting."
    )
    fit <- NULL
  } else if (!fit_has_draws(cached)) {
    message("Cached fit is missing samples. Refitting.")
    fit <- NULL
  } else {
    fit <- cached
  }
}

if (is.null(fit)) {
  message("Cache miss (or FORCE_REFIT=TRUE): fitting model...")

  # Hard checks to prevent silent failure
  if (any(!is.finite(data_list$y))) stop("Non-finite y in data_list (net_ppp).")
  if (any(!is.finite(data_list$w))) stop("Non-finite w in data_list (poss_est).")
  if (any(data_list$w <= 0)) stop("w must be > 0 everywhere.")
  message("y range: ", paste(range(data_list$y), collapse=" to "),
          " | w range: ", paste(range(data_list$w), collapse=" to "))

  # Safe initial values to avoid sigma/tau near zero
  init_fun <- function() list(
    intercept = 0.0,
    sigma = 0.8,
    tau_net = 0.08,
    tau_u   = 0.05,
    b_oppO  = 0.0,
    b_oppD  = 0.0,
    b_home  = 0.0,
    b_score_margin = 0.0,
    b_elapsed_game = 0.0
  )

  fit <- rstan::stan(
    file = stan_path,
    data = data_list,
    chains = 4,
    iter = 2000,
    warmup = 1000,
    seed = 20260111,
    init = init_fun,
    refresh = 50,               # keep chain messages visible
    control = list(adapt_delta = 0.99, max_treedepth = 15)
  )

  if (!fit_has_draws(fit)) {
    stop("Stan fit completed but produced no usable samples. Check Stan output/errors.")
  }

  attr(fit, "cache_signature") <- current_cache_signature
  saveRDS(fit, fit_path)
  message("Model cache saved: ", fit_path)
}

print(fit, pars = c(
  "intercept","tau_net","tau_u",
  "b_oppO","b_oppD","b_home","b_score_margin","b_elapsed_game",
  "sigma"
))

sampler_params <- tryCatch(rstan::get_sampler_params(fit, inc_warmup = FALSE), error = function(e) NULL)
if (!is.null(sampler_params)) {
  diag_df <- dplyr::bind_rows(lapply(seq_along(sampler_params), function(i) {
    tibble::as_tibble(sampler_params[[i]]) %>% dplyr::mutate(chain = i)
  }))
  write_csv(diag_df, file.path(out_dir, "uconn_lineup_core_model_diagnostics.csv"))
}

post <- rstan::extract(fit)

# Under a net-PPP target, the player term is a net effect.
alpha_net <- post$alpha_net

if (is.null(alpha_net) || length(dim(alpha_net)) < 2) {
  stop("alpha_net is not a 2D array. The fit does not contain samples.")
}

alpha_net_mean <- colMeans(alpha_net)
alpha_net_q <- apply(alpha_net, 2, quantile, probs = c(0.05, 0.5, 0.95))

player_out <- tibble(
  player = players,

  net_mean = alpha_net_mean,
  net_p05  = alpha_net_q[1,],
  net_p50  = alpha_net_q[2,],
  net_p95  = alpha_net_q[3,],
  net_pr_pos = colMeans(alpha_net > 0)
) %>%
  arrange(desc(net_mean))

deprecated_player_files <- c(
  "uconn_player_off_def_net_posterior.csv",
  "uconn_player_offense_ranking.csv",
  "uconn_player_defense_ranking.csv"
)

for (root in unique(c(out_dir, file.path(out_dir, "03_players")))) {
  if (!dir.exists(root)) next
  for (fname in deprecated_player_files) {
    p <- file.path(root, fname)
    if (file.exists(p)) {
      ok <- file.remove(p)
      if (isTRUE(ok)) {
        message("Removed deprecated player output: ", normalizePath(p))
      }
    }
  }
}

write_csv(player_out, file.path(out_dir, "uconn_player_net_posterior.csv"))
write_csv(player_out %>% arrange(desc(net_mean)), file.path(out_dir, "uconn_player_net_ranking.csv"))

stints_usage <- stints2 %>%
  mutate(
    start_sec = ms_to_seconds(start_time),
    end_sec   = ms_to_seconds(end_time),
    dur_sec_raw = start_sec - end_sec,
    dur_sec = case_when(
      is.na(dur_sec_raw) ~ NA_real_,
      dur_sec_raw >= 0   ~ dur_sec_raw,
      TRUE               ~ abs(dur_sec_raw)
    ),
    dur_min = dur_sec / 60,
    net_pts_stint = y * w
  )

lineup_usage <- stints_usage %>%
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
  ) %>%
  arrange(desc(possessions))

write_csv(lineup_usage, file.path(out_dir, "uconn_lineup_usage.csv"))

PACE_UCONN <- sum(lineup_usage$possessions, na.rm = TRUE) / sum(lineup_usage$minutes, na.rm = TRUE)

if (!is.finite(PACE_UCONN) || PACE_UCONN <= 0) {
  stop("PACE_UCONN is invalid. Check minutes/possessions calculation in lineup_usage.")
}

message("Computed PACE_UCONN (poss/min): ", round(PACE_UCONN, 3),
        " | implied poss per 40: ", round(PACE_UCONN * 40, 1))

# Lineup synergy posterior + Coach View + Decision Table
u_draws <- post$u
if (is.null(u_draws) || length(dim(u_draws)) < 2) {
  stop("u (lineup synergy) is not a 2D array. The fit does not contain samples.")
}

u_mean <- colMeans(u_draws)
u_q <- apply(u_draws, 2, quantile, probs = c(0.05, 0.5, 0.95))

lineup_bayes <- tibble(
  lineup_id = seq_along(lineups),
  lineup = lineups,
  synergy_mean = u_mean,
  synergy_p05  = u_q[1,],
  synergy_p50  = u_q[2,],
  synergy_p95  = u_q[3,],
  pr_synergy_pos = colMeans(u_draws > 0)
) %>%
  mutate(lineup_pretty = lineup)

write_csv(lineup_bayes, file.path(out_dir, "uconn_lineup_synergy_posterior.csv"))

# Baseline-context full-net predictions for decision rule v2
# Context convention matches rolling backtest training table:
# z-scored game-state covariates = 0, opponent controls = 0, away_or_neutral baseline.
intercept_draws <- as.numeric(post$intercept)
alpha_net_draws <- post$alpha_net
sigma_draws <- as.numeric(post$sigma)

if (!is.numeric(intercept_draws) || length(intercept_draws) == 0) {
  stop("Posterior 'intercept' draws missing/invalid.")
}
if (is.null(alpha_net_draws) || length(dim(alpha_net_draws)) < 2) {
  stop("Posterior 'alpha_net' draws missing/invalid.")
}
if (!is.numeric(sigma_draws) || length(sigma_draws) == 0) {
  stop("Posterior 'sigma' draws missing/invalid.")
}
if (length(intercept_draws) != nrow(alpha_net_draws) || length(sigma_draws) != nrow(alpha_net_draws)) {
  stop("Posterior draw lengths do not align for intercept/alpha_net/sigma.")
}

lineup_decision_pred <- bind_rows(lapply(seq_along(lineups), function(lid) {
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
  pr_pos <- if (any(ok_draws)) {
    mean(pnorm(mu_draws[ok_draws] / pred_sd_draws[ok_draws]), na.rm = TRUE)
  } else {
    NA_real_
  }

  tibble(
    lineup = lineup_str,
    decision_pred_net_ppp_mean = mean(mu_draws, na.rm = TRUE),
    decision_pred_pr_net_pos = pr_pos
  )
}))

lineup_decision_pred <- lineup_decision_pred %>%
  mutate(
    decision_pred_pr_net_pos_raw = decision_pred_pr_net_pos,
    decision_pred_pr_net_pos = apply_decision_calibration(
      p_raw = decision_pred_pr_net_pos_raw,
      calib_model = calibration_model,
      fallback_shrink = 0.85
    )
  )

# Older runs exported a calibrated synergy column. Decision rule v2 uses the
# net-based posterior predictions instead, so that column is no longer written.
coach_view <- lineup_bayes %>%
  left_join(lineup_decision_pred, by = "lineup") %>%
  left_join(
    lineup_usage %>% select(lineup_id, possessions, minutes, raw_net_ppp, games, segments),
    by = "lineup_id"
  ) %>%
  mutate(
    expected_points_per_40 = round(synergy_mean * 40 * PACE_UCONN, 1),

    minutes = round(minutes, 1),
    possessions = round(possessions, 0),
    raw_net_ppp = round(raw_net_ppp, 3),

    synergy_mean = round(synergy_mean, 3),
    synergy_p05 = round(synergy_p05, 3),
    synergy_p95 = round(synergy_p95, 3),
    decision_pred_pr_net_pos_raw = round(decision_pred_pr_net_pos_raw, 3),
    decision_pred_net_ppp_mean = round(decision_pred_net_ppp_mean, 3),
    decision_pred_pr_net_pos = round(decision_pred_pr_net_pos, 3),
    decision_prob = decision_pred_pr_net_pos,

    Decision = case_when(
      possessions >= 70 &
        decision_pred_pr_net_pos >= DECISION_PLAY_MORE_PR_NET_MIN &
        decision_pred_net_ppp_mean >= DECISION_PLAY_MORE_NET_PPP_MIN ~ "PLAY MORE",
      possessions >= 45  &
        decision_pred_pr_net_pos >= DECISION_LEAN_IN_PR_NET_MIN &
        decision_pred_net_ppp_mean >= DECISION_LEAN_IN_NET_PPP_MIN ~ "LEAN IN",
      possessions >= 30  &
        (
          decision_pred_pr_net_pos <= DECISION_LIMIT_WATCH_PR_NET_MAX |
          decision_pred_net_ppp_mean <= DECISION_LIMIT_WATCH_NET_PPP_MAX
        ) ~ "LIMIT / WATCH",
      possessions <  30                           ~ "TOO SMALL",
      TRUE                                       ~ "NEUTRAL"
    )
  ) %>%
  arrange(
    factor(Decision, levels = c("PLAY MORE","LEAN IN","NEUTRAL","LIMIT / WATCH","TOO SMALL")),
    desc(possessions)
  ) %>%
  select(
    lineup_pretty,
    possessions,
    minutes,
    games,
    raw_net_ppp,
    synergy_mean,
    synergy_p05,
    synergy_p95,
    decision_pred_pr_net_pos_raw,
    decision_pred_net_ppp_mean,
    decision_pred_pr_net_pos,
    decision_prob,
    expected_points_per_40,
    Decision
  )

write_csv(coach_view, file.path(out_dir, "uconn_lineup_coach_view.csv"))
write_csv(coach_view, file.path(out_dir, "uconn_lineup_decision_table.csv"))

message("Done. Wrote to: ", normalizePath(out_dir))
message("Model cache: ", normalizePath(fit_path))
