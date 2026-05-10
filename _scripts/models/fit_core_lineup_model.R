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
source("_scripts/utils/lineup_model_utils.R")

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

resolve_path <- function(fname) {
  resolve_project_path(fname)
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

stints_path <- resolve_path("uconn_stints_from_pbp.csv")
games_path  <- resolve_path("uconn_games_meta.csv")
opp_path    <- resolve_path("opponent_controls.csv")
stan_path   <- resolve_path("uconn_lineup_gamelevel_offdef.stan")

out_dir     <- if (dir.exists("_outputs")) "_outputs" else "."
models_dir  <- if (dir.exists("_models")) "_models" else "."

fit_path    <- file.path(models_dir, "uconn_lineup_gamelevel_offdef_fit.rds")

# Tuned parameters come from env vars first, then backtest thresholds, then defaults.
thresholds_path <- file.path("_outputs", "05_decision_audit", "uconn_lineup_decision_rule_v2_thresholds.csv")
thresholds_tbl <- if (file.exists(thresholds_path)) {
  tryCatch(read_csv(thresholds_path, show_col_types = FALSE), error = function(e) NULL)
} else {
  NULL
}

DECISION_V4_W_DEF <- {
  env <- suppressWarnings(as.numeric(Sys.getenv("DECISION_V4_W_DEF", "")))
  if (is.finite(env)) env else read_metric_value(thresholds_tbl, "W_DEF", 0.60)
}
DECISION_V4_W_OFF <- {
  env <- suppressWarnings(as.numeric(Sys.getenv("DECISION_V4_W_OFF", "")))
  if (is.finite(env)) env else read_metric_value(thresholds_tbl, "W_OFF", 0.25)
}
DECISION_V4_W_STYLE <- {
  env <- suppressWarnings(as.numeric(Sys.getenv("DECISION_V4_W_STYLE", "")))
  if (is.finite(env)) env else read_metric_value(thresholds_tbl, "W_STYLE", 0.15)
}
DECISION_V4_ALPHA_OPP <- {
  env <- suppressWarnings(as.numeric(Sys.getenv("DECISION_V4_ALPHA_OPP", "")))
  if (is.finite(env)) env else read_metric_value(thresholds_tbl, "ALPHA_OPP", 0.10)
}
DECISION_V4_DEF_FLOOR_QUANTILE <- {
  env <- suppressWarnings(as.numeric(Sys.getenv("DECISION_V4_DEF_FLOOR_QUANTILE", "")))
  if (is.finite(env)) env else read_metric_value(thresholds_tbl, "DEF_FLOOR_QUANTILE", 0.35)
}
DECISION_V4_DEF_FLOOR_T <- {
  env <- suppressWarnings(as.numeric(Sys.getenv("DECISION_V4_DEF_FLOOR_T", "")))
  if (is.finite(env)) env else read_metric_value(thresholds_tbl, "DEF_FLOOR_T", NA_real_)
}

FLOOR_RIM_PLUS_THREE_SHARE <- 0.6190
FLOOR_FTA_PER_FGA <- 0.2308
CEILING_TOV_PER_FGA <- 0.2000
CEILING_NON_RIM_PAINT_SHARE <- 0.2596

weights_raw <- c(
  w_def = as.numeric(DECISION_V4_W_DEF),
  w_off = as.numeric(DECISION_V4_W_OFF),
  w_style = as.numeric(DECISION_V4_W_STYLE)
)
weights_raw[!is.finite(weights_raw) | weights_raw < 0] <- 0
if (sum(weights_raw) <= 0) {
  weights_raw <- c(w_def = 0.60, w_off = 0.25, w_style = 0.15)
}
weights_norm <- weights_raw / sum(weights_raw)
DECISION_V4_W_DEF <- weights_norm[["w_def"]]
DECISION_V4_W_OFF <- weights_norm[["w_off"]]
DECISION_V4_W_STYLE <- weights_norm[["w_style"]]
if (!is.finite(DECISION_V4_ALPHA_OPP) || DECISION_V4_ALPHA_OPP < 0) DECISION_V4_ALPHA_OPP <- 0.10
if (!is.finite(DECISION_V4_DEF_FLOOR_QUANTILE) ||
    DECISION_V4_DEF_FLOOR_QUANTILE <= 0 ||
    DECISION_V4_DEF_FLOOR_QUANTILE >= 1) {
  DECISION_V4_DEF_FLOOR_QUANTILE <- 0.35
}

message(
  "Decision rule v4 (core) | w_def=", round(DECISION_V4_W_DEF, 3),
  " | w_off=", round(DECISION_V4_W_OFF, 3),
  " | w_style=", round(DECISION_V4_W_STYLE, 3),
  " | alpha_opp=", round(DECISION_V4_ALPHA_OPP, 3),
  " | def_floor_q=", round(DECISION_V4_DEF_FLOOR_QUANTILE, 3),
  " | def_floor_t=", DECISION_V4_DEF_FLOOR_T,
  " | thresholds_file=", file.exists(thresholds_path)
)

model_inputs <- load_common_lineup_model_inputs(
  stints_path = stints_path,
  games_path = games_path,
  opp_path = opp_path
)
stints <- model_inputs$stints
games <- model_inputs$games
games2 <- model_inputs$games_joined
tiny_poss <- model_inputs$tiny_poss %>%
  select(game_file, period, stint_index, start_time, end_time, poss_est, net_pts)

if (nrow(tiny_poss) > 0) {
  message("Found poss_est < 1 (will clamp to 1 to prevent Stan failure). Rows:")
  print(tiny_poss)
}

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
    uconn_lineup_canon = canonicalize_lineup(uconn_lineup),
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
  } else if (!fit_has_draws(cached, "alpha_net")) {
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

  if (!fit_has_draws(fit, "alpha_net")) {
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
  "uconn_player_defense_ranking.csv",
  "uconn_player_net_ranking.csv"
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

deprecated_lineup_files <- c("uconn_lineup_decision_table.csv")

for (root in unique(c(out_dir, file.path(out_dir, "01_lineup_core"), file.path(out_dir, "05_decision_audit")))) {
  if (!dir.exists(root)) next
  for (fname in deprecated_lineup_files) {
    p <- file.path(root, fname)
    if (file.exists(p)) {
      ok <- file.remove(p)
      if (isTRUE(ok)) {
        message("Removed deprecated lineup output: ", normalizePath(p))
      }
    }
  }
}

write_csv(player_out, file.path(out_dir, "uconn_player_net_posterior.csv"))

stints_usage <- stints2 %>%
  mutate(
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

opp_component <- function(oppO_z, oppD_z) {
  plogis(-0.6 * oppO_z + 0.2 * oppD_z)
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

archetype_path <- file.path("_data", "01_core_inputs", "player_archetypes.csv")
arche_map <- load_player_archetypes(archetype_path, active_players = players)

manual_events <- load_manual_defensive_events(
  manual_root = file.path("_data", "03_manual_game_csv"),
  games_joined = games2
)
if (nrow(manual_events) == 0) {
  stop("No manual defensive events loaded from _data/03_manual_game_csv; V3 requires this source.")
}

pair_baseline <- weighted_mean_safe(manual_events$stop_event, rep(1, nrow(manual_events)))
if (!is.finite(pair_baseline)) pair_baseline <- 0.5
pair_tbl <- manual_events %>%
  mutate(pair_key = lapply(defense_lineup_key, lineup_pair_keys_norm)) %>%
  select(stop_event, pair_key) %>%
  tidyr::unnest_longer(pair_key, values_to = "pair_key") %>%
  filter(!is.na(pair_key), nzchar(pair_key)) %>%
  group_by(pair_key) %>%
  summarise(
    events = n(),
    stops = sum(stop_event, na.rm = TRUE),
    pair_survive = (stops + 80 * pair_baseline) / (events + 80),
    .groups = "drop"
  )

pair_split <- stringr::str_split_fixed(pair_tbl$pair_key, "\\|", 2)
pair_ranking <- pair_tbl %>%
  mutate(
    player_1 = pair_split[, 1],
    player_2 = pair_split[, 2],
    raw_stop_rate = if_else(events > 0, stops / events, NA_real_),
    pair_survive = pmin(pmax(pair_survive, 0), 1),
    pair_survive_above_baseline = pair_survive - pair_baseline,
    sample_tier = case_when(
      events >= 200 ~ "HIGH",
      events >= 80 ~ "MEDIUM",
      events >= 30 ~ "LOW",
      TRUE ~ "VERY_LOW"
    )
  ) %>%
  arrange(desc(pair_survive), desc(events), pair_key) %>%
  mutate(
    rank_best_to_worst = row_number(),
    rank_worst_to_best = min_rank(pair_survive)
  ) %>%
  select(
    rank_best_to_worst,
    rank_worst_to_best,
    player_1,
    player_2,
    pair_key,
    events,
    stops,
    raw_stop_rate,
    pair_survive,
    pair_survive_above_baseline,
    sample_tier
  )

write_csv(pair_ranking, file.path(out_dir, "uconn_defensive_two_man_pairs.csv"))

team_def_ppp <- sum(stints2$points_against, na.rm = TRUE) / sum(stints2$poss_est, na.rm = TRUE)
stints_trio <- stints2 %>%
  mutate(
    trio_stop = as.numeric((points_against / poss_est) <= team_def_ppp),
    trio_key = lapply(uconn_lineup_canon, lineup_trio_keys),
    event_w = poss_est,
    stop_w = trio_stop * poss_est
  )
trio_baseline <- weighted_mean_safe(stints_trio$trio_stop, stints_trio$event_w)
if (!is.finite(trio_baseline)) trio_baseline <- 0.5
trio_tbl <- stints_trio %>%
  select(trio_key, event_w, stop_w) %>%
  tidyr::unnest_longer(trio_key, values_to = "trio_key") %>%
  filter(!is.na(trio_key), nzchar(trio_key)) %>%
  group_by(trio_key) %>%
  summarise(
    events = sum(event_w, na.rm = TRUE),
    stops = sum(stop_w, na.rm = TRUE),
    trio_survive = (stops + 120 * trio_baseline) / (events + 120),
    .groups = "drop"
  )

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
leak_map <- leak_tbl %>%
  mutate(lineup = canonicalize_lineup(lineup), pr_leak = as.numeric(pr_leak)) %>%
  select(lineup, pr_leak) %>%
  distinct(lineup, .keep_all = TRUE) %>%
  tibble::deframe()

shot_candidates <- c(
  file.path("_outputs", "01_lineup_core", "uconn_lineup_shot_diet.csv"),
  file.path("_outputs", "uconn_lineup_shot_diet.csv")
)
shot_path <- shot_candidates[file.exists(shot_candidates)][1]
if (length(shot_path) == 0 || !nzchar(shot_path)) {
  stop("Missing required lineup shot diet profile for V4 scoring.")
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
    lineup_pretty_shot = dplyr::first(lineup_pretty),
    shot_fga = sum(fga, na.rm = TRUE),
    rim_plus_three_share = weighted_mean_safe(rim_plus_three_share, pmax(fga, 1)),
    non_rim_paint_share = weighted_mean_safe(non_rim_paint_share, pmax(fga, 1)),
    fta_per_fga_lineup = weighted_mean_safe(fta_per_fga, pmax(fga, 1)),
    tov_per_fga_lineup = weighted_mean_safe(tov_per_fga, pmax(fga, 1)),
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
  stop("Missing required player creation profile for V4 scoring.")
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

lineup_features <- lineup_usage %>%
  transmute(
    lineup = uconn_lineup_canon,
    lineup_key_norm = canonicalize_lineup_norm(uconn_lineup_canon),
    prior_5man_possessions = possessions
  )

lineup_features <- lineup_features %>%
  rowwise() %>%
  mutate(
    pair_keys = list(lineup_pair_keys_norm(lineup)),
    trio_keys = list(lineup_trio_keys(lineup)),
    pair_survive = {
      pk <- pair_keys[[1]]
      if (length(pk) == 0) pair_baseline else {
        m <- match(pk, pair_tbl$pair_key)
        v <- pair_tbl$pair_survive[m]
        ev <- pair_tbl$events[m]
        v[!is.finite(v)] <- pair_baseline
        ev[!is.finite(ev)] <- 0
        if (sum(ev, na.rm = TRUE) > 0) weighted_mean_safe(v, ev) else mean(v, na.rm = TRUE)
      }
    },
    prior_pair_events = {
      pk <- pair_keys[[1]]
      if (length(pk) == 0) 0 else {
        m <- match(pk, pair_tbl$pair_key)
        ev <- pair_tbl$events[m]
        ev[!is.finite(ev)] <- 0
        sum(ev, na.rm = TRUE)
      }
    },
    trio_survive = {
      tk <- trio_keys[[1]]
      if (length(tk) == 0) trio_baseline else {
        m <- match(tk, trio_tbl$trio_key)
        v <- trio_tbl$trio_survive[m]
        ev <- trio_tbl$events[m]
        v[!is.finite(v)] <- trio_baseline
        ev[!is.finite(ev)] <- 0
        if (sum(ev, na.rm = TRUE) > 0) weighted_mean_safe(v, ev) else mean(v, na.rm = TRUE)
      }
    },
    prior_trio_possessions = {
      tk <- trio_keys[[1]]
      if (length(tk) == 0) 0 else {
        m <- match(tk, trio_tbl$trio_key)
        ev <- trio_tbl$events[m]
        ev[!is.finite(ev)] <- 0
        sum(ev, na.rm = TRUE)
      }
    },
    pr_leak = {
      p <- unname(leak_map[lineup])[[1]]
      if (!is.finite(p)) 0.5 else p
    },
    archetype_balance = compute_lineup_archetype_balance(lineup, arche_map),
    creation_score = {
      toks <- split_lineup_players_norm(lineup)
      vals <- as.numeric(player_creation_map[toks])
      if (length(vals) == 0 || all(!is.finite(vals))) 0.5 else mean(vals[is.finite(vals)], na.rm = TRUE)
    }
  ) %>%
  ungroup() %>%
  select(-pair_keys, -trio_keys) %>%
  left_join(shot_tbl, by = "lineup_key_norm")

shot_default_rim_plus_three <- safe_quantile(shot_tbl$rim_plus_three_share, 0.50, default = FLOOR_RIM_PLUS_THREE_SHARE)
shot_default_non_rim_paint <- safe_quantile(shot_tbl$non_rim_paint_share, 0.50, default = CEILING_NON_RIM_PAINT_SHARE)
shot_default_fta <- safe_quantile(shot_tbl$fta_per_fga_lineup, 0.50, default = FLOOR_FTA_PER_FGA)
shot_default_tov <- safe_quantile(shot_tbl$tov_per_fga_lineup, 0.50, default = CEILING_TOV_PER_FGA)

lineup_features <- lineup_features %>%
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
    defense_score_context = 0.9 * defense_score_neutral + 0.1 * opp_component(0, 0),
    defense_score = (1 - DECISION_V4_ALPHA_OPP) * defense_score_neutral + DECISION_V4_ALPHA_OPP * defense_score_context,
    defense_score = pmin(pmax(defense_score, 0), 1),
    decision_survive_base = defense_score_neutral,
    decision_survive_score_raw = defense_score
  )

def_floor_t <- DECISION_V4_DEF_FLOOR_T
if (!is.finite(def_floor_t)) {
  def_floor_t <- safe_quantile(
    lineup_features$defense_score,
    DECISION_V4_DEF_FLOOR_QUANTILE,
    default = 0.50
  )
}
if (!is.finite(def_floor_t)) {
  stop("Invalid V4 defense floor threshold.", call. = FALSE)
}

grid_vals <- c(-0.5, 0, 0.5)
fragile <- rep(FALSE, nrow(lineup_features))
robust <- lineup_features$defense_score

for (i in seq_len(nrow(lineup_features))) {
  pert_scores <- c()
  for (do in grid_vals) {
    for (dd in grid_vals) {
      s_context <- 0.9 * lineup_features$defense_score_neutral[[i]] + 0.1 * opp_component(do, dd)
      s <- (1 - DECISION_V4_ALPHA_OPP) * lineup_features$defense_score_neutral[[i]] + DECISION_V4_ALPHA_OPP * s_context
      s <- min(max(s, 0), 1)
      pert_scores <- c(pert_scores, s)
    }
  }
  base_pass <- is.finite(lineup_features$defense_score[[i]]) && lineup_features$defense_score[[i]] >= def_floor_t
  pert_pass <- pert_scores >= def_floor_t
  if (length(pert_pass) > 0 && any(pert_pass != base_pass)) {
    fragile[[i]] <- TRUE
  }
  robust[[i]] <- min(c(lineup_features$defense_score[[i]], pert_scores), na.rm = TRUE)
}

lineup_features <- lineup_features %>%
  mutate(
    opp_fragile_flag = fragile,
    defense_score_robust = pmin(pmax(robust, 0), 1),
    decision_survive_score_robust = defense_score_robust,
    decision_def_ppp_pred = team_def_ppp + (0.5 - defense_score_robust) * 0.30
  )

coach_view <- lineup_bayes %>%
  left_join(lineup_features, by = c("lineup" = "lineup")) %>%
  left_join(
    lineup_usage %>% select(lineup_id, lineup_pretty, possessions, minutes, raw_net_ppp, games),
    by = c("lineup_id", "lineup_pretty")
  ) %>%
  mutate(
    expected_points_per_40 = round(synergy_mean * 40 * PACE_UCONN, 1),
    offense_upside_raw = 0.60 * zscore_safe(expected_points_per_40) +
      0.25 * zscore_safe(creation_score) +
      0.15 * zscore_safe(fta_per_fga_lineup),
    offense_upside_score = rescale01(offense_upside_raw, default = 0.5),
    composite_score = DECISION_V4_W_DEF * defense_score_robust +
      DECISION_V4_W_OFF * offense_upside_score +
      DECISION_V4_W_STYLE * shot_diet_score,
    composite_score = pmin(pmax(composite_score, 0), 1),
    is_unseen = !is.finite(prior_5man_possessions) |
      !is.finite(prior_pair_events) |
      !is.finite(prior_trio_possessions) |
      (prior_5man_possessions == 0 & prior_pair_events < 20),
    is_low_sample = !is_unseen & (
      prior_5man_possessions < 30 |
        prior_pair_events < 80 |
        prior_trio_possessions < 40
    ),
    defense_floor_pass = !is_unseen & !is_low_sample & is.finite(defense_score_robust) & defense_score_robust >= def_floor_t,
    sample_flag = case_when(
      is_unseen ~ "unseen",
      is_low_sample ~ "low_sample",
      tolower(coalesce(shot_sample_flag, "")) == "ok" ~ "ok",
      TRUE ~ "small_sample"
    ),
    minutes = round(minutes, 1),
    possessions = round(possessions, 0),
    raw_net_ppp = round(raw_net_ppp, 3),
    synergy_mean = round(synergy_mean, 3),
    synergy_p05 = round(synergy_p05, 3),
    synergy_p95 = round(synergy_p95, 3),
    defense_score = round(defense_score_robust, 3),
    decision_survive_score_raw = round(defense_score, 3),
    decision_survive_score_robust = round(defense_score, 3),
    offense_upside_score = round(offense_upside_score, 3),
    shot_diet_score = round(shot_diet_score, 3),
    creation_score = round(creation_score, 3),
    composite_score = round(composite_score, 3),
    decision_def_ppp_pred = round(decision_def_ppp_pred, 3),
    prior_pair_events = round(prior_pair_events, 1),
    prior_trio_possessions = round(prior_trio_possessions, 1),
    opp_fragile_flag = as.logical(opp_fragile_flag)
  )

upside_df <- coach_view %>% filter(defense_floor_pass, is.finite(composite_score))
up_q_low <- safe_quantile(upside_df$composite_score, 1 / 3, default = NA_real_)
up_q_high <- safe_quantile(upside_df$composite_score, 2 / 3, default = NA_real_)
if (!is.finite(up_q_low) || !is.finite(up_q_high) || up_q_high < up_q_low) {
  up_q_low <- 0.33
  up_q_high <- 0.66
}

coach_view <- coach_view %>%
  mutate(
    decision_label = case_when(
      is_unseen ~ "UNSEEN",
      is_low_sample ~ "LOW_SAMPLE",
      !defense_floor_pass ~ "DEF_FLOOR_FAIL",
      composite_score >= up_q_high ~ "DEF_FLOOR_PASS_UPSIDE_HIGH",
      composite_score < up_q_low ~ "DEF_FLOOR_PASS_UPSIDE_LOW",
      TRUE ~ "DEF_FLOOR_PASS_UPSIDE_MED"
    ),
    Decision = decision_label
  ) %>%
  arrange(
    factor(
      decision_label,
      levels = c(
        "DEF_FLOOR_PASS_UPSIDE_HIGH",
        "DEF_FLOOR_PASS_UPSIDE_MED",
        "DEF_FLOOR_PASS_UPSIDE_LOW",
        "DEF_FLOOR_FAIL",
        "LOW_SAMPLE",
        "UNSEEN"
      )
    ),
    desc(possessions)
  ) %>%
  select(
    lineup_pretty,
    lineup_key_norm,
    possessions,
    minutes,
    games,
    raw_net_ppp,
    synergy_mean,
    synergy_p05,
    synergy_p95,
    decision_survive_score_raw,
    decision_survive_score_robust,
    defense_score,
    offense_upside_score,
    shot_diet_score,
    creation_score,
    composite_score,
    decision_def_ppp_pred,
    prior_pair_events,
    prior_trio_possessions,
    opp_fragile_flag,
    expected_points_per_40,
    defense_floor_pass,
    sample_flag,
    decision_label,
    Decision
  )

write_csv(coach_view, file.path(out_dir, "uconn_lineup_coach_view.csv"))
write_csv(
  coach_view %>%
    select(
      lineup_pretty,
      lineup_key_norm,
      decision_label,
      defense_floor_pass,
      defense_score,
      offense_upside_score,
      shot_diet_score,
      creation_score,
      composite_score,
      decision_def_ppp_pred,
      opp_fragile_flag,
      sample_flag,
      possessions,
      minutes,
      games
    ),
  file.path(out_dir, "uconn_lineup_decision_board.csv")
)

message("Done. Wrote to: ", normalizePath(out_dir))
message("Model cache: ", normalizePath(fit_path))
