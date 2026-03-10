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

# Defense-only lineup model for leak-risk outputs.

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

stints_path <- resolve_path("uconn_stints_from_pbp.csv")
games_path  <- resolve_path("uconn_games_meta.csv")
opp_path    <- resolve_path("opponent_controls.csv")
stan_path   <- resolve_path("uconn_lineup_gamelevel_defonly.stan")

out_dir    <- if (dir.exists("_outputs")) "_outputs" else "."
models_dir <- if (dir.exists("_models")) "_models" else "."

fit_path   <- file.path(models_dir, "uconn_lineup_gamelevel_defonly_fit.rds")

model_inputs <- load_common_lineup_model_inputs(
  stints_path = stints_path,
  games_path = games_path,
  opp_path = opp_path
)
stints <- model_inputs$stints
games <- model_inputs$games
games2 <- model_inputs$games_joined
tiny_poss <- model_inputs$tiny_poss %>%
  select(game_file, period, stint_index, start_time, end_time, poss_est, points_against)

if (nrow(tiny_poss) > 0) {
  message("Found poss_est < 1 (will clamp to 1). Rows:")
  print(tiny_poss)
}

missing <- games2 %>%
  filter(is.na(opp_adjO)) %>%
  select(game_date, opponent, game_file)

if (nrow(missing) > 0) {
  print(missing)
  stop("Some games did not match opponent_controls.csv (opp_adjO missing). Fix and rerun.")
}

# y_def is points allowed per possession.
stints2 <- stints %>%
  filter(
    lineup_size == 5,
    !is.na(poss_est), poss_est > 0,
    !is.na(points_against),
    !is.na(score_margin_start),
    !is.na(elapsed_game_sec)
  ) %>%
  mutate(
    game_id = as.integer(factor(game_file, levels = games2$game_file)),
    uconn_lineup_canon = canonicalize_lineup(uconn_lineup),
    y_def = points_against / poss_est,
    w = poss_est
  ) %>%
  mutate(
    score_margin_start_z = zscore_safe(score_margin_start),
    elapsed_game_sec_z = zscore_safe(elapsed_game_sec)
  )

eligible_by_game <- stints2 %>%
  count(game_file, name = "eligible_rows") %>%
  arrange(desc(eligible_rows))

message("Eligible defense rows by game (top 10):")
print(head(eligible_by_game, 10))

if (nrow(stints2) < 10) stop("Too few defense rows after filtering. Check poss_est/points_against.")

players <- sort(unique(unlist(str_split(stints2$uconn_lineup_canon, "\\|"))))
player_id <- setNames(seq_along(players), players)

lineups <- sort(unique(stints2$uconn_lineup_canon))
lineup_id <- setNames(seq_along(lineups), lineups)

uconn5_mat <- t(sapply(
  str_split(stints2$uconn_lineup_canon, "\\|"),
  function(x) player_id[x]
))

opp_adjO_z <- as.numeric(scale(games2$opp_adjO))
opp_adjO_z[!is.finite(opp_adjO_z)] <- 0

data_list <- list(
  N = nrow(stints2),
  P = length(players),
  L = length(lineups),
  uconn5 = uconn5_mat,
  lineup_id = as.integer(lineup_id[stints2$uconn_lineup_canon]),
  game_id = stints2$game_id,
  G = nrow(games2),
  y_def = stints2$y_def,
  w = stints2$w,
  opp_adjO_z = opp_adjO_z,
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

FORCE_REFIT <- tolower(Sys.getenv("FORCE_REFIT","false")) == "true"
fit <- NULL

if (!FORCE_REFIT && file.exists(fit_path)) {
  message("Cache hit: loading defense fit from ", fit_path)
  cached <- readRDS(fit_path)

  cached_sig <- attr(cached, "cache_signature")

  if (is.null(cached_sig) || !identical(cached_sig, current_cache_signature)) {
    message("Cached defense fit invalid (cache signature mismatch). Refitting.")
    fit <- NULL
  } else if (!fit_has_draws(cached, "alpha_def")) {
    message("Cached defense fit missing draws. Refitting.")
    fit <- NULL
  } else {
    fit <- cached
  }
}

if (is.null(fit)) {
  message("Cache miss (or FORCE_REFIT=TRUE): fitting defense model...")

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
    chains = 4,
    iter = 2000,
    warmup = 1000,
    seed = 20260121,
    init = init_fun,
    refresh = 50,
    control = list(adapt_delta = 0.995, max_treedepth = 15)
  )

  if (!fit_has_draws(fit, "alpha_def")) {
    stop("Defense fit produced no usable samples. Check Stan chain output above.")
  }

  attr(fit, "cache_signature") <- current_cache_signature
  saveRDS(fit, fit_path)
  message("Defense model cache saved: ", fit_path)
}

print(fit, pars = c(
  "intercept_def","tau_def","tau_u_def",
  "b_oppO","b_home","b_score_margin","b_elapsed_game",
  "sigma"
))

# Positive `u_def` means the lineup defended worse than baseline.
post <- rstan::extract(fit)

u_def_draws <- post$u_def
if (is.null(u_def_draws) || length(dim(u_def_draws)) < 2) stop("u_def missing/invalid in posterior.")

u_def_mean <- colMeans(u_def_draws)
u_def_q <- apply(u_def_draws, 2, quantile, probs = c(0.05, 0.5, 0.95))
pr_leak <- colMeans(u_def_draws > 0)

lineup_def_posterior <- tibble(
  lineup_id = seq_along(lineups),
  lineup = lineups,
  u_def_mean = u_def_mean,
  u_def_p05 = u_def_q[1,],
  u_def_p50 = u_def_q[2,],
  u_def_p95 = u_def_q[3,],
  pr_leak = pr_leak
) %>%
  mutate(lineup_pretty = lineup)

write_csv(lineup_def_posterior, file.path(out_dir, "uconn_lineup_def_leaks_posterior.csv"))

sampler_params <- tryCatch(rstan::get_sampler_params(fit, inc_warmup = FALSE), error = function(e) NULL)
if (!is.null(sampler_params)) {
  diag_df <- bind_rows(lapply(seq_along(sampler_params), function(i) {
    as_tibble(sampler_params[[i]]) %>% mutate(chain = i)
  }))
  write_csv(diag_df, file.path(out_dir, "uconn_lineup_def_leaks_model_diagnostics.csv"))
}

message("Done. Wrote to: ", normalizePath(out_dir))
message(" - uconn_lineup_def_leaks_posterior.csv")
message(" - uconn_lineup_def_leaks_model_diagnostics.csv (if available)")
message("Model cache: ", normalizePath(fit_path))
