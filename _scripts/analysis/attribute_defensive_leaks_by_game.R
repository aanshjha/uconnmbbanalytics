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

# Break down poor defensive games to lineup-level contributors and flag
# repeat offenders across the sample.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(tidyr)
})

source("_scripts/utils/project_paths.R")

# Helpers
resolve_path <- function(fname) {
  resolve_project_path(
    fname,
    extra_dirs = c(
      file.path("_outputs", "01_lineup_core"),
      file.path("_outputs", "02_defense_leaks"),
      file.path("_outputs", "03_players"),
      file.path("_outputs", "04_games_trends")
    )
  )
}

# Robust M:SS or MM:SS parser -> seconds
mmss_to_seconds <- function(x) {
  x <- as.character(x)
  x <- str_trim(x)
  x[x == ""] <- NA_character_
  
  out <- rep(NA_real_, length(x))
  
  ok <- !is.na(x) & str_detect(x, "^[0-9]+:[0-5][0-9]$")
  if (any(ok)) {
    parts <- str_split_fixed(x[ok], ":", 2)
    mm <- suppressWarnings(as.numeric(parts[, 1]))
    ss <- suppressWarnings(as.numeric(parts[, 2]))
    out[ok] <- mm * 60 + ss
  }
  
  # If something comes in as H:MM:SS (rare), handle it
  ok_h <- !is.na(x) & str_detect(x, "^[0-9]+:[0-5][0-9]:[0-5][0-9]$")
  if (any(ok_h)) {
    parts <- str_split_fixed(x[ok_h], ":", 3)
    hh <- suppressWarnings(as.numeric(parts[, 1]))
    mm <- suppressWarnings(as.numeric(parts[, 2]))
    ss <- suppressWarnings(as.numeric(parts[, 3]))
    out[ok_h] <- hh * 3600 + mm * 60 + ss
  }
  
  out
}

canon_lineup <- function(lineup_str) {
  if (is.na(lineup_str)) return(NA_character_)
  toks <- str_split(lineup_str, "\\|", simplify = TRUE)
  toks <- as.character(toks)
  toks <- str_trim(toks)
  toks <- toks[toks != ""]
  toks <- sort(toks)
  paste(toks, collapse = "|")
}

# Tunable parameters
MIN_POSS_PER_GAME_LINEUP <- 6
BAD_GAME_QUANTILE        <- 0.80
TOPK_PER_BAD_GAME        <- 3

# Load inputs
stints_path <- resolve_path("uconn_stints_from_pbp.csv")
games_path  <- resolve_path("uconn_games_meta.csv")

# Defensive posterior file preference
post_path <- NULL
if (file.exists(file.path("_outputs", "uconn_lineup_def_leaks_posterior.csv"))) {
  post_path <- file.path("_outputs", "uconn_lineup_def_leaks_posterior.csv")
} else if (file.exists(resolve_path("uconn_lineup_def_leaks_posterior.csv"))) {
  post_path <- resolve_path("uconn_lineup_def_leaks_posterior.csv")
} else if (file.exists(file.path("_outputs", "uconn_lineup_def_leaks_coach_table.csv"))) {
  post_path <- file.path("_outputs", "uconn_lineup_def_leaks_coach_table.csv")
} else if (file.exists(resolve_path("uconn_lineup_def_leaks_coach_table.csv"))) {
  post_path <- resolve_path("uconn_lineup_def_leaks_coach_table.csv")
} else {
  stop("Could not find defensive posterior/coach table output.")
}

out_dir <- if (dir.exists("_outputs")) "_outputs" else "."

# FORCE start/end as character to avoid hms auto-parsing surprises
stints <- read_csv(
  stints_path,
  show_col_types = FALSE,
  col_types = cols(
    start_time = col_character(),
    end_time   = col_character()
  )
)

games  <- read_csv(games_path, show_col_types = FALSE)
post   <- read_csv(post_path, show_col_types = FALSE)

# Compute pace from lineup_usage (most reliable)
usage <- read_csv(resolve_path("uconn_lineup_usage.csv"), show_col_types = FALSE)
PACE_UCONN <- sum(usage$possessions, na.rm = TRUE) / sum(usage$minutes, na.rm = TRUE)

if (!is.finite(PACE_UCONN) || PACE_UCONN <= 0) {
  stop("PACE_UCONN is invalid from uconn_lineup_usage.csv. Check minutes/possessions.")
}

message(
  "Computed PACE_UCONN (poss/min): ", round(PACE_UCONN, 3),
  " | implied poss per 40: ", round(PACE_UCONN * 40, 1)
)

# Clean stints to match model inclusion
stints2 <- stints %>%
  filter(!str_detect(game_file, "Exhibition")) %>%
  filter(lineup_size == 5) %>%
  mutate(
    poss_est = as.numeric(poss_est),
    points_against = as.numeric(points_against),
    points_for = as.numeric(points_for)
  ) %>%
  filter(!is.na(poss_est), poss_est > 0) %>%
  mutate(
    start_sec = mmss_to_seconds(start_time),
    end_sec   = mmss_to_seconds(end_time),
    dur_sec_raw = start_sec - end_sec,
    dur_sec = case_when(
      is.na(dur_sec_raw) ~ NA_real_,
      dur_sec_raw >= 0   ~ dur_sec_raw,
      TRUE               ~ abs(dur_sec_raw)
    ),
    dur_min = dur_sec / 60
  )

# sanity: if basically all dur_min are NA, stop
if (sum(!is.na(stints2$dur_min)) < 10) {
  bad <- stints2 %>% select(game_file, game_date, start_time, end_time) %>% distinct() %>% head(20)
  print(bad)
  stop("Could not parse start_time/end_time into minutes. Fix time formatting in stints.")
}

# Canonicalize lineups
stints2 <- stints2 %>%
  mutate(
    lineup_key = vapply(uconn_lineup, canon_lineup, character(1)),
    lineup_pretty = lineup_key
  )

# Normalize posterior schema
post2 <- post

if (!("lineup" %in% names(post2)) && ("lineup_key" %in% names(post2))) {
  post2 <- post2 %>% rename(lineup = lineup_key)
}
if (!("lineup" %in% names(post2))) {
  stop("Posterior file does not contain a 'lineup' column (or lineup_key).")
}

need_cols <- c("u_def_mean", "pr_leak")
missing_cols <- setdiff(need_cols, names(post2))
if (length(missing_cols) > 0) {
  stop("Posterior file missing columns: ", paste(missing_cols, collapse = ", "))
}

post2 <- post2 %>%
  mutate(lineup_key = vapply(lineup, canon_lineup, character(1)))

# Join stints to defensive posterior
joined <- stints2 %>%
  left_join(
    post2 %>% select(lineup_key, u_def_mean, pr_leak,
                     any_of(c("u_def_p05","u_def_p50","u_def_p95"))),
    by = "lineup_key"
  )

if (all(is.na(joined$u_def_mean))) {
  stop("No defensive posteriors joined. Check lineup canonicalization (lineup_key mismatch).")
}

# Aggregate per game x lineup
game_attribution <- joined %>%
  group_by(game_file, game_date, uconn_is_home, lineup_key, lineup_pretty) %>%
  summarise(
    poss_in_game = sum(poss_est, na.rm = TRUE),
    minutes_in_game = sum(dur_min, na.rm = TRUE),   # true minutes
    pts_allowed_in_game = sum(points_against, na.rm = TRUE),
    u_def_mean = first(u_def_mean),
    pr_leak    = first(pr_leak),
    u_def_p05  = first(if ("u_def_p05" %in% names(joined)) u_def_p05 else NA_real_),
    u_def_p50  = first(if ("u_def_p50" %in% names(joined)) u_def_p50 else NA_real_),
    u_def_p95  = first(if ("u_def_p95" %in% names(joined)) u_def_p95 else NA_real_),
    .groups = "drop"
  ) %>%
  filter(poss_in_game >= MIN_POSS_PER_GAME_LINEUP) %>%
  mutate(
    leak_pts_est = u_def_mean * poss_in_game,
    expected_pts_allowed_per_40 = u_def_mean * 40 * PACE_UCONN   # pace-scaled per-40
  )

# Game-level defense (used to define "bad games")
game_def <- joined %>%
  group_by(game_file, game_date) %>%
  summarise(
    poss = sum(poss_est, na.rm = TRUE),
    pts_allowed = sum(points_against, na.rm = TRUE),
    pts_allowed_per_poss = pts_allowed / poss,
    .groups = "drop"
  )

bad_cut <- quantile(game_def$pts_allowed_per_poss, probs = BAD_GAME_QUANTILE, na.rm = TRUE)

game_def <- game_def %>%
  mutate(bad_def_game = pts_allowed_per_poss >= bad_cut)

game_attribution <- game_attribution %>%
  left_join(game_def %>% select(game_file, pts_allowed_per_poss, bad_def_game), by = "game_file") %>%
  arrange(game_file, desc(leak_pts_est)) %>%
  mutate(
    leak_flag_strict = (u_def_mean > 0) & (pr_leak >= 0.80),
    dont_overreact = case_when(
      poss_in_game < 12 ~ "LOW SAMPLE",
      pr_leak < 0.65    ~ "LOW CONFIDENCE",
      TRUE              ~ ""
    )
  )

# Primary culprit per game
primary_culprit <- game_attribution %>%
  group_by(game_file) %>%
  slice_max(order_by = leak_pts_est, n = 1, with_ties = FALSE) %>%
  ungroup()

# Repeat offenders
repeat_strict <- game_attribution %>%
  filter(leak_flag_strict) %>%
  group_by(lineup_key, lineup_pretty) %>%
  summarise(
    games_flagged = n_distinct(game_file),
    total_poss_flagged = sum(poss_in_game, na.rm = TRUE),
    mean_u_def = mean(u_def_mean, na.rm = TRUE),
    mean_pr_leak = mean(pr_leak, na.rm = TRUE),
    definition = "STRICT: pr>=0.80 & u_def>0",
    .groups = "drop"
  ) %>%
  filter(games_flagged >= 3)

topk_in_bad <- game_attribution %>%
  filter(bad_def_game) %>%
  group_by(game_file) %>%
  slice_max(order_by = leak_pts_est, n = TOPK_PER_BAD_GAME, with_ties = TRUE) %>%
  ungroup()

repeat_topk <- topk_in_bad %>%
  group_by(lineup_key, lineup_pretty) %>%
  summarise(
    games_flagged = n_distinct(game_file),
    total_poss_flagged = sum(poss_in_game, na.rm = TRUE),
    mean_u_def = mean(u_def_mean, na.rm = TRUE),
    mean_pr_leak = mean(pr_leak, na.rm = TRUE),
    definition = paste0("TOP-", TOPK_PER_BAD_GAME, " leak contributors in BAD games"),
    .groups = "drop"
  ) %>%
  filter(games_flagged >= 3)

repeat_offenders <- bind_rows(repeat_strict, repeat_topk) %>%
  arrange(desc(games_flagged), desc(total_poss_flagged))

# Write outputs
write_csv(game_attribution, file.path(out_dir, "uconn_def_leak_lineups_by_game.csv"))
write_csv(primary_culprit,  file.path(out_dir, "uconn_def_leak_primary_culprit_by_game.csv"))
write_csv(repeat_offenders, file.path(out_dir, "uconn_def_leak_repeat_offenders.csv"))

message("Done. Wrote:")
message(" - ", file.path(out_dir, "uconn_def_leak_lineups_by_game.csv"))
message(" - ", file.path(out_dir, "uconn_def_leak_primary_culprit_by_game.csv"))
message(" - ", file.path(out_dir, "uconn_def_leak_repeat_offenders.csv"))
message("Bad-game cutoff (pts_allowed_per_poss) at quantile ", BAD_GAME_QUANTILE, ": ", round(bad_cut, 3))
