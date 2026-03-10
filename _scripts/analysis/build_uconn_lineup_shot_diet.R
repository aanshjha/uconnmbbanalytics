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

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
})

source("_scripts/utils/manual_game_data.R")

args <- commandArgs(trailingOnly = TRUE)

arg_value <- function(key, default = NULL) {
  hit <- grep(paste0("^--", key, "="), args, value = TRUE)
  if (length(hit) == 0) return(default)
  sub(paste0("^--", key, "="), "", hit[[1]])
}

pct <- function(num, den) {
  num_num <- suppressWarnings(as.numeric(num))
  den_num <- suppressWarnings(as.numeric(den))
  out <- num_num / den_num
  out[is.na(den_num) | den_num <= 0] <- NA_real_
  out
}

manual_root <- manual_csv_root_dir()
out_dir <- arg_value("out-dir", file.path("_outputs", "01_lineup_core"))
out_path <- file.path(out_dir, "uconn_lineup_shot_diet.csv")
stable_out_path <- file.path(out_dir, "uconn_lineup_shot_diet_stable.csv")
min_event_rows <- suppressWarnings(as.integer(arg_value("min-event-rows", "15")))
if (!is.finite(min_event_rows) || min_event_rows < 1) min_event_rows <- 15L
stable_min_event_rows <- suppressWarnings(as.integer(arg_value("stable-min-event-rows", "50")))
if (!is.finite(stable_min_event_rows) || stable_min_event_rows < 1) stable_min_event_rows <- 50L
stable_min_games <- suppressWarnings(as.integer(arg_value("stable-min-games", "5")))
if (!is.finite(stable_min_games) || stable_min_games < 1) stable_min_games <- 5L

manual_games <- load_manual_games(manual_root)
message("Loading manual-game CSVs: ", n_distinct(manual_games$source_file))

manual_games <- manual_games %>%
  mutate(
    across(
      c(FGA, FGM, FTA, FTM, FGA3, FGM3, PTS, TOV),
      ~ suppressWarnings(as.numeric(.x))
    ),
    is_uconn_offense = as.logical(is_uconn_offense),
    game_date = as.Date(game_date)
  )

uconn_off <- manual_games %>%
  filter(
    is_uconn_offense %in% TRUE,
    !is.na(offense_lineup_key),
    offense_lineup_key != "",
    offense_lineup_key != "UNKNOWN_LINEUP"
  ) %>%
  mutate(
    lineup_pretty = offense_lineup_key,
    lineup_players_n = str_count(lineup_pretty, fixed(" | ")) + 1L,
    assisted_make = FGA == 1 & FGM == 1 & !is.na(AssistPlayer) & AssistPlayer != "",
    self_created_make = FGA == 1 & FGM == 1 & (is.na(AssistPlayer) | AssistPlayer == ""),
    live_ball_turnover = TOV == 1 & !is.na(StealPlayer) & StealPlayer != ""
  ) %>%
  filter(lineup_players_n == 5)

if (nrow(uconn_off) == 0) {
  stop("No five-player UConn offense lineup rows found in manual-game CSVs.", call. = FALSE)
}

team_totals <- uconn_off %>%
  summarise(
    team_fga = sum(FGA, na.rm = TRUE),
    team_points = sum(PTS, na.rm = TRUE)
  )

lineup_profile <- uconn_off %>%
  group_by(lineup_pretty) %>%
  summarise(
    games = n_distinct(game_file),
    opponents = n_distinct(opponent),
    tracked_event_rows = n(),
    fga = sum(FGA, na.rm = TRUE),
    fgm = sum(FGM, na.rm = TRUE),
    fg_pct = pct(fgm, fga),
    fta = sum(FTA, na.rm = TRUE),
    ftm = sum(FTM, na.rm = TRUE),
    points = sum(PTS, na.rm = TRUE),
    turnovers = sum(TOV, na.rm = TRUE),
    live_ball_turnovers = sum(live_ball_turnover %in% TRUE, na.rm = TRUE),
    assisted_makes = sum(assisted_make %in% TRUE, na.rm = TRUE),
    self_created_makes = sum(self_created_make %in% TRUE, na.rm = TRUE),
    assisted_make_rate = pct(assisted_makes, fgm),
    self_created_make_rate = pct(self_created_makes, fgm),
    rim_fga = sum(FGA == 1 & shot_zone == "RIM", na.rm = TRUE),
    paint_fga = sum(FGA == 1 & paint_zone == "PAINT", na.rm = TRUE),
    corner_3_fga = sum(FGA == 1 & shot_zone == "CORNER_3", na.rm = TRUE),
    above_break_3_fga = sum(FGA == 1 & shot_zone == "ABOVE_BREAK_3", na.rm = TRUE),
    rim_share = pct(rim_fga, fga),
    paint_share = pct(paint_fga, fga),
    corner_3_share = pct(corner_3_fga, fga),
    above_break_3_share = pct(above_break_3_fga, fga),
    fta_per_fga = pct(fta, fga),
    tov_per_fga = pct(turnovers, fga),
    sample_flag = if_else(tracked_event_rows >= min_event_rows, "ok", "small_sample"),
    .groups = "drop"
  ) %>%
  mutate(
    uconn_fga_share = pct(fga, team_totals$team_fga[[1]]),
    uconn_points_share = pct(points, team_totals$team_points[[1]])
  ) %>%
  arrange(desc(points), desc(fga), lineup_pretty)

stable_lineup_profile <- lineup_profile %>%
  filter(
    tracked_event_rows >= stable_min_event_rows,
    games >= stable_min_games
  )

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
write_csv(lineup_profile, out_path)
write_csv(stable_lineup_profile, stable_out_path)

message("Done. Wrote:")
message(" - ", out_path)
message(
  " - ", stable_out_path,
  " (stable = tracked_event_rows>=", stable_min_event_rows,
  ", games>=", stable_min_games, ")"
)
