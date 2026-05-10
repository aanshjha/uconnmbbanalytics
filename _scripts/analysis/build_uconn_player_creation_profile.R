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
source("_scripts/utils/lineup_model_utils.R")

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
out_dir <- arg_value("out-dir", file.path("_outputs", "03_players"))
out_path <- file.path(out_dir, "uconn_player_creation_profile.csv")
min_event_rows <- suppressWarnings(as.integer(arg_value("min-event-rows", "12")))
if (!is.finite(min_event_rows) || min_event_rows < 1) min_event_rows <- 12L

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
    !is.na(UsagePlayer),
    UsagePlayer != "",
    UsagePlayer != "TEAM"
  ) %>%
  mutate(
    usage_player_key_norm = normalize_player_key(UsagePlayer),
    assist_player_key_norm = normalize_player_key(AssistPlayer),
    assisted_make = FGA == 1 & FGM == 1 & !is.na(AssistPlayer) & AssistPlayer != "",
    self_created_make = FGA == 1 & FGM == 1 & (is.na(AssistPlayer) | AssistPlayer == ""),
    live_ball_turnover = TOV == 1 & !is.na(StealPlayer) & StealPlayer != ""
  ) %>%
  filter(!is.na(usage_player_key_norm), nzchar(usage_player_key_norm))

if (nrow(uconn_off) == 0) {
  stop("No UConn offense rows found in manual-game CSVs.", call. = FALSE)
}

team_totals <- uconn_off %>%
  summarise(
    team_fga = sum(FGA, na.rm = TRUE),
    team_points = sum(PTS, na.rm = TRUE),
    team_assists_recorded = sum(FGA == 1 & FGM == 1 & !is.na(AssistPlayer) & AssistPlayer != "", na.rm = TRUE)
  )

assist_profile <- uconn_off %>%
  filter(
    FGA == 1,
    FGM == 1,
    !is.na(AssistPlayer),
    AssistPlayer != "",
    AssistPlayer != "TEAM",
    !is.na(assist_player_key_norm),
    nzchar(assist_player_key_norm)
  ) %>%
  group_by(player_key_norm = assist_player_key_norm) %>%
  summarise(
    player = sort(unique(AssistPlayer))[1],
    assists_recorded = n(),
    rim_assists = sum(shot_zone == "RIM", na.rm = TRUE),
    paint_assists = sum(paint_zone == "PAINT", na.rm = TRUE),
    corner_3_assists = sum(shot_zone == "CORNER_3", na.rm = TRUE),
    above_break_3_assists = sum(shot_zone == "ABOVE_BREAK_3", na.rm = TRUE),
    .groups = "drop"
  )

player_profile <- uconn_off %>%
  group_by(player_key_norm = usage_player_key_norm) %>%
  summarise(
    player = sort(unique(UsagePlayer))[1],
    games = n_distinct(game_file),
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
    sample_flag = if_else(tracked_event_rows >= min_event_rows, "ok", "small_sample"),
    .groups = "drop"
  ) %>%
  left_join(assist_profile, by = "player_key_norm", suffix = c("", "_assist")) %>%
  mutate(
    player = coalesce(player, player_assist),
    assists_recorded = coalesce(assists_recorded, 0L),
    rim_assists = coalesce(rim_assists, 0L),
    paint_assists = coalesce(paint_assists, 0L),
    corner_3_assists = coalesce(corner_3_assists, 0L),
    above_break_3_assists = coalesce(above_break_3_assists, 0L),
    ast_to_tov = pct(assists_recorded, turnovers),
    created_scoring_actions = self_created_makes + assists_recorded,
    uconn_fga_share = pct(fga, team_totals$team_fga[[1]]),
    uconn_points_share = pct(points, team_totals$team_points[[1]]),
    uconn_assist_share = pct(assists_recorded, team_totals$team_assists_recorded[[1]])
  ) %>%
  select(
    player,
    player_key_norm,
    games,
    tracked_event_rows,
    fga,
    fgm,
    fg_pct,
    fta,
    ftm,
    points,
    turnovers,
    live_ball_turnovers,
    assisted_makes,
    self_created_makes,
    assisted_make_rate,
    self_created_make_rate,
    rim_fga,
    paint_fga,
    corner_3_fga,
    above_break_3_fga,
    rim_share,
    paint_share,
    corner_3_share,
    above_break_3_share,
    assists_recorded,
    rim_assists,
    paint_assists,
    corner_3_assists,
    above_break_3_assists,
    ast_to_tov,
    created_scoring_actions,
    uconn_fga_share,
    uconn_points_share,
    uconn_assist_share,
    sample_flag
  ) %>%
  arrange(desc(points), desc(created_scoring_actions), desc(fga), player)

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
write_csv(player_profile, out_path)

message("Done. Wrote:")
message(" - ", out_path)
