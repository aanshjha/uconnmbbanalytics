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
  library(lubridate)
  library(tidyr)
  library(ggplot2)
})

source("_scripts/utils/manual_game_data.R")

args <- commandArgs(trailingOnly = TRUE)

arg_value <- function(key, default = NULL) {
  hit <- grep(paste0("^--", key, "="), args, value = TRUE)
  if (length(hit) == 0) return(default)
  sub(paste0("^--", key, "="), "", hit[[1]])
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) return(y)
  x
}

slugify <- function(x) {
  x %>%
    tolower() %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_replace_all("^_+|_+$", "")
}

pct <- function(num, den) {
  num_num <- suppressWarnings(as.numeric(num))
  den_num <- suppressWarnings(as.numeric(den))
  out <- num_num / den_num
  out[is.na(den_num) | den_num <= 0] <- NA_real_
  out
}

fmt_pct <- function(x, digits = 1) {
  ifelse(is.na(x), "NA", paste0(round(100 * x, digits), "%"))
}

pretty_zone_label <- function(x) {
  x %>%
    str_replace_all("_", " ") %>%
    str_replace_all("\\bABOVE BREAK 3\\b", "Above Break 3") %>%
    str_replace_all("\\bCORNER 3\\b", "Corner 3") %>%
    str_replace_all("\\bSHORT MID\\b", "Short Mid") %>%
    str_replace_all("\\bLONG MID\\b", "Long Mid") %>%
    str_replace_all("\\bRIM\\b", "Rim") %>%
    str_replace_all("\\bHEAVE\\b", "Heave") %>%
    str_replace_all("\\bLEFT\\b", "Left") %>%
    str_replace_all("\\bRIGHT\\b", "Right") %>%
    str_replace_all("\\bCENTER\\b", "Center")
}

pretty_shot_type_label <- function(x) {
  case_when(
    is.na(x) ~ "Other",
    x == "JumpShot" ~ "Jump Shot",
    x == "LayUpShot" ~ "Layup",
    x == "TipShot" ~ "Tip Shot",
    x == "DunkShot" ~ "Dunk",
    TRUE ~ x %>%
      str_replace_all("([a-z])([A-Z])", "\\1 \\2") %>%
      str_replace_all("Shot$", "") %>%
      str_trim()
  )
}

collapse_top_counts <- function(x, n = 3) {
  vals <- x[!is.na(x) & x != ""]
  if (length(vals) == 0) return(NA_character_)
  tab <- sort(table(vals), decreasing = TRUE)
  paste(head(paste0(names(tab), " (", as.integer(tab), ")"), n), collapse = " | ")
}

meeting_site_label <- function(site_type, uconn_is_home) {
  case_when(
    site_type == "neutral" ~ "neutral",
    uconn_is_home %in% TRUE ~ "home",
    TRUE ~ "away"
  )
}

manual_root <- manual_csv_root_dir()
out_dir <- arg_value("out-dir", file.path("_outputs", "07_opps", "manual_game_scouts"))
opponents_arg <- arg_value("opponents", "")
dates_arg <- arg_value("dates", "")
min_player_events <- suppressWarnings(as.integer(arg_value("min-player-events", "5")))
min_lineup_events <- suppressWarnings(as.integer(arg_value("min-lineup-events", "8")))

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

manual_games <- load_manual_games(manual_root)
message("Loading manual-game CSVs: ", n_distinct(manual_games$source_file))

manual_games <- manual_games %>%
  mutate(
    across(
      c(
        FGA, FGM, FTA, FTM, FGA3, FGM3, PTS, OREB, DREB, AST, TOV, STL, BLK,
        period_number, game_play_number, type_id, team_id, athlete_id_1, athlete_id_2,
        points_attempted, start_game_seconds_remaining, end_game_seconds_remaining,
        away_score, home_score, score_value, game_id, season_type, sequence_number
      ),
      ~ suppressWarnings(as.numeric(.x))
    ),
    across(c(uconn_is_home, is_uconn_offense, scoring_play, shooting_play, clutch_flag), ~ as.logical(.x)),
    game_date = as.Date(game_date),
    opponent = as.character(opponent),
    game_file = as.character(game_file),
    site_type = as.character(site_type),
    competition_bucket = normalize_competition_bucket(competition_bucket),
    meeting_site = meeting_site_label(site_type, uconn_is_home),
    opponent_slug = slugify(opponent),
    game_slug = paste0(as.character(game_date), "__", opponent_slug, "__", meeting_site),
    meeting_slug = paste0(as.character(game_date), "__", meeting_site),
    offense_lineup_key = coalesce(offense_lineup_key, "UNKNOWN_LINEUP"),
    defense_lineup_key = coalesce(defense_lineup_key, "UNKNOWN_LINEUP"),
    UsagePlayer = coalesce(UsagePlayer, "UNKNOWN_PLAYER"),
    AssistPlayer = na_if(AssistPlayer, "UNKNOWN_PLAYER"),
    ReboundPlayer = na_if(ReboundPlayer, "UNKNOWN_PLAYER"),
    StealPlayer = na_if(StealPlayer, "UNKNOWN_PLAYER"),
    BlockPlayer = na_if(BlockPlayer, "UNKNOWN_PLAYER"),
    shot_event = FGA == 1 | FTA == 1,
    made_field_goal = FGA == 1 & FGM == 1,
    assisted_make = made_field_goal & !is.na(AssistPlayer),
    self_created_make = made_field_goal & is.na(AssistPlayer),
    live_ball_turnover = TOV == 1 & !is.na(StealPlayer)
  )

unknown_bucket_files <- manual_games %>%
  filter(is.na(competition_bucket)) %>%
  distinct(source_file) %>%
  pull(source_file)

if (length(unknown_bucket_files) > 0) {
  stop(
    "Could not map these manual-game CSVs to conference/non-conference: ",
    paste(sort(unknown_bucket_files), collapse = ", "),
    call. = FALSE
  )
}

opponent_filters <- str_split(opponents_arg, ",")[[1]] %>% str_trim()
opponent_filters <- opponent_filters[opponent_filters != ""]
if (length(opponent_filters) > 0) {
  opponent_norm <- slugify(opponent_filters)
  manual_games <- manual_games %>% filter(opponent_slug %in% opponent_norm)
}

date_filters <- str_split(dates_arg, ",")[[1]] %>% str_trim()
date_filters <- date_filters[date_filters != ""]
if (length(date_filters) > 0) {
  keep_dates <- suppressWarnings(as.Date(date_filters))
  manual_games <- manual_games %>% filter(game_date %in% keep_dates)
}

if (nrow(manual_games) == 0) {
  stop("No games left after filters.", call. = FALSE)
}

manual_games <- manual_games %>%
  mutate(
    coordinate_x_num = suppressWarnings(as.numeric(coordinate_x)),
    coordinate_y_num = suppressWarnings(as.numeric(coordinate_y)),
    offense_is_home = case_when(
      is_uconn_offense %in% TRUE & uconn_is_home %in% TRUE ~ TRUE,
      is_uconn_offense %in% TRUE & uconn_is_home %in% FALSE ~ FALSE,
      is_uconn_offense %in% FALSE & uconn_is_home %in% TRUE ~ FALSE,
      is_uconn_offense %in% FALSE & uconn_is_home %in% FALSE ~ TRUE,
      TRUE ~ NA
    ),
    coord_oriented_x = if_else(offense_is_home %in% TRUE, coordinate_x_num, -coordinate_x_num),
    coord_oriented_y = if_else(offense_is_home %in% TRUE, coordinate_y_num, -coordinate_y_num),
    coord_from_rim = 41.75 - coord_oriented_x,
    coord_lateral = coord_oriented_y,
    valid_shot_map = FGA == 1 &
      is.finite(coord_from_rim) &
      is.finite(coord_lateral) &
      abs(coordinate_x_num) < 100 &
      abs(coordinate_y_num) < 100 &
      coord_from_rim >= -3 &
      coord_from_rim <= 47 &
      abs(coord_lateral) <= 25
  )

games_index <- manual_games %>%
  distinct(
    game_slug, meeting_slug, opponent, opponent_slug, game_date, meeting_site,
    game_file, uconn_is_home, site_type, competition_bucket
  ) %>%
  arrange(competition_bucket, game_date, opponent, meeting_site)

if (nrow(games_index) == 0) {
  stop("Could not build game index from manual-game CSVs.", call. = FALSE)
}

build_team_zone_profile <- function(df) {
  shot_rows <- df %>% filter(FGA == 1)
  if (nrow(shot_rows) == 0) {
    return(tibble(
      shot_zone = character(),
      shot_side = character(),
      paint_zone = character(),
      shot_zone_detail = character(),
      event_rows = integer(),
      fga = numeric(),
      fgm = numeric(),
      fg_pct = numeric(),
      fta = numeric(),
      ftm = numeric(),
      ft_pct = numeric(),
      points = numeric(),
      share_of_fga = numeric()
    ))
  }

  shot_rows %>%
    group_by(shot_zone, shot_side, paint_zone, shot_zone_detail) %>%
    summarise(
      event_rows = n(),
      fga = sum(FGA, na.rm = TRUE),
      fgm = sum(FGM, na.rm = TRUE),
      fg_pct = pct(fgm, fga),
      fta = sum(FTA, na.rm = TRUE),
      ftm = sum(FTM, na.rm = TRUE),
      ft_pct = pct(ftm, fta),
      points = sum(PTS, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      share_of_fga = pct(fga, sum(fga, na.rm = TRUE))
    ) %>%
    arrange(desc(fga), desc(fta), shot_zone_detail)
}

build_player_profile <- function(df, min_events) {
  df %>%
    group_by(UsagePlayer) %>%
    summarise(
      event_rows = n(),
      fga_events = sum(FGA == 1, na.rm = TRUE),
      fga = sum(FGA, na.rm = TRUE),
      fgm = sum(FGM, na.rm = TRUE),
      fg_pct = pct(fgm, fga),
      fta = sum(FTA, na.rm = TRUE),
      ftm = sum(FTM, na.rm = TRUE),
      ft_pct = pct(ftm, fta),
      points = sum(PTS, na.rm = TRUE),
      turnovers = sum(TOV, na.rm = TRUE),
      assisted_makes = sum(assisted_make %in% TRUE, na.rm = TRUE),
      self_created_makes = sum(self_created_make %in% TRUE, na.rm = TRUE),
      assisted_make_rate = pct(assisted_makes, fgm),
      rim_fga = sum(FGA == 1 & shot_zone == "RIM", na.rm = TRUE),
      paint_fga = sum(FGA == 1 & paint_zone == "PAINT", na.rm = TRUE),
      corner_3_fga = sum(FGA == 1 & shot_zone == "CORNER_3", na.rm = TRUE),
      above_break_3_fga = sum(FGA == 1 & shot_zone == "ABOVE_BREAK_3", na.rm = TRUE),
      sample_flag = if_else(event_rows >= min_events, "ok", "small_sample"),
      .groups = "drop"
    ) %>%
    arrange(desc(points), desc(fga), desc(turnovers), UsagePlayer)
}

build_player_zone_detail <- function(df) {
  df %>%
    filter(FGA == 1) %>%
    group_by(UsagePlayer, shot_zone, shot_side, shot_zone_detail) %>%
    summarise(
      event_rows = n(),
      fga = sum(FGA, na.rm = TRUE),
      fgm = sum(FGM, na.rm = TRUE),
      fg_pct = pct(fgm, fga),
      fta = sum(FTA, na.rm = TRUE),
      ftm = sum(FTM, na.rm = TRUE),
      points = sum(PTS, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(UsagePlayer, desc(fga), desc(fta), shot_zone_detail)
}

build_turnover_victims <- function(df, min_events) {
  df %>%
    filter(TOV == 1) %>%
    group_by(UsagePlayer) %>%
    summarise(
      turnovers = n(),
      live_ball_turnovers = sum(live_ball_turnover %in% TRUE, na.rm = TRUE),
      dead_ball_turnovers = turnovers - live_ball_turnovers,
      stealers_seen = collapse_top_counts(StealPlayer),
      turnover_examples = collapse_top_counts(text),
      sample_flag = if_else(turnovers >= min_events, "ok", "small_sample"),
      .groups = "drop"
    ) %>%
    arrange(desc(turnovers), desc(live_ball_turnovers), UsagePlayer)
}

build_turnover_creators <- function(df, min_events) {
  df %>%
    filter(STL == 1, !is.na(StealPlayer)) %>%
    group_by(StealPlayer) %>%
    summarise(
      steals_forced = n(),
      turnover_examples = collapse_top_counts(text),
      victims = collapse_top_counts(UsagePlayer),
      sample_flag = if_else(steals_forced >= min_events, "ok", "small_sample"),
      .groups = "drop"
    ) %>%
    arrange(desc(steals_forced), StealPlayer)
}

build_assisted_vs_self <- function(df, min_events) {
  df %>%
    filter(FGA == 1, FGM == 1) %>%
    group_by(UsagePlayer) %>%
    summarise(
      made_fg = n(),
      assisted_makes = sum(assisted_make %in% TRUE, na.rm = TRUE),
      self_created_makes = sum(self_created_make %in% TRUE, na.rm = TRUE),
      assisted_make_rate = pct(assisted_makes, made_fg),
      rim_makes = sum(shot_zone == "RIM", na.rm = TRUE),
      three_point_makes = sum(FGM3, na.rm = TRUE),
      sample_flag = if_else(made_fg >= min_events, "ok", "small_sample"),
      .groups = "drop"
    ) %>%
    arrange(desc(made_fg), desc(assisted_makes), UsagePlayer)
}

build_assist_connections <- function(df) {
  df %>%
    filter(FGA == 1, FGM == 1, !is.na(AssistPlayer)) %>%
    group_by(AssistPlayer, UsagePlayer) %>%
    summarise(
      assisted_makes = n(),
      rim_assists = sum(shot_zone == "RIM", na.rm = TRUE),
      three_point_assists = sum(FGM3, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(assisted_makes), AssistPlayer, UsagePlayer)
}

build_defensive_lineup_allowance <- function(df, min_events) {
  df %>%
    group_by(defense_lineup_key) %>%
    summarise(
      event_rows = n(),
      uconn_fga = sum(FGA, na.rm = TRUE),
      uconn_fgm = sum(FGM, na.rm = TRUE),
      uconn_fg_pct = pct(uconn_fgm, uconn_fga),
      uconn_3pa = sum(FGA3, na.rm = TRUE),
      uconn_3pm = sum(FGM3, na.rm = TRUE),
      uconn_fta = sum(FTA, na.rm = TRUE),
      uconn_ftm = sum(FTM, na.rm = TRUE),
      uconn_tov_forced = sum(TOV, na.rm = TRUE),
      opponent_steals = sum(STL, na.rm = TRUE),
      uconn_points = sum(PTS, na.rm = TRUE),
      uconn_rim_fga = sum(FGA == 1 & shot_zone == "RIM", na.rm = TRUE),
      uconn_paint_fga = sum(FGA == 1 & paint_zone == "PAINT", na.rm = TRUE),
      uconn_corner_3_fga = sum(FGA == 1 & shot_zone == "CORNER_3", na.rm = TRUE),
      uconn_above_break_3_fga = sum(FGA == 1 & shot_zone == "ABOVE_BREAK_3", na.rm = TRUE),
      sample_flag = if_else(event_rows >= min_events, "ok", "small_sample"),
      .groups = "drop"
    ) %>%
    arrange(desc(uconn_points), desc(uconn_paint_fga), desc(uconn_corner_3_fga), defense_lineup_key)
}

build_offensive_lineup_profile <- function(df, min_events) {
  df %>%
    group_by(offense_lineup_key) %>%
    summarise(
      event_rows = n(),
      opponent_fga = sum(FGA, na.rm = TRUE),
      opponent_fgm = sum(FGM, na.rm = TRUE),
      opponent_fg_pct = pct(opponent_fgm, opponent_fga),
      opponent_3pa = sum(FGA3, na.rm = TRUE),
      opponent_3pm = sum(FGM3, na.rm = TRUE),
      opponent_fta = sum(FTA, na.rm = TRUE),
      opponent_ftm = sum(FTM, na.rm = TRUE),
      opponent_tov = sum(TOV, na.rm = TRUE),
      opponent_points = sum(PTS, na.rm = TRUE),
      opponent_rim_fga = sum(FGA == 1 & shot_zone == "RIM", na.rm = TRUE),
      opponent_paint_fga = sum(FGA == 1 & paint_zone == "PAINT", na.rm = TRUE),
      opponent_corner_3_fga = sum(FGA == 1 & shot_zone == "CORNER_3", na.rm = TRUE),
      opponent_above_break_3_fga = sum(FGA == 1 & shot_zone == "ABOVE_BREAK_3", na.rm = TRUE),
      sample_flag = if_else(event_rows >= min_events, "ok", "small_sample"),
      .groups = "drop"
    ) %>%
    arrange(desc(opponent_points), desc(opponent_fga), offense_lineup_key)
}

build_lineup_matchup_matrix <- function(df, min_events) {
  df %>%
    mutate(
      team_context = if_else(is_uconn_offense %in% TRUE, "UConn offense", "Opponent offense")
    ) %>%
    group_by(team_context, offense_lineup_key, defense_lineup_key) %>%
    summarise(
      event_rows = n(),
      fga = sum(FGA, na.rm = TRUE),
      fgm = sum(FGM, na.rm = TRUE),
      fg_pct = pct(fgm, fga),
      fta = sum(FTA, na.rm = TRUE),
      ftm = sum(FTM, na.rm = TRUE),
      three_pa = sum(FGA3, na.rm = TRUE),
      three_pm = sum(FGM3, na.rm = TRUE),
      points = sum(PTS, na.rm = TRUE),
      turnovers = sum(TOV, na.rm = TRUE),
      live_ball_turnovers = sum(live_ball_turnover %in% TRUE, na.rm = TRUE),
      assisted_makes = sum(assisted_make %in% TRUE, na.rm = TRUE),
      self_created_makes = sum(self_created_make %in% TRUE, na.rm = TRUE),
      assisted_make_rate = pct(assisted_makes, fgm),
      rim_fga = sum(FGA == 1 & shot_zone == "RIM", na.rm = TRUE),
      paint_fga = sum(FGA == 1 & paint_zone == "PAINT", na.rm = TRUE),
      corner_3_fga = sum(FGA == 1 & shot_zone == "CORNER_3", na.rm = TRUE),
      above_break_3_fga = sum(FGA == 1 & shot_zone == "ABOVE_BREAK_3", na.rm = TRUE),
      sample_flag = if_else(event_rows >= min_events, "ok", "small_sample"),
      .groups = "drop"
    ) %>%
    arrange(team_context, desc(points), desc(fga), offense_lineup_key, defense_lineup_key)
}

build_clutch_log <- function(df) {
  df %>%
    filter(clutch_flag %in% TRUE) %>%
    mutate(team_context = if_else(is_uconn_offense %in% TRUE, "UConn offense", "Opponent offense")) %>%
    select(
      game_file, game_date, opponent, meeting_site, period_number, clock_display_value,
      start_game_seconds_remaining, end_game_seconds_remaining, team_context,
      UsagePlayer, AssistPlayer, StealPlayer, BlockPlayer,
      FGA, FGM, FTA, FTM, FGA3, FGM3, TOV, PTS,
      margin_before, margin_after, offense_lineup_key, defense_lineup_key,
      shot_zone_detail, text
    ) %>%
    arrange(start_game_seconds_remaining)
}

build_meeting_summary <- function(opp_off, opp_def, meeting_meta) {
  opp_fga <- sum(opp_off$FGA, na.rm = TRUE)
  opp_fgm <- sum(opp_off$FGM, na.rm = TRUE)
  opp_fta <- sum(opp_off$FTA, na.rm = TRUE)
  opp_ftm <- sum(opp_off$FTM, na.rm = TRUE)
  opp_fgm_assisted <- sum(opp_off$assisted_make %in% TRUE, na.rm = TRUE)
  opp_fgm_self <- sum(opp_off$self_created_make %in% TRUE, na.rm = TRUE)

  tibble(
    game_file = meeting_meta$game_file[[1]],
    game_date = meeting_meta$game_date[[1]],
    opponent = meeting_meta$opponent[[1]],
    opponent_slug = meeting_meta$opponent_slug[[1]],
    competition_bucket = meeting_meta$competition_bucket[[1]],
    meeting_site = meeting_meta$meeting_site[[1]],
    site_type_raw = meeting_meta$site_type[[1]],
    uconn_is_home = meeting_meta$uconn_is_home[[1]],
    opponent_off_event_rows = nrow(opp_off),
    opponent_def_event_rows = nrow(opp_def),
    opponent_fga = opp_fga,
    opponent_fgm = opp_fgm,
    opponent_fg_pct = pct(opp_fgm, opp_fga),
    opponent_3pa = sum(opp_off$FGA3, na.rm = TRUE),
    opponent_3pm = sum(opp_off$FGM3, na.rm = TRUE),
    opponent_3p_pct = pct(sum(opp_off$FGM3, na.rm = TRUE), sum(opp_off$FGA3, na.rm = TRUE)),
    opponent_fta = opp_fta,
    opponent_ftm = opp_ftm,
    opponent_ft_pct = pct(opp_ftm, opp_fta),
    opponent_points = sum(opp_off$PTS, na.rm = TRUE),
    opponent_turnovers = sum(opp_off$TOV, na.rm = TRUE),
    opponent_live_ball_turnovers = sum(opp_off$live_ball_turnover %in% TRUE, na.rm = TRUE),
    opponent_assisted_makes = opp_fgm_assisted,
    opponent_self_created_makes = opp_fgm_self,
    opponent_assisted_make_rate = pct(opp_fgm_assisted, opp_fgm),
    opponent_rim_fga = sum(opp_off$FGA == 1 & opp_off$shot_zone == "RIM", na.rm = TRUE),
    opponent_paint_fga = sum(opp_off$FGA == 1 & opp_off$paint_zone == "PAINT", na.rm = TRUE),
    opponent_corner_3_fga = sum(opp_off$FGA == 1 & opp_off$shot_zone == "CORNER_3", na.rm = TRUE),
    opponent_above_break_3_fga = sum(opp_off$FGA == 1 & opp_off$shot_zone == "ABOVE_BREAK_3", na.rm = TRUE),
    opponent_rim_share = pct(sum(opp_off$FGA == 1 & opp_off$shot_zone == "RIM", na.rm = TRUE), opp_fga),
    opponent_paint_share = pct(sum(opp_off$FGA == 1 & opp_off$paint_zone == "PAINT", na.rm = TRUE), opp_fga),
    uconn_fga_vs_opp_def = sum(opp_def$FGA, na.rm = TRUE),
    uconn_fgm_vs_opp_def = sum(opp_def$FGM, na.rm = TRUE),
    uconn_fg_pct_vs_opp_def = pct(sum(opp_def$FGM, na.rm = TRUE), sum(opp_def$FGA, na.rm = TRUE)),
    uconn_3pa_vs_opp_def = sum(opp_def$FGA3, na.rm = TRUE),
    uconn_fta_vs_opp_def = sum(opp_def$FTA, na.rm = TRUE),
    uconn_points_vs_opp_def = sum(opp_def$PTS, na.rm = TRUE),
    opponent_steals_forced = sum(opp_def$STL, na.rm = TRUE),
    opponent_def_rim_allowed = sum(opp_def$FGA == 1 & opp_def$shot_zone == "RIM", na.rm = TRUE),
    opponent_def_paint_allowed = sum(opp_def$FGA == 1 & opp_def$paint_zone == "PAINT", na.rm = TRUE),
    opponent_def_corner_3_allowed = sum(opp_def$FGA == 1 & opp_def$shot_zone == "CORNER_3", na.rm = TRUE),
    clutch_event_rows = sum((bind_rows(opp_off, opp_def))$clutch_flag %in% TRUE, na.rm = TRUE)
  )
}

circle_points <- function(cx, cy, r, start = 0, end = 2 * pi, n = 181) {
  theta <- seq(start, end, length.out = n)
  tibble(
    x = cx + r * cos(theta),
    y = cy + r * sin(theta)
  )
}

half_court_paths <- function() {
  arc_radius <- 22.15
  arc_angle <- asin(21.75 / arc_radius)
  arc_x <- sqrt(arc_radius^2 - 21.75^2)

  list(
    boundary = tibble(
      x = c(-4, 47, 47, -4, -4),
      y = c(-25, -25, 25, 25, -25)
    ),
    paint = tibble(
      x = c(-4, 13.75, 13.75, -4, -4),
      y = c(-6, -6, 6, 6, -6)
    ),
    backboard = tibble(
      x = c(-1.25, -1.25),
      y = c(-3, 3)
    ),
    rim = circle_points(0, 0, 0.75),
    restricted = circle_points(0, 0, 4, -pi / 2, pi / 2),
    ft_circle = circle_points(13.75, 0, 6),
    three_top = circle_points(0, 0, arc_radius, -arc_angle, arc_angle),
    three_left = tibble(
      x = c(-4, arc_x),
      y = c(21.75, 21.75)
    ),
    three_right = tibble(
      x = c(-4, arc_x),
      y = c(-21.75, -21.75)
    ),
    center_mark = tibble(
      x = c(47, 47),
      y = c(-3, 3)
    )
  )
}

plot_zone_profile <- function(profile_df, out_path, title, subtitle) {
  if (nrow(profile_df) == 0) return(FALSE)

  plot_df <- profile_df %>%
    filter((fga > 0 | fta > 0), !is.na(shot_zone_detail)) %>%
    mutate(
      zone_label = pretty_zone_label(shot_zone_detail)
    ) %>%
    group_by(shot_zone_detail, zone_label, shot_zone) %>%
    summarise(
      fga = sum(fga, na.rm = TRUE),
      fgm = sum(fgm, na.rm = TRUE),
      fta = sum(fta, na.rm = TRUE),
      ftm = sum(ftm, na.rm = TRUE),
      points = sum(points, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      fg_pct = pct(fgm, fga),
      share_of_fga = pct(fga, sum(fga, na.rm = TRUE)),
      value_label = paste0(
        fga,
        " FGA | ",
        round(100 * share_of_fga, 1),
        "% share | FG ",
        ifelse(is.na(fg_pct), "NA", paste0(round(100 * fg_pct, 1), "%"))
      )
    ) %>%
    slice_max(order_by = share_of_fga, n = 12, with_ties = FALSE) %>%
    arrange(share_of_fga) %>%
    mutate(zone_label = factor(zone_label, levels = unique(zone_label)))

  if (nrow(plot_df) == 0) return(FALSE)

  xmax <- max(plot_df$share_of_fga, na.rm = TRUE)
  xmax <- ifelse(is.finite(xmax), xmax, 0.3)
  xmax <- max(0.3, xmax + 0.09)

  p <- ggplot(plot_df, aes(x = share_of_fga, y = zone_label, fill = shot_zone)) +
    geom_col(width = 0.68, alpha = 0.94) +
    geom_text(
      aes(label = value_label),
      hjust = 0,
      nudge_x = 0.008,
      size = 3.05,
      family = "Helvetica"
    ) +
    scale_x_continuous(
      labels = function(x) paste0(round(100 * x), "%"),
      limits = c(0, xmax),
      expand = expansion(mult = c(0, 0.02))
    ) +
    scale_fill_manual(
      values = c(
        "RIM" = "#0c2340",
        "SHORT_MID" = "#c67d2f",
        "LONG_MID" = "#9c4f2f",
        "CORNER_3" = "#1f7a8c",
        "ABOVE_BREAK_3" = "#4d9f70",
        "FT" = "#7f8c8d",
        "HEAVE" = "#8e5a9f",
        "UNKNOWN" = "#b0b0b0"
      ),
      drop = FALSE
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Share of tracked FGA",
      y = NULL,
      fill = "Zone"
    ) +
    coord_cartesian(clip = "off") +
    theme_minimal(base_size = 12, base_family = "Helvetica") +
    theme(
      plot.background = element_rect(fill = "#fbf8f1", color = NA),
      panel.background = element_rect(fill = "#fbf8f1", color = NA),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "#d8d2c4", linewidth = 0.35),
      axis.text.y = element_text(color = "#3a342d", size = 11),
      axis.text.x = element_text(color = "#5e574b"),
      axis.title.x = element_text(color = "#3a342d", margin = margin(t = 10)),
      plot.title = element_text(face = "bold", size = 18, color = "#1e1b18"),
      plot.subtitle = element_text(size = 12.5, color = "#4f4a43", margin = margin(b = 10)),
      legend.position = "bottom"
    )

  ggsave(out_path, p, width = 10.5, height = 6.25, dpi = 300)
  TRUE
}

plot_shot_map <- function(df, out_path, title, subtitle) {
  shot_type_shapes <- c(
    "Jump Shot" = 21,
    "Layup" = 22,
    "Tip Shot" = 23,
    "Dunk" = 24,
    "Other" = 25
  )

  shots <- df %>%
    filter(valid_shot_map %in% TRUE) %>%
    mutate(
      shot_result = if_else(FGM == 1, "Made", "Missed"),
      shot_type_label = pretty_shot_type_label(type_text),
      shot_type_label = factor(shot_type_label, levels = names(shot_type_shapes))
    )

  if (nrow(shots) == 0) return(FALSE)

  court <- half_court_paths()

  summary_line <- paste0(
    sum(shots$FGA, na.rm = TRUE), " FGA | ",
    round(100 * pct(sum(shots$FGM, na.rm = TRUE), sum(shots$FGA, na.rm = TRUE)), 1), "% FG | ",
    sum(shots$FGA3, na.rm = TRUE), " 3PA"
  )

  p <- ggplot(shots, aes(x = coord_from_rim, y = coord_lateral)) +
    annotate("rect", xmin = -4, xmax = 47, ymin = -25, ymax = 25, fill = "#f6f0dd", color = NA) +
    annotate("rect", xmin = -4, xmax = 13.75, ymin = -6, ymax = 6, fill = "#ead8b0", alpha = 0.65, color = NA) +
    geom_path(data = court$boundary, aes(x = x, y = y), inherit.aes = FALSE, linewidth = 0.7, color = "#5c4d3d") +
    geom_path(data = court$paint, aes(x = x, y = y), inherit.aes = FALSE, linewidth = 0.7, color = "#5c4d3d") +
    geom_path(data = court$backboard, aes(x = x, y = y), inherit.aes = FALSE, linewidth = 1.1, color = "#5c4d3d") +
    geom_path(data = court$rim, aes(x = x, y = y), inherit.aes = FALSE, linewidth = 0.9, color = "#c26a2d") +
    geom_path(data = court$restricted, aes(x = x, y = y), inherit.aes = FALSE, linewidth = 0.7, color = "#5c4d3d") +
    geom_path(data = court$ft_circle, aes(x = x, y = y), inherit.aes = FALSE, linewidth = 0.7, color = "#5c4d3d") +
    geom_path(data = court$three_top, aes(x = x, y = y), inherit.aes = FALSE, linewidth = 0.8, color = "#5c4d3d") +
    geom_path(data = court$three_left, aes(x = x, y = y), inherit.aes = FALSE, linewidth = 0.8, color = "#5c4d3d") +
    geom_path(data = court$three_right, aes(x = x, y = y), inherit.aes = FALSE, linewidth = 0.8, color = "#5c4d3d") +
    geom_path(data = court$center_mark, aes(x = x, y = y), inherit.aes = FALSE, linewidth = 0.8, color = "#5c4d3d") +
    geom_point(
      aes(fill = shot_result, color = shot_result, shape = shot_type_label),
      size = 3.1,
      stroke = 0.55,
      alpha = 0.92
    ) +
    scale_shape_manual(values = shot_type_shapes, name = "Shot Type", drop = TRUE) +
    scale_fill_manual(
      name = "Result",
      values = c("Made" = "#0c2340", "Missed" = "#f8f3eb")
    ) +
    scale_color_manual(
      name = "Result",
      values = c("Made" = "#0c2340", "Missed" = "#b23a48")
    ) +
    guides(
      color = "none",
      fill = guide_legend(
        order = 1,
        override.aes = list(shape = 21, size = 4, stroke = 0.55)
      ),
      shape = guide_legend(
        order = 2,
        override.aes = list(fill = "#f8f3eb", color = "#3a342d", size = 4, stroke = 0.6)
      )
    ) +
    coord_fixed(xlim = c(-4.5, 63), ylim = c(-25.5, 25.5), clip = "off") +
    labs(
      title = title,
      subtitle = paste(subtitle, summary_line, sep = " | "),
      fill = NULL,
      color = NULL,
      shape = NULL
    ) +
    theme_void(base_size = 12, base_family = "Helvetica") +
    theme(
      plot.background = element_rect(fill = "#fbf8f1", color = NA),
      panel.background = element_rect(fill = "#fbf8f1", color = NA),
      plot.title = element_text(face = "bold", size = 18, hjust = 0, color = "#1e1b18"),
      plot.subtitle = element_text(size = 12.5, hjust = 0, color = "#4f4a43", margin = margin(b = 10)),
      legend.position = c(0.985, 0.05),
      legend.justification = c(1, 0),
      legend.box = "vertical",
      legend.background = element_rect(fill = "#fffaf0", color = "#d6cab8", linewidth = 0.35),
      legend.title = element_text(face = "bold", size = 11, color = "#2f2a24"),
      legend.text = element_text(size = 10, color = "#2f2a24"),
      plot.margin = margin(10, 18, 10, 10)
    )

  ggsave(out_path, p, width = 10.5, height = 5.8, dpi = 300)
  TRUE
}

render_summary_md <- function(summary_tbl,
                              shot_profile,
                              defense_allowed_profile,
                              player_profile,
                              turnover_victims,
                              turnover_creators,
                              defensive_lineups,
                              offensive_lineups,
                              meeting_dir) {
  top_shooter <- if (nrow(player_profile) > 0) player_profile[1, ] else NULL
  top_victim <- if (nrow(turnover_victims) > 0) turnover_victims[1, ] else NULL
  top_creator <- if (nrow(turnover_creators) > 0) turnover_creators[1, ] else NULL
  top_off_lineup <- if (nrow(offensive_lineups) > 0) offensive_lineups[1, ] else NULL
  top_def_lineup <- defensive_lineups %>%
    arrange(desc(uconn_paint_fga + uconn_corner_3_fga), desc(uconn_points)) %>%
    slice_head(n = 1)
  top_off_zones <- if (nrow(shot_profile) > 0) {
    shot_profile %>% slice_head(n = 3) %>%
      transmute(line = paste0(pretty_zone_label(shot_zone_detail), " | ", fga, " FGA | ", fmt_pct(fg_pct)))
  } else {
    tibble(line = character())
  }
  top_def_zones <- if (nrow(defense_allowed_profile) > 0) {
    defense_allowed_profile %>% slice_head(n = 3) %>%
      transmute(line = paste0(pretty_zone_label(shot_zone_detail), " | ", fga, " UConn FGA | ", fmt_pct(fg_pct)))
  } else {
    tibble(line = character())
  }

  lines <- c(
    paste0("# Opponent Scout | ", summary_tbl$opponent[[1]], " | ", summary_tbl$game_date[[1]], " | ", summary_tbl$meeting_site[[1]]),
    "",
    "Source note: built from manual-game CSVs that track shots, turnovers, and attached lineup/context fields. This is not full possession-level play-by-play.",
    "",
    "## Quick Read",
    paste0("- Opponent offense: ", summary_tbl$opponent_fga[[1]], " FGA, ", fmt_pct(summary_tbl$opponent_fg_pct[[1]]), " FG, ", summary_tbl$opponent_3pa[[1]], " 3PA, ", summary_tbl$opponent_turnovers[[1]], " turnovers."),
    paste0("- Opponent shot diet: rim share ", fmt_pct(summary_tbl$opponent_rim_share[[1]]), ", paint share ", fmt_pct(summary_tbl$opponent_paint_share[[1]]), ", assisted make rate ", fmt_pct(summary_tbl$opponent_assisted_make_rate[[1]]), "."),
    paste0("- Opponent defense vs UConn: ", summary_tbl$uconn_fga_vs_opp_def[[1]], " UConn FGA allowed, ", summary_tbl$uconn_points_vs_opp_def[[1]], " points allowed on tracked events, ", summary_tbl$opponent_steals_forced[[1]], " steals forced."),
    paste0("- Clutch event rows: ", summary_tbl$clutch_event_rows[[1]], "."),
    "",
    "## Top Flags"
  )

  if (!is.null(top_shooter)) {
    lines <- c(
      lines,
      paste0("- Top opponent usage scorer: ", top_shooter$UsagePlayer[[1]], " | ", top_shooter$points[[1]], " points | ", top_shooter$fga[[1]], " FGA.")
    )
  }
  if (!is.null(top_victim)) {
    lines <- c(
      lines,
      paste0("- Most turnover-prone opponent handler: ", top_victim$UsagePlayer[[1]], " | ", top_victim$turnovers[[1]], " turnovers.")
    )
  }
  if (!is.null(top_creator)) {
    lines <- c(
      lines,
      paste0("- Opponent defensive creator: ", top_creator$StealPlayer[[1]], " | ", top_creator$steals_forced[[1]], " steals forced.")
    )
  }
  if (!is.null(top_off_lineup) && nrow(top_off_lineup) > 0) {
    lines <- c(
      lines,
      paste0("- Highest-volume opponent offensive lineup: ", top_off_lineup$offense_lineup_key[[1]], " | ", top_off_lineup$opponent_fga[[1]], " FGA | ", top_off_lineup$opponent_points[[1]], " points.")
    )
  }
  if (nrow(top_def_lineup) > 0) {
    lines <- c(
      lines,
      paste0("- Opponent defensive lineup most exposed in tracked shot quality: ", top_def_lineup$defense_lineup_key[[1]], " | paint FGA allowed ", top_def_lineup$uconn_paint_fga[[1]], " | corner 3 FGA allowed ", top_def_lineup$uconn_corner_3_fga[[1]], ".")
    )
  }

  lines <- c(
    lines,
    "",
    "## Zone Flags",
    "- Opponent offense top zones:"
  )

  if (nrow(top_off_zones) == 0) {
    lines <- c(lines, "NA")
  } else {
    lines <- c(lines, paste0("  - ", top_off_zones$line))
  }

  lines <- c(
    lines,
    "- Opponent defense allowed top zones:"
  )

  if (nrow(top_def_zones) == 0) {
    lines <- c(lines, "NA")
  } else {
    lines <- c(lines, paste0("  - ", top_def_zones$line))
  }

  lines <- c(
    lines,
    "",
    "## Files",
    "- `summary/meeting_summary.csv`",
    "- `summary/report_summary.md`",
    "- `shots/opponent_shot_map.png`",
    "- `shots/uconn_shot_map.png`",
    "- `shots/opponent_zone_profile.png`",
    "- `shots/uconn_zone_profile.png`",
    "- `shots/opponent_shot_profile.csv`",
    "- `shots/uconn_shot_profile.csv`",
    "- `turnovers/creators.csv`",
    "- `turnovers/victims.csv`",
    "- `creation/self_created_vs_assisted.csv`",
    "- `creation/assist_connections.csv`",
    "- `lineups/defense_shot_allowance.csv`",
    "- `lineups/opponent_offense_profile.csv`",
    "- `lineups/matchup_matrix.csv`",
    "- `players/profile.csv`",
    "- `players/zone_detail.csv`",
    "- `clutch/event_log.csv`"
  )

  writeLines(lines, con = file.path(meeting_dir, "summary", "report_summary.md"))
}

build_player_meeting_comparison <- function(df, min_events) {
  df %>%
    filter(is_uconn_offense %in% FALSE) %>%
    group_by(game_date, meeting_site, game_file, UsagePlayer) %>%
    summarise(
      event_rows = n(),
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
      rim_fga = sum(FGA == 1 & shot_zone == "RIM", na.rm = TRUE),
      paint_fga = sum(FGA == 1 & paint_zone == "PAINT", na.rm = TRUE),
      corner_3_fga = sum(FGA == 1 & shot_zone == "CORNER_3", na.rm = TRUE),
      above_break_3_fga = sum(FGA == 1 & shot_zone == "ABOVE_BREAK_3", na.rm = TRUE),
      sample_flag = if_else(event_rows >= min_events, "ok", "small_sample"),
      .groups = "drop"
    ) %>%
    arrange(game_date, desc(points), desc(fga), UsagePlayer)
}

meeting_summaries <- list()

for (i in seq_len(nrow(games_index))) {
  meeting <- games_index[i, ]
  meeting_df <- manual_games %>% filter(game_slug == meeting$game_slug[[1]])
  if (nrow(meeting_df) == 0) next

  bucket_dir <- file.path(out_dir, meeting$competition_bucket[[1]])
  opponent_dir <- file.path(bucket_dir, meeting$opponent_slug[[1]])
  meeting_dir <- file.path(opponent_dir, meeting$meeting_slug[[1]])
  dir.create(meeting_dir, recursive = TRUE, showWarnings = FALSE)

  summary_dir <- file.path(meeting_dir, "summary")
  shots_dir <- file.path(meeting_dir, "shots")
  turnovers_dir <- file.path(meeting_dir, "turnovers")
  creation_dir <- file.path(meeting_dir, "creation")
  lineups_dir <- file.path(meeting_dir, "lineups")
  players_dir <- file.path(meeting_dir, "players")
  clutch_dir <- file.path(meeting_dir, "clutch")

  unlink(c(summary_dir, shots_dir, turnovers_dir, creation_dir, lineups_dir, players_dir, clutch_dir), recursive = TRUE)
  dir.create(summary_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(shots_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(turnovers_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(creation_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(lineups_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(players_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(clutch_dir, recursive = TRUE, showWarnings = FALSE)

  opp_off <- meeting_df %>% filter(is_uconn_offense %in% FALSE)
  opp_def <- meeting_df %>% filter(is_uconn_offense %in% TRUE)

  meeting_summary <- build_meeting_summary(opp_off, opp_def, meeting)
  shot_profile <- build_team_zone_profile(opp_off)
  defense_allowed_profile <- build_team_zone_profile(opp_def)
  player_profile <- build_player_profile(opp_off, min_player_events)
  player_zone_detail <- build_player_zone_detail(opp_off)
  turnover_victims <- build_turnover_victims(opp_off, min_player_events)
  turnover_creators <- build_turnover_creators(opp_def, min_player_events)
  assisted_vs_self <- build_assisted_vs_self(opp_off, min_player_events)
  assist_connections <- build_assist_connections(opp_off)
  defensive_lineups <- build_defensive_lineup_allowance(opp_def, min_lineup_events)
  offensive_lineups <- build_offensive_lineup_profile(opp_off, min_lineup_events)
  matchup_matrix <- build_lineup_matchup_matrix(meeting_df, min_lineup_events)
  clutch_log <- build_clutch_log(meeting_df)

  write_csv(meeting_summary, file.path(summary_dir, "meeting_summary.csv"))
  write_csv(shot_profile, file.path(shots_dir, "opponent_shot_profile.csv"))
  write_csv(defense_allowed_profile, file.path(shots_dir, "uconn_shot_profile.csv"))
  write_csv(player_profile, file.path(players_dir, "profile.csv"))
  write_csv(player_zone_detail, file.path(players_dir, "zone_detail.csv"))
  write_csv(turnover_victims, file.path(turnovers_dir, "victims.csv"))
  write_csv(turnover_creators, file.path(turnovers_dir, "creators.csv"))
  write_csv(assisted_vs_self, file.path(creation_dir, "self_created_vs_assisted.csv"))
  write_csv(assist_connections, file.path(creation_dir, "assist_connections.csv"))
  write_csv(defensive_lineups, file.path(lineups_dir, "defense_shot_allowance.csv"))
  write_csv(offensive_lineups, file.path(lineups_dir, "opponent_offense_profile.csv"))
  write_csv(matchup_matrix, file.path(lineups_dir, "matchup_matrix.csv"))
  write_csv(clutch_log, file.path(clutch_dir, "event_log.csv"))

  unlink(file.path(meeting_dir, c(
    "meeting_summary.csv",
    "opponent_shot_profile.csv",
    "opponent_defense_allowed_shot_profile.csv",
    "opponent_player_profile.csv",
    "opponent_player_zone_detail.csv",
    "opponent_turnover_victims.csv",
    "opponent_turnover_creators.csv",
    "opponent_assisted_vs_self_created.csv",
    "opponent_assist_connections.csv",
    "opponent_defensive_lineup_shot_allowance.csv",
    "opponent_offensive_lineup_profile.csv",
    "clutch_event_log.csv",
    "report_summary.md",
    "opponent_offense_shot_map.png",
    "opponent_defense_shot_map.png",
    "opponent_offense_zone_profile.png",
    "opponent_defense_allowed_zone_profile.png",
    "opponent_shot_map_vs_uconn.png",
    "uconn_shot_map_vs_opponent.png",
    "opponent_zone_profile_vs_uconn.png",
    "uconn_zone_profile_vs_opponent.png"
  )))

  plot_shot_map(
    opp_off,
    file.path(shots_dir, "opponent_shot_map.png"),
    paste0(meeting$opponent[[1]], " Shot Map vs UConn"),
    paste0(meeting$game_date[[1]], " | ", meeting$meeting_site[[1]], " | tracked FGA only")
  )
  plot_shot_map(
    opp_def,
    file.path(shots_dir, "uconn_shot_map.png"),
    paste0("UConn Shot Map vs ", meeting$opponent[[1]]),
    paste0(meeting$game_date[[1]], " | ", meeting$meeting_site[[1]], " | UConn tracked FGA only")
  )
  plot_zone_profile(
    shot_profile,
    file.path(shots_dir, "opponent_zone_profile.png"),
    paste0(meeting$opponent[[1]], " Zone Profile vs UConn"),
    paste0(meeting$game_date[[1]], " | ", meeting$meeting_site[[1]])
  )
  plot_zone_profile(
    defense_allowed_profile,
    file.path(shots_dir, "uconn_zone_profile.png"),
    paste0("UConn Zone Profile vs ", meeting$opponent[[1]]),
    paste0(meeting$game_date[[1]], " | ", meeting$meeting_site[[1]])
  )

  render_summary_md(
    summary_tbl = meeting_summary,
    shot_profile = shot_profile,
    defense_allowed_profile = defense_allowed_profile,
    player_profile = player_profile,
    turnover_victims = turnover_victims,
    turnover_creators = turnover_creators,
    defensive_lineups = defensive_lineups,
    offensive_lineups = offensive_lineups,
    meeting_dir = meeting_dir
  )

  meeting_summaries[[length(meeting_summaries) + 1L]] <- meeting_summary %>%
    mutate(
      output_dir = meeting_dir,
      report_summary_path = file.path(summary_dir, "report_summary.md")
    )

  message(
    "[DONE] ", meeting$opponent[[1]], " | ", meeting$game_date[[1]], " | ",
    meeting$meeting_site[[1]], " -> ", meeting_dir
  )
}

manifest <- bind_rows(meeting_summaries) %>%
  arrange(competition_bucket, game_date, opponent, meeting_site)

if (nrow(manifest) == 0) {
  stop("No opponent scout outputs were written.", call. = FALSE)
}

write_csv(manifest, file.path(out_dir, "manual_game_scout_manifest.csv"))

for (bucket in unique(manifest$competition_bucket)) {
  bucket_dir <- file.path(out_dir, bucket)
  dir.create(bucket_dir, recursive = TRUE, showWarnings = FALSE)

  bucket_manifest <- manifest %>%
    filter(competition_bucket == bucket) %>%
    arrange(game_date, opponent, meeting_site)

  write_csv(bucket_manifest, file.path(bucket_dir, "manual_game_scout_manifest.csv"))

  for (opp in unique(bucket_manifest$opponent_slug)) {
    opp_dir <- file.path(bucket_dir, opp)
    opp_summary <- bucket_manifest %>%
      filter(opponent_slug == opp) %>%
      select(
        competition_bucket, opponent, game_date, meeting_site, game_file,
        opponent_fga, opponent_fg_pct, opponent_3pa, opponent_turnovers,
        opponent_assisted_make_rate, opponent_rim_share, opponent_paint_share,
        uconn_fga_vs_opp_def, opponent_steals_forced, opponent_def_paint_allowed,
        opponent_def_corner_3_allowed, clutch_event_rows, output_dir
      ) %>%
      arrange(game_date)

    write_csv(opp_summary, file.path(opp_dir, "meeting_comparison.csv"))

    opp_player_comparison <- manual_games %>%
      filter(
        competition_bucket == bucket,
        opponent_slug == opp
      ) %>%
      build_player_meeting_comparison(min_player_events)

    write_csv(opp_player_comparison, file.path(opp_dir, "player_meeting_comparison.csv"))
  }
}

message("\nComplete.")
message("Scout root: ", normalizePath(out_dir))
message("Meetings written: ", nrow(manifest))
