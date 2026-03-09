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
  library(hoopR)
  library(dplyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(jsonlite)
})

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(key, default = NULL) {
  hit <- grep(paste0("^--", key, "="), args, value = TRUE)
  if (length(hit) == 0) return(default)
  sub(paste0("^--", key, "="), "", hit[[1]])
}

to_bool <- function(x, default = FALSE) {
  if (is.null(x) || is.na(x) || x == "") return(default)
  tolower(x) %in% c("1", "true", "t", "yes", "y")
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) return(y)
  x
}

normalize_name <- function(x) {
  x %>%
    tolower() %>%
    str_replace_all("[^a-z0-9]+", " ") %>%
    str_squish()
}

clean_text <- function(x) {
  x %>%
    str_replace_all("[\r\n]+", " ") %>%
    str_replace_all("\\s+", " ") %>%
    str_trim()
}

clean_name <- function(x) {
  x %>%
    clean_text() %>%
    str_remove("\\.$")
}

player_or_team <- function(x) {
  x1 <- clean_name(x)
  ifelse(
    x1 == "" | str_detect(str_to_lower(x1), "^team$|^uconn$|seton hall|huskies|pirates"),
    "TEAM",
    x1
  )
}

parse_shooter <- function(text) {
  m <- str_match(text, regex("^(.+?)\\s+(makes|misses|made|missed)\\b", ignore_case = TRUE))
  clean_name(ifelse(is.na(m[, 2]), NA_character_, m[, 2]))
}

parse_assist <- function(text) {
  m1 <- str_match(text, regex("\\((.+?) assists\\)", ignore_case = TRUE))
  m2 <- str_match(text, regex("Assisted by\\s+(.+?)\\.?$", ignore_case = TRUE))
  raw <- ifelse(!is.na(m1[, 2]), m1[, 2], ifelse(!is.na(m2[, 2]), m2[, 2], NA_character_))
  clean_name(raw)
}

parse_turnover_player <- function(text) {
  text_l <- str_to_lower(text)
  team_turnover <- str_detect(text_l, "^team\\b") | str_detect(text_l, "team\\s+shot clock turnover")
  m <- str_match(
    text,
    regex("^(.+?)\\s+(bad pass|traveling|double dribble|offensive foul|turnover|charging|illegal screen|lane violation|out of bounds|5-second|3-second|three second|shot clock turnover)\\b", ignore_case = TRUE)
  )
  raw <- ifelse(team_turnover, "TEAM", ifelse(is.na(m[, 2]), NA_character_, m[, 2]))
  player_or_team(raw)
}

parse_rebound_player <- function(text) {
  m <- str_match(text, "^(.+?)\\s+(Offensive|Defensive) Rebound\\.")
  raw <- ifelse(is.na(m[, 2]), NA_character_, m[, 2])
  player_or_team(raw)
}

parse_steal_player <- function(text) {
  m <- str_match(text, "^(.+?)\\s+Steal\\.")
  clean_name(ifelse(is.na(m[, 2]), NA_character_, m[, 2]))
}

parse_block_player <- function(text) {
  m <- str_match(text, "^(.+?)\\s+Block\\.")
  clean_name(ifelse(is.na(m[, 2]), NA_character_, m[, 2]))
}

resolve_player_from_roster <- function(raw_name, roster_df, max_dist = 2) {
  if (nrow(roster_df) == 0) return(clean_name(raw_name))
  key <- normalize_name(raw_name)
  exact <- roster_df %>% filter(name_key == key) %>% slice(1)
  if (nrow(exact) > 0) return(exact$full_name[[1]])
  d <- adist(key, roster_df$name_key) %>% as.numeric()
  if (length(d) == 0 || all(!is.finite(d))) return(clean_name(raw_name))
  i <- which.min(d)
  if (is.finite(d[[i]]) && d[[i]] <= max_dist) return(roster_df$full_name[[i]])
  clean_name(raw_name)
}

collapse_lineup <- function(lineup_vec) {
  x <- lineup_vec %||% character()
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) return(NA_character_)
  paste(x, collapse = ", ")
}

canonicalize_lineup_key <- function(x) {
  if (length(x) == 0) return(character())
  vapply(x, function(val) {
    if (is.na(val) || val == "") return(NA_character_)
    parts <- str_split(as.character(val), "\\s*,\\s*", simplify = FALSE)[[1]]
    parts <- parts[!is.na(parts) & parts != ""]
    if (length(parts) == 0) return(NA_character_)
    paste(sort(unique(parts)), collapse = " | ")
  }, character(1))
}

init_team_lineup <- function(rosters, team_id) {
  rr <- rosters %>%
    filter(team_id == !!team_id, did_not_play != TRUE) %>%
    mutate(full_name = clean_name(full_name)) %>%
    filter(!is.na(full_name), full_name != "")

  starters <- rr %>% filter(starter %in% TRUE) %>% pull(full_name)
  starters <- unique(starters)

  if (length(starters) < 5) {
    fill <- rr %>% pull(full_name)
    fill <- fill[!fill %in% starters]
    starters <- c(starters, fill)
  }

  unique(starters)[seq_len(min(5, length(unique(starters))))]
}

extract_player_from_play <- function(play_type, txt) {
  ptype <- as.character(play_type %||% "")
  txt <- as.character(txt %||% "")
  out <- NA_character_
  if (ptype %in% c("JumpShot", "LayUpShot", "DunkShot", "TipShot", "MadeFreeThrow")) out <- parse_shooter(txt)
  if (ptype == "Steal") out <- parse_steal_player(txt)
  if (ptype == "Block Shot") out <- parse_block_player(txt)
  if (ptype %in% c("Offensive Rebound", "Defensive Rebound")) out <- parse_rebound_player(txt)
  if (ptype %in% c("Lost Ball Turnover", "Turnover")) out <- parse_turnover_player(txt)
  out <- clean_name(out)
  ifelse(is.na(out) | out == "" | out == "TEAM", NA_character_, out)
}

build_lineup_context <- function(pbp, rosters) {
  home_team_id <- as.character(na.omit(pbp$home_team_id)[1] %||% NA_character_)
  away_team_id <- as.character(na.omit(pbp$away_team_id)[1] %||% NA_character_)
  team_ids <- c(home_team_id, away_team_id)
  team_ids <- team_ids[!is.na(team_ids)]

  if (!all(c("team_id", "full_name", "starter", "did_not_play") %in% names(rosters))) {
    rosters2 <- tibble(
      team_id = character(),
      full_name = character(),
      name_key = character(),
      starter = logical(),
      did_not_play = logical()
    )
  } else {
    rosters2 <- rosters %>%
      transmute(
        team_id = as.character(team_id),
        full_name = clean_name(full_name),
        name_key = normalize_name(full_name),
        starter = starter %in% TRUE,
        did_not_play = did_not_play %in% TRUE
      ) %>%
      filter(!is.na(team_id), !is.na(full_name), full_name != "")
  }

  lineups <- list()
  roster_map <- list()
  for (tid in team_ids) {
    lineups[[tid]] <- init_team_lineup(rosters2, tid)
    roster_map[[tid]] <- rosters2 %>% filter(team_id == !!tid) %>% distinct(name_key, full_name, .keep_all = TRUE)

    # Fallback when roster endpoint is missing or starter flags are unavailable.
    if (length(lineups[[tid]]) < 5) {
      sub_out <- pbp %>%
        filter(as.character(team_id) == tid, type_text == "Substitution", str_detect(text, "subbing out for")) %>%
        mutate(raw = str_match(text, "^(.*?)\\s+subbing out for\\s+.*$")[, 2]) %>%
        pull(raw)
      sub_out <- sub_out[!is.na(sub_out) & sub_out != ""]
      sub_out <- vapply(sub_out, function(z) resolve_player_from_roster(z, roster_map[[tid]]), character(1))

      from_plays <- pbp %>%
        filter(as.character(team_id) == tid) %>%
        mutate(p = mapply(extract_player_from_play, type_text, text, USE.NAMES = FALSE)) %>%
        pull(p)
      from_plays <- from_plays[!is.na(from_plays) & from_plays != ""]

      candidates <- unique(c(lineups[[tid]], sub_out, from_plays))
      lineups[[tid]] <- candidates[seq_len(min(5, length(candidates)))]
    }
  }

  update_sub <- function(row) {
    txt <- row$text %||% ""
    if (!str_detect(txt, "subbing (out|in) for")) return()

    tid <- as.character(row$team_id %||% NA_character_)
    if (is.na(tid) || !(tid %in% names(lineups))) return()

    if (str_detect(txt, "subbing out for")) {
      raw <- str_match(txt, "^(.*?)\\s+subbing out for\\s+.*$")[, 2]
      pnm <- resolve_player_from_roster(raw, roster_map[[tid]])
      lineups[[tid]] <<- setdiff(lineups[[tid]], pnm)
    }

    if (str_detect(txt, "subbing in for")) {
      raw <- str_match(txt, "^(.*?)\\s+subbing in for\\s+.*$")[, 2]
      pnm <- resolve_player_from_roster(raw, roster_map[[tid]])
      if (!(pnm %in% lineups[[tid]])) {
        lineups[[tid]] <<- c(lineups[[tid]], pnm)
      }
      # If lineup overflows from unresolved sub-out rows, keep most recent five.
      if (length(lineups[[tid]]) > 5) {
        lineups[[tid]] <<- tail(lineups[[tid]], 5)
      }
    }
  }

  get_lineups_for_event <- function(off_tid) {
    off_tid <- as.character(off_tid %||% NA_character_)
    if (is.na(off_tid) || !(off_tid %in% names(lineups))) {
      return(c(NA_character_, NA_character_))
    }
    def_tid <- setdiff(names(lineups), off_tid)
    def_tid <- if (length(def_tid) == 0) NA_character_ else def_tid[[1]]
    c(collapse_lineup(lineups[[off_tid]]), collapse_lineup(lineups[[def_tid]]))
  }

  list(update_sub = update_sub, get_lineups_for_event = get_lineups_for_event)
}

derive_shot_zone_columns <- function(df, home_id) {
  # hoopR transformed coordinates are mirrored by home/away with hoop centers at +/-41.75.
  x_raw <- suppressWarnings(as.numeric(df$coordinate_x))
  y_raw <- suppressWarnings(as.numeric(df$coordinate_y))

  offense_is_home <- as.character(df$team_id) == as.character(home_id)
  fallback_sign <- x_raw >= 0
  offense_is_home <- ifelse(is.na(offense_is_home), fallback_sign, offense_is_home)

  x_rel <- ifelse(offense_is_home, x_raw, -x_raw)
  y_rel <- ifelse(offense_is_home, y_raw, -y_raw)
  has_xy <- is.finite(x_rel) & is.finite(y_rel)

  hoop_x <- 41.75
  dx <- x_rel - hoop_x
  dy <- y_rel
  dist_ft <- sqrt(dx^2 + dy^2)

  is_fga <- df$FGA == 1L
  is_fga3 <- df$FGA3 == 1L
  is_ft <- df$FTA == 1L

  # NCAA men geometry:
  # - Corner 3 from hoop center: 21'9" = 21.75 ft
  # - Paint lane width: 12 ft (6 ft each side of lane center)
  # - Paint depth (rim -> free-throw line center): 13.75 ft
  CORNER_3_CUTOFF <- 21.75
  PAINT_HALF_WIDTH <- 6
  PAINT_DEPTH <- 13.75

  shot_zone <- rep("UNKNOWN", nrow(df))
  shot_zone[is_ft] <- "FT"
  shot_zone[is_fga3 & has_xy & abs(dy) >= CORNER_3_CUTOFF] <- "CORNER_3"
  shot_zone[is_fga3 & has_xy & abs(dy) < CORNER_3_CUTOFF] <- "ABOVE_BREAK_3"
  shot_zone[is_fga & !is_fga3 & has_xy & dist_ft <= 4] <- "RIM"
  shot_zone[is_fga & !is_fga3 & has_xy & dist_ft > 4 & dist_ft <= 14] <- "SHORT_MID"
  shot_zone[is_fga & !is_fga3 & has_xy & dist_ft > 14] <- "LONG_MID"
  shot_zone[is_fga & has_xy & dist_ft >= 30] <- "HEAVE"

  shot_side <- rep("UNKNOWN", nrow(df))
  shot_side[is_ft] <- "CENTER"
  shot_side[is_fga & has_xy & dy <= -1] <- "LEFT"
  shot_side[is_fga & has_xy & dy >= 1] <- "RIGHT"
  shot_side[is_fga & has_xy & abs(dy) < 1] <- "CENTER"

  # Paint is only in front of the rim (toward midcourt), not behind baseline.
  in_paint <- has_xy & abs(dy) <= PAINT_HALF_WIDTH & dx <= 0 & dx >= -PAINT_DEPTH
  paint_zone <- rep("UNKNOWN", nrow(df))
  paint_zone[is_ft] <- "NON_PAINT"
  paint_zone[is_fga & has_xy & in_paint] <- "PAINT"
  paint_zone[is_fga & has_xy & !in_paint] <- "NON_PAINT"

  shot_zone_detail <- ifelse(
    shot_zone %in% c("FT", "HEAVE", "UNKNOWN"),
    shot_zone,
    paste0(shot_side, "_", shot_zone)
  )

  df$shot_zone <- shot_zone
  df$shot_side <- shot_side
  df$paint_zone <- paint_zone
  df$shot_zone_detail <- shot_zone_detail
  df
}

build_manual_table <- function(pbp, rosters, game_meta = NULL, season_context = NULL) {
  pbp_norm <- pbp
  pbp_norm$play_id <- if ("play_id" %in% names(pbp_norm)) {
    coalesce(as.character(pbp_norm$play_id), as.character(pbp_norm$id))
  } else {
    as.character(pbp_norm$id)
  }

  pbp_norm <- pbp_norm %>%
    arrange(sequence_number) %>%
    mutate(game_play_number_derived = row_number()) %>%
    mutate(
      points_attempted_norm = case_when(
        points_attempted %in% c(1, 2, 3) ~ as.integer(points_attempted),
        score_value %in% c(1, 2, 3) ~ as.integer(score_value),
        type_text == "MadeFreeThrow" ~ 1L,
        TRUE ~ NA_integer_
      )
    )

  game_file_value <- if (!is.null(game_meta) && "game_file" %in% names(game_meta)) {
    as.character(game_meta$game_file[[1]])
  } else {
    NA_character_
  }

  opponent_value <- if (!is.null(game_meta) && "opponent" %in% names(game_meta)) {
    as.character(game_meta$opponent[[1]])
  } else {
    NA_character_
  }

  uconn_is_home_value <- if (!is.null(game_meta) && "uconn_is_home_flag" %in% names(game_meta)) {
    as.logical(game_meta$uconn_is_home_flag[[1]])
  } else {
    NA
  }

  site_type_value <- if (!is.null(game_meta) && "site_type" %in% names(game_meta)) {
    as.character(game_meta$site_type[[1]])
  } else {
    NA_character_
  }

  context_tbl <- if (!is.null(season_context) && nrow(season_context) > 0) {
    season_context %>%
      transmute(
        game_id = as.integer(game_id),
        sequence_number = suppressWarnings(as.numeric(sequence_number)),
        game_play_number = suppressWarnings(as.integer(game_play_number)),
        start_game_seconds_remaining = suppressWarnings(as.integer(start_game_seconds_remaining)),
        end_game_seconds_remaining = suppressWarnings(as.integer(end_game_seconds_remaining)),
        home_timeout_called = as.logical(home_timeout_called),
        away_timeout_called = as.logical(away_timeout_called),
        context_source = as.character(context_source %||% "season_feed")
      )
  } else {
    tibble(
      game_id = integer(),
      sequence_number = numeric(),
      game_play_number = integer(),
      start_game_seconds_remaining = integer(),
      end_game_seconds_remaining = integer(),
      home_timeout_called = logical(),
      away_timeout_called = logical(),
      context_source = character()
    )
  }

  source_filtered <- pbp_norm %>%
    left_join(context_tbl, by = c("game_id", "sequence_number")) %>%
    filter((shooting_play %in% TRUE & points_attempted_norm %in% c(1, 2, 3)) | str_detect(type_text, "Turnover")) %>%
    mutate(
      row_key = paste0(play_id, "::", sequence_number),
      context_game_play_number = game_play_number,
      context_source = coalesce(context_source, ""),
      timeout_interval_order = coalesce(as.integer(context_game_play_number), as.integer(game_play_number_derived)),
      game_play_number = case_when(
        context_source == "summary_fallback" ~ as.integer(game_play_number_derived),
        TRUE ~ coalesce(as.integer(context_game_play_number), as.integer(game_play_number_derived))
      ),
      points_attempted = points_attempted_norm,
      game_file = game_file_value,
      opponent = opponent_value,
      uconn_is_home = uconn_is_home_value,
      site_type = site_type_value
    )

  timeout_context <- context_tbl %>%
    filter(
      !is.na(game_play_number),
      home_timeout_called %in% TRUE | away_timeout_called %in% TRUE
    ) %>%
    arrange(game_play_number) %>%
    transmute(
      timeout_order = as.integer(game_play_number),
      home_timeout_called = home_timeout_called %in% TRUE,
      away_timeout_called = away_timeout_called %in% TRUE
    )

  if (nrow(source_filtered) > 0) {
    source_filtered$home_timeout_called <- FALSE
    source_filtered$away_timeout_called <- FALSE
  }

  if (nrow(source_filtered) > 0 && nrow(timeout_context) > 0) {
    ordered_idx <- order(source_filtered$timeout_interval_order, source_filtered$sequence_number)
    next_orders <- c(source_filtered$timeout_interval_order[ordered_idx][-1], Inf)

    for (j in seq_along(ordered_idx)) {
      row_i <- ordered_idx[[j]]
      hits <- timeout_context$timeout_order > source_filtered$timeout_interval_order[[row_i]] &
        timeout_context$timeout_order < next_orders[[j]]
      source_filtered$home_timeout_called[[row_i]] <- any(timeout_context$home_timeout_called[hits])
      source_filtered$away_timeout_called[[row_i]] <- any(timeout_context$away_timeout_called[hits])
    }
  }

  home_id <- as.character(na.omit(source_filtered$home_team_id)[1] %||% NA_character_)
  away_id <- as.character(na.omit(source_filtered$away_team_id)[1] %||% NA_character_)
  home_name <- clean_name(na.omit(source_filtered$home_team_full_name)[1] %||% NA_character_)
  away_name <- clean_name(na.omit(source_filtered$away_team_full_name)[1] %||% NA_character_)
  home_is_uconn <- str_detect(normalize_name(home_name %||% ""), "uconn|connecticut huskies|connecticut")
  away_is_uconn <- str_detect(normalize_name(away_name %||% ""), "uconn|connecticut huskies|connecticut")
  uconn_team_id <- NA_character_
  if (isTRUE(home_is_uconn)) {
    uconn_team_id <- home_id
  } else if (isTRUE(away_is_uconn)) {
    uconn_team_id <- away_id
  } else if (!is.na(uconn_is_home_value)) {
    uconn_team_id <- if (isTRUE(uconn_is_home_value)) home_id else away_id
  }

  base <- source_filtered %>%
    mutate(
      row_key = paste0(play_id, "::", sequence_number),
      base_kind = if_else(str_detect(type_text, "Turnover"), "turnover", "shot"),
      UsagePlayer = NA_character_,
      AssistPlayer = NA_character_,
      ReboundPlayer = NA_character_,
      StealPlayer = NA_character_,
      BlockPlayer = NA_character_,
      OffenseTeam = case_when(
        as.character(team_id) == home_id ~ home_name,
        as.character(team_id) == away_id ~ away_name,
        TRUE ~ NA_character_
      ),
      DefenseTeam = case_when(
        as.character(team_id) == home_id ~ away_name,
        as.character(team_id) == away_id ~ home_name,
        TRUE ~ NA_character_
      ),
      OffenseOnCourt = NA_character_,
      DefenseOnCourt = NA_character_,
      FGA = 0L, FGM = 0L, FTA = 0L, FTM = 0L, FGA3 = 0L, FGM3 = 0L, PTS = 0L,
      OREB = 0L, DREB = 0L, AST = 0L, TOV = 0L, STL = 0L, BLK = 0L
    )

  is_shot <- base$base_kind == "shot"
  is_to <- base$base_kind == "turnover"

  base$UsagePlayer[is_shot] <- parse_shooter(base$text[is_shot])
  base$AssistPlayer[is_shot] <- parse_assist(base$text[is_shot])
  base$FGA[is_shot] <- as.integer(base$points_attempted_norm[is_shot] %in% c(2, 3))
  base$FGM[is_shot] <- as.integer(base$FGA[is_shot] == 1 & base$scoring_play[is_shot] %in% TRUE)
  base$FTA[is_shot] <- as.integer(base$points_attempted_norm[is_shot] == 1)
  base$FTM[is_shot] <- as.integer(base$FTA[is_shot] == 1 & base$scoring_play[is_shot] %in% TRUE)
  base$FGA3[is_shot] <- as.integer(base$points_attempted_norm[is_shot] == 3)
  base$FGM3[is_shot] <- as.integer(base$FGA3[is_shot] == 1 & base$scoring_play[is_shot] %in% TRUE)
  base$PTS[is_shot] <- as.integer(ifelse(base$scoring_play[is_shot] %in% TRUE, base$points_attempted_norm[is_shot], 0))
  base$AST[is_shot] <- as.integer(!is.na(base$AssistPlayer[is_shot]) & base$AssistPlayer[is_shot] != "")

  base$UsagePlayer[is_to] <- parse_turnover_player(base$text[is_to])
  base$TOV[is_to] <- 1L

  steals <- pbp %>% filter(type_text == "Steal")
  if (nrow(steals) > 0) {
    for (i in seq_len(nrow(steals))) {
      s <- steals[i, ]
      cand <- which(
        base$base_kind == "turnover" &
          base$period_number == s$period_number &
          base$sequence_number < s$sequence_number &
          base$STL == 0L
      )
      if (length(cand) == 0) next
      cand <- cand[which.max(base$sequence_number[cand])]
      if ((s$sequence_number - base$sequence_number[cand]) <= 3 || s$clock_display_value == base$clock_display_value[cand]) {
        base$StealPlayer[cand] <- parse_steal_player(s$text)
        base$STL[cand] <- 1L
      }
    }
  }

  blocks <- pbp %>% filter(type_text == "Block Shot")
  if (nrow(blocks) > 0) {
    for (i in seq_len(nrow(blocks))) {
      b <- blocks[i, ]
      cand <- which(
        base$base_kind == "shot" &
          base$period_number == b$period_number &
          base$sequence_number < b$sequence_number &
          base$FGA == 1L &
          base$FGM == 0L &
          (is.na(base$BlockPlayer) | base$BlockPlayer == "")
      )
      if (length(cand) == 0) next
      cand <- cand[which.max(base$sequence_number[cand])]
      if ((b$sequence_number - base$sequence_number[cand]) <= 3 || b$clock_display_value == base$clock_display_value[cand]) {
        base$BlockPlayer[cand] <- parse_block_player(b$text)
        base$BLK[cand] <- 1L
      }
    }
  }

  rebounds <- pbp %>% filter(type_text %in% c("Offensive Rebound", "Defensive Rebound"))
  if (nrow(rebounds) > 0) {
    for (i in seq_len(nrow(rebounds))) {
      r <- rebounds[i, ]
      cand <- which(
        base$base_kind == "shot" &
          base$period_number == r$period_number &
          base$sequence_number < r$sequence_number &
          base$FGM == 0L &
          (is.na(base$ReboundPlayer) | base$ReboundPlayer == "")
      )
      if (length(cand) == 0) next
      cand <- cand[which.max(base$sequence_number[cand])]
      if ((r$sequence_number - base$sequence_number[cand]) <= 6) {
        base$ReboundPlayer[cand] <- parse_rebound_player(r$text)
        if (r$type_text == "Offensive Rebound") base$OREB[cand] <- 1L
        if (r$type_text == "Defensive Rebound") base$DREB[cand] <- 1L
      }
    }
  }

  # Build on-court context from starters + substitutions.
  ctx <- build_lineup_context(pbp, rosters)
  base_idx <- setNames(seq_len(nrow(base)), base$row_key)
  for (i in seq_len(nrow(pbp))) {
    p <- pbp[i, ]
    if (as.character(p$type_text) == "Substitution") {
      ctx$update_sub(p)
      next
    }

    play_key <- as.character((p$play_id %||% p$id))
    key <- paste0(play_key, "::", p$sequence_number)
    if (!(key %in% names(base_idx))) next
    bi <- base_idx[[key]]
    line_pair <- ctx$get_lineups_for_event(p$team_id)
    base$OffenseOnCourt[[bi]] <- line_pair[[1]]
    base$DefenseOnCourt[[bi]] <- line_pair[[2]]
  }

  home_score_int <- suppressWarnings(as.integer(base$home_score))
  away_score_int <- suppressWarnings(as.integer(base$away_score))
  margin_after <- if (isTRUE(home_is_uconn)) {
    home_score_int - away_score_int
  } else if (isTRUE(away_is_uconn)) {
    away_score_int - home_score_int
  } else if (!is.na(uconn_is_home_value)) {
    if (isTRUE(uconn_is_home_value)) home_score_int - away_score_int else away_score_int - home_score_int
  } else {
    rep(NA_integer_, nrow(base))
  }

  is_uconn_offense <- if (!is.na(uconn_team_id)) {
    as.character(base$team_id) == uconn_team_id
  } else {
    rep(NA, nrow(base))
  }

  uconn_margin_delta <- ifelse(
    is.na(is_uconn_offense),
    NA_integer_,
    ifelse(is_uconn_offense, base$PTS, -base$PTS)
  )

  base$is_uconn_offense <- is_uconn_offense
  base$margin_after <- margin_after
  base$margin_before <- ifelse(is.na(uconn_margin_delta), NA_integer_, margin_after - uconn_margin_delta)
  base$clutch_flag <- !is.na(base$start_game_seconds_remaining) &
    base$start_game_seconds_remaining <= 300L &
    !is.na(base$margin_before) &
    abs(base$margin_before) <= 5L
  base$offense_lineup_key <- canonicalize_lineup_key(base$OffenseOnCourt)
  base$defense_lineup_key <- canonicalize_lineup_key(base$DefenseOnCourt)

  base <- derive_shot_zone_columns(base, home_id = home_id)

  stat_cols <- c(
    "UsagePlayer", "AssistPlayer", "ReboundPlayer", "StealPlayer", "BlockPlayer",
    "FGA", "FGM", "FTA", "FTM", "FGA3", "FGM3", "PTS", "OREB", "DREB",
    "AST", "TOV", "STL", "BLK"
  )
  context_cols <- c(
    "game_file", "game_id", "play_id", "game_date", "season_type",
    "opponent", "uconn_is_home", "site_type", "period_number", "game_play_number",
    "type_id", "team_id", "athlete_id_1", "athlete_id_2", "points_attempted",
    "start_game_seconds_remaining", "end_game_seconds_remaining"
  )
  derived_cols <- c(
    "is_uconn_offense", "margin_before", "margin_after", "clutch_flag",
    "offense_lineup_key", "defense_lineup_key"
  )
  extra_cols <- c(
    "sequence_number", "text",
    "OffenseTeam", "DefenseTeam",
    "OffenseOnCourt", "DefenseOnCourt",
    "away_score", "home_score", "scoring_play", "score_value", "clock_display_value",
    "shooting_play", "short_description", "type_text", "period_display_value",
    "shot_zone", "shot_side", "paint_zone", "shot_zone_detail",
    "coordinate_x_raw", "coordinate_y_raw", "coordinate_x", "coordinate_y"
  )

  out <- base %>%
    mutate(clock_display_value = as.character(clock_display_value)) %>%
    select(all_of(c(stat_cols, context_cols, derived_cols, extra_cols)))

  out
}

date_event_cache <- new.env(parent = emptyenv())
season_pbp_cache <- new.env(parent = emptyenv())

empty_game_context <- function() {
  tibble(
    game_id = integer(),
    sequence_number = numeric(),
    game_play_number = integer(),
    start_game_seconds_remaining = integer(),
    end_game_seconds_remaining = integer(),
    home_timeout_called = logical(),
    away_timeout_called = logical(),
    context_source = character()
  )
}

parse_clock_seconds <- function(clock_value) {
  clock_chr <- as.character(clock_value %||% NA_character_)
  if (is.na(clock_chr) || clock_chr == "") return(NA_integer_)
  parts <- str_split(clock_chr, ":", simplify = TRUE)
  if (ncol(parts) != 2) return(NA_integer_)
  mins <- suppressWarnings(as.integer(parts[, 1]))
  secs <- suppressWarnings(as.integer(parts[, 2]))
  if (is.na(mins) || is.na(secs)) return(NA_integer_)
  mins * 60L + secs
}

period_length_seconds <- function(period_number) {
  ifelse(period_number <= 2L, 1200L, 300L)
}

team_aliases <- function(team_obj) {
  aliases <- c(
    as.character(team_obj$displayName %||% NA_character_),
    as.character(team_obj$shortDisplayName %||% NA_character_),
    as.character(team_obj$location %||% NA_character_)
  )
  aliases <- normalize_name(aliases)
  aliases <- aliases[!is.na(aliases) & aliases != ""]
  unique(aliases)
}

derive_timeout_flags <- function(play, home_aliases, away_aliases) {
  blob <- normalize_name(paste(
    as.character(play$type$text %||% ""),
    as.character(play$text %||% "")
  ))

  if (!str_detect(blob, "timeout") || str_detect(blob, "official tv timeout")) {
    return(c(FALSE, FALSE))
  }

  c(
    any(vapply(home_aliases, function(alias) str_detect(blob, fixed(alias)), logical(1))),
    any(vapply(away_aliases, function(alias) str_detect(blob, fixed(alias)), logical(1)))
  )
}

build_game_context_from_summary <- function(game_id) {
  game_id_value <- as.integer(game_id)
  if (is.na(game_id_value)) return(empty_game_context())

  url <- paste0(
    "https://site.api.espn.com/apis/site/v2/sports/basketball/mens-college-basketball/summary?event=",
    game_id_value
  )

  payload <- tryCatch(
    fromJSON(url, simplifyDataFrame = FALSE),
    error = function(e) NULL
  )

  plays <- payload$plays %||% list()
  if (length(plays) == 0) return(empty_game_context())

  box_teams <- payload$boxscore$teams %||% list()
  home_team <- NULL
  away_team <- NULL
  for (tm in box_teams) {
    home_away <- tolower(as.character(tm$homeAway %||% ""))
    if (home_away == "home") home_team <- tm$team
    if (home_away == "away") away_team <- tm$team
  }

  home_aliases <- team_aliases(home_team)
  away_aliases <- team_aliases(away_team)
  max_period <- suppressWarnings(max(vapply(plays, function(play) {
    as.integer(play$period$number %||% NA_integer_)
  }, integer(1)), na.rm = TRUE))
  if (!is.finite(max_period)) return(empty_game_context())

  play_total_remaining <- function(play) {
    period_num <- suppressWarnings(as.integer(play$period$number %||% NA_integer_))
    clock_secs <- parse_clock_seconds(play$clock$displayValue %||% NA_character_)
    if (is.na(period_num) || is.na(clock_secs)) return(NA_integer_)
    future_periods <- seq.int(period_num + 1L, max_period)
    future_secs <- if (length(future_periods) == 0) 0L else sum(period_length_seconds(future_periods))
    as.integer(clock_secs + future_secs)
  }

  bind_rows(lapply(seq_along(plays), function(i) {
    play <- plays[[i]]
    next_play <- if (i < length(plays)) plays[[i + 1L]] else NULL
    timeout_flags <- derive_timeout_flags(play, home_aliases, away_aliases)

    tibble(
      game_id = game_id_value,
      sequence_number = suppressWarnings(as.numeric(play$sequenceNumber %||% NA_character_)),
      game_play_number = as.integer(i),
      start_game_seconds_remaining = play_total_remaining(play),
      end_game_seconds_remaining = if (is.null(next_play)) NA_integer_ else play_total_remaining(next_play),
      home_timeout_called = as.logical(timeout_flags[[1]]),
      away_timeout_called = as.logical(timeout_flags[[2]]),
      context_source = "summary_fallback"
    )
  })) %>%
    filter(!is.na(sequence_number))
}

get_game_context <- function(season, game_id) {
  game_id_value <- as.integer(game_id)
  season_key <- as.character(season %||% "")
  if (season_key == "" || is.na(game_id_value)) {
    return(empty_game_context())
  }

  if (!exists(season_key, envir = season_pbp_cache, inherits = FALSE)) {
    season_tbl <- tryCatch(
      load_mbb_pbp(season = as.integer(season)) %>%
        transmute(
          game_id = as.integer(game_id),
          sequence_number = suppressWarnings(as.numeric(sequence_number)),
          game_play_number = suppressWarnings(as.integer(game_play_number)),
          start_game_seconds_remaining = suppressWarnings(as.integer(start_game_seconds_remaining)),
          end_game_seconds_remaining = suppressWarnings(as.integer(end_game_seconds_remaining)),
          home_timeout_called = as.logical(home_timeout_called),
          away_timeout_called = as.logical(away_timeout_called),
          context_source = "season_feed"
        ),
      error = function(e) {
        empty_game_context()
      }
    )
    assign(season_key, season_tbl, envir = season_pbp_cache)
  }

  season_game_context <- get(season_key, envir = season_pbp_cache, inherits = FALSE) %>%
    filter(game_id == game_id_value)

  if (
    nrow(season_game_context) > 0 &&
    (any(!is.na(season_game_context$start_game_seconds_remaining)) ||
      any(!is.na(season_game_context$end_game_seconds_remaining)))
  ) {
    return(season_game_context)
  }

  build_game_context_from_summary(game_id_value)
}

get_events_for_date <- function(game_date) {
  key <- as.character(game_date)
  if (exists(key, envir = date_event_cache, inherits = FALSE)) {
    return(get(key, envir = date_event_cache, inherits = FALSE))
  }

  url <- paste0(
    "https://site.api.espn.com/apis/site/v2/sports/basketball/mens-college-basketball/scoreboard?dates=",
    format(game_date, "%Y%m%d"),
    "&limit=400"
  )

  payload <- tryCatch(
    fromJSON(url, simplifyDataFrame = FALSE),
    error = function(e) NULL
  )

  if (is.null(payload) || is.null(payload$events) || length(payload$events) == 0) {
    out <- tibble(
      game_date = as.Date(character()),
      game_id = integer(),
      home_team_full_name = character(),
      away_team_full_name = character(),
      status_name = character()
    )
    assign(key, out, envir = date_event_cache)
    return(out)
  }

  out <- bind_rows(lapply(payload$events, function(ev) {
    comp <- ev$competitions[[1]]
    c1 <- comp$competitors[[1]]
    c2 <- comp$competitors[[2]]

    home_comp <- if (tolower(c1$homeAway %||% "") == "home") c1 else c2
    away_comp <- if (tolower(c1$homeAway %||% "") == "away") c1 else c2

    tibble(
      game_date = as.Date(substr(comp$date %||% NA_character_, 1, 10)),
      game_id = suppressWarnings(as.integer(ev$id %||% NA_character_)),
      home_team_full_name = as.character(home_comp$team$displayName %||% NA_character_),
      away_team_full_name = as.character(away_comp$team$displayName %||% NA_character_),
      status_name = as.character(comp$status$type$name %||% NA_character_)
    )
  }))

  assign(key, out, envir = date_event_cache)
  out
}

find_event_id <- function(game_date, opponent, uconn_is_home) {
  candidate_dates <- as.Date(c(game_date, game_date - days(1), game_date + days(1)))
  day_events <- bind_rows(lapply(seq_along(candidate_dates), function(i) {
    d <- candidate_dates[[i]]
    ev <- get_events_for_date(d)
    if (nrow(ev) == 0) return(ev)
    ev %>%
      mutate(
        requested_game_date = game_date,
        event_lookup_date = d,
        day_offset = as.integer(d - game_date)
      )
  }))
  if (nrow(day_events) == 0) return(NA_integer_)

  opp_norm <- normalize_name(opponent)
  cand <- day_events %>%
    mutate(
      home_norm = normalize_name(home_team_full_name),
      away_norm = normalize_name(away_team_full_name),
      has_uconn_home = str_detect(home_norm, "uconn|connecticut huskies"),
      has_uconn_away = str_detect(away_norm, "uconn|connecticut huskies"),
      has_opp_home = str_detect(home_norm, fixed(opp_norm)),
      has_opp_away = str_detect(away_norm, fixed(opp_norm)),
      has_uconn = has_uconn_home | has_uconn_away,
      has_opp = has_opp_home | has_opp_away,
      # Fuzzy fallback for typos in metadata opponent names.
      other_team_norm = if_else(has_uconn_home, away_norm, home_norm),
      opp_dist = adist(opp_norm, other_team_norm) %>% as.numeric(),
      is_final = status_name %in% c("STATUS_FINAL", "STATUS_FINAL_OVERTIME")
    ) %>%
    filter(has_uconn)

  if (nrow(cand) == 0) return(NA_integer_)

  # Prefer exact opponent match first.
  cand_exact <- cand %>% filter(has_opp)
  if (nrow(cand_exact) > 0) cand <- cand_exact

  if (isTRUE(uconn_is_home)) {
    cand2 <- cand %>% filter(has_uconn_home)
    if (nrow(cand2) > 0) cand <- cand2
  } else {
    cand2 <- cand %>% filter(has_uconn_away)
    if (nrow(cand2) > 0) cand <- cand2
  }

  # Prefer completed games, nearest date, and closest opponent string distance.
  cand <- cand %>%
    arrange(desc(is_final), abs(day_offset), opp_dist)

  cand$game_id[[1]]
}

overwrite <- to_bool(arg_value("overwrite", "false"), default = FALSE)
include_exhibitions <- to_bool(arg_value("include-exhibitions", "false"), default = FALSE)
out_dir <- arg_value("out-dir", "_data/03_manual_game_csv/_games")
game_files_arg <- arg_value("game-files", "")

if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

games_meta <- read_csv("_data/01_core_inputs/uconn_games_meta.csv", show_col_types = FALSE) %>%
  mutate(
    game_date_parsed = mdy(game_date),
    uconn_is_home_flag = tolower(as.character(uconn_is_home)) %in% c("true", "1", "t", "yes", "y")
  )

if (!include_exhibitions) {
  games_meta <- games_meta %>% filter(!str_detect(game_file, regex("Exhibition", ignore_case = TRUE)))
}

if (!is.na(game_files_arg) && game_files_arg != "") {
  requested <- str_split(game_files_arg, ",")[[1]] %>% str_trim()
  games_meta <- games_meta %>% filter(game_file %in% requested)
}

games_meta <- games_meta %>% filter(!is.na(game_date_parsed))
if (nrow(games_meta) == 0) stop("No games selected after filters.", call. = FALSE)

results <- list()
for (i in seq_len(nrow(games_meta))) {
  g <- games_meta[i, ]
  game_file <- g$game_file[[1]]
  out_name <- str_remove(game_file, "\\.pdf$") %>% str_trim()
  out_path <- file.path(out_dir, paste0(out_name, ".csv"))

  if (file.exists(out_path) && !overwrite) {
    message("[SKIP] ", game_file, " (exists)")
    results[[length(results) + 1]] <- tibble(game_file = game_file, status = "skipped_exists", out_path = out_path)
    next
  }

  event_id <- find_event_id(
    game_date = g$game_date_parsed[[1]],
    opponent = g$opponent[[1]],
    uconn_is_home = g$uconn_is_home_flag[[1]]
  )

  if (is.na(event_id)) {
    message("[MISS] ", game_file, " (no matching ESPN event)")
    results[[length(results) + 1]] <- tibble(game_file = game_file, status = "missing_event", out_path = out_path)
    next
  }

  message("[RUN ] ", game_file, " -> event ", event_id)
  pbp <- espn_mbb_pbp(game_id = event_id) %>%
    mutate(
      sequence_number = as.numeric(sequence_number),
      period_number = as.integer(period_number),
      text = clean_text(text),
      type_text = as.character(type_text)
    ) %>%
    arrange(sequence_number)

  rosters <- tryCatch(
    espn_mbb_game_rosters(game_id = event_id),
    error = function(e) tibble()
  )
  season_value <- suppressWarnings(as.integer(na.omit(pbp$season)[1] %||% NA_integer_))
  season_context <- get_game_context(season = season_value, game_id = event_id)
  out_tbl <- build_manual_table(
    pbp = pbp,
    rosters = rosters,
    game_meta = g,
    season_context = season_context
  )
  write_csv(out_tbl, out_path, na = "")
  message("[DONE] ", out_name, ".csv (rows=", nrow(out_tbl), ")")

  results[[length(results) + 1]] <- tibble(game_file = game_file, status = "written", out_path = out_path, rows = nrow(out_tbl))
}

res <- bind_rows(results)
summary_path <- file.path(out_dir, "_espn_generation_summary.csv")
write_csv(res, summary_path)

message("\nComplete.")
message("Summary: ", normalizePath(summary_path))
message("Written: ", sum(res$status == "written", na.rm = TRUE))
message("Skipped existing: ", sum(res$status == "skipped_exists", na.rm = TRUE))
message("Missing events: ", sum(res$status == "missing_event", na.rm = TRUE))
