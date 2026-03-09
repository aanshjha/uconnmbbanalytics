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
  library(tibble)
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

unique_preserve <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) return(x)
  x[!duplicated(x)]
}

collapse_int_values <- function(x, sep = "|") {
  vals <- suppressWarnings(as.integer(x))
  vals <- vals[!is.na(vals)]
  vals <- vals[!duplicated(vals)]
  if (length(vals) == 0) return(NA_character_)
  paste(vals, collapse = sep)
}

collapse_chr_values <- function(x, sep = ", ") {
  vals <- unique_preserve(as.character(x))
  if (length(vals) == 0) return(NA_character_)
  paste(vals, collapse = sep)
}

extract_player_from_play <- function(play_type, txt) {
  ptype <- as.character(play_type %||% "")
  txt <- as.character(txt %||% "")
  out <- NA_character_
  if (ptype %in% c("JumpShot", "LayUpShot", "DunkShot", "TipShot", "MadeFreeThrow")) {
    out <- str_match(txt, regex("^(.+?)\\s+(makes|misses|made|missed)\\b", ignore_case = TRUE))[1, 2]
  }
  if (ptype == "Steal") out <- str_match(txt, "^(.+?)\\s+Steal\\.")[1, 2]
  if (ptype == "Block Shot") out <- str_match(txt, "^(.+?)\\s+Block\\.")[1, 2]
  if (ptype %in% c("Offensive Rebound", "Defensive Rebound")) {
    out <- str_match(txt, "^(.+?)\\s+(Offensive|Defensive) Rebound\\.")[1, 2]
  }
  if (ptype %in% c("Lost Ball Turnover", "Turnover")) {
    out <- str_match(
      txt,
      regex("^(.+?)\\s+(bad pass|traveling|double dribble|offensive foul|turnover|charging|illegal screen|lane violation|out of bounds|5-second|3-second|three second|shot clock turnover)\\b", ignore_case = TRUE)
    )[1, 2]
  }
  out <- clean_name(out)
  ifelse(is.na(out) | out == "" | out == "TEAM", NA_character_, out)
}

resolve_player_record <- function(raw_name, roster_df, max_dist = 2) {
  raw_name <- clean_name(raw_name)
  if (is.na(raw_name) || raw_name == "") {
    return(list(full_name = NA_character_, athlete_id = NA_integer_))
  }
  if (nrow(roster_df) == 0) {
    return(list(full_name = raw_name, athlete_id = NA_integer_))
  }

  key <- normalize_name(raw_name)
  exact <- roster_df %>% filter(name_key == key) %>% slice(1)
  if (nrow(exact) > 0) {
    return(list(full_name = exact$full_name[[1]], athlete_id = as.integer(exact$athlete_id[[1]])))
  }

  d <- adist(key, roster_df$name_key) %>% as.numeric()
  if (length(d) == 0 || all(!is.finite(d))) {
    return(list(full_name = raw_name, athlete_id = NA_integer_))
  }

  i <- which.min(d)
  if (is.finite(d[[i]]) && d[[i]] <= max_dist) {
    return(list(full_name = roster_df$full_name[[i]], athlete_id = as.integer(roster_df$athlete_id[[i]])))
  }

  list(full_name = raw_name, athlete_id = NA_integer_)
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

build_lineup_state <- function(pbp, rosters) {
  home_team_id <- as.character(na.omit(pbp$home_team_id)[1] %||% NA_character_)
  away_team_id <- as.character(na.omit(pbp$away_team_id)[1] %||% NA_character_)
  team_ids <- c(home_team_id, away_team_id)
  team_ids <- team_ids[!is.na(team_ids)]

  if (!all(c("team_id", "athlete_id", "full_name", "starter", "did_not_play") %in% names(rosters))) {
    rosters2 <- tibble(
      team_id = character(),
      athlete_id = integer(),
      full_name = character(),
      name_key = character(),
      starter = logical(),
      did_not_play = logical()
    )
  } else {
    rosters2 <- rosters %>%
      transmute(
        team_id = as.character(team_id),
        athlete_id = as.integer(athlete_id),
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
    roster_map[[tid]] <- rosters2 %>%
      filter(team_id == !!tid) %>%
      distinct(name_key, .keep_all = TRUE)

    team_aliases <- if (tid == home_team_id) {
      c(
        na.omit(pbp$home_team_name)[1] %||% NA_character_,
        na.omit(pbp$home_team_full_name)[1] %||% NA_character_,
        na.omit(pbp$home_team_mascot)[1] %||% NA_character_,
        na.omit(pbp$home_team_abbrev)[1] %||% NA_character_
      )
    } else if (tid == away_team_id) {
      c(
        na.omit(pbp$away_team_name)[1] %||% NA_character_,
        na.omit(pbp$away_team_full_name)[1] %||% NA_character_,
        na.omit(pbp$away_team_mascot)[1] %||% NA_character_,
        na.omit(pbp$away_team_abbrev)[1] %||% NA_character_
      )
    } else {
      character()
    }
    team_alias_keys <- unique(normalize_name(team_aliases[!is.na(team_aliases) & team_aliases != ""]))

    if (length(lineups[[tid]]) < 5) {
      team_pbp <- pbp %>%
        filter(as.character(team_id) == tid) %>%
        arrange(sequence_number)

      first_sub_seq <- team_pbp %>%
        filter(type_text == "Substitution") %>%
        summarise(first_seq = suppressWarnings(min(sequence_number, na.rm = TRUE))) %>%
        pull(first_seq)
      if (length(first_sub_seq) == 0 || !is.finite(first_sub_seq)) first_sub_seq <- Inf

      first_sub_cluster_end <- team_pbp %>%
        filter(type_text == "Substitution", sequence_number >= first_sub_seq) %>%
        summarise(
          end_seq = {
            if (n() == 0) {
              NA_real_
            } else {
              first_clock <- first(clock_display_value)
              first_period <- first(period_number)
              max(sequence_number[clock_display_value == first_clock & period_number == first_period], na.rm = TRUE)
            }
          }
        ) %>%
        pull(end_seq)
      if (length(first_sub_cluster_end) == 0 || !is.finite(first_sub_cluster_end)) first_sub_cluster_end <- first_sub_seq

      sub_out_early <- team_pbp %>%
        filter(
          type_text == "Substitution",
          str_detect(text, "subbing out for"),
          is.infinite(first_sub_cluster_end) | sequence_number <= first_sub_cluster_end
        ) %>%
        mutate(player_name = clean_name(str_match(text, "^(.*?)\\s+subbing out for\\s+.*$")[, 2])) %>%
        pull(player_name)

      from_plays_early <- team_pbp %>%
        filter(sequence_number < first_sub_seq) %>%
        mutate(p = mapply(extract_player_from_play, type_text, text, USE.NAMES = FALSE)) %>%
        pull(p)

      sub_out_all <- team_pbp %>%
        filter(type_text == "Substitution", str_detect(text, "subbing out for")) %>%
        mutate(player_name = clean_name(str_match(text, "^(.*?)\\s+subbing out for\\s+.*$")[, 2])) %>%
        pull(player_name)

      from_plays <- team_pbp %>%
        filter(as.character(team_id) == tid) %>%
        mutate(p = mapply(extract_player_from_play, type_text, text, USE.NAMES = FALSE)) %>%
        pull(p)
      from_plays <- from_plays[!is.na(from_plays) & from_plays != ""]

      fallback_names <- unique(c(sub_out_early, from_plays_early, sub_out_all, from_plays))
      fallback_names <- fallback_names[!is.na(fallback_names) & fallback_names != ""]
      fallback_names <- vapply(
        fallback_names,
        function(z) resolve_player_record(z, roster_map[[tid]])$full_name %||% clean_name(z),
        character(1)
      )
      fallback_names <- fallback_names[
        !(normalize_name(fallback_names) %in% team_alias_keys)
      ]

      candidates <- unique(c(lineups[[tid]], fallback_names))
      lineups[[tid]] <- candidates[seq_len(min(5, length(candidates)))]
    }
  }

  list(lineups = lineups, roster_map = roster_map)
}

parse_sub_row <- function(text) {
  txt <- clean_text(text)
  out_match <- str_match(txt, "^(.*?)\\s+subbing out for\\s+.*$")
  in_match <- str_match(txt, "^(.*?)\\s+subbing in for\\s+.*$")

  if (!is.na(out_match[1, 2])) {
    return(list(direction = "out", player_name = clean_name(out_match[1, 2])))
  }
  if (!is.na(in_match[1, 2])) {
    return(list(direction = "in", player_name = clean_name(in_match[1, 2])))
  }
  list(direction = "unknown", player_name = NA_character_)
}

apply_substitution_name <- function(lineup_vec, player_name, direction) {
  lineup_vec <- lineup_vec %||% character()
  lineup_vec <- lineup_vec[!is.na(lineup_vec) & lineup_vec != ""]
  player_name <- clean_name(player_name)

  if (is.na(player_name) || player_name == "") return(lineup_vec)

  if (direction == "out") {
    lineup_vec <- setdiff(lineup_vec, player_name)
  } else if (direction == "in") {
    if (!(player_name %in% lineup_vec)) lineup_vec <- c(lineup_vec, player_name)
    if (length(lineup_vec) > 5) lineup_vec <- tail(lineup_vec, 5)
  }

  unique(lineup_vec)
}

date_event_cache <- new.env(parent = emptyenv())
season_pbp_cache <- new.env(parent = emptyenv())

get_game_context <- function(season, game_id) {
  game_id_value <- as.integer(game_id)
  season_key <- as.character(season %||% "")
  if (season_key == "" || is.na(game_id_value)) {
    return(tibble(
      game_id = integer(),
      sequence_number = numeric(),
      game_play_number = integer(),
      start_game_seconds_remaining = integer(),
      end_game_seconds_remaining = integer(),
      home_timeout_called = logical(),
      away_timeout_called = logical()
    ))
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
          away_timeout_called = as.logical(away_timeout_called)
        ),
      error = function(e) {
        tibble(
          game_id = integer(),
          sequence_number = numeric(),
          game_play_number = integer(),
          start_game_seconds_remaining = integer(),
          end_game_seconds_remaining = integer(),
          home_timeout_called = logical(),
          away_timeout_called = logical()
        )
      }
    )
    assign(season_key, season_tbl, envir = season_pbp_cache)
  }

  get(season_key, envir = season_pbp_cache, inherits = FALSE) %>%
    filter(game_id == game_id_value)
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
      other_team_norm = if_else(has_uconn_home, away_norm, home_norm),
      opp_dist = adist(opp_norm, other_team_norm) %>% as.numeric(),
      is_final = status_name %in% c("STATUS_FINAL", "STATUS_FINAL_OVERTIME")
    ) %>%
    filter(has_uconn)

  if (nrow(cand) == 0) return(NA_integer_)

  cand_exact <- cand %>% filter(has_opp)
  if (nrow(cand_exact) > 0) cand <- cand_exact

  if (isTRUE(uconn_is_home)) {
    cand2 <- cand %>% filter(has_uconn_home)
    if (nrow(cand2) > 0) cand <- cand2
  } else {
    cand2 <- cand %>% filter(has_uconn_away)
    if (nrow(cand2) > 0) cand <- cand2
  }

  cand <- cand %>%
    arrange(desc(is_final), abs(day_offset), opp_dist)

  cand$game_id[[1]]
}

build_sub_group_table <- function(pbp, rosters, game_meta = NULL, season_context = NULL) {
  pbp_norm <- pbp
  pbp_norm$play_id <- if ("play_id" %in% names(pbp_norm)) {
    coalesce(as.character(pbp_norm$play_id), as.character(pbp_norm$id))
  } else {
    as.character(pbp_norm$id)
  }

  pbp_norm <- pbp_norm %>%
    arrange(sequence_number) %>%
    mutate(game_play_number_derived = row_number())

  game_file_value <- if (!is.null(game_meta) && "game_file" %in% names(game_meta)) as.character(game_meta$game_file[[1]]) else NA_character_
  opponent_value <- if (!is.null(game_meta) && "opponent" %in% names(game_meta)) as.character(game_meta$opponent[[1]]) else NA_character_
  uconn_is_home_value <- if (!is.null(game_meta) && "uconn_is_home_flag" %in% names(game_meta)) as.logical(game_meta$uconn_is_home_flag[[1]]) else NA
  site_type_value <- if (!is.null(game_meta) && "site_type" %in% names(game_meta)) as.character(game_meta$site_type[[1]]) else NA_character_

  context_tbl <- if (!is.null(season_context) && nrow(season_context) > 0) {
    season_context %>%
      transmute(
        game_id = as.integer(game_id),
        sequence_number = suppressWarnings(as.numeric(sequence_number)),
        game_play_number = suppressWarnings(as.integer(game_play_number)),
        start_game_seconds_remaining = suppressWarnings(as.integer(start_game_seconds_remaining)),
        end_game_seconds_remaining = suppressWarnings(as.integer(end_game_seconds_remaining)),
        home_timeout_called = as.logical(home_timeout_called),
        away_timeout_called = as.logical(away_timeout_called)
      )
  } else {
    tibble(
      game_id = integer(),
      sequence_number = numeric(),
      game_play_number = integer(),
      start_game_seconds_remaining = integer(),
      end_game_seconds_remaining = integer(),
      home_timeout_called = logical(),
      away_timeout_called = logical()
    )
  }

  pbp_ctx <- pbp_norm %>%
    left_join(context_tbl, by = c("game_id", "sequence_number")) %>%
    mutate(
      game_play_number = coalesce(game_play_number, as.integer(game_play_number_derived)),
      game_file = game_file_value,
      opponent = opponent_value,
      uconn_is_home = uconn_is_home_value,
      site_type = site_type_value,
      pbp_index = row_number(),
      is_sub = type_text == "Substitution"
    )

  if (!any(pbp_ctx$is_sub)) {
    return(tibble(
      game_file = character(),
      game_id = integer(),
      dead_ball_id = character(),
      sub_group_id = character(),
      team_order_in_dead_ball = integer(),
      sequence_number_start = numeric(),
      sequence_number_end = numeric(),
      game_play_number_start = integer(),
      game_play_number_end = integer(),
      play_id_start = character(),
      play_id_end = character(),
      game_date = as.Date(character()),
      season = integer(),
      season_type = integer(),
      opponent = character(),
      uconn_is_home = logical(),
      site_type = character(),
      period_number = integer(),
      clock_display_value = character(),
      start_game_seconds_remaining = integer(),
      end_game_seconds_remaining = integer(),
      team_id = integer(),
      team_name = character(),
      is_uconn_team = logical(),
      sub_rows = integer(),
      players_out = character(),
      players_in = character(),
      athlete_ids_out = character(),
      athlete_ids_in = character(),
      lineup_before = character(),
      lineup_after = character(),
      lineup_before_key = character(),
      lineup_after_key = character(),
      away_score = integer(),
      home_score = integer(),
      margin_before = integer(),
      margin_after = integer(),
      team_margin_before = integer(),
      team_margin_after = integer(),
      one_possession_flag = logical(),
      clutch_flag = logical(),
      home_timeout_called = logical(),
      away_timeout_called = logical(),
      after_timeout_flag = logical(),
      raw_sub_text = character()
    ))
  }

  home_id <- as.character(na.omit(pbp_ctx$home_team_id)[1] %||% NA_character_)
  away_id <- as.character(na.omit(pbp_ctx$away_team_id)[1] %||% NA_character_)
  home_name <- clean_name(na.omit(pbp_ctx$home_team_full_name)[1] %||% NA_character_)
  away_name <- clean_name(na.omit(pbp_ctx$away_team_full_name)[1] %||% NA_character_)
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

  team_name_for_id <- function(team_id_value) {
    tid <- as.character(team_id_value %||% NA_character_)
    if (is.na(tid)) return(NA_character_)
    if (tid == home_id) return(home_name)
    if (tid == away_id) return(away_name)
    NA_character_
  }

  dead_ball_counter <- 0L
  current_dead_ball <- NA_integer_
  prev_is_sub <- FALSE
  prev_period <- NA_integer_
  prev_clock <- NA_character_
  pbp_ctx$dead_ball_raw <- NA_integer_

  for (i in seq_len(nrow(pbp_ctx))) {
    if (!pbp_ctx$is_sub[[i]]) {
      prev_is_sub <- FALSE
      next
    }
    same_cluster <- prev_is_sub &&
      identical(pbp_ctx$period_number[[i]], prev_period) &&
      identical(pbp_ctx$clock_display_value[[i]], prev_clock)
    if (!same_cluster) {
      dead_ball_counter <- dead_ball_counter + 1L
      current_dead_ball <- dead_ball_counter
    }
    pbp_ctx$dead_ball_raw[[i]] <- current_dead_ball
    prev_is_sub <- TRUE
    prev_period <- pbp_ctx$period_number[[i]]
    prev_clock <- pbp_ctx$clock_display_value[[i]]
  }

  lineup_state <- build_lineup_state(pbp_ctx, rosters)
  lineups <- lineup_state$lineups
  roster_map <- lineup_state$roster_map

  out_rows <- list()
  out_i <- 0L
  dead_ball_ids <- sort(unique(na.omit(pbp_ctx$dead_ball_raw)))

  for (db in dead_ball_ids) {
    group_rows <- pbp_ctx %>%
      filter(dead_ball_raw == db) %>%
      arrange(sequence_number, game_play_number, pbp_index)
    if (nrow(group_rows) == 0) next

    prev_non_sub <- pbp_ctx %>%
      filter(pbp_index < min(group_rows$pbp_index), !is_sub) %>%
      slice_tail(n = 1)
    after_timeout_flag <- nrow(prev_non_sub) > 0 &&
      (
        str_detect(str_to_lower(prev_non_sub$type_text[[1]] %||% ""), "timeout") ||
          str_detect(str_to_lower(prev_non_sub$text[[1]] %||% ""), "timeout")
      )

    dead_ball_id <- paste0(group_rows$game_id[[1]], "_db_", sprintf("%03d", db))
    team_ids_in_order <- unique(as.character(group_rows$team_id))
    team_ids_in_order <- team_ids_in_order[!is.na(team_ids_in_order)]

    for (team_pos in seq_along(team_ids_in_order)) {
      tid <- team_ids_in_order[[team_pos]]
      team_rows <- group_rows %>%
        filter(as.character(team_id) == tid) %>%
        arrange(sequence_number, game_play_number, pbp_index)
      if (nrow(team_rows) == 0) next

      lineup_before_vec <- if (tid %in% names(lineups)) lineups[[tid]] else character()
      lineup_after_vec <- lineup_before_vec
      team_roster <- if (tid %in% names(roster_map)) roster_map[[tid]] else tibble()

      player_out_names <- character()
      player_in_names <- character()
      athlete_ids_out <- integer()
      athlete_ids_in <- integer()

      for (k in seq_len(nrow(team_rows))) {
        sub_info <- parse_sub_row(team_rows$text[[k]])
        resolved <- resolve_player_record(sub_info$player_name, team_roster)
        player_name <- resolved$full_name %||% sub_info$player_name
        athlete_id_value <- suppressWarnings(as.integer(team_rows$athlete_id_1[[k]] %||% resolved$athlete_id %||% NA_integer_))

        if (sub_info$direction == "out") {
          player_out_names <- c(player_out_names, player_name)
          athlete_ids_out <- c(athlete_ids_out, athlete_id_value)
        } else if (sub_info$direction == "in") {
          player_in_names <- c(player_in_names, player_name)
          athlete_ids_in <- c(athlete_ids_in, athlete_id_value)
        }

        lineup_after_vec <- apply_substitution_name(lineup_after_vec, player_name, sub_info$direction)
      }

      if (tid %in% names(lineups)) {
        lineups[[tid]] <- lineup_after_vec
      }

      away_score_int <- suppressWarnings(as.integer(team_rows$away_score[[1]] %||% NA_integer_))
      home_score_int <- suppressWarnings(as.integer(team_rows$home_score[[1]] %||% NA_integer_))
      margin_before <- if (isTRUE(home_is_uconn)) {
        home_score_int - away_score_int
      } else if (isTRUE(away_is_uconn)) {
        away_score_int - home_score_int
      } else if (!is.na(uconn_is_home_value)) {
        if (isTRUE(uconn_is_home_value)) home_score_int - away_score_int else away_score_int - home_score_int
      } else {
        NA_integer_
      }
      margin_after <- margin_before
      is_uconn_team <- if (!is.na(uconn_team_id)) tid == uconn_team_id else NA
      team_margin_before <- if (is.na(is_uconn_team)) NA_integer_ else if (isTRUE(is_uconn_team)) margin_before else -margin_before
      team_margin_after <- team_margin_before
      one_possession_flag <- !is.na(margin_before) && abs(margin_before) <= 3L
      clutch_flag <- !is.na(team_rows$start_game_seconds_remaining[[1]]) &&
        team_rows$start_game_seconds_remaining[[1]] <= 300L &&
        !is.na(margin_before) &&
        abs(margin_before) <= 5L

      out_i <- out_i + 1L
      out_rows[[out_i]] <- tibble(
        game_file = game_file_value,
        game_id = suppressWarnings(as.integer(team_rows$game_id[[1]] %||% NA_integer_)),
        dead_ball_id = dead_ball_id,
        sub_group_id = paste0(dead_ball_id, "_t_", tid),
        team_order_in_dead_ball = as.integer(team_pos),
        sequence_number_start = suppressWarnings(as.numeric(min(team_rows$sequence_number, na.rm = TRUE))),
        sequence_number_end = suppressWarnings(as.numeric(max(team_rows$sequence_number, na.rm = TRUE))),
        game_play_number_start = suppressWarnings(as.integer(min(team_rows$game_play_number, na.rm = TRUE))),
        game_play_number_end = suppressWarnings(as.integer(max(team_rows$game_play_number, na.rm = TRUE))),
        play_id_start = as.character(team_rows$play_id[[1]] %||% NA_character_),
        play_id_end = as.character(team_rows$play_id[[nrow(team_rows)]] %||% NA_character_),
        game_date = as.Date(team_rows$game_date[[1]] %||% NA_character_),
        season = suppressWarnings(as.integer(team_rows$season[[1]] %||% NA_integer_)),
        season_type = suppressWarnings(as.integer(team_rows$season_type[[1]] %||% NA_integer_)),
        opponent = opponent_value,
        uconn_is_home = as.logical(uconn_is_home_value),
        site_type = site_type_value,
        period_number = suppressWarnings(as.integer(team_rows$period_number[[1]] %||% NA_integer_)),
        clock_display_value = as.character(team_rows$clock_display_value[[1]] %||% NA_character_),
        start_game_seconds_remaining = suppressWarnings(as.integer(team_rows$start_game_seconds_remaining[[1]] %||% NA_integer_)),
        end_game_seconds_remaining = suppressWarnings(as.integer(team_rows$end_game_seconds_remaining[[1]] %||% NA_integer_)),
        team_id = suppressWarnings(as.integer(tid)),
        team_name = team_name_for_id(tid),
        is_uconn_team = is_uconn_team,
        sub_rows = as.integer(nrow(team_rows)),
        players_out = collapse_chr_values(player_out_names),
        players_in = collapse_chr_values(player_in_names),
        athlete_ids_out = collapse_int_values(athlete_ids_out),
        athlete_ids_in = collapse_int_values(athlete_ids_in),
        lineup_before = collapse_lineup(lineup_before_vec),
        lineup_after = collapse_lineup(lineup_after_vec),
        lineup_before_key = canonicalize_lineup_key(collapse_lineup(lineup_before_vec)),
        lineup_after_key = canonicalize_lineup_key(collapse_lineup(lineup_after_vec)),
        away_score = away_score_int,
        home_score = home_score_int,
        margin_before = margin_before,
        margin_after = margin_after,
        team_margin_before = team_margin_before,
        team_margin_after = team_margin_after,
        one_possession_flag = one_possession_flag,
        clutch_flag = clutch_flag,
        home_timeout_called = any(team_rows$home_timeout_called %in% TRUE),
        away_timeout_called = any(team_rows$away_timeout_called %in% TRUE),
        after_timeout_flag = after_timeout_flag,
        raw_sub_text = paste(team_rows$text, collapse = " || ")
      )
    }
  }

  bind_rows(out_rows)
}

overwrite <- to_bool(arg_value("overwrite", "false"), default = FALSE)
include_exhibitions <- to_bool(arg_value("include-exhibitions", "false"), default = FALSE)
out_path <- arg_value("out-path", "_data/02_derived_inputs/v04_sub_groups.csv")
summary_path <- arg_value("summary-path", "_data/02_derived_inputs/v04_sub_groups_generation_summary.csv")
game_files_arg <- arg_value("game-files", "")

dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(summary_path), recursive = TRUE, showWarnings = FALSE)

if (file.exists(out_path) && !overwrite) {
  stop("Output file already exists. Re-run with --overwrite=true to rebuild.", call. = FALSE)
}

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
tables <- list()
table_i <- 0L

for (i in seq_len(nrow(games_meta))) {
  g <- games_meta[i, ]
  game_file <- g$game_file[[1]]

  event_id <- find_event_id(
    game_date = g$game_date_parsed[[1]],
    opponent = g$opponent[[1]],
    uconn_is_home = g$uconn_is_home_flag[[1]]
  )

  if (is.na(event_id)) {
    message("[MISS] ", game_file, " (no matching ESPN event)")
    results[[length(results) + 1]] <- tibble(game_file = game_file, status = "missing_event", game_id = NA_integer_, rows = NA_integer_)
    next
  }

  message("[RUN ] ", game_file, " -> event ", event_id)
  pbp <- espn_mbb_pbp(game_id = event_id) %>%
    mutate(
      sequence_number = suppressWarnings(as.numeric(sequence_number)),
      period_number = suppressWarnings(as.integer(period_number)),
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
  out_tbl <- build_sub_group_table(
    pbp = pbp,
    rosters = rosters,
    game_meta = g,
    season_context = season_context
  )

  table_i <- table_i + 1L
  tables[[table_i]] <- out_tbl
  results[[length(results) + 1]] <- tibble(
    game_file = game_file,
    status = "written",
    game_id = as.integer(event_id),
    rows = nrow(out_tbl)
  )
  message("[DONE] ", game_file, " (rows=", nrow(out_tbl), ")")
}

res <- bind_rows(results)
final_tbl <- bind_rows(tables) %>%
  arrange(game_date, game_id, period_number, sequence_number_start, team_order_in_dead_ball)

write_csv(final_tbl, out_path, na = "")
write_csv(res, summary_path, na = "")

message("\nComplete.")
message("Output: ", normalizePath(out_path))
message("Summary: ", normalizePath(summary_path))
message("Rows written: ", nrow(final_tbl))
message("Games written: ", sum(res$status == "written", na.rm = TRUE))
message("Missing events: ", sum(res$status == "missing_event", na.rm = TRUE))
