fit_has_draws <- function(fit, par) {
  if (is.null(fit)) return(FALSE)

  draws_n <- tryCatch(nrow(as.data.frame(fit)), error = function(e) 0)
  if (is.na(draws_n) || draws_n == 0) return(FALSE)

  arr <- tryCatch(rstan::extract(fit, pars = par)[[par]], error = function(e) NULL)
  if (is.null(arr)) return(FALSE)

  dims <- dim(arr)
  if (is.null(dims) || length(dims) < 2) return(FALSE)

  TRUE
}

safe_md5 <- function(path) {
  x <- tryCatch(unname(tools::md5sum(path)), error = function(e) NA_character_)
  as.character(x[[1]])
}

ms_to_seconds <- function(x) {
  x <- as.character(x)
  x <- stringr::str_trim(x)
  suppressWarnings(lubridate::period_to_seconds(lubridate::ms(x)))
}

parse_period_index <- function(period) {
  p <- stringr::str_trim(as.character(period))
  out <- suppressWarnings(as.integer(p))

  idx1 <- is.na(out) & stringr::str_detect(p, stringr::regex("^1st\\s+Half", ignore_case = TRUE))
  idx2 <- is.na(out) & stringr::str_detect(p, stringr::regex("^2nd\\s+Half", ignore_case = TRUE))
  out[idx1] <- 1L
  out[idx2] <- 2L

  ot_match <- stringr::str_match(p, stringr::regex("^OT\\s*(\\d+)", ignore_case = TRUE))[, 2]
  ot_num <- suppressWarnings(as.integer(ot_match))
  idx_ot <- is.na(out) & !is.na(ot_num)
  out[idx_ot] <- 2L + ot_num[idx_ot]

  out
}

canonicalize_lineup <- function(lineup_vec) {
  vapply(
    stringr::str_split(lineup_vec, "\\|"),
    function(x) paste(sort(stringr::str_trim(x)), collapse = "|"),
    character(1)
  )
}

split_lineup_players <- function(lineup_key) {
  toks <- unlist(stringr::str_split(as.character(lineup_key), "\\|"))
  toks <- stringr::str_trim(toks)
  toks[nzchar(toks)]
}

normalize_player_token <- function(x) {
  x <- as.character(x)
  norm <- vapply(x, function(tok) {
    tok <- stringr::str_squish(tok)
    if (grepl(",", tok, fixed = TRUE)) {
      parts <- stringr::str_split(tok, ",", simplify = TRUE)
      if (ncol(parts) >= 2) {
        lhs <- stringr::str_trim(parts[1])
        rhs <- stringr::str_trim(parts[2])
        if (nzchar(lhs) && nzchar(rhs)) tok <- paste(rhs, lhs)
      }
    }
    tok <- tolower(gsub("[^[:alnum:] ]", " ", tok, perl = TRUE))
    parts <- unlist(stringr::str_split(tok, "\\s+"))
    parts <- parts[nzchar(parts)]
    if (length(parts) == 0) return("")
    paste(sort(parts), collapse = "")
  }, character(1))
  norm[nzchar(norm)]
}

split_lineup_players_norm <- function(lineup_key) {
  toks <- split_lineup_players(lineup_key)
  unique(normalize_player_token(toks))
}

normalize_player_key <- function(player_vec) {
  vapply(as.character(player_vec), function(tok) {
    norm <- normalize_player_token(tok)
    if (length(norm) == 0) return(NA_character_)
    as.character(norm[[1]])
  }, character(1))
}

canonicalize_lineup_norm <- function(lineup_vec) {
  vapply(as.character(lineup_vec), function(lineup_key) {
    toks <- sort(unique(split_lineup_players_norm(lineup_key)))
    if (length(toks) == 0) return(NA_character_)
    paste(toks, collapse = "|")
  }, character(1))
}

lineup_pair_keys <- function(lineup_key) {
  players <- sort(split_lineup_players(lineup_key))
  if (length(players) < 2) return(character(0))
  vapply(combn(players, 2, simplify = FALSE), function(x) paste(x, collapse = "|"), character(1))
}

lineup_pair_keys_norm <- function(lineup_key) {
  players <- sort(split_lineup_players_norm(lineup_key))
  if (length(players) < 2) return(character(0))
  vapply(combn(players, 2, simplify = FALSE), function(x) paste(x, collapse = "|"), character(1))
}

lineup_trio_keys <- function(lineup_key) {
  players <- sort(split_lineup_players(lineup_key))
  if (length(players) < 3) return(character(0))
  vapply(combn(players, 3, simplify = FALSE), function(x) paste(x, collapse = "|"), character(1))
}

load_player_archetypes <- function(archetype_path, active_players = NULL) {
  if (!file.exists(archetype_path)) {
    stop("Missing required archetype file: ", archetype_path, call. = FALSE)
  }

  arche <- readr::read_csv(archetype_path, show_col_types = FALSE) %>%
    dplyr::mutate(
      player = stringr::str_trim(as.character(player)),
      archetype = toupper(stringr::str_trim(as.character(archetype)))
    )

  required <- c("player", "archetype")
  missing <- setdiff(required, names(arche))
  if (length(missing) > 0) {
    stop("player_archetypes.csv missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  arche <- arche %>%
    dplyr::filter(!is.na(player), nzchar(player), !is.na(archetype), nzchar(archetype))

  allowed <- c("GUARD", "WING", "BIG")
  bad <- arche %>%
    dplyr::filter(!(archetype %in% allowed))
  if (nrow(bad) > 0) {
    stop(
      "player_archetypes.csv has invalid archetype values. Allowed: GUARD,WING,BIG. Bad players: ",
      paste(utils::head(bad$player, 10), collapse = ", "),
      call. = FALSE
    )
  }

  dup <- arche %>%
    dplyr::count(player, name = "n") %>%
    dplyr::filter(n > 1)
  if (nrow(dup) > 0) {
    stop(
      "player_archetypes.csv has duplicate players. Examples: ",
      paste(utils::head(dup$player, 10), collapse = ", "),
      call. = FALSE
    )
  }

  if (!is.null(active_players) && length(active_players) > 0) {
    active_tbl <- tibble::tibble(player = sort(unique(stringr::str_trim(as.character(active_players)))))
    missing_players <- active_tbl %>%
      dplyr::anti_join(arche %>% dplyr::select(player), by = "player")
    if (nrow(missing_players) > 0) {
      stop(
        "player_archetypes.csv is missing active players: ",
        paste(missing_players$player, collapse = ", "),
        call. = FALSE
      )
    }
  }

  arche %>% dplyr::select(player, archetype)
}

compute_lineup_archetype_balance <- function(lineup_key, arche_map) {
  if (is.null(arche_map) || nrow(arche_map) == 0) return(0.5)
  players <- split_lineup_players(lineup_key)
  if (length(players) == 0) return(0.5)

  map <- stats::setNames(arche_map$archetype, arche_map$player)
  arche <- unname(map[players])
  arche <- arche[!is.na(arche)]
  if (length(arche) == 0) return(0.5)

  cnt_guard <- sum(arche == "GUARD")
  cnt_wing <- sum(arche == "WING")
  cnt_big <- sum(arche == "BIG")

  # Best shape is 2 guards + 2 wings + 1 big. Score in [0,1].
  deviation <- abs(cnt_guard - 2) + abs(cnt_wing - 2) + abs(cnt_big - 1)
  score <- 1 - (deviation / 8)
  max(0, min(1, score))
}

load_manual_defensive_events <- function(manual_root, games_joined = NULL) {
  if (!dir.exists(manual_root)) {
    return(tibble::tibble())
  }

  subdirs <- c("_conf", "_nc", "_bet", "_ncaatourn")
  roots <- file.path(manual_root, subdirs)
  files <- unlist(lapply(roots[file.exists(roots)], function(d) {
    list.files(d, pattern = "\\.csv$", full.names = TRUE, recursive = FALSE)
  }), use.names = FALSE)
  files <- files[!grepl("_espn_generation_summary\\.csv$", files)]
  if (length(files) == 0) return(tibble::tibble())

  all_rows <- lapply(files, function(path) {
    tryCatch(
      readr::read_csv(path, show_col_types = FALSE),
      error = function(e) NULL
    )
  })
  all_rows <- all_rows[!vapply(all_rows, is.null, logical(1))]
  if (length(all_rows) == 0) return(tibble::tibble())

  dat <- dplyr::bind_rows(all_rows)
  required <- c("game_file", "game_play_number", "is_uconn_offense", "score_value", "defense_lineup_key")
  missing <- setdiff(required, names(dat))
  if (length(missing) > 0) {
    stop("Manual game CSVs missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  if (!("game_date" %in% names(dat))) dat$game_date <- NA_character_

  out <- dat %>%
    dplyr::mutate(
      game_file = as.character(game_file),
      game_play_number = suppressWarnings(as.integer(game_play_number)),
      is_uconn_offense = as.logical(is_uconn_offense),
      score_value = suppressWarnings(as.numeric(score_value)),
      defense_lineup_key = canonicalize_lineup(defense_lineup_key),
      game_date = suppressWarnings(lubridate::ymd(game_date))
    ) %>%
    dplyr::filter(
      !is.na(game_file),
      nzchar(game_file),
      !is.na(game_play_number),
      !is.na(is_uconn_offense),
      is_uconn_offense == FALSE,
      !is.na(defense_lineup_key),
      nzchar(defense_lineup_key)
    ) %>%
    dplyr::group_by(game_file, game_play_number) %>%
    dplyr::arrange(dplyr::desc(!is.na(score_value))) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      stop_event = dplyr::if_else(!is.na(score_value) & score_value <= 0, 1, 0)
    ) %>%
    dplyr::select(game_file, game_play_number, game_date, defense_lineup_key, stop_event)

  if (!is.null(games_joined) && nrow(games_joined) > 0 && "game_file" %in% names(games_joined)) {
    join_cols <- c("game_file")
    add_cols <- character(0)
    if ("global_game_id" %in% names(games_joined)) add_cols <- c(add_cols, "global_game_id")
    if ("game_date_parsed" %in% names(games_joined)) add_cols <- c(add_cols, "game_date_parsed")
    out <- out %>%
      dplyr::left_join(games_joined %>% dplyr::select(dplyr::all_of(c(join_cols, add_cols))), by = "game_file")
  }

  out
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

fit_scaler <- function(x) {
  x <- as.numeric(x)
  m <- mean(x, na.rm = TRUE)
  s <- sd(x, na.rm = TRUE)
  if (!is.finite(m)) m <- 0
  if (!is.finite(s) || s <= 0) s <- NA_real_
  list(center = m, scale = s)
}

apply_scaler <- function(x, scaler) {
  x <- as.numeric(x)
  if (is.null(scaler) || !is.finite(scaler$center) || !is.finite(scaler$scale) || scaler$scale <= 0) {
    out <- rep(0, length(x))
  } else {
    out <- (x - scaler$center) / scaler$scale
  }
  out[!is.finite(out)] <- 0
  out
}

weighted_mean_safe <- function(x, w) {
  x <- as.numeric(x)
  w <- as.numeric(w)
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

clamp_prob <- function(p, eps = 1e-4) {
  p <- as.numeric(p)
  pmin(pmax(p, eps), 1 - eps)
}

load_common_lineup_model_inputs <- function(stints_path,
                                            games_path,
                                            opp_path,
                                            add_global_game_id = FALSE) {
  stints <- readr::read_csv(
    stints_path,
    show_col_types = FALSE,
    col_types = readr::cols(
      start_time = readr::col_character(),
      end_time = readr::col_character()
    )
  )

  games <- readr::read_csv(games_path, show_col_types = FALSE)
  if (!("site_type" %in% names(games))) games$site_type <- NA_character_

  games <- games %>%
    dplyr::filter(!stringr::str_detect(game_file, "Exhibition"))
  stints <- stints %>%
    dplyr::filter(!stringr::str_detect(game_file, "Exhibition")) %>%
    dplyr::mutate(
      poss_est = as.numeric(poss_est),
      points_for = as.numeric(points_for),
      points_against = as.numeric(points_against),
      net_pts = points_for - points_against
    )

  tiny_poss <- stints %>%
    dplyr::filter(!is.na(poss_est), poss_est > 0, poss_est < 1) %>%
    dplyr::select(
      game_file,
      period,
      stint_index,
      start_time,
      end_time,
      poss_est,
      points_for,
      points_against,
      net_pts
    )

  stints <- stints %>%
    dplyr::mutate(
      poss_est = dplyr::if_else(!is.na(poss_est) & poss_est > 0 & poss_est < 1, 1, poss_est),
      net_ppp = dplyr::if_else(!is.na(poss_est) & poss_est > 0, net_pts / poss_est, NA_real_),
      start_sec = ms_to_seconds(start_time),
      end_sec = ms_to_seconds(end_time),
      dur_sec_raw = start_sec - end_sec,
      dur_sec = dplyr::case_when(
        is.na(dur_sec_raw) ~ NA_real_,
        dur_sec_raw >= 0 ~ dur_sec_raw,
        TRUE ~ abs(dur_sec_raw)
      ),
      dur_min = dur_sec / 60,
      period_num = parse_period_index(period),
      start_clock_sec = start_sec,
      end_clock_sec = end_sec,
      elapsed_game_sec = dplyr::case_when(
        is.na(period_num) | is.na(start_clock_sec) ~ NA_real_,
        period_num <= 1 ~ pmax(0, 1200 - start_clock_sec),
        period_num == 2 ~ 1200 + pmax(0, 1200 - start_clock_sec),
        TRUE ~ 2400 + pmax(0, period_num - 3) * 300 + pmax(0, 300 - pmin(start_clock_sec, 300))
      ),
      .row_id_restore = dplyr::row_number()
    ) %>%
    dplyr::group_by(game_file) %>%
    dplyr::arrange(period_num, dplyr::desc(start_clock_sec), stint_index, .by_group = TRUE) %>%
    dplyr::mutate(
      score_margin_start = dplyr::lag(cumsum(dplyr::coalesce(net_pts, 0)), default = 0)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.row_id_restore) %>%
    dplyr::select(-.row_id_restore)

  opponent_controls <- readr::read_csv(opp_path, show_col_types = FALSE) %>%
    dplyr::mutate(
      game_date = format(lubridate::mdy(game_date), "%m/%d/%y"),
      opponent = stringr::str_trim(opponent)
    )

  games <- games %>%
    dplyr::mutate(
      game_date_parsed = suppressWarnings(lubridate::mdy(game_date)),
      game_date = format(game_date_parsed, "%m/%d/%y"),
      opponent = stringr::str_trim(opponent),
      site_home = dplyr::if_else(uconn_is_home, 1.0, 0.0)
    )

  if (isTRUE(add_global_game_id)) {
    games <- games %>%
      dplyr::arrange(game_date_parsed, game_file) %>%
      dplyr::mutate(global_game_id = dplyr::row_number())
  }

  games_joined <- games %>%
    dplyr::left_join(opponent_controls, by = c("game_date", "opponent"))

  list(
    stints = stints,
    games = games,
    games_joined = games_joined,
    opponent_controls = opponent_controls,
    tiny_poss = tiny_poss
  )
}
