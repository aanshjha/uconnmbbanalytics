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
