suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(DT)
})

source("_scripts/dashboard/data_loader.R")

pretty_label <- function(raw) {
  tools::toTitleCase(gsub("_", " ", raw))
}

with_trace_colnames <- function(df) {
  if (is.null(df) || ncol(df) == 0) return(df)
  out <- df
  names(out) <- sprintf("%s (%s)", pretty_label(names(df)), names(df))
  out
}

column_trace_table <- function(df) {
  if (is.null(df) || ncol(df) == 0) {
    return(data.frame(raw_field = character(), coach_label = character(), type = character(), stringsAsFactors = FALSE))
  }

  data.frame(
    raw_field = names(df),
    coach_label = pretty_label(names(df)),
    type = vapply(df, function(x) paste(class(x), collapse = ","), character(1)),
    stringsAsFactors = FALSE
  )
}

fmt_pct <- function(x, digits = 1) {
  if (!is.finite(x)) return("--")
  paste0(format(round(100 * x, digits), nsmall = digits, trim = TRUE), "%")
}

fmt_num <- function(x, digits = 2) {
  if (!is.finite(x)) return("--")
  format(round(x, digits), nsmall = digits, trim = TRUE, big.mark = ",")
}

fmt_int <- function(x) {
  if (!is.finite(x)) return("--")
  format(as.integer(round(x)), big.mark = ",", trim = TRUE)
}

num_col_or_na <- function(df, col) {
  if (!col %in% names(df)) return(rep(NA_real_, nrow(df)))
  to_num(df[[col]])
}

first_num_or_na <- function(x) {
  v <- to_num(x)
  if (length(v) == 0) return(NA_real_)
  v[[1]]
}

first_nonempty <- function(...) {
  vals <- list(...)
  for (v in vals) {
    txt <- trimws(to_chr(v))
    txt <- txt[!is.na(txt) & txt != ""]
    if (length(txt) > 0) return(txt[[1]])
  }
  "--"
}

split_five <- function(x, fallback = NULL) {
  txt <- trimws(to_chr(x))
  txt <- txt[!is.na(txt) & txt != ""]

  if (length(txt) == 0 && !is.null(fallback)) {
    txt <- trimws(to_chr(fallback))
    txt <- txt[!is.na(txt) & txt != ""]
  }

  if (length(txt) == 0) return(rep("--", 5))

  first <- txt[[1]]
  delim <- if (grepl(",", first, fixed = TRUE)) "," else "\\|"
  parts <- trimws(unlist(strsplit(first, delim, perl = TRUE)))
  parts <- parts[parts != ""]
  if (length(parts) == 0) return(rep("--", 5))

  out <- c(parts, rep("--", 5))
  out[seq_len(5)]
}

metric_pair <- function(u_val, o_val, pct = FALSE, digits = 1, u_reason = "", o_reason = "") {
  u_txt <- if (pct) fmt_pct(u_val, digits = digits) else if (digits == 0) fmt_int(u_val) else fmt_num(u_val, digits = digits)
  o_txt <- if (pct) fmt_pct(o_val, digits = digits) else if (digits == 0) fmt_int(o_val) else fmt_num(o_val, digits = digits)

  reasons <- character()
  if (!is.finite(u_val) && nz(u_reason)) reasons <- c(reasons, paste0("UConn: ", u_reason))
  if (!is.finite(o_val) && nz(o_reason)) reasons <- c(reasons, paste0("Opp: ", o_reason))

  list(
    u_txt = u_txt,
    o_txt = o_txt,
    reason = paste(reasons, collapse = " | ")
  )
}

missing_cols <- function(df, cols) {
  setdiff(cols, names(df))
}

sum_col <- function(df, col) {
  sum(to_num(df[[col]]), na.rm = TRUE)
}

calc_side_four_factors <- function(df_side, poss_den) {
  if (is.null(df_side) || nrow(df_side) == 0) {
    return(list(
      efg = list(value = NA_real_, reason = "No side rows"),
      to = list(value = NA_real_, reason = "No side rows"),
      oreb = list(value = NA_real_, reason = "No side rows"),
      ftr = list(value = NA_real_, reason = "No side rows")
    ))
  }

  miss_efg <- missing_cols(df_side, c("FGM", "FGM3", "FGA"))
  if (length(miss_efg) == 0) {
    fga <- sum_col(df_side, "FGA")
    fgm <- sum_col(df_side, "FGM")
    fgm3 <- sum_col(df_side, "FGM3")
    efg <- if (is.finite(fga) && fga > 0) (fgm + 0.5 * fgm3) / fga else NA_real_
    efg_reason <- if (is.finite(fga) && fga > 0) "" else "FGA denominator missing"
  } else {
    efg <- NA_real_
    efg_reason <- sprintf("Missing columns: %s", paste(miss_efg, collapse = ", "))
  }

  miss_to <- missing_cols(df_side, c("TOV"))
  to_reason_parts <- character()
  if (length(miss_to) > 0) {
    to_reason_parts <- c(to_reason_parts, sprintf("Missing columns: %s", paste(miss_to, collapse = ", ")))
  }
  if (!(is.finite(poss_den) && poss_den > 0)) {
    to_reason_parts <- c(to_reason_parts, "Poss denominator missing")
  }
  if (length(to_reason_parts) == 0) {
    to_rate <- sum_col(df_side, "TOV") / poss_den
    to_reason <- ""
  } else {
    to_rate <- NA_real_
    to_reason <- paste(to_reason_parts, collapse = " | ")
  }

  miss_oreb <- missing_cols(df_side, c("OREB", "DREB"))
  if (length(miss_oreb) == 0) {
    oreb <- sum_col(df_side, "OREB")
    dreb <- sum_col(df_side, "DREB")
    oreb_den <- oreb + dreb
    oreb_rate <- if (is.finite(oreb_den) && oreb_den > 0) oreb / oreb_den else NA_real_
    oreb_reason <- if (is.finite(oreb_den) && oreb_den > 0) "" else "OREB+DREB denominator missing"
  } else {
    oreb_rate <- NA_real_
    oreb_reason <- sprintf("Missing columns: %s", paste(miss_oreb, collapse = ", "))
  }

  miss_ftr <- missing_cols(df_side, c("FTA", "FGA"))
  if (length(miss_ftr) == 0) {
    fga_ftr <- sum_col(df_side, "FGA")
    fta <- sum_col(df_side, "FTA")
    ftr <- if (is.finite(fga_ftr) && fga_ftr > 0) fta / fga_ftr else NA_real_
    ftr_reason <- if (is.finite(fga_ftr) && fga_ftr > 0) "" else "FGA denominator missing"
  } else {
    ftr <- NA_real_
    ftr_reason <- sprintf("Missing columns: %s", paste(miss_ftr, collapse = ", "))
  }

  list(
    efg = list(value = efg, reason = efg_reason),
    to = list(value = to_rate, reason = to_reason),
    oreb = list(value = oreb_rate, reason = oreb_reason),
    ftr = list(value = ftr, reason = ftr_reason)
  )
}

calc_third_row_metrics <- function(u_rows, scheme, oreb_pair) {
  values <- list(
    paint = "--",
    mid_range = "--",
    three = "--",
    off_bounce = "--",
    side_cs = if (scheme$available) scheme$values$side_cs %||% "--" else "--",
    inside_cs = if (scheme$available) scheme$values$inside_cs %||% "--" else "--",
    post_up = if (scheme$available) scheme$values$post_up %||% "--" else "--",
    shot_clock = if (scheme$available) scheme$values$shot_clock %||% "--" else "--",
    oreb = oreb_pair$u_txt,
    specialty = if (scheme$available) scheme$values$specialty %||% "--" else "--"
  )

  reasons <- character()
  if (is.null(u_rows) || nrow(u_rows) == 0) {
    reasons <- c(reasons, "No UConn offense rows in selected game")
    return(list(values = values, reason = paste(reasons, collapse = " | ")))
  }

  have_fga <- "FGA" %in% names(u_rows)
  total_fga <- if (have_fga) sum_col(u_rows, "FGA") else NA_real_

  if (!have_fga) {
    reasons <- c(reasons, "Missing columns for paint/mid/three: FGA")
  }

  if (have_fga && "paint_zone" %in% names(u_rows)) {
    if (is.finite(total_fga) && total_fga > 0) {
      paint_fga <- sum(to_num(u_rows$FGA)[toupper(to_chr(u_rows$paint_zone)) == "PAINT"], na.rm = TRUE)
      values$paint <- fmt_pct(paint_fga / total_fga, 1)
    } else {
      reasons <- c(reasons, "Paint share unavailable: FGA denominator missing")
    }
  } else {
    reasons <- c(reasons, "Missing columns for paint share: paint_zone")
  }

  if (have_fga && "shot_zone" %in% names(u_rows)) {
    if (is.finite(total_fga) && total_fga > 0) {
      mids <- toupper(to_chr(u_rows$shot_zone)) %in% c("SHORT_MID", "LONG_MID")
      mid_fga <- sum(to_num(u_rows$FGA)[mids], na.rm = TRUE)
      values$mid_range <- fmt_pct(mid_fga / total_fga, 1)
    } else {
      reasons <- c(reasons, "Mid-range share unavailable: FGA denominator missing")
    }
  } else {
    reasons <- c(reasons, "Missing columns for mid-range share: shot_zone")
  }

  if (have_fga && "FGA3" %in% names(u_rows)) {
    if (is.finite(total_fga) && total_fga > 0) {
      values$three <- fmt_pct(sum_col(u_rows, "FGA3") / total_fga, 1)
    } else {
      reasons <- c(reasons, "Three share unavailable: FGA denominator missing")
    }
  } else {
    reasons <- c(reasons, "Missing columns for three share: FGA3")
  }

  if (all(c("FGM", "AssistPlayer") %in% names(u_rows))) {
    made_fg_u <- sum_col(u_rows, "FGM")
    self_created_makes <- sum(to_num(u_rows$FGM)[!nz(u_rows$AssistPlayer)], na.rm = TRUE)
    if (is.finite(made_fg_u) && made_fg_u > 0) {
      values$off_bounce <- fmt_pct(self_created_makes / made_fg_u, 1)
    } else {
      reasons <- c(reasons, "Off-bounce unavailable: FGM denominator missing")
    }
  } else {
    miss <- setdiff(c("FGM", "AssistPlayer"), names(u_rows))
    reasons <- c(reasons, sprintf("Missing columns for off-bounce: %s", paste(miss, collapse = ", ")))
  }

  list(values = values, reason = paste(unique(reasons), collapse = " | "))
}

row_pattern_count <- function(df, cols, pattern) {
  if (is.null(df) || nrow(df) == 0) return(0L)
  cols <- intersect(cols, names(df))
  if (length(cols) == 0) return(0L)

  flags <- rep(FALSE, nrow(df))
  for (cn in cols) {
    vals <- toupper(trimws(to_chr(df[[cn]])))
    flags <- flags | grepl(pattern, vals, perl = TRUE)
  }

  sum(flags, na.rm = TRUE)
}

derive_is_shot <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(logical())
  shooting_flag <- if ("shooting_play" %in% names(df)) as_logical_vec(df$shooting_play) else rep(FALSE, nrow(df))
  fga <- num_col_or_na(df, "FGA")
  fta <- num_col_or_na(df, "FTA")
  (shooting_flag %in% TRUE) | (is.finite(fga) & fga > 0) | (is.finite(fta) & fta > 0)
}

derive_shot_type <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(character())

  n <- nrow(df)
  out <- rep("Non-shot play", n)
  is_shot <- derive_is_shot(df)

  fga <- num_col_or_na(df, "FGA")
  fga3 <- num_col_or_na(df, "FGA3")
  fta <- num_col_or_na(df, "FTA")

  type_txt <- if ("type_text" %in% names(df)) toupper(trimws(to_chr(df$type_text))) else rep("", n)
  zone_txt <- if ("shot_zone" %in% names(df)) toupper(trimws(to_chr(df$shot_zone))) else rep("", n)
  detail_txt <- if ("shot_zone_detail" %in% names(df)) toupper(trimws(to_chr(df$shot_zone_detail))) else rep("", n)

  out[is_shot] <- "Two"
  out[is_shot & is.finite(fta) & fta > 0 & !(is.finite(fga) & fga > 0)] <- "Free Throw"
  out[is_shot & is.finite(fga3) & fga3 > 0] <- "Three"
  out[is_shot & out == "Two" & zone_txt %in% c("SHORT_MID", "LONG_MID")] <- "Mid-Range"
  out[is_shot & out == "Two" & zone_txt %in% c("PAINT", "RESTRICTED_AREA")] <- "At Rim"
  out[is_shot & out == "Two" & grepl("LAYUP|DUNK|TIP", type_txt)] <- "At Rim"
  out[is_shot & out == "Two" & grepl("THREE|3", detail_txt)] <- "Three"
  out[is_shot & out == "Two" & grepl("MID", detail_txt)] <- "Mid-Range"
  out
}

derive_shot_location_zone <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(character())

  n <- nrow(df)
  zone <- rep("No shot location", n)
  detail <- if ("shot_zone_detail" %in% names(df)) trimws(to_chr(df$shot_zone_detail)) else rep("", n)
  shot_zone <- if ("shot_zone" %in% names(df)) trimws(to_chr(df$shot_zone)) else rep("", n)
  shot_side <- if ("shot_side" %in% names(df)) trimws(to_chr(df$shot_side)) else rep("", n)
  paint_zone <- if ("paint_zone" %in% names(df)) trimws(to_chr(df$paint_zone)) else rep("", n)
  is_shot <- derive_is_shot(df)

  for (i in seq_len(n)) {
    if (!isTRUE(is_shot[[i]])) next

    base <- ""
    if (nz(detail[[i]]) && toupper(detail[[i]]) != "UNKNOWN") {
      base <- detail[[i]]
    } else if (nz(shot_zone[[i]]) && toupper(shot_zone[[i]]) != "UNKNOWN") {
      base <- pretty_label(shot_zone[[i]])
    } else if (nz(paint_zone[[i]]) && toupper(paint_zone[[i]]) != "UNKNOWN") {
      base <- pretty_label(paint_zone[[i]])
    }

    if (!nz(base)) base <- "Shot location unavailable"
    if (nz(shot_side[[i]]) && toupper(shot_side[[i]]) != "UNKNOWN") {
      base <- paste0(base, " (", pretty_label(shot_side[[i]]), ")")
    }

    zone[[i]] <- base
  }

  zone
}

derive_shot_result <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(character())

  n <- nrow(df)
  out <- rep("Non-shot play", n)
  fga <- num_col_or_na(df, "FGA")
  fgm <- num_col_or_na(df, "FGM")
  fta <- num_col_or_na(df, "FTA")
  ftm <- num_col_or_na(df, "FTM")

  is_fg <- is.finite(fga) & fga > 0
  is_ft_only <- !is_fg & is.finite(fta) & fta > 0

  out[is_fg & is.finite(fgm) & fgm > 0] <- "Made FG"
  out[is_fg & !(is.finite(fgm) & fgm > 0)] <- "Missed FG"
  out[is_ft_only & is.finite(ftm) & ftm > 0] <- "Made FT"
  out[is_ft_only & !(is.finite(ftm) & ftm > 0)] <- "Missed FT"
  out
}

top_label_count <- function(x, default = "--") {
  vals <- trimws(to_chr(x))
  vals <- vals[!is.na(vals) & vals != "" & vals != "No shot location" & vals != "Shot location unavailable"]
  if (length(vals) == 0) return(default)

  tab <- sort(table(vals), decreasing = TRUE)
  top <- names(tab)[[1]]
  cnt <- as.integer(tab[[1]])
  sprintf("%s (%s)", top, cnt)
}

as_logical_strict <- function(x) {
  if (length(x) == 0) return(logical())
  if (is.logical(x)) return(ifelse(is.na(x), NA, x))

  txt <- tolower(trimws(to_chr(x)))
  out <- rep(NA, length(txt))
  out[txt %in% c("true", "t", "1", "yes", "y")] <- TRUE
  out[txt %in% c("false", "f", "0", "no", "n")] <- FALSE
  out
}

fill_logical_nearest <- function(x, default = FALSE) {
  n <- length(x)
  if (n == 0) return(logical())

  out <- as.logical(x)

  last_seen <- NA
  for (i in seq_len(n)) {
    if (!is.na(out[[i]])) {
      last_seen <- out[[i]]
    } else if (!is.na(last_seen)) {
      out[[i]] <- last_seen
    }
  }

  next_seen <- NA
  for (i in rev(seq_len(n))) {
    if (!is.na(out[[i]])) {
      next_seen <- out[[i]]
    } else if (!is.na(next_seen)) {
      out[[i]] <- next_seen
    }
  }

  out[is.na(out)] <- isTRUE(default)
  out
}

derive_possession_ids <- function(period_num, side_flag) {
  n <- length(side_flag)
  if (n == 0) return(integer())

  out <- integer(n)
  cur <- 0L
  prev_side <- NA
  prev_period <- NA_real_

  for (i in seq_len(n)) {
    this_side <- side_flag[[i]]
    this_period <- period_num[[i]]

    is_new <- i == 1L
    if (i > 1L) {
      side_change <- !is.na(this_side) && !is.na(prev_side) && (this_side != prev_side)
      period_change <- is.finite(this_period) && is.finite(prev_period) && (this_period != prev_period)
      is_new <- side_change || period_change
    }

    if (is_new) cur <- cur + 1L
    out[[i]] <- cur

    if (!is.na(this_side)) prev_side <- this_side
    if (is.finite(this_period)) prev_period <- this_period
  }

  out
}

ordered_unique_concat <- function(x, sep = " | ", default = "--") {
  vals <- trimws(to_chr(x))
  vals <- vals[!is.na(vals) & vals != ""]
  if (length(vals) == 0) return(default)
  vals <- vals[!duplicated(vals)]
  paste(vals, collapse = sep)
}

aggregate_possessions <- function(rows_side) {
  if (is.null(rows_side) || nrow(rows_side) == 0) return(data.frame())

  num_cols <- c("FGA", "FGM", "FTA", "FTM", "FGA3", "FGM3", "PTS", "OREB", "DREB", "AST", "TOV", "STL", "BLK")
  txt_cols <- c(
    "UsagePlayer", "AssistPlayer", "ReboundPlayer", "StealPlayer", "BlockPlayer",
    "OffenseOnCourt", "DefenseOnCourt", "type_text", "shot_zone", "shot_side", "paint_zone", "shot_zone_detail"
  )

  for (cn in num_cols) {
    if (!cn %in% names(rows_side)) rows_side[[cn]] <- NA_real_
    rows_side[[cn]] <- to_num(rows_side[[cn]])
  }

  for (cn in txt_cols) {
    if (!cn %in% names(rows_side)) rows_side[[cn]] <- ""
  }

  if (!"clock_label" %in% names(rows_side)) rows_side$clock_label <- "--"
  if (!"period_number" %in% names(rows_side)) rows_side$period_number <- NA_real_
  if (!"sequence_number" %in% names(rows_side)) rows_side$sequence_number <- NA_real_
  if (!"game_play_number" %in% names(rows_side)) rows_side$game_play_number <- NA_real_
  if (!"play_id" %in% names(rows_side)) rows_side$play_id <- NA_character_
  if (!"possession_event_count" %in% names(rows_side)) rows_side$possession_event_count <- 1L

  rows_side <- rows_side %>%
    mutate(
      period_num = to_num(.data$period_number),
      seq_num = to_num(.data$sequence_number),
      play_num = to_num(.data$game_play_number),
      play_id_chr = to_chr(.data$play_id)
    )

  rows_side %>%
    group_by(.data$possession_id, .data$possession_side) %>%
    summarise(
      period_number = suppressWarnings(as.integer(round(first(.data$period_num)))),
      clock_start = first(.data$clock_label),
      clock_end = dplyr::last(.data$clock_label),
      sequence_start = first(.data$seq_num),
      sequence_end = dplyr::last(.data$seq_num),
      game_play_start = first(.data$play_num),
      game_play_end = dplyr::last(.data$play_num),
      play_id_start = first(.data$play_id_chr),
      play_id_end = dplyr::last(.data$play_id_chr),
      possession_event_count = suppressWarnings(as.integer(round(first(.data$possession_event_count)))),
      across(all_of(num_cols), ~ sum(.x, na.rm = TRUE)),
      across(all_of(txt_cols), ~ ordered_unique_concat(.x)),
      .groups = "drop"
    ) %>%
    arrange(.data$possession_id)
}

draw_full_court <- function() {
  plot(
    c(-47, 47), c(-25, 25),
    type = "n", xlab = "", ylab = "", axes = FALSE,
    asp = 1, xaxs = "i", yaxs = "i"
  )

  rect(-47, -25, 47, 25, border = "#cbd5e1", col = "#f8fafc")
  segments(-47, 0, 47, 0, col = "#94a3b8", lwd = 1.1)
  symbols(0, 0, circles = 6, inches = FALSE, add = TRUE, fg = "#cbd5e1", bg = NA)

  rect(-41.75, -6, -28, 6, border = "#94a3b8", lwd = 1.1)
  rect(28, -6, 41.75, 6, border = "#94a3b8", lwd = 1.1)
  points(c(-41.75, 41.75), c(0, 0), pch = 21, bg = "#0a2240", col = "#0a2240", cex = 1)

  theta <- seq(-1.2, 1.2, length.out = 180)
  lines(-41.75 + 21.75 * cos(theta), 21.75 * sin(theta), col = "#94a3b8", lwd = 1.1)
  lines(41.75 - 21.75 * cos(theta), 21.75 * sin(theta), col = "#94a3b8", lwd = 1.1)
  segments(-47, -21.75, -33, -21.75, col = "#94a3b8", lwd = 1.1)
  segments(-47, 21.75, -33, 21.75, col = "#94a3b8", lwd = 1.1)
  segments(33, -21.75, 47, -21.75, col = "#94a3b8", lwd = 1.1)
  segments(33, 21.75, 47, 21.75, col = "#94a3b8", lwd = 1.1)
}

compute_scheme_metrics <- function(game_id, scheme_tables) {
  if (!is.finite(game_id)) {
    return(list(available = FALSE, values = list(), reason = "game_id missing"))
  }

  possessions <- scheme_tables$possessions %||% data.frame()
  scheme_tags <- scheme_tables$scheme_tags %||% data.frame()
  events <- scheme_tables$events %||% data.frame()
  breakdown_tags <- scheme_tables$breakdown_tags %||% data.frame()

  pos_game <- data.frame()
  if (nrow(possessions) > 0 && "game_id" %in% names(possessions)) {
    pos_game <- possessions[to_num(possessions$game_id) == game_id, , drop = FALSE]
  }

  event_game <- data.frame()
  if (nrow(events) > 0 && "game_id" %in% names(events)) {
    event_game <- events[to_num(events$game_id) == game_id, , drop = FALSE]
  }

  st_game <- data.frame()
  if (nrow(scheme_tags) > 0) {
    keep <- rep(FALSE, nrow(scheme_tags))

    if ("possession_id" %in% names(scheme_tags) && nrow(pos_game) > 0 && "possession_id" %in% names(pos_game)) {
      keep <- keep | (to_chr(scheme_tags$possession_id) %in% to_chr(pos_game$possession_id))
    }

    if ("event_id" %in% names(scheme_tags) && nrow(event_game) > 0 && "event_id" %in% names(event_game)) {
      keep <- keep | (to_chr(scheme_tags$event_id) %in% to_chr(event_game$event_id))
    }

    st_game <- scheme_tags[keep, , drop = FALSE]
  }

  br_game <- data.frame()
  if (nrow(breakdown_tags) > 0) {
    keep <- rep(FALSE, nrow(breakdown_tags))

    if ("possession_id" %in% names(breakdown_tags) && nrow(pos_game) > 0 && "possession_id" %in% names(pos_game)) {
      keep <- keep | (to_chr(breakdown_tags$possession_id) %in% to_chr(pos_game$possession_id))
    }

    if ("event_id" %in% names(breakdown_tags) && nrow(event_game) > 0 && "event_id" %in% names(event_game)) {
      keep <- keep | (to_chr(breakdown_tags$event_id) %in% to_chr(event_game$event_id))
    }

    br_game <- breakdown_tags[keep, , drop = FALSE]
  }

  has_data <- (nrow(pos_game) + nrow(st_game) + nrow(br_game)) > 0
  if (!has_data) {
    return(list(available = FALSE, values = list(), reason = "No tagged scheme data for selected game"))
  }

  transition_rate <- NA_real_
  if ("transition_flag" %in% names(pos_game)) {
    tf <- as_logical_vec(pos_game$transition_flag)
    if (length(tf) > 0) transition_rate <- mean(tf, na.rm = TRUE)
  }

  offense_cols <- c("offense_action_family", "offense_action", "offense_action_detail")
  defense_cols <- c("defense_coverage_family", "defense_coverage", "defense_coverage_detail")

  values <- list(
    transition = if (is.finite(transition_rate)) fmt_pct(transition_rate, 1) else "--",
    quicks = fmt_int(row_pattern_count(st_game, offense_cols, "\\bQUICK(S)?\\b")),
    sets = fmt_int(row_pattern_count(st_game, offense_cols, "\\bSET(S)?\\b|HALF[_ ]?COURT[_ ]?SET")),
    breakdowns = fmt_int(nrow(br_game)),
    blob_o = fmt_int(row_pattern_count(st_game, offense_cols, "\\bBLOB\\b")),
    zone_o = fmt_int(row_pattern_count(st_game, offense_cols, "\\bZONE\\b")),
    press_o = fmt_int(row_pattern_count(st_game, offense_cols, "\\bPRESS\\b")),
    blob_d = fmt_int(row_pattern_count(st_game, defense_cols, "\\bBLOB\\b")),
    zone_d = fmt_int(row_pattern_count(st_game, defense_cols, "\\bZONE\\b")),
    press_d = fmt_int(row_pattern_count(st_game, defense_cols, "\\bPRESS\\b")),
    side_cs = fmt_int(row_pattern_count(st_game, offense_cols, "SIDE.*C(ATCH)?\\s*\\&?\\s*S(HOOT)?|CATCH.?AND.?SHOOT.*SIDE")),
    inside_cs = fmt_int(row_pattern_count(st_game, offense_cols, "INSIDE.*C(ATCH)?\\s*\\&?\\s*S(HOOT)?|CATCH.?AND.?SHOOT.*INSIDE")),
    post_up = fmt_int(row_pattern_count(st_game, offense_cols, "POST[_ ]?UP")),
    shot_clock = fmt_int(row_pattern_count(pos_game, c("special_situation", "chance_type"), "SHOT[_ ]?CLOCK")),
    specialty = fmt_int(sum(nz(pos_game$special_situation), na.rm = TRUE))
  )

  list(available = TRUE, values = values, reason = "")
}

make_board_tile <- function(label, value = "--") {
  div(
    class = "board-tile-wrap",
    div(class = "board-tile-label", label),
    div(class = "board-tile-value", value)
  )
}

make_kpi_card <- function(label, metric) {
  div(
    class = "kpi-card",
    div(class = "kpi-label", label),
    div(class = "kpi-side", paste0("UConn: ", metric$u_txt)),
    div(class = "kpi-side", paste0("Opp: ", metric$o_txt)),
    if (nz(metric$reason)) div(class = "kpi-reason", metric$reason)
  )
}

how_to_read_block <- function(text) {
  div(
    class = "howto-box",
    tags$strong("How to read this: "),
    tags$span(text)
  )
}

status_block <- function(type = c("ok", "warn", "error"), title, items = character()) {
  type <- match.arg(type)
  cls <- paste("status-box", paste0("status-", type))

  body <- if (length(items) > 0) {
    tags$ul(lapply(items, tags$li))
  } else {
    NULL
  }

  div(
    class = cls,
    tags$strong(title),
    body
  )
}

section_card <- function(title, subtitle = NULL, ..., card_class = "") {
  div(
    class = paste("section-card", card_class),
    div(
      class = "section-head",
      h3(class = "section-title", title),
      if (!is.null(subtitle) && nzchar(subtitle)) div(class = "section-subtitle", subtitle)
    ),
    div(class = "section-body", ...)
  )
}

table_shell <- function(..., shell_class = "") {
  div(
    class = paste("table-shell", shell_class),
    div(class = "table-scroll", ...)
  )
}

coach_sections <- c(
  "Season Scope",
  "Lineups",
  "Defense",
  "Players",
  "Decision Quality",
  "Opponent Scouts"
)

section_toggle_id <- function(section) {
  paste0("show_all_", gsub("[^a-z0-9]+", "_", tolower(section)))
}

make_coach_answer_card <- function(row) {
  div(
    class = "coach-answer-card",
    div(
      class = "coach-answer-head",
      span(class = "coach-status-tag", "Confirmed"),
      span(class = "coach-question-id", first_nonempty(row$question_id))
    ),
    div(class = "coach-question-text", first_nonempty(row$question_text)),
    div(class = "coach-answer-text", first_nonempty(row$answer_text)),
    div(
      class = "coach-evidence",
      paste("Data source:", first_nonempty(row$source_paths), "| Rows used:", first_nonempty(row$evidence_rows))
    )
  )
}

bucket_tab_ui <- function(title, input_id, info_id, schema_id, table_id, how_to_read = NULL) {
  tabPanel(
    title,
    div(
      class = "read-shell",
      if (!is.null(how_to_read) && nzchar(how_to_read)) how_to_read_block(how_to_read),
      section_card(
        title = paste(title, "Selection"),
        subtitle = "Pick a CSV and confirm row/column availability before drilling into details.",
        selectInput(input_id, "CSV File", choices = c("Select CSV" = "")),
        div(class = "section-inline-meta", textOutput(info_id))
      ),
      section_card(
        title = "Column Trace",
        subtitle = "Field mapping for this file.",
        table_shell(DT::DTOutput(schema_id))
      ),
      section_card(
        title = "Rows Used",
        subtitle = "Evidence rows with coach-facing labels.",
        table_shell(DT::DTOutput(table_id))
      )
    )
  )
}

build_dashboard_app <- function(output_dir = "_outputs") {
  ui <- fluidPage(
    tags$head(
      tags$style(HTML("\n        :root { --bg:#f5f2e8; --panel:#ffffff; --ink:#111827; --muted:#4b5563; --line:#d6d3c8; --uconn-navy:#0a2240; --uconn-red:#990033; --uconn-red-dark:#6f0227; --coach-gold:#d7b56d; }\n        body { background:linear-gradient(180deg,#f6f3ea 0%,#efe8da 100%); color:var(--ink); font-family:'Trebuchet MS','Gill Sans','Avenir Next',sans-serif; }\n        h1,h2,h3,h4 { font-family:'Avenir Next Condensed','Franklin Gothic Medium','Trebuchet MS',sans-serif; letter-spacing:0.2px; }\n        .shiny-input-container { margin-bottom:10px; }\n        .tab-content { padding-top:12px; }\n        .howto-box { border:1px solid #cbc3b1; background:#fbf7ef; color:#2f2a20; border-radius:8px; padding:8px 10px; margin-bottom:10px; font-size:13px; }\n        .status-box { border-radius:8px; padding:10px; margin-bottom:10px; }\n        .status-box ul { margin:8px 0 0 18px; }\n        .status-error { border:1px solid #a93d2f; background:#fff0eb; color:#7f1d1d; }\n        .status-warn { border:1px solid #946200; background:#fff8df; color:#6b4900; }\n        .status-ok { border:1px solid #245c2f; background:#edf7ef; color:#145214; }\n        .coach-hero { border:1px solid #d0c7b4; background:linear-gradient(130deg,#0a2240 0%,#163761 60%,#30507a 100%); color:#f8f6f1; border-radius:12px; padding:16px; margin-bottom:12px; box-shadow:0 2px 4px rgba(15,23,42,0.12); }\n        .coach-hero-title { font-size:30px; font-weight:900; line-height:1; margin-bottom:6px; }\n        .coach-hero-sub { font-size:14px; opacity:0.94; }\n        .coach-health-grid { display:flex; flex-wrap:wrap; gap:8px; margin:8px 0 12px; }\n        .coach-health-card { border:1px solid #d2cab9; background:#ffffff; border-radius:8px; padding:8px 10px; min-width:150px; flex:1 1 150px; }\n        .coach-health-label { color:#5a4f3a; font-size:12px; font-weight:700; margin-bottom:3px; text-transform:uppercase; }\n        .coach-health-value { color:#1f2937; font-size:22px; font-weight:900; line-height:1.15; }\n        .coach-section { border:1px solid #d4cbba; background:#fffdfa; border-radius:10px; padding:12px; margin-bottom:12px; }\n        .coach-section-head { display:flex; align-items:flex-end; justify-content:space-between; gap:8px; flex-wrap:wrap; }\n        .coach-section-title { font-size:24px; font-weight:900; color:#0f172a; margin:0; }\n        .coach-section-summary { color:#5b6473; font-size:13px; font-weight:700; }\n        .coach-answer-list { display:flex; flex-direction:column; gap:8px; margin-top:8px; }\n        .coach-answer-card { border:1px solid #dfd7c7; border-left:4px solid #0a2240; border-radius:8px; padding:10px; background:#ffffff; }\n        .coach-answer-head { display:flex; align-items:center; gap:8px; margin-bottom:4px; }\n        .coach-status-tag { background:#e8f5e9; color:#145214; border:1px solid #85b58e; border-radius:999px; padding:2px 8px; font-size:11px; font-weight:800; text-transform:uppercase; }\n        .coach-question-id { font-size:12px; font-weight:800; color:#6b7280; }\n        .coach-question-text { font-size:14px; font-weight:800; color:#111827; margin-bottom:2px; }\n        .coach-answer-text { font-size:16px; font-weight:800; color:#0f172a; margin-bottom:2px; }\n        .coach-evidence { font-size:11px; color:#677184; word-break:break-word; }\n        .coach-empty { font-size:13px; color:#6b7280; margin-top:6px; }\n        .board-root { background:var(--panel); border:1px solid var(--line); border-radius:10px; padding:14px; box-shadow:0 1px 2px rgba(15,23,42,0.08); }\n        .board-title-row { display:flex; align-items:center; gap:10px; margin-bottom:10px; flex-wrap:wrap; }\n        .board-wordmark { min-width:180px; height:52px; border:1px solid #bcc5d4; background:linear-gradient(135deg,#eef1f6,#dde4ef); display:flex; align-items:center; justify-content:center; font-size:22px; font-weight:800; font-style:italic; color:var(--uconn-navy); letter-spacing:0.3px; border-radius:8px; }\n        .board-title-text { font-size:36px; line-height:1; color:var(--uconn-red); font-weight:800; }\n        .board-controls { border:1px solid var(--line); background:#f9fbff; border-radius:8px; padding:10px; margin-bottom:10px; }\n        .board-block { border:1px solid var(--line); background:#fbfcff; border-radius:8px; padding:8px; margin-bottom:8px; }\n        .board-row { display:flex; gap:8px; flex-wrap:wrap; }\n        .board-tile-wrap { min-width:120px; flex:1 1 120px; }\n        .board-tile-label { font-size:12px; font-weight:700; margin-bottom:3px; color:#273244; }\n        .board-tile-value { background:linear-gradient(180deg,var(--uconn-red),var(--uconn-red-dark)); color:#fff; border:1px solid #4a0c1f; border-radius:6px; padding:8px; min-height:38px; display:flex; align-items:center; justify-content:center; font-weight:700; text-align:center; }\n        .board-note { font-size:12px; color:var(--muted); margin:6px 0; }\n        .kpi-row { display:flex; flex-wrap:wrap; gap:8px; margin-top:6px; }\n        .kpi-card { background:#f9fafc; border:1px solid var(--line); border-radius:8px; min-width:180px; flex:1 1 180px; padding:9px; }\n        .kpi-label { font-size:16px; color:var(--uconn-navy); text-transform:uppercase; font-weight:800; margin-bottom:4px; }\n        .kpi-side { font-size:18px; color:#111827; font-weight:800; line-height:1.25; }\n        .kpi-reason { font-size:12px; color:var(--muted); margin-top:5px; }\n        .summary-grid { display:flex; flex-wrap:wrap; gap:8px; margin:6px 0 10px; }\n        .summary-card { border:1px solid var(--line); background:#ffffff; border-radius:8px; padding:8px 10px; min-width:160px; flex:1 1 160px; }\n        .summary-label { color:#334155; font-size:12px; font-weight:700; margin-bottom:3px; }\n        .summary-value { color:#0f172a; font-size:18px; font-weight:800; line-height:1.2; }\n      "))
      ,
      tags$style(HTML("\n        .read-shell { display:flex; flex-direction:column; gap:16px; padding-bottom:8px; }\n        .section-card { border:1px solid #ccd5df; background:#ffffff; border-radius:12px; padding:14px 16px; box-shadow:0 1px 2px rgba(15,23,42,0.06); }\n        .section-head { display:flex; flex-wrap:wrap; align-items:flex-end; justify-content:space-between; gap:6px 14px; margin-bottom:10px; }\n        .section-title { margin:0; font-size:24px; font-weight:900; line-height:1.1; color:#0f172a; }\n        .section-subtitle { font-size:13px; line-height:1.45; font-weight:600; color:#334155; max-width:92ch; }\n        .section-inline-meta { margin-top:6px; font-size:13px; color:#334155; font-weight:700; }\n        .section-body > :last-child { margin-bottom:0; }\n        .table-shell { border:1px solid #d4dce7; border-radius:10px; background:#fcfdff; padding:10px; }\n        .table-scroll { width:100%; overflow-x:auto; -webkit-overflow-scrolling:touch; }\n        .table-shell table { width:100%; }\n        .table-shell table th, .table-shell table td { padding:9px 10px; font-size:14px; line-height:1.35; }\n        .table-shell .dataTables_wrapper { margin-bottom:0; }\n        .table-shell .dataTables_scroll { overflow:auto; }\n        .dataTables_wrapper table.dataTable thead th { font-size:13px; padding:10px 12px; color:#0f172a; background:#f2f6fc; border-bottom:1px solid #c8d3e0; }\n        .dataTables_wrapper table.dataTable tbody td { font-size:14px; padding:9px 12px; line-height:1.4; color:#0f172a; }\n        .dataTables_wrapper table.dataTable tbody tr.selected td { background:#dbeafe !important; color:#0b2447 !important; font-weight:800; border-top:1px solid #1e3a8a; border-bottom:1px solid #1e3a8a; }\n        .board-filter-grid { margin-left:-6px; margin-right:-6px; }\n        .board-filter-grid .col-sm-3, .board-filter-grid .col-sm-6 { padding-left:6px; padding-right:6px; }\n        .board-context-strip { position:sticky; top:0; z-index:20; border:1px solid #a8bed8; background:linear-gradient(120deg,#eef4ff 0%,#f7fbff 100%); border-radius:12px; padding:10px 12px; box-shadow:0 2px 6px rgba(15,23,42,0.08); }\n        .board-context-head { display:flex; flex-wrap:wrap; align-items:center; justify-content:space-between; gap:6px 12px; margin-bottom:8px; }\n        .board-context-title { margin:0; font-size:22px; font-weight:900; color:#0f172a; line-height:1.1; }\n        .board-context-sub { font-size:13px; font-weight:700; color:#334155; }\n        .context-chip-row { display:flex; flex-wrap:wrap; gap:8px; }\n        .context-chip { border:1px solid #7e9bbf; background:#ffffff; border-radius:999px; padding:5px 10px; font-size:12px; line-height:1.25; color:#0f172a; font-weight:700; display:inline-flex; align-items:center; gap:6px; }\n        .context-chip-label { color:#334155; font-weight:800; text-transform:uppercase; font-size:11px; letter-spacing:0.35px; }\n        .context-chip-active { border-width:2px; border-color:#1e3a8a; background:#ebf3ff; }\n        .context-chip-active::after { content:'ACTIVE'; font-size:10px; font-weight:900; color:#0f172a; letter-spacing:0.35px; }\n        .board-kpi-grid { display:grid; grid-template-columns:repeat(3, minmax(220px, 1fr)); gap:10px; }\n        .kpi-card { min-width:0; }\n        .board-core-grid { display:grid; grid-template-columns:1fr 1fr; gap:12px; align-items:start; }\n        .board-core-panel { border:1px solid #cfd8e2; background:#fbfcff; border-radius:10px; padding:10px; min-width:0; }\n        .board-core-panel.is-active { border-width:2px; border-color:#1e3a8a; box-shadow:inset 0 0 0 2px #dbeafe; }\n        .board-core-head { display:flex; flex-wrap:wrap; align-items:center; justify-content:space-between; gap:6px 10px; margin-bottom:8px; }\n        .board-core-title { font-size:22px; font-weight:900; color:#0f172a; margin:0; }\n        .board-core-state { font-size:12px; font-weight:800; color:#1f2937; border:1px solid #9aaec6; border-radius:999px; padding:4px 9px; background:#f8fbff; }\n        .board-core-state.active { border-color:#1e3a8a; border-width:2px; background:#e7f0ff; }\n        .board-panel-section-title { font-size:14px; font-weight:900; color:#0f172a; text-transform:uppercase; margin:10px 0 6px; letter-spacing:0.25px; }\n        .map-shell { border:1px solid #d4dce7; border-radius:10px; background:#ffffff; padding:8px; width:100%; overflow-x:auto; }\n        .map-shell .shiny-plot-output { min-height:360px; width:100%; }\n        .board-support-subsection { border-top:1px solid #e2e8f0; padding-top:10px; margin-top:10px; }\n        .board-support-subsection:first-child { border-top:none; padding-top:0; margin-top:0; }\n        .board-subtitle { margin:0 0 8px; font-size:16px; font-weight:900; color:#0f172a; }\n        .board-row { display:flex; gap:10px; flex-wrap:wrap; }\n        .board-tile-wrap { min-width:170px; flex:1 1 170px; }\n        .board-tile-label { font-size:12px; font-weight:800; margin-bottom:3px; color:#1f2937; }\n        .board-tile-value { background:linear-gradient(180deg,#8d173e,#6f0227); color:#ffffff; border:1px solid #4a0c1f; border-radius:8px; padding:9px; min-height:44px; display:flex; align-items:center; justify-content:center; font-weight:800; text-align:center; line-height:1.3; }\n        .board-note { font-size:12px; color:#334155; margin:6px 0; line-height:1.35; }\n        .summary-grid { gap:10px; }\n        .summary-card { border-radius:10px; padding:10px; }\n        a:focus, a:focus-visible, button:focus, button:focus-visible, input:focus, input:focus-visible, select:focus, select:focus-visible, textarea:focus, textarea:focus-visible, .btn:focus, .btn:focus-visible, .nav > li > a:focus, .nav > li > a:focus-visible { outline:3px solid #1d4ed8 !important; outline-offset:2px; box-shadow:none !important; }\n        @media (max-width: 991px) {\n          .board-context-strip { position:static; top:auto; }\n          .board-kpi-grid { grid-template-columns:repeat(2, minmax(200px, 1fr)); }\n          .board-core-grid { grid-template-columns:1fr; }\n        }\n        @media (max-width: 768px) {\n          .section-card { padding:12px; }\n          .section-title { font-size:21px; }\n          .board-kpi-grid { grid-template-columns:1fr; }\n          .board-tile-wrap { min-width:100%; flex:1 1 100%; }\n          .map-shell .shiny-plot-output { min-height:300px; }\n          .table-shell { padding:8px; }\n        }\n        @media (max-width: 390px) {\n          .section-title { font-size:19px; }\n          .board-context-title { font-size:20px; }\n          .map-shell .shiny-plot-output { min-height:260px; }\n          .dataTables_wrapper table.dataTable thead th, .dataTables_wrapper table.dataTable tbody td { padding:8px 9px; font-size:13px; }\n        }\n      "))
    ),
    titlePanel("UConn Coach Evidence Dashboard — Historical Archive"),
    tags$div(
      style = "border:2px solid #990033;background:#fff0eb;color:#6f0227;padding:12px;margin-bottom:16px;border-radius:8px;",
      tags$strong("Historical results — not validated for staff decisions. "),
      "Lineup and model outputs predate source reconciliation. See docs/RELIABILITY_RESET.md for current findings."
    ),
    sidebarLayout(
      sidebarPanel(
        actionButton("refresh_btn", "Refresh Dashboard Data"),
        tags$hr(),
        p(strong("Output Folder:")),
        textOutput("output_dir_txt"),
        p(strong("Data Snapshot Time:"), textOutput("as_of_txt", inline = TRUE)),
        p(strong("Last Reload Time:"), textOutput("loaded_at_txt", inline = TRUE))
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Coach View",
            div(
              class = "read-shell",
              section_card(
                title = "Coach Briefing",
                subtitle = "Plain-language answers are grouped by section. Missing answers remain hidden here and are tracked in Technical Data.",
                div(
                  class = "coach-hero",
                  div(class = "coach-hero-title", "Coach Briefing"),
                  div(class = "coach-hero-sub", "Summary first, supporting evidence second.")
                )
              ),
              section_card(
                title = "Section Coverage",
                subtitle = "Quick snapshot of confirmed answers vs items waiting on data.",
                uiOutput("coach_health_ui")
              ),
              section_card(
                title = "Coach Evidence",
                subtitle = "Confirmed answers are surfaced first for faster scanning.",
                uiOutput("coach_sections_ui")
              ),
              section_card(
                title = "Coach Glossary",
                subtitle = "Definitions used across dashboard metrics.",
                div(
                  class = "howto-box",
                  tags$strong("Coach glossary:"),
                  tags$br(),
                  "PPP = points per possession",
                  tags$br(),
                  "eFG% = shooting efficiency adjusted for threes",
                  tags$br(),
                  "OReb% = offensive rebound rate",
                  tags$br(),
                  "FT rate = free throws per field-goal attempt",
                  tags$br(),
                  "RCI = role concentration index",
                  tags$br(),
                  "ECE = calibration error"
                )
              )
            )
          ),
          tabPanel(
            "Game Board",
            div(
              class = "read-shell",
              section_card(
                title = "Filter and Context",
                subtitle = "Select competition, opponent, and game. Current game context remains visible while scrolling on desktop and tablet.",
                how_to_read_block("The board keeps all current calculations and selection wiring, then presents KPI, core analysis, and supporting evidence in separate bands."),
                fluidRow(
                  class = "board-filter-grid",
                  column(3, selectInput("board_competition", "Competition", choices = c("Select competition" = ""))),
                  column(3, selectInput("board_opponent", "Opponent", choices = c("Select opponent" = ""))),
                  column(6, selectInput("board_game", "Game", choices = c("Select game" = "")))
                )
              ),
              uiOutput("game_board_ui"),
              uiOutput("board_pos_ui")
            )
          ),
          tabPanel(
            "Player Out",
            div(
              class = "read-shell",
              section_card(
                title = "Scenario Filter",
                subtitle = "Pick a player-out scenario, then review summary before detailed tables.",
                how_to_read_block("Survivors are eligible lineups that remain; lost options are eligible lineups that include the removed player."),
                uiOutput("player_out_banner"),
                selectInput("player_out_scenario", "Player Out Scenario", choices = c("Select player" = "")),
                uiOutput("player_out_context_ui")
              ),
              section_card(
                title = "Scenario Summary",
                subtitle = "Key scenario counts and limits.",
                uiOutput("player_out_summary_cards")
              ),
              section_card(
                title = "Top Survivors",
                subtitle = "Best remaining lineup options under the selected scenario.",
                table_shell(DT::DTOutput("player_out_top_table"))
              ),
              section_card(
                title = "Lost Options",
                subtitle = "Lineups removed by the selected player-out constraint.",
                table_shell(DT::DTOutput("player_out_lost_table"))
              ),
              section_card(
                title = "Detailed Evidence",
                subtitle = "All rows for the selected scenario.",
                table_shell(DT::DTOutput("player_out_full_table"))
              ),
              section_card(
                title = "Column Trace",
                subtitle = "Raw-field-to-label mapping for the selected scenario.",
                table_shell(DT::DTOutput("player_out_schema_table"))
              )
            )
          ),
          tabPanel(
            "Technical Data",
            div(
              class = "read-shell",
              section_card(
                title = "Technical Data Explorer",
                subtitle = "Summary diagnostics first, then file-level evidence by bucket."
              ),
              tabsetPanel(
                tabPanel(
                  "Data Health",
                  div(
                    class = "read-shell",
                    section_card(
                      title = "Load Status",
                      subtitle = "Required-source status and contract checks.",
                      uiOutput("overview_banner")
                    ),
                    section_card(
                      title = "Reliability Checks for This Data Load",
                      subtitle = "Pass/fail checks for key thresholds.",
                      table_shell(tableOutput("qc_table"))
                    ),
                    section_card(
                      title = "Question Coverage",
                      subtitle = "Confirmed vs unavailable question counts.",
                      table_shell(DT::DTOutput("question_coverage_table"))
                    ),
                    section_card(
                      title = "Full Question Status (All 40)",
                      subtitle = "Detailed per-question availability and reasons.",
                      table_shell(DT::DTOutput("question_status_table"))
                    ),
                    section_card(
                      title = "Source Status",
                      subtitle = "Source file rows, columns, and load messages.",
                      table_shell(DT::DTOutput("status_table"))
                    ),
                    section_card(
                      title = "Bucket File Counts",
                      subtitle = "CSV counts by bucket in this data load.",
                      table_shell(DT::DTOutput("bucket_counts_table"))
                    )
                  )
                ),
                bucket_tab_ui("01 Lineup Core", "file_01", "file_01_info", "schema_01", "table_01"),
                bucket_tab_ui("02 Defense Leaks", "file_02", "file_02_info", "schema_02", "table_02"),
                bucket_tab_ui("03 Players", "file_03", "file_03_info", "schema_03", "table_03"),
                bucket_tab_ui("04 Games Trends", "file_04", "file_04_info", "schema_04", "table_04"),
                bucket_tab_ui(
                  "05 Decision Audit",
                  "file_05",
                  "file_05_info",
                  "schema_05",
                  "table_05",
                  how_to_read = "Use by-bucket holdout tables to compare predicted vs observed outcomes and calibration gaps before trusting lineup decisions."
                ),
                bucket_tab_ui("06 Scheme Matchups", "file_06", "file_06_info", "schema_06", "table_06"),
                bucket_tab_ui("07 Opponent Scouts", "file_07", "file_07_info", "schema_07", "table_07")
              )
            )
          )
        )
      )
    )
  )

  server <- function(input, output, session) {
    refresh_tick <- reactiveVal(0)

    observeEvent(input$refresh_btn, {
      refresh_tick(refresh_tick() + 1)
    })

    dash_data <- reactive({
      refresh_tick()
      load_dashboard_data(output_dir = output_dir)
    })

    output$output_dir_txt <- renderText({
      normalizePath(output_dir, winslash = "/", mustWork = FALSE)
    })

    output$as_of_txt <- renderText({
      d <- dash_data()
      if (is.na(d$as_of)) return("NA")
      format(d$as_of, "%Y-%m-%d %H:%M:%S %Z")
    })

    output$loaded_at_txt <- renderText({
      format(dash_data()$loaded_at, "%Y-%m-%d %H:%M:%S %Z")
    })

    output$coach_health_ui <- renderUI({
      d <- dash_data()
      coverage <- d$question_coverage_summary %||% data.frame()
      all_q <- d$question_answers_all %||% data.frame()
      if (!is.null(coverage) && nrow(coverage) > 0) {
        coverage <- coverage[to_chr(coverage$section) %in% coach_sections, , drop = FALSE]
      }

      if (is.null(coverage) || nrow(coverage) == 0 || is.null(all_q) || nrow(all_q) == 0) {
        return(status_block("warn", "Coach section answers are not available yet.", "Open Technical Data > Data Health for source diagnostics."))
      }

      total_available <- sum(to_num(coverage$available_n), na.rm = TRUE)
      total_unavailable <- sum(to_num(coverage$unavailable_n), na.rm = TRUE)
      total_questions <- sum(to_num(coverage$total_questions), na.rm = TRUE)

      div(
        div(
          class = "coach-health-grid",
          div(class = "coach-health-card", div(class = "coach-health-label", "Questions We Can Answer Now"), div(class = "coach-health-value", fmt_int(total_available))),
          div(class = "coach-health-card", div(class = "coach-health-label", "Questions Waiting on Data"), div(class = "coach-health-value", fmt_int(total_unavailable))),
          div(class = "coach-health-card", div(class = "coach-health-label", "Total Questions"), div(class = "coach-health-value", fmt_int(total_questions)))
        ),
        if (total_unavailable > 0) div(
          class = "board-note",
          sprintf("%s answers are hidden here because required evidence is missing. See Technical Data > Data Health for details.", fmt_int(total_unavailable))
        )
      )
    })

    output$coach_sections_ui <- renderUI({
      d <- dash_data()
      available <- d$question_answers_available %||% data.frame()
      all_q <- d$question_answers_all %||% data.frame()
      coverage <- d$question_coverage_summary %||% data.frame()

      if (is.null(all_q) || nrow(all_q) == 0) {
        return(status_block("warn", "No question answers loaded.", "Technical Data still remains fully available."))
      }

      sections <- lapply(coach_sections, function(section_name) {
        sec_all <- all_q[all_q$section == section_name, , drop = FALSE]
        sec_available <- available[available$section == section_name, , drop = FALSE]
        sec_available <- sec_available[order(to_num(sec_available$priority), sec_available$question_id), , drop = FALSE]

        min_priority <- suppressWarnings(min(to_num(sec_available$priority), na.rm = TRUE))
        if (!is.finite(min_priority)) min_priority <- NA_real_

        show_all_id <- section_toggle_id(section_name)
        show_all <- isTRUE(input[[show_all_id]])

        if (show_all) {
          sec_show <- sec_available
        } else if (is.finite(min_priority)) {
          sec_show <- sec_available[to_num(sec_available$priority) == min_priority, , drop = FALSE]
        } else {
          sec_show <- sec_available[0, , drop = FALSE]
        }

        cov_row <- coverage[coverage$section == section_name, , drop = FALSE]
        available_n <- if (nrow(cov_row) > 0) fmt_int(to_num(cov_row$available_n)[[1]]) else fmt_int(nrow(sec_available))
        unavailable_n <- if (nrow(cov_row) > 0) fmt_int(to_num(cov_row$unavailable_n)[[1]]) else "0"

        div(
          class = "coach-section",
          div(
            class = "coach-section-head",
            h3(class = "coach-section-title", section_name),
            div(class = "coach-section-summary", sprintf("Confirmed: %s | Waiting on data: %s", available_n, unavailable_n))
          ),
          checkboxInput(show_all_id, "Show every answer in this section", value = show_all),
          if (nrow(sec_show) > 0) {
            div(class = "coach-answer-list", lapply(seq_len(nrow(sec_show)), function(i) make_coach_answer_card(sec_show[i, , drop = FALSE])))
          } else if (nrow(sec_available) > 0) {
            div(class = "coach-empty", "No priority answers in this section. Turn on 'Show every answer in this section' to view everything available.")
          } else {
            div(class = "coach-empty", "No answers are shown for this section because required evidence is missing.")
          }
        )
      })

      do.call(tagList, sections)
    })

    output$overview_banner <- renderUI({
      d <- dash_data()
      if (length(d$errors) > 0) {
        return(status_block("error", "Data contract issues detected.", d$errors))
      }

      if (length(d$warnings) > 0) {
        return(status_block("warn", "Loaded with warnings.", utils::head(d$warnings, 8)))
      }

      status_block("ok", "All required sources loaded.")
    })

    output$question_coverage_table <- DT::renderDT({
      cov <- dash_data()$question_coverage_summary %||% data.frame()
      if (is.null(cov) || nrow(cov) == 0) return(data.frame())
      cov %>%
        dplyr::mutate(
          total_questions = as.integer(round(to_num(.data$total_questions))),
          available_n = as.integer(round(to_num(.data$available_n))),
          unavailable_n = as.integer(round(to_num(.data$unavailable_n)))
        )
    }, options = list(pageLength = 10, autoWidth = TRUE, searching = FALSE, paging = FALSE, info = FALSE))

    output$question_status_table <- DT::renderDT({
      qa <- dash_data()$question_answers_all %||% data.frame()
      if (is.null(qa) || nrow(qa) == 0) return(data.frame())

      qa %>%
        dplyr::mutate(
          status = ifelse(.data$status == "AVAILABLE", "CONFIRMED", "UNAVAILABLE"),
          reason = ifelse(.data$status == "AVAILABLE", "", .data$unavailable_reason)
        ) %>%
        dplyr::select(.data$question_id, .data$section, .data$priority, .data$status, .data$question_text, .data$answer_text, .data$source_paths, .data$evidence_rows, .data$reason)
    }, options = list(pageLength = 20, autoWidth = TRUE))

    output$qc_table <- renderTable({
      dash_data()$qc %>%
        dplyr::mutate(
          value = ifelse(is.finite(.data$value), round(.data$value, 4), NA_real_),
          threshold = round(.data$threshold, 4),
          result = ifelse(.data$pass, "PASS", "FAIL")
        ) %>%
        dplyr::select(.data$check, .data$comparator, .data$threshold, .data$value, .data$result)
    }, striped = TRUE, bordered = TRUE, spacing = "s")

    output$status_table <- DT::renderDT({
      st <- dash_data()$status
      st %>%
        dplyr::mutate(
          required = ifelse(.data$required, "YES", "NO"),
          modified_at = ifelse(is.na(.data$modified_at), "NA", format(.data$modified_at, "%Y-%m-%d %H:%M:%S"))
        ) %>%
        dplyr::select(.data$source_type, .data$bucket, .data$required, .data$status, .data$rows, .data$cols, .data$modified_at, .data$path, .data$message)
    }, options = list(pageLength = 20, autoWidth = TRUE))

    output$bucket_counts_table <- DT::renderDT({
      d <- dash_data()
      buckets <- c(
        "01_lineup_core",
        "02_defense_leaks",
        "03_players",
        "04_games_trends",
        "05_decision_audit",
        "06_scheme_matchups",
        "07_opps"
      )
      data.frame(
        bucket = buckets,
        csv_files = vapply(buckets, function(b) length(d$outputs_by_bucket[[b]] %||% character()), integer(1)),
        stringsAsFactors = FALSE
      )
    }, options = list(pageLength = 10, autoWidth = TRUE))

    register_bucket_view <- function(bucket, input_id, info_id, schema_id, table_id) {
      observe({
        d <- dash_data()
        files <- d$outputs_by_bucket[[bucket]] %||% character()
        files <- sort(files)

        cur <- isolate(input[[input_id]])
        sel <- if (!is.null(cur) && nzchar(cur) && cur %in% files) cur else if (length(files) > 0) files[[1]] else ""

        choices <- c("Select CSV" = "")
        if (length(files) > 0) {
          choices <- c(choices, stats::setNames(files, files))
        }

        updateSelectInput(session, input_id, choices = choices, selected = sel)
      })

      selected_df <- reactive({
        rel <- input[[input_id]] %||% ""
        if (!nzchar(rel)) return(NULL)
        dash_data()$outputs_by_path[[rel]]
      })

      output[[info_id]] <- renderText({
        rel <- input[[input_id]] %||% ""
        files <- dash_data()$outputs_by_bucket[[bucket]] %||% character()
        if (length(files) == 0) return(sprintf("No CSV files found in %s.", bucket))
        if (!nzchar(rel)) return("No file selected.")
        df <- selected_df()
        if (is.null(df)) return(sprintf("%s | data unavailable", rel))
        sprintf("%s | rows=%s | cols=%s", rel, nrow(df), ncol(df))
      })

      output[[schema_id]] <- DT::renderDT({
        df <- selected_df()
        if (is.null(df)) return(data.frame())
        column_trace_table(df)
      }, options = list(pageLength = 25, autoWidth = TRUE))

      output[[table_id]] <- DT::renderDT({
        df <- selected_df()
        if (is.null(df)) return(data.frame())
        with_trace_colnames(df)
      }, options = list(pageLength = 25, autoWidth = TRUE))
    }

    register_bucket_view("01_lineup_core", "file_01", "file_01_info", "schema_01", "table_01")
    register_bucket_view("02_defense_leaks", "file_02", "file_02_info", "schema_02", "table_02")
    register_bucket_view("03_players", "file_03", "file_03_info", "schema_03", "table_03")
    register_bucket_view("04_games_trends", "file_04", "file_04_info", "schema_04", "table_04")
    register_bucket_view("05_decision_audit", "file_05", "file_05_info", "schema_05", "table_05")
    register_bucket_view("06_scheme_matchups", "file_06", "file_06_info", "schema_06", "table_06")
    register_bucket_view("07_opps", "file_07", "file_07_info", "schema_07", "table_07")

    player_out_rel <- file.path("05_decision_audit", "uconn_availability_stress_test_report.csv")
    player_out_required_cols <- c(
      "generated_at_utc", "scenario_player_out", "top_n_requested", "max_pr_leak",
      "survivors_n", "lost_n", "section", "rank", "lineup_key", "lineup_pretty",
      "possessions", "sample_tier", "trust_baseline", "pr_leak"
    )

    player_out_status_row <- reactive({
      st <- dash_data()$status
      if (is.null(st) || nrow(st) == 0 || !"path" %in% names(st)) return(NULL)
      rows <- st[st$path == player_out_rel, , drop = FALSE]
      if (nrow(rows) == 0) return(NULL)
      rows[1, , drop = FALSE]
    })

    player_out_report <- reactive({
      dash_data()$outputs_by_path[[player_out_rel]]
    })

    player_out_errors <- reactive({
      errs <- character()
      st <- player_out_status_row()

      if (is.null(st)) {
        errs <- c(errs, sprintf("Missing source status row for %s", player_out_rel))
      } else {
        status_code <- to_chr(st$status)
        if (length(status_code) == 0 || status_code[[1]] != "OK") {
          status_label <- if (length(status_code) > 0 && nzchar(status_code[[1]])) status_code[[1]] else "UNKNOWN"
          msg <- trimws(to_chr(st$message))
          msg <- msg[!is.na(msg) & msg != ""]
          suffix <- if (length(msg) > 0) paste0(" (", msg[[1]], ")") else ""
          errs <- c(errs, sprintf("Player Out source not ready: %s%s", status_label, suffix))
        }
      }

      df <- player_out_report()
      if (is.null(df)) {
        errs <- c(errs, sprintf("Missing required output CSV: %s", player_out_rel))
      } else {
        miss <- setdiff(player_out_required_cols, names(df))
        if (length(miss) > 0) {
          errs <- c(errs, sprintf("Player Out report missing columns: %s", paste(miss, collapse = ", ")))
        }
      }

      unique(errs)
    })

    output$player_out_banner <- renderUI({
      errs <- player_out_errors()
      if (length(errs) > 0) {
        return(status_block("error", "Player Out module unavailable.", errs))
      }

      df <- player_out_report()
      scenarios <- if (!is.null(df) && "scenario_player_out" %in% names(df)) {
        unique(trimws(to_chr(df$scenario_player_out)))
      } else {
        character()
      }
      scenarios <- scenarios[!is.na(scenarios) & scenarios != ""]

      div(
        class = "board-note",
        sprintf("Source: %s | scenarios=%d | rows=%d", player_out_rel, length(scenarios), nrow(df))
      )
    })

    observe({
      errs <- player_out_errors()
      if (length(errs) > 0) {
        updateSelectInput(session, "player_out_scenario", choices = c("Select player" = ""), selected = "")
        return()
      }

      df <- player_out_report()
      scenarios <- sort(unique(trimws(to_chr(df$scenario_player_out))))
      scenarios <- scenarios[!is.na(scenarios) & scenarios != ""]

      cur <- isolate(input$player_out_scenario)
      sel <- if (!is.null(cur) && nzchar(cur) && cur %in% scenarios) cur else if (length(scenarios) > 0) scenarios[[1]] else ""

      choices <- c("Select player" = "")
      if (length(scenarios) > 0) choices <- c(choices, stats::setNames(scenarios, scenarios))
      updateSelectInput(session, "player_out_scenario", choices = choices, selected = sel)
    })

    player_out_selected <- reactive({
      if (length(player_out_errors()) > 0) return(data.frame())
      player <- input$player_out_scenario %||% ""
      if (!nzchar(player)) return(data.frame())
      df <- player_out_report()
      if (is.null(df) || !"scenario_player_out" %in% names(df)) return(data.frame())
      df[trimws(to_chr(df$scenario_player_out)) == player, , drop = FALSE]
    })

    output$player_out_context_ui <- renderUI({
      if (length(player_out_errors()) > 0) return(NULL)

      scenario <- input$player_out_scenario %||% ""
      if (!nzchar(scenario)) {
        return(div(class = "board-note", "Select a scenario to pin context metadata."))
      }

      df <- player_out_selected()
      summary_row <- df[df$section == "SUMMARY", , drop = FALSE]
      if (nrow(summary_row) > 0) summary_row <- summary_row[1, , drop = FALSE]

      survivors_n <- if (nrow(summary_row) > 0) fmt_int(first_num_or_na(summary_row$survivors_n)) else "--"
      lost_n <- if (nrow(summary_row) > 0) fmt_int(first_num_or_na(summary_row$lost_n)) else "--"
      generated_at <- if (nrow(summary_row) > 0) first_nonempty(summary_row$generated_at_utc) else "--"

      div(
        class = "board-context-strip",
        div(
          class = "board-context-head",
          h4(class = "board-context-title", "Player Out Context"),
          div(class = "board-context-sub", "Sticky at >=992px, standard flow below 992px.")
        ),
        div(
          class = "context-chip-row",
          div(class = "context-chip context-chip-active", span(class = "context-chip-label", "Scenario"), span(scenario)),
          div(class = "context-chip", span(class = "context-chip-label", "Survivors"), span(survivors_n)),
          div(class = "context-chip", span(class = "context-chip-label", "Lost"), span(lost_n)),
          div(class = "context-chip", span(class = "context-chip-label", "Rows"), span(fmt_int(nrow(df)))),
          div(class = "context-chip", span(class = "context-chip-label", "Generated UTC"), span(generated_at))
        )
      )
    })

    output$player_out_summary_cards <- renderUI({
      if (length(player_out_errors()) > 0) return(NULL)

      df <- player_out_selected()
      if (nrow(df) == 0) {
        return(div(class = "board-note", "Select a player-out scenario to view summary and evidence tables."))
      }

      summary_row <- df[df$section == "SUMMARY", , drop = FALSE]
      if (nrow(summary_row) == 0) {
        return(status_block("error", "Missing SUMMARY row in Player Out report.", sprintf("Scenario: %s", input$player_out_scenario %||% "")))
      }
      summary_row <- summary_row[1, , drop = FALSE]

      div(
        class = "summary-grid",
        div(class = "summary-card", div(class = "summary-label", "Survivors"), div(class = "summary-value", fmt_int(first_num_or_na(summary_row$survivors_n)))),
        div(class = "summary-card", div(class = "summary-label", "Lost Options"), div(class = "summary-value", fmt_int(first_num_or_na(summary_row$lost_n)))),
        div(class = "summary-card", div(class = "summary-label", "Top N Requested"), div(class = "summary-value", fmt_int(first_num_or_na(summary_row$top_n_requested)))),
        div(class = "summary-card", div(class = "summary-label", "Max PR Leak"), div(class = "summary-value", fmt_pct(first_num_or_na(summary_row$max_pr_leak), digits = 1))),
        div(class = "summary-card", div(class = "summary-label", "Generated UTC"), div(class = "summary-value", first_nonempty(summary_row$generated_at_utc)))
      )
    })

    output$player_out_top_table <- DT::renderDT({
      if (length(player_out_errors()) > 0) return(data.frame())
      df <- player_out_selected()
      if (nrow(df) == 0) return(data.frame())
      top <- df[df$section == "TOP_SURVIVOR", , drop = FALSE]
      cols <- intersect(c("rank", "lineup_pretty", "lineup_key", "possessions", "sample_tier", "trust_baseline", "pr_leak", "u_def_mean", "u_def_p05", "u_def_p95"), names(top))
      with_trace_colnames(top[, cols, drop = FALSE])
    }, options = list(pageLength = 20, autoWidth = TRUE))

    output$player_out_lost_table <- DT::renderDT({
      if (length(player_out_errors()) > 0) return(data.frame())
      df <- player_out_selected()
      if (nrow(df) == 0) return(data.frame())
      lost <- df[df$section == "LOST_OPTION", , drop = FALSE]
      cols <- intersect(c("rank", "lineup_pretty", "lineup_key", "possessions", "sample_tier", "trust_baseline", "pr_leak", "u_def_mean", "u_def_p05", "u_def_p95"), names(lost))
      with_trace_colnames(lost[, cols, drop = FALSE])
    }, options = list(pageLength = 20, autoWidth = TRUE))

    output$player_out_full_table <- DT::renderDT({
      if (length(player_out_errors()) > 0) return(data.frame())
      df <- player_out_selected()
      if (nrow(df) == 0) return(data.frame())
      with_trace_colnames(df)
    }, options = list(pageLength = 25, autoWidth = TRUE))

    output$player_out_schema_table <- DT::renderDT({
      if (length(player_out_errors()) > 0) return(data.frame())
      df <- player_out_selected()
      if (nrow(df) == 0) return(data.frame())
      column_trace_table(df)
    }, options = list(pageLength = 25, autoWidth = TRUE))

    board_index <- reactive({
      dash_data()$board_index
    })

    observe({
      idx <- board_index()
      comps <- sort(unique(to_chr(idx$competition_bucket)))
      comps <- comps[!is.na(comps) & comps != ""]

      cur <- isolate(input$board_competition)
      sel <- if (!is.null(cur) && nzchar(cur) && cur %in% comps) cur else ""
      updateSelectInput(session, "board_competition", choices = c("Select competition" = "", stats::setNames(comps, comps)), selected = sel)
    })

    board_comp_idx <- reactive({
      idx <- board_index()
      comp <- input$board_competition %||% ""
      if (!nzchar(comp)) return(idx[0, , drop = FALSE])
      idx[idx$competition_bucket == comp, , drop = FALSE]
    })

    observe({
      idx <- board_comp_idx()
      opps <- sort(unique(to_chr(idx$opponent)))
      opps <- opps[!is.na(opps) & opps != ""]

      cur <- isolate(input$board_opponent)
      sel <- if (!is.null(cur) && nzchar(cur) && cur %in% opps) cur else ""
      updateSelectInput(session, "board_opponent", choices = c("Select opponent" = "", stats::setNames(opps, opps)), selected = sel)
    })

    board_game_idx <- reactive({
      idx <- board_comp_idx()
      opp <- input$board_opponent %||% ""
      if (!nzchar(opp)) return(idx[0, , drop = FALSE])
      idx[idx$opponent == opp, , drop = FALSE]
    })

    observe({
      idx <- board_game_idx()

      labels <- to_chr(idx$game_label)
      values <- to_chr(idx$game_key)
      keep <- !is.na(values) & values != ""
      labels <- labels[keep]
      values <- values[keep]

      cur <- isolate(input$board_game)
      sel <- if (!is.null(cur) && nzchar(cur) && cur %in% values) cur else ""

      choices <- c("Select game" = "")
      if (length(values) > 0) choices <- c(choices, stats::setNames(values, labels))
      updateSelectInput(session, "board_game", choices = choices, selected = sel)
    })

    selected_game_rows <- reactive({
      g <- input$board_game %||% ""
      if (!nzchar(g)) return(data.frame())

      mg <- dash_data()$manual_games
      if (is.null(mg) || nrow(mg) == 0 || !"game_file" %in% names(mg)) return(data.frame())

      rows <- mg[to_chr(mg$game_file) == g, , drop = FALSE]
      if (nrow(rows) == 0) return(data.frame())

      rows <- rows %>%
        dplyr::mutate(
          row_index_raw = dplyr::row_number(),
          seq_num = num_col_or_na(., "sequence_number"),
          play_num = num_col_or_na(., "game_play_number"),
          play_id_num = num_col_or_na(., "play_id")
        ) %>%
        dplyr::arrange(
          dplyr::if_else(is.finite(.data$seq_num), .data$seq_num, Inf),
          dplyr::if_else(is.finite(.data$play_num), .data$play_num, Inf),
          dplyr::if_else(is.finite(.data$play_id_num), .data$play_id_num, Inf),
          .data$row_index_raw
        )

      rows
    })

    board_event_rows <- reactive({
      rows <- selected_game_rows()
      if (nrow(rows) == 0) return(data.frame())

      period_label <- if ("period_display_value" %in% names(rows)) trimws(to_chr(rows$period_display_value)) else rep("", nrow(rows))
      if ("period_number" %in% names(rows)) {
        period_num <- to_num(rows$period_number)
        fallback <- ifelse(is.finite(period_num), paste0("Period ", as.integer(round(period_num))), "Period --")
      } else {
        fallback <- rep("Period --", nrow(rows))
      }
      period_label[!nz(period_label)] <- fallback[!nz(period_label)]

      clock_label <- if ("clock_display_value" %in% names(rows)) trimws(to_chr(rows$clock_display_value)) else rep("", nrow(rows))
      clock_label[!nz(clock_label)] <- "--"

      short_desc <- if ("short_description" %in% names(rows)) to_chr(rows$short_description) else rep("", nrow(rows))
      long_desc <- if ("text" %in% names(rows)) to_chr(rows$text) else rep("", nrow(rows))
      event_desc <- trimws(ifelse(nz(short_desc), short_desc, long_desc))
      event_desc[!nz(event_desc)] <- "--"

      period_num <- num_col_or_na(rows, "period_number")
      side_raw <- if ("is_uconn_offense" %in% names(rows)) as_logical_strict(rows$is_uconn_offense) else rep(NA, nrow(rows))
      side_filled <- fill_logical_nearest(side_raw, default = FALSE)
      possession_id <- derive_possession_ids(period_num, side_filled)
      possession_event_count <- as.integer(ave(rep(1L, nrow(rows)), possession_id, FUN = sum))

      shot_type <- derive_shot_type(rows)
      shot_zone <- derive_shot_location_zone(rows)
      shot_result <- derive_shot_result(rows)
      shot_x_raw <- num_col_or_na(rows, "coordinate_x_raw")
      shot_y_raw <- num_col_or_na(rows, "coordinate_y_raw")
      shot_x_norm <- num_col_or_na(rows, "coordinate_x")
      shot_y_norm <- num_col_or_na(rows, "coordinate_y")
      coord_finite <- is.finite(shot_x_raw) & is.finite(shot_y_raw)
      coord_bounds <- coord_finite &
        abs(shot_x_raw) <= 60 & abs(shot_y_raw) <= 30 &
        shot_x_raw >= -47 & shot_x_raw <= 47 &
        shot_y_raw >= -25 & shot_y_raw <= 25
      shot_label <- ifelse(
        coord_finite & shot_zone != "No shot location",
        sprintf("%s [x=%.1f, y=%.1f]", shot_zone, shot_x_raw, shot_y_raw),
        shot_zone
      )

      rows$row_index <- seq_len(nrow(rows))
      rows$period_label <- period_label
      rows$clock_label <- clock_label
      rows$event_desc_label <- event_desc
      rows$is_shot_event <- derive_is_shot(rows)
      rows$shot_type_label <- shot_type
      rows$shot_location_zone <- shot_zone
      rows$shot_location_label <- shot_label
      rows$shot_result_label <- shot_result
      rows$is_uconn_offense_raw <- side_raw
      rows$is_uconn_offense_final <- side_filled
      rows$possession_id <- possession_id
      rows$possession_side <- ifelse(side_filled %in% TRUE, "UConn", "Opponent")
      rows$possession_event_count <- possession_event_count
      rows$coord_x_plot <- shot_x_raw
      rows$coord_y_plot <- shot_y_raw
      rows$coord_valid <- coord_bounds
      rows$coordinate_x_raw_num <- shot_x_raw
      rows$coordinate_y_raw_num <- shot_y_raw
      rows$coordinate_x_num <- shot_x_norm
      rows$coordinate_y_num <- shot_y_norm
      rows
    })

    board_side_rows <- reactive({
      rows <- board_event_rows()
      if (nrow(rows) == 0) {
        return(list(UConn = data.frame(), Opponent = data.frame()))
      }

      list(
        UConn = rows[rows$possession_side == "UConn", , drop = FALSE],
        Opponent = rows[rows$possession_side == "Opponent", , drop = FALSE]
      )
    })

    board_side_possessions <- reactive({
      sides <- board_side_rows()
      list(
        UConn = aggregate_possessions(sides$UConn),
        Opponent = aggregate_possessions(sides$Opponent)
      )
    })

    board_side_shots_valid <- reactive({
      sides <- board_side_rows()
      list(
        UConn = sides$UConn[sides$UConn$is_shot_event %in% TRUE & sides$UConn$coord_valid %in% TRUE, , drop = FALSE],
        Opponent = sides$Opponent[sides$Opponent$is_shot_event %in% TRUE & sides$Opponent$coord_valid %in% TRUE, , drop = FALSE]
      )
    })

    board_selected_possession_u <- reactiveVal(NA_integer_)
    board_selected_possession_o <- reactiveVal(NA_integer_)
    board_selected_event_u <- reactiveVal(NA_integer_)
    board_selected_event_o <- reactiveVal(NA_integer_)
    board_selected_side <- reactiveVal("UConn")

    observeEvent(board_event_rows(), {
      sides <- board_side_rows()

      u_ids <- unique(suppressWarnings(as.integer(sides$UConn$possession_id)))
      u_ids <- u_ids[is.finite(u_ids)]
      o_ids <- unique(suppressWarnings(as.integer(sides$Opponent$possession_id)))
      o_ids <- o_ids[is.finite(o_ids)]

      board_selected_possession_u(if (length(u_ids) > 0) u_ids[[1]] else NA_integer_)
      board_selected_possession_o(if (length(o_ids) > 0) o_ids[[1]] else NA_integer_)
      board_selected_event_u(NA_integer_)
      board_selected_event_o(NA_integer_)

      if (length(u_ids) > 0) {
        board_selected_side("UConn")
      } else if (length(o_ids) > 0) {
        board_selected_side("Opponent")
      }
    }, ignoreInit = FALSE)

    # Game Board ID contract freeze (must not change):
    # board_competition, board_opponent, board_game, board_pos_ui, game_board_ui,
    # board_uconn_pos_table, board_opp_pos_table, board_uconn_detail_table, board_opp_detail_table,
    # board_uconn_shot_map, board_opp_shot_map, board_uconn_pos_table_rows_selected,
    # board_opp_pos_table_rows_selected, board_uconn_shot_map_click, board_opp_shot_map_click.
    # Interaction contract freeze (must not change):
    # DT row-selection behavior, *_rows_selected observers, and shot-map click wiring.
    output$board_pos_ui <- renderUI({
      rows <- board_event_rows()
      state <- board_state()
      if (nrow(rows) == 0 || !isTRUE(state$selected)) {
        return(section_card(
          title = "Game Board",
          subtitle = "Select competition, opponent, and game to populate KPI, core analysis, and supporting metrics.",
          div(class = "board-note", "No game selected.")
        ))
      }

      sides <- board_side_rows()
      pos <- board_side_possessions()

      u_non_ft <- sides$UConn[sides$UConn$is_shot_event %in% TRUE & sides$UConn$shot_type_label != "Free Throw", , drop = FALSE]
      o_non_ft <- sides$Opponent[sides$Opponent$is_shot_event %in% TRUE & sides$Opponent$shot_type_label != "Free Throw", , drop = FALSE]
      u_invalid <- sum(!(u_non_ft$coord_valid %in% TRUE), na.rm = TRUE)
      o_invalid <- sum(!(o_non_ft$coord_valid %in% TRUE), na.rm = TRUE)

      u_active <- identical(state$active_side, "UConn")
      o_active <- identical(state$active_side, "Opponent")

      tagList(
        section_card(
          title = "KPI Band",
          subtitle = "Balanced-density KPI grid with unchanged calculations.",
          div(
            class = "board-kpi-grid",
            make_kpi_card("Estimated possessions", state$poss_pair),
            make_kpi_card("Points per possession (PPP)", state$ppp_pair),
            make_kpi_card("Shooting efficiency (eFG%)", state$efg_pair),
            make_kpi_card("Turnover rate (TO%)", state$to_pair),
            make_kpi_card("Offensive rebound rate (OReb%)", state$oreb_pair),
            make_kpi_card("Free throw rate (FT rate)", state$ftr_pair)
          )
        ),
        section_card(
          title = "Core Analysis Band",
          subtitle = "Mirrored UConn/Opponent panels with possession table, shot map, and event detail.",
          div(
            class = "board-note",
            sprintf(
              "Derived possessions: %s total events | %s UConn possessions | %s Opponent possessions.",
              fmt_int(nrow(rows)),
              fmt_int(nrow(pos$UConn)),
              fmt_int(nrow(pos$Opponent))
            )
          ),
          div(
            class = "board-core-grid",
            div(
              class = if (u_active) "board-core-panel is-active" else "board-core-panel",
              div(
                class = "board-core-head",
                h4(class = "board-core-title", "UConn Panel"),
                div(class = if (u_active) "board-core-state active" else "board-core-state", if (u_active) "Active selection" else "Secondary panel")
              ),
              div(class = "board-panel-section-title", "Possessions"),
              table_shell(DT::DTOutput("board_uconn_pos_table")),
              div(class = "board-panel-section-title", "Shot Map"),
              div(class = "map-shell", plotOutput("board_uconn_shot_map", click = "board_uconn_shot_map_click", height = "360px")),
              div(class = "board-note", sprintf("Invalid non-FT shot coordinates excluded from map: %s", fmt_int(u_invalid))),
              div(class = "board-panel-section-title", "Possession Event Detail"),
              table_shell(DT::DTOutput("board_uconn_detail_table"))
            ),
            div(
              class = if (o_active) "board-core-panel is-active" else "board-core-panel",
              div(
                class = "board-core-head",
                h4(class = "board-core-title", "Opponent Panel"),
                div(class = if (o_active) "board-core-state active" else "board-core-state", if (o_active) "Active selection" else "Secondary panel")
              ),
              div(class = "board-panel-section-title", "Possessions"),
              table_shell(DT::DTOutput("board_opp_pos_table")),
              div(class = "board-panel-section-title", "Shot Map"),
              div(class = "map-shell", plotOutput("board_opp_shot_map", click = "board_opp_shot_map_click", height = "360px")),
              div(class = "board-note", sprintf("Invalid non-FT shot coordinates excluded from map: %s", fmt_int(o_invalid))),
              div(class = "board-panel-section-title", "Possession Event Detail"),
              table_shell(DT::DTOutput("board_opp_detail_table"))
            )
          )
        ),
        section_card(
          title = "Supporting Metrics Band",
          subtitle = "Grouped context, shot mix, scheme tags, and third-row metrics.",
          div(
            class = "board-support-subsection",
            h4(class = "board-subtitle", "On-Court and Event Context"),
            div(
              class = "board-row",
              make_board_tile("On Court 1", state$on_court[[1]]),
              make_board_tile("On Court 2", state$on_court[[2]]),
              make_board_tile("On Court 3", state$on_court[[3]]),
              make_board_tile("On Court 4", state$on_court[[4]]),
              make_board_tile("On Court 5", state$on_court[[5]]),
              make_board_tile("Against", state$against),
              make_board_tile("Score", state$score),
              make_board_tile("Opponent", state$opponent),
              make_board_tile("Game Type", state$competition_bucket),
              make_board_tile("Previous play", state$prev_poss),
              make_board_tile("Shot Type", state$shot_type),
              make_board_tile("Shot Location", state$shot_location),
              make_board_tile("Shot Result", state$shot_result)
            )
          ),
          div(
            class = "board-support-subsection",
            h4(class = "board-subtitle", "Shot Mix Snapshot"),
            div(
              class = "board-row",
              make_board_tile("UConn Shot Mix", state$shot_mix_u),
              make_board_tile("Opp Shot Mix", state$shot_mix_o)
            )
          ),
          div(
            class = "board-support-subsection",
            h4(class = "board-subtitle", "Scheme Tags"),
            if (isTRUE(state$scheme$available)) {
              div(
                class = "board-row",
                make_board_tile("Transition", state$scheme$values$transition %||% "--"),
                make_board_tile("Quick actions", state$scheme$values$quicks %||% "--"),
                make_board_tile("Set plays", state$scheme$values$sets %||% "--"),
                make_board_tile("Breakdowns", state$scheme$values$breakdowns %||% "--"),
                make_board_tile("Baseline inbound offense", state$scheme$values$blob_o %||% "--"),
                make_board_tile("Zone offense", state$scheme$values$zone_o %||% "--"),
                make_board_tile("Press offense", state$scheme$values$press_o %||% "--"),
                make_board_tile("Baseline inbound defense", state$scheme$values$blob_d %||% "--"),
                make_board_tile("Zone defense", state$scheme$values$zone_d %||% "--"),
                make_board_tile("Press defense", state$scheme$values$press_d %||% "--")
              )
            } else {
              div(class = "board-note", "No tagged scheme data for this game. Scheme tiles are hidden.")
            }
          ),
          div(
            class = "board-support-subsection",
            h4(class = "board-subtitle", "Third-Row Metrics"),
            div(
              class = "board-row",
              make_board_tile("Paint", state$third_row$paint),
              make_board_tile("Mid-Range", state$third_row$mid_range),
              make_board_tile("Three", state$third_row$three),
              make_board_tile("Off Bounce", state$third_row$off_bounce),
              make_board_tile("Side catch-and-shoot", state$third_row$side_cs),
              make_board_tile("Inside catch-and-shoot", state$third_row$inside_cs),
              make_board_tile("Post Up", state$third_row$post_up),
              make_board_tile("Late-clock plays", state$third_row$shot_clock),
              make_board_tile("Offensive rebound rate", state$third_row$oreb),
              make_board_tile("Specialty", state$third_row$specialty)
            ),
            if (nz(state$third_row_reason)) {
              div(class = "board-note", paste("Third-row data limits:", state$third_row_reason))
            }
          )
        )
      )
    })

    output$board_uconn_pos_table <- DT::renderDT({
      pos <- board_side_possessions()$UConn
      if (nrow(pos) == 0) {
        return(DT::datatable(
          data.frame(note = "No UConn possessions for this game."),
          rownames = FALSE,
          selection = "none",
          options = list(dom = "t")
        ))
      }

      show_cols <- intersect(
        c(
          "possession_id", "period_number", "clock_start", "clock_end", "possession_event_count",
          "FGA", "FGM", "FTA", "FTM", "FGA3", "FGM3", "PTS", "OREB", "DREB", "AST", "TOV", "STL", "BLK",
          "UsagePlayer", "AssistPlayer", "ReboundPlayer", "StealPlayer", "BlockPlayer",
          "OffenseOnCourt", "DefenseOnCourt", "type_text", "shot_zone", "shot_side", "paint_zone", "shot_zone_detail"
        ),
        names(pos)
      )
      show <- pos[, show_cols, drop = FALSE]
      sel_pid <- suppressWarnings(as.integer(board_selected_possession_u()))
      sel_idx <- if (is.finite(sel_pid)) which(pos$possession_id == sel_pid)[1] else NA_integer_
      if (!is.finite(sel_idx)) sel_idx <- NULL

      DT::datatable(
        show,
        rownames = FALSE,
        selection = list(mode = "single", selected = sel_idx),
        options = list(pageLength = 8, autoWidth = TRUE, scrollX = TRUE)
      )
    })

    output$board_opp_pos_table <- DT::renderDT({
      pos <- board_side_possessions()$Opponent
      if (nrow(pos) == 0) {
        return(DT::datatable(
          data.frame(note = "No Opponent possessions for this game."),
          rownames = FALSE,
          selection = "none",
          options = list(dom = "t")
        ))
      }

      show_cols <- intersect(
        c(
          "possession_id", "period_number", "clock_start", "clock_end", "possession_event_count",
          "FGA", "FGM", "FTA", "FTM", "FGA3", "FGM3", "PTS", "OREB", "DREB", "AST", "TOV", "STL", "BLK",
          "UsagePlayer", "AssistPlayer", "ReboundPlayer", "StealPlayer", "BlockPlayer",
          "OffenseOnCourt", "DefenseOnCourt", "type_text", "shot_zone", "shot_side", "paint_zone", "shot_zone_detail"
        ),
        names(pos)
      )
      show <- pos[, show_cols, drop = FALSE]
      sel_pid <- suppressWarnings(as.integer(board_selected_possession_o()))
      sel_idx <- if (is.finite(sel_pid)) which(pos$possession_id == sel_pid)[1] else NA_integer_
      if (!is.finite(sel_idx)) sel_idx <- NULL

      DT::datatable(
        show,
        rownames = FALSE,
        selection = list(mode = "single", selected = sel_idx),
        options = list(pageLength = 8, autoWidth = TRUE, scrollX = TRUE)
      )
    })

    observeEvent(input$board_uconn_pos_table_rows_selected, {
      idx <- input$board_uconn_pos_table_rows_selected
      pos <- board_side_possessions()$UConn
      if (length(idx) != 1 || nrow(pos) == 0) return()
      if (!is.finite(idx[[1]]) || idx[[1]] < 1 || idx[[1]] > nrow(pos)) return()
      board_selected_possession_u(as.integer(pos$possession_id[[idx[[1]]]]))
      board_selected_event_u(NA_integer_)
      board_selected_side("UConn")
    }, ignoreInit = TRUE)

    observeEvent(input$board_opp_pos_table_rows_selected, {
      idx <- input$board_opp_pos_table_rows_selected
      pos <- board_side_possessions()$Opponent
      if (length(idx) != 1 || nrow(pos) == 0) return()
      if (!is.finite(idx[[1]]) || idx[[1]] < 1 || idx[[1]] > nrow(pos)) return()
      board_selected_possession_o(as.integer(pos$possession_id[[idx[[1]]]]))
      board_selected_event_o(NA_integer_)
      board_selected_side("Opponent")
    }, ignoreInit = TRUE)

    board_uconn_detail_rows <- reactive({
      rows <- board_side_rows()$UConn
      if (nrow(rows) == 0) return(rows)
      pid <- suppressWarnings(as.integer(board_selected_possession_u()))
      if (!is.finite(pid)) return(rows[0, , drop = FALSE])
      rows[rows$possession_id == pid, , drop = FALSE]
    })

    board_opp_detail_rows <- reactive({
      rows <- board_side_rows()$Opponent
      if (nrow(rows) == 0) return(rows)
      pid <- suppressWarnings(as.integer(board_selected_possession_o()))
      if (!is.finite(pid)) return(rows[0, , drop = FALSE])
      rows[rows$possession_id == pid, , drop = FALSE]
    })

    output$board_uconn_detail_table <- DT::renderDT({
      rows <- board_uconn_detail_rows()
      if (nrow(rows) == 0) {
        return(DT::datatable(
          data.frame(note = "Select a UConn possession to view event details."),
          rownames = FALSE,
          selection = "none",
          options = list(dom = "t")
        ))
      }

      show_cols <- intersect(
        c(
          "row_index", "possession_id", "possession_side", "possession_event_count", "period_number", "clock_label",
          "sequence_number", "game_play_number", "play_id", "is_uconn_offense", "is_uconn_offense_final",
          "UsagePlayer", "AssistPlayer", "ReboundPlayer", "StealPlayer", "BlockPlayer",
          "OffenseOnCourt", "DefenseOnCourt",
          "type_text", "shot_zone", "shot_side", "paint_zone", "shot_zone_detail",
          "FGA", "FGM", "FTA", "FTM", "FGA3", "FGM3", "PTS", "OREB", "DREB", "AST", "TOV", "STL", "BLK",
          "coordinate_x_raw", "coordinate_y_raw", "coordinate_x", "coordinate_y", "coord_valid",
          "event_desc_label"
        ),
        names(rows)
      )
      show <- rows[, show_cols, drop = FALSE]

      DT::datatable(
        show,
        rownames = FALSE,
        selection = "none",
        options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE)
      )
    })

    output$board_opp_detail_table <- DT::renderDT({
      rows <- board_opp_detail_rows()
      if (nrow(rows) == 0) {
        return(DT::datatable(
          data.frame(note = "Select an Opponent possession to view event details."),
          rownames = FALSE,
          selection = "none",
          options = list(dom = "t")
        ))
      }

      show_cols <- intersect(
        c(
          "row_index", "possession_id", "possession_side", "possession_event_count", "period_number", "clock_label",
          "sequence_number", "game_play_number", "play_id", "is_uconn_offense", "is_uconn_offense_final",
          "UsagePlayer", "AssistPlayer", "ReboundPlayer", "StealPlayer", "BlockPlayer",
          "OffenseOnCourt", "DefenseOnCourt",
          "type_text", "shot_zone", "shot_side", "paint_zone", "shot_zone_detail",
          "FGA", "FGM", "FTA", "FTM", "FGA3", "FGM3", "PTS", "OREB", "DREB", "AST", "TOV", "STL", "BLK",
          "coordinate_x_raw", "coordinate_y_raw", "coordinate_x", "coordinate_y", "coord_valid",
          "event_desc_label"
        ),
        names(rows)
      )
      show <- rows[, show_cols, drop = FALSE]

      DT::datatable(
        show,
        rownames = FALSE,
        selection = "none",
        options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE)
      )
    })

    output$board_uconn_shot_map <- renderPlot({
      shots <- board_side_shots_valid()$UConn
      sel_pid <- suppressWarnings(as.integer(board_selected_possession_u()))
      if (is.finite(sel_pid)) {
        shots <- shots[shots$possession_id == sel_pid, , drop = FALSE]
      } else {
        shots <- shots[0, , drop = FALSE]
      }
      shots <- shots[shots$shot_type_label != "Free Throw", , drop = FALSE]

      par(mar = c(1.5, 1.5, 2.2, 1.5))
      draw_full_court()
      title(main = "UConn Shot Map (Selected Possession, Non-FT)", cex.main = 0.95)

      if (nrow(shots) == 0) {
        text(0, 0, "No valid non-FT shot coordinates for selected UConn possession.", cex = 0.9)
        return(invisible(NULL))
      }

      result_txt <- to_chr(shots$shot_result_label)
      shot_cols <- ifelse(
        result_txt == "Made FG",
        "#15803d",
        ifelse(result_txt == "Made FT", "#1d4ed8", "#b91c1c")
      )
      shot_pch <- ifelse(shots$shot_type_label == "Three", 17L, ifelse(shots$shot_type_label == "Free Throw", 15L, 16L))
      points(shots$coord_x_plot, shots$coord_y_plot, pch = shot_pch, col = shot_cols, cex = 1.05)

      if (is.finite(sel_pid)) {
        pos_pts <- shots[shots$possession_id == sel_pid, , drop = FALSE]
        if (nrow(pos_pts) > 0) {
          points(pos_pts$coord_x_plot, pos_pts$coord_y_plot, pch = 21, bg = NA, col = "#111827", cex = 1.6, lwd = 1.2)
        }
      }

      sel_evt <- suppressWarnings(as.integer(board_selected_event_u()))
      if (is.finite(sel_evt)) {
        ev <- shots[shots$row_index == sel_evt, , drop = FALSE]
        if (nrow(ev) > 0) {
          points(ev$coord_x_plot, ev$coord_y_plot, pch = 21, bg = "#fbbf24", col = "#111827", cex = 2.0, lwd = 1.3)
        }
      }
    })

    output$board_opp_shot_map <- renderPlot({
      shots <- board_side_shots_valid()$Opponent
      sel_pid <- suppressWarnings(as.integer(board_selected_possession_o()))
      if (is.finite(sel_pid)) {
        shots <- shots[shots$possession_id == sel_pid, , drop = FALSE]
      } else {
        shots <- shots[0, , drop = FALSE]
      }
      shots <- shots[shots$shot_type_label != "Free Throw", , drop = FALSE]

      par(mar = c(1.5, 1.5, 2.2, 1.5))
      draw_full_court()
      title(main = "Opponent Shot Map (Selected Possession, Non-FT)", cex.main = 0.95)

      if (nrow(shots) == 0) {
        text(0, 0, "No valid non-FT shot coordinates for selected Opponent possession.", cex = 0.9)
        return(invisible(NULL))
      }

      result_txt <- to_chr(shots$shot_result_label)
      shot_cols <- ifelse(
        result_txt == "Made FG",
        "#15803d",
        ifelse(result_txt == "Made FT", "#1d4ed8", "#b91c1c")
      )
      shot_pch <- ifelse(shots$shot_type_label == "Three", 17L, ifelse(shots$shot_type_label == "Free Throw", 15L, 16L))
      points(shots$coord_x_plot, shots$coord_y_plot, pch = shot_pch, col = shot_cols, cex = 1.05)

      if (is.finite(sel_pid)) {
        pos_pts <- shots[shots$possession_id == sel_pid, , drop = FALSE]
        if (nrow(pos_pts) > 0) {
          points(pos_pts$coord_x_plot, pos_pts$coord_y_plot, pch = 21, bg = NA, col = "#111827", cex = 1.6, lwd = 1.2)
        }
      }

      sel_evt <- suppressWarnings(as.integer(board_selected_event_o()))
      if (is.finite(sel_evt)) {
        ev <- shots[shots$row_index == sel_evt, , drop = FALSE]
        if (nrow(ev) > 0) {
          points(ev$coord_x_plot, ev$coord_y_plot, pch = 21, bg = "#fbbf24", col = "#111827", cex = 2.0, lwd = 1.3)
        }
      }
    })

    observeEvent(input$board_uconn_shot_map_click, {
      click <- input$board_uconn_shot_map_click
      shots <- board_side_shots_valid()$UConn
      sel_pid <- suppressWarnings(as.integer(board_selected_possession_u()))
      if (is.finite(sel_pid)) {
        shots <- shots[shots$possession_id == sel_pid, , drop = FALSE]
      } else {
        shots <- shots[0, , drop = FALSE]
      }
      shots <- shots[shots$shot_type_label != "Free Throw", , drop = FALSE]
      if (is.null(click) || nrow(shots) == 0) return()
      if (!is.finite(click$x) || !is.finite(click$y)) return()

      dist2 <- (shots$coord_x_plot - click$x) ^ 2 + (shots$coord_y_plot - click$y) ^ 2
      pick <- which.min(dist2)
      if (length(pick) == 1 && is.finite(dist2[[pick]])) {
        board_selected_possession_u(as.integer(shots$possession_id[[pick]]))
        board_selected_event_u(as.integer(shots$row_index[[pick]]))
        board_selected_side("UConn")
      }
    }, ignoreInit = TRUE)

    observeEvent(input$board_opp_shot_map_click, {
      click <- input$board_opp_shot_map_click
      shots <- board_side_shots_valid()$Opponent
      sel_pid <- suppressWarnings(as.integer(board_selected_possession_o()))
      if (is.finite(sel_pid)) {
        shots <- shots[shots$possession_id == sel_pid, , drop = FALSE]
      } else {
        shots <- shots[0, , drop = FALSE]
      }
      shots <- shots[shots$shot_type_label != "Free Throw", , drop = FALSE]
      if (is.null(click) || nrow(shots) == 0) return()
      if (!is.finite(click$x) || !is.finite(click$y)) return()

      dist2 <- (shots$coord_x_plot - click$x) ^ 2 + (shots$coord_y_plot - click$y) ^ 2
      pick <- which.min(dist2)
      if (length(pick) == 1 && is.finite(dist2[[pick]])) {
        board_selected_possession_o(as.integer(shots$possession_id[[pick]]))
        board_selected_event_o(as.integer(shots$row_index[[pick]]))
        board_selected_side("Opponent")
      }
    }, ignoreInit = TRUE)

    board_state <- reactive({
      rows <- board_event_rows()
      if (nrow(rows) == 0) {
        return(list(selected = FALSE, message = "Select a game to view board metrics."))
      }

      sides <- board_side_rows()
      pos <- board_side_possessions()

      focus_side <- board_selected_side() %||% "UConn"
      if (!(focus_side %in% c("UConn", "Opponent"))) focus_side <- "UConn"

      focus_rows <- data.frame()
      selected_pid <- NA_integer_
      if (focus_side == "UConn") {
        pid <- suppressWarnings(as.integer(board_selected_possession_u()))
        if (is.finite(pid)) selected_pid <- as.integer(pid)
        if (is.finite(pid)) {
          focus_rows <- sides$UConn[sides$UConn$possession_id == pid, , drop = FALSE]
        }
      } else {
        pid <- suppressWarnings(as.integer(board_selected_possession_o()))
        if (is.finite(pid)) selected_pid <- as.integer(pid)
        if (is.finite(pid)) {
          focus_rows <- sides$Opponent[sides$Opponent$possession_id == pid, , drop = FALSE]
        }
      }
      if (nrow(focus_rows) == 0) focus_rows <- rows[1, , drop = FALSE]

      row <- focus_rows[1, , drop = FALSE]
      idx <- suppressWarnings(as.integer(row$row_index[[1]]))
      if (!is.finite(idx)) idx <- 1L
      prev <- if (idx > 1 && idx <= nrow(rows)) rows[idx - 1, , drop = FALSE] else NULL

      is_uconn_off <- as_logical_strict(row$is_uconn_offense_final)[[1]]
      u_src <- if (isTRUE(is_uconn_off)) row$OffenseOnCourt else row$DefenseOnCourt
      o_src <- if (isTRUE(is_uconn_off)) row$DefenseOnCourt else row$OffenseOnCourt

      u_players <- split_five(u_src)
      opp_players <- split_five(o_src)

      away <- to_num(row$away_score)
      home <- to_num(row$home_score)
      score_text <- if (is.finite(away) && is.finite(home)) paste0(as.integer(round(away)), "-", as.integer(round(home))) else "--"

      prev_poss <- if (!is.null(prev)) {
        first_nonempty(prev$short_description, prev$text, "Start of game")
      } else {
        "Start of game"
      }

      game_file <- first_nonempty(row$game_file)
      opponent <- first_nonempty(row$opponent)
      comp_bucket <- first_nonempty(row$competition_bucket)

      u_pos_n <- nrow(pos$UConn)
      o_pos_n <- nrow(pos$Opponent)
      u_pts <- if (u_pos_n > 0) sum(to_num(pos$UConn$PTS), na.rm = TRUE) else NA_real_
      o_pts <- if (o_pos_n > 0) sum(to_num(pos$Opponent$PTS), na.rm = TRUE) else NA_real_

      poss_pair <- metric_pair(
        if (u_pos_n > 0) u_pos_n else NA_real_,
        if (o_pos_n > 0) o_pos_n else NA_real_,
        pct = FALSE,
        digits = 0,
        u_reason = "No UConn possessions",
        o_reason = "No Opponent possessions"
      )
      ppp_pair <- metric_pair(
        if (u_pos_n > 0) u_pts / u_pos_n else NA_real_,
        if (o_pos_n > 0) o_pts / o_pos_n else NA_real_,
        pct = FALSE,
        digits = 2,
        u_reason = "UConn possession denominator missing",
        o_reason = "Opponent possession denominator missing"
      )

      u_four <- calc_side_four_factors(pos$UConn, if (u_pos_n > 0) u_pos_n else NA_real_)
      o_four <- calc_side_four_factors(pos$Opponent, if (o_pos_n > 0) o_pos_n else NA_real_)
      efg_pair <- metric_pair(u_four$efg$value, o_four$efg$value, pct = TRUE, digits = 1, u_reason = u_four$efg$reason, o_reason = o_four$efg$reason)
      to_pair <- metric_pair(u_four$to$value, o_four$to$value, pct = TRUE, digits = 1, u_reason = u_four$to$reason, o_reason = o_four$to$reason)
      oreb_pair <- metric_pair(u_four$oreb$value, o_four$oreb$value, pct = TRUE, digits = 1, u_reason = u_four$oreb$reason, o_reason = o_four$oreb$reason)
      ftr_pair <- metric_pair(u_four$ftr$value, o_four$ftr$value, pct = TRUE, digits = 1, u_reason = u_four$ftr$reason, o_reason = o_four$ftr$reason)

      game_id_num <- if ("game_id" %in% names(row)) first_num_or_na(row$game_id) else NA_real_
      scheme <- compute_scheme_metrics(game_id_num, dash_data()$scheme_tables)
      third_row_result <- calc_third_row_metrics(u_rows = sides$UConn, scheme = scheme, oreb_pair = oreb_pair)
      third_row <- third_row_result$values

      u_shots <- sides$UConn[sides$UConn$is_shot_event %in% TRUE, , drop = FALSE]
      o_shots <- sides$Opponent[sides$Opponent$is_shot_event %in% TRUE, , drop = FALSE]
      shot_mix_u <- if (nrow(u_shots) > 0) {
        paste0("Top type: ", top_label_count(u_shots$shot_type_label), " | Top location: ", top_label_count(u_shots$shot_location_zone))
      } else {
        "Top type/location unavailable"
      }
      shot_mix_o <- if (nrow(o_shots) > 0) {
        paste0("Top type: ", top_label_count(o_shots$shot_type_label), " | Top location: ", top_label_count(o_shots$shot_location_zone))
      } else {
        "Top type/location unavailable"
      }

      list(
        selected = TRUE,
        game_key = input$board_game %||% game_file,
        row_index = idx,
        row_total = nrow(rows),
        event_desc = first_nonempty(row$event_desc_label, row$short_description, row$text),
        game_file = game_file,
        opponent = opponent,
        competition_bucket = comp_bucket,
        active_side = focus_side,
        selected_possession_id = selected_pid,
        on_court = u_players,
        against = paste(opp_players, collapse = " | "),
        score = score_text,
        prev_poss = prev_poss,
        shot_type = first_nonempty(row$shot_type_label),
        shot_location = first_nonempty(row$shot_location_label),
        shot_result = first_nonempty(row$shot_result_label),
        shot_mix_u = shot_mix_u,
        shot_mix_o = shot_mix_o,
        scheme = scheme,
        third_row = third_row,
        third_row_reason = third_row_result$reason,
        poss_pair = poss_pair,
        ppp_pair = ppp_pair,
        efg_pair = efg_pair,
        to_pair = to_pair,
        oreb_pair = oreb_pair,
        ftr_pair = ftr_pair
      )
    })

    output$game_board_ui <- renderUI({
      state <- board_state()

      if (!isTRUE(state$selected)) {
        return(section_card(
          title = "Current Game Context",
          subtitle = "Sticky context appears after selecting a game.",
          div(class = "board-note", state$message)
        ))
      }

      pid_txt <- if (is.finite(state$selected_possession_id)) fmt_int(state$selected_possession_id) else "--"

      section_card(
        title = "Current Game Context",
        subtitle = "game_key, opponent, event index, active side, and selected possession stay visible while scrolling at widths >=992px.",
        div(
          class = "board-context-strip",
          div(
            class = "board-context-head",
            h4(class = "board-context-title", "Game Board Context"),
            div(class = "board-context-sub", state$event_desc)
          ),
          div(
            class = "context-chip-row",
            div(class = "context-chip", span(class = "context-chip-label", "Game Key"), span(state$game_key)),
            div(class = "context-chip", span(class = "context-chip-label", "Opponent"), span(state$opponent)),
            div(class = "context-chip", span(class = "context-chip-label", "Event"), span(sprintf("%d/%d", state$row_index, state$row_total))),
            div(class = "context-chip context-chip-active", span(class = "context-chip-label", "Active Side"), span(state$active_side)),
            div(class = "context-chip", span(class = "context-chip-label", "Possession"), span(pid_txt))
          )
        )
      )
    })
  }

  shinyApp(ui = ui, server = server)
}

run_dashboard_app <- function(output_dir = "_outputs", host = "127.0.0.1", port = 3838L) {
  app <- build_dashboard_app(output_dir = output_dir)
  shiny::runApp(app, host = host, port = as.integer(port), launch.browser = interactive())
}
