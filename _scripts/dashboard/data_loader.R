`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) return(y)
  x
}

if (!exists("%>%", mode = "function")) {
  `%>%` <- dplyr::`%>%`
}

to_num <- function(x) suppressWarnings(as.numeric(x))

to_chr <- function(x) {
  if (is.null(x)) return(character())
  as.character(x)
}

nz <- function(x) {
  y <- trimws(to_chr(x))
  !is.na(y) & y != ""
}

as_logical_vec <- function(x) {
  if (is.logical(x)) return(x)
  y <- tolower(trimws(to_chr(x)))
  y %in% c("true", "t", "1", "yes", "y")
}

parse_any_date <- function(x) {
  raw <- trimws(to_chr(x))
  out <- as.Date(rep(NA_character_, length(raw)))

  if (length(raw) == 0) return(out)

  fmts <- c("%Y-%m-%d", "%m/%d/%Y", "%m/%d/%y", "%Y/%m/%d")
  remaining <- rep(TRUE, length(raw))

  for (fmt in fmts) {
    if (!any(remaining)) break
    parsed <- as.Date(raw[remaining], format = fmt)
    hit <- !is.na(parsed)
    if (any(hit)) {
      idx <- which(remaining)[hit]
      out[idx] <- parsed[hit]
      remaining[idx] <- FALSE
    }
  }

  out
}

fmt_q_int <- function(x) {
  if (!is.finite(x)) return("--")
  format(as.integer(round(x)), big.mark = ",", trim = TRUE)
}

fmt_q_num <- function(x, digits = 2) {
  if (!is.finite(x)) return("--")
  format(round(x, digits), nsmall = digits, trim = TRUE, big.mark = ",")
}

fmt_q_pct <- function(x, digits = 1) {
  if (!is.finite(x)) return("--")
  paste0(format(round(100 * x, digits), nsmall = digits, trim = TRUE), "%")
}

safe_weighted_mean <- function(x, w) {
  xv <- to_num(x)
  wv <- to_num(w)
  keep <- is.finite(xv) & is.finite(wv) & wv > 0
  if (!any(keep)) return(NA_real_)
  sum(xv[keep] * wv[keep]) / sum(wv[keep])
}

first_text <- function(x, default = "--") {
  vals <- trimws(to_chr(x))
  vals <- vals[!is.na(vals) & vals != ""]
  if (length(vals) == 0) return(default)
  vals[[1]]
}

metric_lookup <- function(metric_df, metric_name) {
  if (is.null(metric_df) || nrow(metric_df) == 0) return(NA_real_)
  if (!all(c("metric", "value") %in% names(metric_df))) return(NA_real_)
  hit <- metric_df$value[trimws(to_chr(metric_df$metric)) == metric_name]
  if (length(hit) == 0) return(NA_real_)
  to_num(hit[[1]])
}

metric_lookup_text <- function(metric_df, metric_name) {
  if (is.null(metric_df) || nrow(metric_df) == 0) return("")
  if (!all(c("metric", "value") %in% names(metric_df))) return("")
  hit <- metric_df$value[trimws(to_chr(metric_df$metric)) == metric_name]
  if (length(hit) == 0) return("")
  first_text(hit, default = "")
}

safe_read_csv <- function(path) {
  tryCatch(
    {
      suppressMessages(readr::read_csv(path, show_col_types = FALSE, progress = FALSE))
    },
    error = function(e) {
      structure(list(error = conditionMessage(e)), class = "read_error")
    }
  )
}

required_output_specs <- function() {
  data.frame(
    relative_path = c(
      file.path("01_lineup_core", "uconn_lineup_coach_view.csv"),
      file.path("02_defense_leaks", "uconn_lineup_def_leaks_coach_table.csv"),
      file.path("03_players", "uconn_player_rci_coach_table.csv"),
      file.path("05_decision_audit", "uconn_lineup_decision_rolling_backtest_by_bucket.csv"),
      file.path("05_decision_audit", "uconn_pred_pr_net_pos_calibration_metrics.csv"),
      file.path("05_decision_audit", "uconn_availability_stress_test_report.csv")
    ),
    stringsAsFactors = FALSE
  )
}

expected_output_buckets <- function() {
  c(
    "01_lineup_core",
    "02_defense_leaks",
    "03_players",
    "04_games_trends",
    "05_decision_audit",
    "06_scheme_matchups",
    "07_opps"
  )
}

required_columns_by_relative_path <- function() {
  list(
    "01_lineup_core/uconn_lineup_coach_view.csv" = c(
      "lineup_pretty", "possessions", "decision_survive_score_robust", "decision_def_ppp_pred", "Decision", "expected_points_per_40"
    ),
    "02_defense_leaks/uconn_lineup_def_leaks_coach_table.csv" = c(
      "lineup_pretty", "possessions", "pr_leak", "confidence", "leak_flag"
    ),
    "03_players/uconn_player_rci_coach_table.csv" = c(
      "player", "total_possessions", "RCI", "net_pr_pos", "role_type", "recommendation"
    ),
    "05_decision_audit/uconn_lineup_decision_rolling_backtest_by_bucket.csv" = c(
      "Decision", "total_holdout_possessions", "weighted_survive_score_robust", "weighted_observed_survive4_rate", "weighted_observed_def_ppp", "weighted_opp_fragile_rate"
    ),
    "05_decision_audit/uconn_pred_pr_net_pos_calibration_metrics.csv" = c(
      "metric", "value"
    ),
    "05_decision_audit/uconn_availability_stress_test_report.csv" = c(
      "generated_at_utc",
      "scenario_player_out",
      "top_n_requested",
      "max_pr_leak",
      "survivors_n",
      "lost_n",
      "section",
      "rank",
      "lineup_key",
      "lineup_pretty",
      "possessions",
      "sample_tier",
      "trust_baseline",
      "pr_leak"
    )
  )
}

required_manual_game_columns <- function() {
  c(
    "game_file",
    "opponent",
    "is_uconn_offense",
    "OffenseOnCourt",
    "DefenseOnCourt",
    "away_score",
    "home_score",
    "FGA",
    "FGM",
    "FGM3",
    "FTA",
    "TOV",
    "OREB",
    "DREB"
  )
}

list_bucketed_output_csvs <- function(output_dir = "_outputs") {
  if (!dir.exists(output_dir)) {
    return(data.frame(
      bucket = character(),
      relative_path = character(),
      full_path = character(),
      stringsAsFactors = FALSE
    ))
  }

  buckets <- list.dirs(output_dir, recursive = FALSE, full.names = FALSE)
  buckets <- buckets[grepl("^[0-9]{2}_", buckets)]

  rows <- list()
  idx <- 1L

  for (bucket in sort(buckets)) {
    bucket_dir <- file.path(output_dir, bucket)
    csvs <- list.files(bucket_dir, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
    if (length(csvs) == 0) next

    for (p in sort(csvs)) {
      rel <- sub(paste0("^", output_dir, "/"), "", p)
      rows[[idx]] <- data.frame(
        bucket = bucket,
        relative_path = rel,
        full_path = p,
        stringsAsFactors = FALSE
      )
      idx <- idx + 1L
    }
  }

  if (length(rows) == 0) {
    return(data.frame(
      bucket = character(),
      relative_path = character(),
      full_path = character(),
      stringsAsFactors = FALSE
    ))
  }

  dplyr::bind_rows(rows)
}

load_scheme_tables <- function() {
  base <- file.path("_data", "05_projects", "scheme_matchup_project")

  files <- c(
    possessions = "possessions.csv",
    scheme_tags = "scheme_tags.csv",
    events = "events.csv",
    breakdown_tags = "breakdown_tags.csv",
    player_dev_tags = "player_dev_tags.csv",
    clip_playlist = "clip_playlist.csv"
  )

  tables <- list()
  status_rows <- list()
  warnings <- character()

  i <- 1L
  for (nm in names(files)) {
    path <- file.path(base, files[[nm]])
    if (!file.exists(path)) {
      tables[[nm]] <- data.frame()
      status_rows[[i]] <- dplyr::tibble(
        source_type = "scheme_input",
        bucket = "scheme_matchup_project",
        path = path,
        required = FALSE,
        status = "MISSING_OPTIONAL",
        message = "Scheme table missing.",
        rows = NA_integer_,
        cols = NA_integer_,
        modified_at = as.POSIXct(NA)
      )
      i <- i + 1L
      next
    }

    df <- safe_read_csv(path)
    if (inherits(df, "read_error")) {
      tables[[nm]] <- data.frame()
      warnings <- c(warnings, sprintf("Scheme table read failed (%s): %s", path, df$error))
      status_rows[[i]] <- dplyr::tibble(
        source_type = "scheme_input",
        bucket = "scheme_matchup_project",
        path = path,
        required = FALSE,
        status = "READ_ERROR",
        message = df$error,
        rows = NA_integer_,
        cols = NA_integer_,
        modified_at = file.info(path)$mtime[[1]]
      )
      i <- i + 1L
      next
    }

    tables[[nm]] <- df
    status_rows[[i]] <- dplyr::tibble(
      source_type = "scheme_input",
      bucket = "scheme_matchup_project",
      path = path,
      required = FALSE,
      status = "OK",
      message = "",
      rows = nrow(df),
      cols = ncol(df),
      modified_at = file.info(path)$mtime[[1]]
    )
    i <- i + 1L
  }

  list(
    tables = tables,
    status = dplyr::bind_rows(status_rows),
    warnings = warnings
  )
}

load_manual_games <- function() {
  dirs <- c(
    file.path("_data", "03_manual_game_csv", "_conf"),
    file.path("_data", "03_manual_game_csv", "_nc")
  )

  files <- unlist(lapply(dirs, function(d) {
    if (!dir.exists(d)) return(character())
    list.files(d, pattern = "\\.csv$", full.names = TRUE)
  }), use.names = FALSE)

  files <- sort(unique(files))

  if (length(files) == 0) {
    return(list(
      games = data.frame(),
      status = dplyr::tibble(
        source_type = "manual_games",
        bucket = "manual_csv",
        path = file.path("_data", "03_manual_game_csv", "_conf|_nc"),
        required = TRUE,
        status = "MISSING_REQUIRED",
        message = "No manual game CSV files found in _conf/_nc.",
        rows = NA_integer_,
        cols = NA_integer_,
        modified_at = as.POSIXct(NA)
      ),
      errors = c("No manual game CSV files found in _data/03_manual_game_csv/_conf or _nc")
    ))
  }

  chunks <- vector("list", length(files))
  bad <- character()

  for (i in seq_along(files)) {
    path <- files[[i]]
    df <- safe_read_csv(path)

    if (inherits(df, "read_error")) {
      bad <- c(bad, sprintf("Manual game CSV read failed (%s): %s", path, df$error))
      next
    }

    bucket <- if (grepl("/_conf/", path)) "conference" else "non_conference"

    df$manual_source_path <- path
    df$competition_bucket <- bucket
    chunks[[i]] <- df
  }

  chunks <- chunks[!vapply(chunks, is.null, logical(1))]
  if (length(chunks) == 0) {
    return(list(
      games = data.frame(),
      status = dplyr::tibble(
        source_type = "manual_games",
        bucket = "manual_csv",
        path = file.path("_data", "03_manual_game_csv", "_conf|_nc"),
        required = TRUE,
        status = "READ_ERROR",
        message = "All manual CSV reads failed.",
        rows = NA_integer_,
        cols = NA_integer_,
        modified_at = as.POSIXct(NA)
      ),
      errors = c(bad, "All manual game CSV reads failed")
    ))
  }

  games <- dplyr::bind_rows(chunks)

  if ("game_date" %in% names(games)) {
    games$game_date_parsed <- parse_any_date(games$game_date)
  } else {
    games$game_date_parsed <- as.Date(NA)
  }

  missing_required <- setdiff(required_manual_game_columns(), names(games))

  status_code <- if (length(bad) == 0) "OK" else "WARN_PARTIAL_READ"
  status_message_parts <- character()
  if (length(bad) > 0) {
    status_message_parts <- c(status_message_parts, paste(utils::head(bad, 3), collapse = " | "))
  }
  if (length(missing_required) > 0) {
    status_code <- "INVALID_SCHEMA"
    status_message_parts <- c(
      status_message_parts,
      sprintf("Missing columns: %s", paste(missing_required, collapse = ", "))
    )
  }

  status <- dplyr::tibble(
    source_type = "manual_games",
    bucket = "manual_csv",
    path = file.path("_data", "03_manual_game_csv", "_conf|_nc"),
    required = TRUE,
    status = status_code,
    message = paste(status_message_parts, collapse = " | "),
    rows = nrow(games),
    cols = ncol(games),
    modified_at = if (length(files) > 0) max(file.info(files)$mtime, na.rm = TRUE) else as.POSIXct(NA)
  )

  errors <- bad
  if (length(missing_required) > 0) {
    errors <- c(
      errors,
      sprintf(
        "Manual game CSV schema missing required columns: %s",
        paste(missing_required, collapse = ", ")
      )
    )
  }

  list(
    games = games,
    status = status,
    errors = errors
  )
}

build_board_index <- function(manual_games, outputs_by_path) {
  if (is.null(manual_games) || nrow(manual_games) == 0) {
    return(dplyr::tibble())
  }

  manual_idx <- manual_games %>%
    dplyr::mutate(
      game_file = to_chr(.data$game_file),
      opponent = to_chr(.data$opponent),
      competition_bucket = to_chr(.data$competition_bucket),
      meeting_site = dplyr::case_when(
        nz(.data$site_type) & tolower(to_chr(.data$site_type)) == "neutral" ~ "neutral",
        as_logical_vec(.data$uconn_is_home) ~ "home",
        TRUE ~ "away"
      ),
      game_date = .data$game_date_parsed
    ) %>%
    dplyr::filter(nz(.data$game_file), nz(.data$opponent), nz(.data$competition_bucket)) %>%
    dplyr::distinct(competition_bucket, opponent, game_file, game_date, meeting_site)

  idx <- manual_idx

  idx <- idx %>%
    dplyr::filter(
      !grepl("exhibition", .data$game_file, ignore.case = TRUE),
      !grepl("exhibition", .data$opponent, ignore.case = TRUE)
    ) %>%
    dplyr::arrange(dplyr::desc(.data$game_date), .data$opponent, .data$game_file) %>%
    dplyr::mutate(
      game_key = .data$game_file,
      game_label = paste(
        ifelse(is.na(.data$game_date), "NA_DATE", as.character(.data$game_date)),
        .data$opponent,
        .data$meeting_site,
        .data$game_file,
        sep = " | "
      )
    )

  idx
}

build_qc_table <- function(outputs_by_path) {
  cal <- outputs_by_path[[file.path("05_decision_audit", "uconn_pred_pr_net_pos_calibration_metrics.csv")]]
  lineup <- outputs_by_path[[file.path("01_lineup_core", "uconn_lineup_coach_view.csv")]]

  metric_value <- function(metric_name) {
    if (is.null(cal) || !all(c("metric", "value") %in% names(cal))) return(NA_real_)
    hit <- cal$value[cal$metric == metric_name]
    if (length(hit) == 0) return(NA_real_)
    to_num(hit[[1]])
  }

  n_rows_used <- metric_value("n_rows_used")
  holdout_possessions <- metric_value("sum_holdout_possessions")
  weighted_ece <- metric_value("weighted_ece_decile")

  lineups_30 <- NA_real_
  if (!is.null(lineup) && "possessions" %in% names(lineup)) {
    lineups_30 <- sum(to_num(lineup$possessions) >= 30, na.rm = TRUE)
  }

  dplyr::bind_rows(
    dplyr::tibble(
      check = "Rows Used",
      comparator = ">=",
      threshold = 150,
      value = n_rows_used,
      pass = is.finite(n_rows_used) && n_rows_used >= 150
    ),
    dplyr::tibble(
      check = "Holdout Possessions",
      comparator = ">=",
      threshold = 500,
      value = holdout_possessions,
      pass = is.finite(holdout_possessions) && holdout_possessions >= 500
    ),
    dplyr::tibble(
      check = "Weighted ECE",
      comparator = "<=",
      threshold = 0.25,
      value = weighted_ece,
      pass = is.finite(weighted_ece) && weighted_ece <= 0.25
    ),
    dplyr::tibble(
      check = "Lineups With >=30 Possessions",
      comparator = ">=",
      threshold = 1,
      value = lineups_30,
      pass = is.finite(lineups_30) && lineups_30 >= 1
    )
  )
}

collect_latest_opponent_scout_sources <- function(outputs_by_path) {
  manifest_rel <- file.path("07_opps", "manual_game_scouts", "manual_game_scout_manifest.csv")
  manifest <- outputs_by_path[[manifest_rel]]

  tables <- list(
    opp_manifest = manifest %||% data.frame()
  )
  paths <- list(
    opp_manifest = manifest_rel
  )
  present <- list(
    opp_manifest = !is.null(manifest)
  )

  latest_dir_rel <- NA_character_
  if (!is.null(manifest) && nrow(manifest) > 0 && all(c("output_dir", "game_date") %in% names(manifest))) {
    game_dates <- parse_any_date(manifest$game_date)
    ord <- order(game_dates, na.last = TRUE, decreasing = TRUE)
    latest <- manifest[ord[1], , drop = FALSE]
    out_dir <- first_text(latest$output_dir, default = "")
    if (nz(out_dir)) {
      latest_dir_rel <- sub("^_outputs/", "", out_dir)
    }
  } else if (!is.null(manifest) && nrow(manifest) > 0 && "output_dir" %in% names(manifest)) {
    out_dir <- first_text(manifest$output_dir, default = "")
    if (nz(out_dir)) {
      latest_dir_rel <- sub("^_outputs/", "", out_dir)
    }
  }

  rel_map <- list(
    opp_meeting_summary = if (!is.na(latest_dir_rel)) file.path(latest_dir_rel, "summary", "meeting_summary.csv") else NA_character_,
    opp_lineup_profile = if (!is.na(latest_dir_rel)) file.path(latest_dir_rel, "lineups", "opponent_offense_profile.csv") else NA_character_,
    opp_shot_profile = if (!is.na(latest_dir_rel)) file.path(latest_dir_rel, "shots", "opponent_shot_profile.csv") else NA_character_,
    opp_clutch_events = if (!is.na(latest_dir_rel)) file.path(latest_dir_rel, "clutch", "event_log.csv") else NA_character_,
    opp_turnover_creators = if (!is.na(latest_dir_rel)) file.path(latest_dir_rel, "turnovers", "creators.csv") else NA_character_,
    opp_turnover_victims = if (!is.na(latest_dir_rel)) file.path(latest_dir_rel, "turnovers", "victims.csv") else NA_character_,
    opp_matchup_matrix = if (!is.na(latest_dir_rel)) file.path(latest_dir_rel, "lineups", "matchup_matrix.csv") else NA_character_
  )

  for (nm in names(rel_map)) {
    rel <- rel_map[[nm]]
    if (!nz(rel)) {
      tables[[nm]] <- data.frame()
      paths[[nm]] <- NA_character_
      present[[nm]] <- FALSE
      next
    }

    hit <- outputs_by_path[[rel]]
    tables[[nm]] <- hit %||% data.frame()
    paths[[nm]] <- rel
    present[[nm]] <- !is.null(hit)
  }

  list(
    tables = tables,
    paths = paths,
    present = present
  )
}

build_question_source_catalog <- function(outputs_by_path) {
  base_map <- list(
    lineup_coach_view = file.path("01_lineup_core", "uconn_lineup_coach_view.csv"),
    lineup_shot_diet_stable = file.path("01_lineup_core", "uconn_lineup_shot_diet_stable.csv"),
    lineup_usage = file.path("01_lineup_core", "uconn_lineup_usage.csv"),
    core_model_diag = file.path("01_lineup_core", "uconn_lineup_core_model_diagnostics.csv"),
    def_leaks_coach = file.path("02_defense_leaks", "uconn_lineup_def_leaks_coach_table.csv"),
    def_repeat_offenders = file.path("02_defense_leaks", "uconn_def_leak_repeat_offenders.csv"),
    def_game_trend = file.path("02_defense_leaks", "uconn_game_level_def_leak_trend.csv"),
    def_holdout_bucket = file.path("02_defense_leaks", "uconn_def_leaks_holdout_validation_by_bucket.csv"),
    def_model_diag = file.path("02_defense_leaks", "uconn_lineup_def_leaks_model_diagnostics.csv"),
    player_rci_coach = file.path("03_players", "uconn_player_rci_coach_table.csv"),
    player_creation = file.path("03_players", "uconn_player_creation_profile.csv"),
    player_rci_windows = file.path("03_players", "uconn_player_rci_three_windows.csv"),
    decision_backtest_bucket = file.path("05_decision_audit", "uconn_lineup_decision_rolling_backtest_by_bucket.csv"),
    decision_calibration_metrics = file.path("05_decision_audit", "uconn_pred_pr_net_pos_calibration_metrics.csv"),
    decision_thresholds = file.path("05_decision_audit", "uconn_lineup_decision_rule_v2_thresholds.csv"),
    decision_eligibility = file.path("05_decision_audit", "uconn_decision_eligibility_by_stint.csv")
  )

  tables <- lapply(base_map, function(rel) outputs_by_path[[rel]] %||% data.frame())
  present <- lapply(base_map, function(rel) !is.null(outputs_by_path[[rel]]))
  paths <- base_map

  scout <- collect_latest_opponent_scout_sources(outputs_by_path = outputs_by_path)
  tables <- c(tables, scout$tables)
  paths <- c(paths, scout$paths)
  present <- c(present, scout$present)

  list(
    tables = tables,
    paths = paths,
    present = present
  )
}

question_success <- function(answer_text) {
  list(ok = TRUE, answer_text = answer_text)
}

question_fail <- function(reason) {
  list(ok = FALSE, reason = reason)
}

top_row_by <- function(df, value_col, decreasing = TRUE) {
  if (is.null(df) || nrow(df) == 0 || !value_col %in% names(df)) return(NULL)
  vals <- to_num(df[[value_col]])
  ord <- order(vals, decreasing = decreasing, na.last = NA)
  if (length(ord) == 0) return(NULL)
  df[ord[1], , drop = FALSE]
}

is_stable_sample <- function(x) {
  txt <- tolower(trimws(to_chr(x)))
  txt != "" & !is.na(txt) & !grepl("small", txt)
}

coach_view_sections <- c(
  "Season Scope",
  "Lineups",
  "Defense",
  "Players",
  "Decision Quality",
  "Opponent Scouts"
)

display_decision_label <- function(x) {
  txt <- toupper(trimws(first_text(x, default = "")))
  if (txt == "DEF_FLOOR_PASS_UPSIDE_HIGH") return("Defense floor pass, high upside (DEF_FLOOR_PASS_UPSIDE_HIGH)")
  if (txt == "DEF_FLOOR_PASS_UPSIDE_MED") return("Defense floor pass, medium upside (DEF_FLOOR_PASS_UPSIDE_MED)")
  if (txt == "DEF_FLOOR_PASS_UPSIDE_LOW") return("Defense floor pass, low upside (DEF_FLOOR_PASS_UPSIDE_LOW)")
  if (txt == "DEF_FLOOR_FAIL") return("Defense floor fail (DEF_FLOOR_FAIL)")
  if (txt == "LOW_SAMPLE") return("Low sample (LOW_SAMPLE)")
  if (txt == "UNSEEN") return("Unseen (UNSEEN)")
  first_text(x)
}

coach_copy_guard_findings <- function(question_rows) {
  if (is.null(question_rows) || nrow(question_rows) == 0) {
    return(dplyr::tibble(question_id = character(), message = character()))
  }

  rows <- question_rows[to_chr(question_rows$section) %in% coach_view_sections, , drop = FALSE]
  if (nrow(rows) == 0) {
    return(dplyr::tibble(question_id = character(), message = character()))
  }

  has_pattern <- function(txt, pattern) {
    grepl(pattern, txt, ignore.case = TRUE, perl = TRUE)
  }

  findings <- list()
  idx <- 1L

  for (i in seq_len(nrow(rows))) {
    qid <- first_text(rows$question_id[i], default = paste0("row_", i))
    status_txt <- toupper(first_text(rows$status[i], default = ""))
    blob <- paste(
      c(
        first_text(rows$question_text[i], default = ""),
        if (status_txt == "AVAILABLE") first_text(rows$answer_text[i], default = "") else ""
      ),
      collapse = " | "
    )
    blob <- trimws(blob)
    if (!nz(blob)) next

    violations <- character()

    if (has_pattern(blob, "\\bholdout\\b")) {
      violations <- c(violations, "Use 'out-of-sample' instead of 'holdout'.")
    }
    if (has_pattern(blob, "\\bbucket\\b")) {
      violations <- c(violations, "Use 'group' instead of 'bucket'.")
    }
    if (has_pattern(blob, "\\bstints?\\b")) {
      violations <- c(violations, "Use 'lineup stretch(es)' instead of 'stint/stints'.")
    }
    if (has_pattern(blob, "divergent\\s+transitions")) {
      violations <- c(violations, "Use 'sampler warning count (divergent transitions)'.")
    }
    if (has_pattern(blob, "calibration\\s+mode/status")) {
      violations <- c(violations, "Use 'probability-adjustment method/status'.")
    }
    if (has_pattern(blob, "\\bPR\\s+thresholds\\b")) {
      violations <- c(violations, "Use 'positive-outcome probability cutoffs' instead of 'PR thresholds'.")
    }
    if (has_pattern(blob, "\\bPR\\s+leak\\b") && !has_pattern(blob, "leak\\s+risk\\s+score\\s*\\(PR\\s+leak\\)")) {
      violations <- c(violations, "Expand 'PR leak' as 'leak risk score (PR leak)'.")
    }
    if (has_pattern(blob, "DEF_FLOOR_PASS_UPSIDE_HIGH") && !has_pattern(blob, "high\\s+upside\\s*\\(DEF_FLOOR_PASS_UPSIDE_HIGH\\)")) {
      violations <- c(violations, "Expand 'DEF_FLOOR_PASS_UPSIDE_HIGH' on first use.")
    }
    if (has_pattern(blob, "DEF_FLOOR_FAIL") && !has_pattern(blob, "Defense\\s+floor\\s+fail\\s*\\(DEF_FLOOR_FAIL\\)")) {
      violations <- c(violations, "Expand 'DEF_FLOOR_FAIL' on first use.")
    }
    if (has_pattern(blob, "\\bPPP\\b") && !has_pattern(blob, "points\\s+per\\s+possession\\s*\\(PPP\\)")) {
      violations <- c(violations, "Expand PPP as 'points per possession (PPP)' on first use.")
    }
    if (has_pattern(blob, "OReb%") && !has_pattern(blob, "offensive\\s+rebound\\s+rate\\s*\\(OReb%\\)")) {
      violations <- c(violations, "Expand OReb% as 'offensive rebound rate (OReb%)' on first use.")
    }
    if (has_pattern(blob, "\\bRCI\\b") && !has_pattern(blob, "role\\s+concentration\\s+index\\s*\\(RCI\\)")) {
      violations <- c(violations, "Expand RCI as 'role concentration index (RCI)' on first use.")
    }
    if (has_pattern(blob, "\\bECE\\b") && !has_pattern(blob, "calibration\\s+error\\s*\\(ECE\\)")) {
      violations <- c(violations, "Expand ECE as 'calibration error (ECE)' on first use.")
    }

    if (length(violations) > 0) {
      for (msg in unique(violations)) {
        findings[[idx]] <- dplyr::tibble(question_id = qid, message = msg)
        idx <- idx + 1L
      }
    }
  }

  if (length(findings) == 0) {
    return(dplyr::tibble(question_id = character(), message = character()))
  }

  dplyr::bind_rows(findings) %>%
    dplyr::distinct(.data$question_id, .data$message)
}

build_question_specs <- function() {
  spec <- function(question_id, section, priority, question_text, required_sources, required_columns, compute) {
    list(
      question_id = question_id,
      section = section,
      priority = as.integer(priority),
      question_text = question_text,
      required_sources = required_sources,
      required_columns = required_columns,
      compute = compute
    )
  }

  list(
    spec(
      "Q01", "Season Scope", 1L,
      "How many lineup combinations are in the coach table?",
      required_sources = c("lineup_coach_view"),
      required_columns = list(lineup_coach_view = c("lineup_pretty")),
      compute = function(src) {
        df <- src$lineup_coach_view
        n <- length(unique(trimws(to_chr(df$lineup_pretty))[nz(df$lineup_pretty)]))
        if (n <= 0) return(question_fail("No lineup rows available."))
        question_success(sprintf("%s lineup combinations are in the coach table.", fmt_q_int(n)))
      }
    ),
    spec(
      "Q02", "Season Scope", 2L,
      "How many total possessions are covered by the lineup table?",
      required_sources = c("lineup_coach_view"),
      required_columns = list(lineup_coach_view = c("possessions")),
      compute = function(src) {
        total_poss <- sum(to_num(src$lineup_coach_view$possessions), na.rm = TRUE)
        if (!is.finite(total_poss) || total_poss <= 0) return(question_fail("Total possessions not available."))
        question_success(sprintf("Lineup evidence covers %s possessions.", fmt_q_int(total_poss)))
      }
    ),
    spec(
      "Q03", "Season Scope", 1L,
      "What share of tracked possessions are tagged defense-floor pass high upside?",
      required_sources = c("lineup_coach_view"),
      required_columns = list(lineup_coach_view = c("Decision", "possessions")),
      compute = function(src) {
        df <- src$lineup_coach_view
        poss <- to_num(df$possessions)
        dec <- toupper(trimws(to_chr(df$Decision)))
        total <- sum(poss, na.rm = TRUE)
        guardable <- sum(poss[dec == "DEF_FLOOR_PASS_UPSIDE_HIGH"], na.rm = TRUE)
        if (!is.finite(total) || total <= 0) return(question_fail("Possession denominator missing."))
        question_success(sprintf(
          "%s of tracked possessions are tagged defense-floor pass high upside (DEF_FLOOR_PASS_UPSIDE_HIGH) (%s of %s).",
          fmt_q_pct(guardable / total, 1),
          fmt_q_int(guardable),
          fmt_q_int(total)
        ))
      }
    ),
    spec(
      "Q04", "Season Scope", 2L,
      "What is the possession-weighted average robust survive score?",
      required_sources = c("lineup_coach_view"),
      required_columns = list(lineup_coach_view = c("decision_survive_score_robust", "possessions")),
      compute = function(src) {
        df <- src$lineup_coach_view
        wavg <- safe_weighted_mean(df$decision_survive_score_robust, df$possessions)
        if (!is.finite(wavg)) return(question_fail("Weighted robust survive score unavailable."))
        question_success(sprintf("The possession-weighted robust survive score is %s.", fmt_q_pct(wavg, 1)))
      }
    ),
    spec(
      "Q05", "Season Scope", 1L,
      "Which lineup has the most possessions?",
      required_sources = c("lineup_coach_view"),
      required_columns = list(lineup_coach_view = c("lineup_pretty", "possessions")),
      compute = function(src) {
        top <- top_row_by(src$lineup_coach_view, "possessions", decreasing = TRUE)
        if (is.null(top)) return(question_fail("No lineup possession values available."))
        question_success(sprintf(
          "%s has the most possessions at %s.",
          first_text(top$lineup_pretty),
          fmt_q_int(to_num(top$possessions)[[1]])
        ))
      }
    ),
    spec(
      "Q06", "Season Scope", 1L,
      "Which lineup has the highest expected points per 40 minutes?",
      required_sources = c("lineup_coach_view"),
      required_columns = list(lineup_coach_view = c("lineup_pretty", "expected_points_per_40")),
      compute = function(src) {
        top <- top_row_by(src$lineup_coach_view, "expected_points_per_40", decreasing = TRUE)
        if (is.null(top)) return(question_fail("Expected points per 40 unavailable."))
        question_success(sprintf(
          "%s leads at %s expected points per 40.",
          first_text(top$lineup_pretty),
          fmt_q_num(to_num(top$expected_points_per_40)[[1]], 1)
        ))
      }
    ),
    spec(
      "Q07", "Lineups", 1L,
      "Which lineup with enough data has the best field-goal percentage?",
      required_sources = c("lineup_shot_diet_stable"),
      required_columns = list(lineup_shot_diet_stable = c("lineup_pretty", "fg_pct", "sample_flag", "fga")),
      compute = function(src) {
        df <- src$lineup_shot_diet_stable
        keep <- is_stable_sample(df$sample_flag) & is.finite(to_num(df$fg_pct))
        cand <- df[keep, , drop = FALSE]
        top <- top_row_by(cand, "fg_pct", decreasing = TRUE)
        if (is.null(top)) return(question_fail("No lineup field-goal values available for lineups with enough data."))
        question_success(sprintf(
          "%s has the best field-goal percentage at %s (%s attempts).",
          first_text(top$lineup_pretty),
          fmt_q_pct(to_num(top$fg_pct)[[1]], 1),
          fmt_q_int(to_num(top$fga)[[1]])
        ))
      }
    ),
    spec(
      "Q08", "Lineups", 2L,
      "Which lineup with enough data gets to the line most often (free throws per field-goal attempt)?",
      required_sources = c("lineup_shot_diet_stable"),
      required_columns = list(lineup_shot_diet_stable = c("lineup_pretty", "fta_per_fga", "sample_flag")),
      compute = function(src) {
        df <- src$lineup_shot_diet_stable
        keep <- is_stable_sample(df$sample_flag) & is.finite(to_num(df$fta_per_fga))
        top <- top_row_by(df[keep, , drop = FALSE], "fta_per_fga", decreasing = TRUE)
        if (is.null(top)) return(question_fail("Free-throw rate values unavailable for lineups with enough data."))
        question_success(sprintf(
          "%s gets to the line the most at %s free throws per field-goal attempt (FTA per FGA).",
          first_text(top$lineup_pretty),
          fmt_q_num(to_num(top$fta_per_fga)[[1]], 3)
        ))
      }
    ),
    spec(
      "Q09", "Lineups", 2L,
      "Which lineup with enough data protects the ball best (lowest turnovers per field-goal attempt)?",
      required_sources = c("lineup_shot_diet_stable"),
      required_columns = list(lineup_shot_diet_stable = c("lineup_pretty", "tov_per_fga", "sample_flag")),
      compute = function(src) {
        df <- src$lineup_shot_diet_stable
        keep <- is_stable_sample(df$sample_flag) & is.finite(to_num(df$tov_per_fga))
        top <- top_row_by(df[keep, , drop = FALSE], "tov_per_fga", decreasing = FALSE)
        if (is.null(top)) return(question_fail("Turnover rate values unavailable for lineups with enough data."))
        question_success(sprintf(
          "%s protects the ball best at %s turnovers per field-goal attempt (TOV per FGA).",
          first_text(top$lineup_pretty),
          fmt_q_num(to_num(top$tov_per_fga)[[1]], 3)
        ))
      }
    ),
    spec(
      "Q10", "Lineups", 2L,
      "Which lineup with enough data creates the most assisted makes?",
      required_sources = c("lineup_shot_diet_stable"),
      required_columns = list(lineup_shot_diet_stable = c("lineup_pretty", "assisted_makes", "sample_flag")),
      compute = function(src) {
        df <- src$lineup_shot_diet_stable
        keep <- is_stable_sample(df$sample_flag) & is.finite(to_num(df$assisted_makes))
        top <- top_row_by(df[keep, , drop = FALSE], "assisted_makes", decreasing = TRUE)
        if (is.null(top)) return(question_fail("Assisted-make counts unavailable."))
        question_success(sprintf(
          "%s has the most assisted makes (%s).",
          first_text(top$lineup_pretty),
          fmt_q_int(to_num(top$assisted_makes)[[1]])
        ))
      }
    ),
    spec(
      "Q11", "Lineups", 1L,
      "Which lineup is used the most by minutes?",
      required_sources = c("lineup_usage"),
      required_columns = list(lineup_usage = c("lineup_pretty", "minutes")),
      compute = function(src) {
        top <- top_row_by(src$lineup_usage, "minutes", decreasing = TRUE)
        if (is.null(top)) return(question_fail("Lineup minutes unavailable."))
        question_success(sprintf(
          "%s is the most-used lineup at %s minutes.",
          first_text(top$lineup_pretty),
          fmt_q_num(to_num(top$minutes)[[1]], 1)
        ))
      }
    ),
    spec(
      "Q12", "Lineups", 2L,
      "How many lineup rows have enough data for comparison?",
      required_sources = c("lineup_shot_diet_stable"),
      required_columns = list(lineup_shot_diet_stable = c("sample_flag")),
      compute = function(src) {
        df <- src$lineup_shot_diet_stable
        stable_n <- sum(is_stable_sample(df$sample_flag), na.rm = TRUE)
        total_n <- nrow(df)
        if (total_n <= 0) return(question_fail("No lineup shot diet rows available."))
        question_success(sprintf(
          "%s of %s lineup rows have enough data for comparison.",
          fmt_q_int(stable_n),
          fmt_q_int(total_n)
        ))
      }
    ),
    spec(
      "Q13", "Defense", 1L,
      "How many lineups currently carry a defensive leak warning?",
      required_sources = c("def_leaks_coach"),
      required_columns = list(def_leaks_coach = c("leak_flag")),
      compute = function(src) {
        df <- src$def_leaks_coach
        flagged <- sum(grepl("LEAK", toupper(to_chr(df$leak_flag))), na.rm = TRUE)
        total <- nrow(df)
        if (total <= 0) return(question_fail("No defense leak rows available."))
        question_success(sprintf("%s of %s lineups carry a defensive leak warning.", fmt_q_int(flagged), fmt_q_int(total)))
      }
    ),
    spec(
      "Q14", "Defense", 1L,
      "Which lineup has the highest leak risk score (PR leak)?",
      required_sources = c("def_leaks_coach"),
      required_columns = list(def_leaks_coach = c("lineup_pretty", "pr_leak")),
      compute = function(src) {
        top <- top_row_by(src$def_leaks_coach, "pr_leak", decreasing = TRUE)
        if (is.null(top)) return(question_fail("PR leak values unavailable."))
        question_success(sprintf(
          "%s has the highest leak risk score (PR leak) at %s.",
          first_text(top$lineup_pretty),
          fmt_q_pct(to_num(top$pr_leak)[[1]], 1)
        ))
      }
    ),
    spec(
      "Q15", "Defense", 2L,
      "Which lineup has the highest expected points allowed per 40?",
      required_sources = c("def_leaks_coach"),
      required_columns = list(def_leaks_coach = c("lineup_pretty", "expected_pts_allowed_per_40")),
      compute = function(src) {
        top <- top_row_by(src$def_leaks_coach, "expected_pts_allowed_per_40", decreasing = TRUE)
        if (is.null(top)) return(question_fail("Expected points allowed per 40 unavailable."))
        question_success(sprintf(
          "%s allows the most at %s expected points per 40.",
          first_text(top$lineup_pretty),
          fmt_q_num(to_num(top$expected_pts_allowed_per_40)[[1]], 1)
        ))
      }
    ),
    spec(
      "Q16", "Defense", 2L,
      "Which lineup appears most often in repeat leak-warning tracking?",
      required_sources = c("def_repeat_offenders"),
      required_columns = list(def_repeat_offenders = c("lineup_pretty", "games_flagged", "total_poss_flagged")),
      compute = function(src) {
        top <- top_row_by(src$def_repeat_offenders, "games_flagged", decreasing = TRUE)
        if (is.null(top)) return(question_fail("Repeat leak-warning table has no rows."))
        question_success(sprintf(
          "%s appears most often (%s games flagged, %s possessions flagged).",
          first_text(top$lineup_pretty),
          fmt_q_int(to_num(top$games_flagged)[[1]]),
          fmt_q_int(to_num(top$total_poss_flagged)[[1]])
        ))
      }
    ),
    spec(
      "Q17", "Defense", 1L,
      "What does the latest game-level leak risk trend show?",
      required_sources = c("def_game_trend"),
      required_columns = list(def_game_trend = c("game_date", "game_file", "def_leak_mean", "def_leak_p05", "def_leak_p95")),
      compute = function(src) {
        df <- src$def_game_trend
        if (nrow(df) == 0) return(question_fail("No game-level trend rows available."))

        dts <- parse_any_date(df$game_date)
        ord <- order(dts, na.last = TRUE, decreasing = TRUE)
        row <- if (length(ord) > 0) df[ord[1], , drop = FALSE] else df[nrow(df), , drop = FALSE]
        date_txt <- if (length(ord) > 0 && !is.na(dts[ord[1]])) as.character(dts[ord[1]]) else first_text(row$game_date)

        question_success(sprintf(
          "Latest trend game (%s, %s): leak risk score (PR leak) mean %s with 90%% range %s to %s.",
          first_text(row$game_file),
          date_txt,
          fmt_q_num(to_num(row$def_leak_mean)[[1]], 3),
          fmt_q_num(to_num(row$def_leak_p05)[[1]], 3),
          fmt_q_num(to_num(row$def_leak_p95)[[1]], 3)
        ))
      }
    ),
    spec(
      "Q18", "Defense", 2L,
      "How did the highest-risk out-of-sample group perform?",
      required_sources = c("def_holdout_bucket"),
      required_columns = list(def_holdout_bucket = c("risk_bucket", "weighted_pr_leak", "weighted_observed_leaky_rate", "total_holdout_possessions")),
      compute = function(src) {
        df <- src$def_holdout_bucket
        if (nrow(df) == 0) return(question_fail("No defense out-of-sample group rows available."))

        high_idx <- which(grepl("HIGH", toupper(to_chr(df$risk_bucket))))
        row <- if (length(high_idx) > 0) df[high_idx[1], , drop = FALSE] else top_row_by(df, "weighted_pr_leak", decreasing = TRUE)
        if (is.null(row) || nrow(row) == 0) return(question_fail("Unable to identify the highest-risk group row."))

        question_success(sprintf(
          "%s group: leak risk score (PR leak) %s, observed leaky rate %s over %s out-of-sample possessions.",
          first_text(row$risk_bucket),
          fmt_q_pct(to_num(row$weighted_pr_leak)[[1]], 1),
          fmt_q_pct(to_num(row$weighted_observed_leaky_rate)[[1]], 1),
          fmt_q_int(to_num(row$total_holdout_possessions)[[1]])
        ))
      }
    ),
    spec(
      "Q19", "Players", 1L,
      "Which player has the highest role concentration index (RCI)?",
      required_sources = c("player_rci_coach"),
      required_columns = list(player_rci_coach = c("player", "RCI")),
      compute = function(src) {
        top <- top_row_by(src$player_rci_coach, "RCI", decreasing = TRUE)
        if (is.null(top)) return(question_fail("RCI values unavailable."))
        question_success(sprintf(
          "%s leads role concentration index (RCI) at %s.",
          first_text(top$player),
          fmt_q_num(to_num(top$RCI)[[1]], 3)
        ))
      }
    ),
    spec(
      "Q20", "Players", 1L,
      "Which player has the highest net-positive probability impact?",
      required_sources = c("player_rci_coach"),
      required_columns = list(player_rci_coach = c("player", "net_pr_pos")),
      compute = function(src) {
        top <- top_row_by(src$player_rci_coach, "net_pr_pos", decreasing = TRUE)
        if (is.null(top)) return(question_fail("net_pr_pos values unavailable."))
        question_success(sprintf(
          "%s leads net-positive probability at %s.",
          first_text(top$player),
          fmt_q_pct(to_num(top$net_pr_pos)[[1]], 1)
        ))
      }
    ),
    spec(
      "Q21", "Players", 2L,
      "Which player has the most tracked possessions?",
      required_sources = c("player_rci_coach"),
      required_columns = list(player_rci_coach = c("player", "total_possessions")),
      compute = function(src) {
        top <- top_row_by(src$player_rci_coach, "total_possessions", decreasing = TRUE)
        if (is.null(top)) return(question_fail("Player possession totals unavailable."))
        question_success(sprintf(
          "%s has the most tracked possessions (%s).",
          first_text(top$player),
          fmt_q_int(to_num(top$total_possessions)[[1]])
        ))
      }
    ),
    spec(
      "Q22", "Players", 1L,
      "Which player has the highest self-created make rate?",
      required_sources = c("player_creation"),
      required_columns = list(player_creation = c("player", "self_created_make_rate")),
      compute = function(src) {
        top <- top_row_by(src$player_creation, "self_created_make_rate", decreasing = TRUE)
        if (is.null(top)) return(question_fail("Self-created make rate unavailable."))
        question_success(sprintf(
          "%s has the highest self-created make rate at %s.",
          first_text(top$player),
          fmt_q_pct(to_num(top$self_created_make_rate)[[1]], 1)
        ))
      }
    ),
    spec(
      "Q23", "Players", 2L,
      "Which player has the best assist-to-turnover ratio?",
      required_sources = c("player_creation"),
      required_columns = list(player_creation = c("player", "ast_to_tov")),
      compute = function(src) {
        top <- top_row_by(src$player_creation, "ast_to_tov", decreasing = TRUE)
        if (is.null(top)) return(question_fail("Assist-to-turnover ratio unavailable."))
        question_success(sprintf(
          "%s has the best assist-to-turnover ratio at %s.",
          first_text(top$player),
          fmt_q_num(to_num(top$ast_to_tov)[[1]], 2)
        ))
      }
    ),
    spec(
      "Q24", "Players", 2L,
      "Who has the biggest role concentration index (RCI) jump from phase 2 to phase 3?",
      required_sources = c("player_rci_windows"),
      required_columns = list(player_rci_windows = c("player", "delta_rci_2_to_3")),
      compute = function(src) {
        top <- top_row_by(src$player_rci_windows, "delta_rci_2_to_3", decreasing = TRUE)
        if (is.null(top)) return(question_fail("RCI phase delta unavailable."))
        question_success(sprintf(
          "%s has the largest phase-2 to phase-3 role concentration index (RCI) change at %s.",
          first_text(top$player),
          fmt_q_num(to_num(top$delta_rci_2_to_3)[[1]], 3)
        ))
      }
    ),
    spec(
      "Q25", "Decision Quality", 1L,
      "How did defense-floor pass high-upside lineups perform in out-of-sample check?",
      required_sources = c("decision_backtest_bucket"),
      required_columns = list(decision_backtest_bucket = c("Decision", "weighted_survive_score_robust", "weighted_observed_survive4_rate", "weighted_observed_def_ppp")),
      compute = function(src) {
        df <- src$decision_backtest_bucket
        row <- df[toupper(trimws(to_chr(df$Decision))) == "DEF_FLOOR_PASS_UPSIDE_HIGH", , drop = FALSE]
        if (nrow(row) == 0) return(question_fail("DEF_FLOOR_PASS_UPSIDE_HIGH row missing in out-of-sample group table."))
        row <- row[1, , drop = FALSE]
        question_success(sprintf(
          "Defense floor pass, high upside (DEF_FLOOR_PASS_UPSIDE_HIGH): robust survive score %s, observed survive-4 rate %s, observed defensive points per possession %s.",
          fmt_q_pct(to_num(row$weighted_survive_score_robust)[[1]], 1),
          fmt_q_pct(to_num(row$weighted_observed_survive4_rate)[[1]], 1),
          fmt_q_num(to_num(row$weighted_observed_def_ppp)[[1]], 3)
        ))
      }
    ),
    spec(
      "Q26", "Decision Quality", 2L,
      "How did defense-floor fail lineups perform in out-of-sample check?",
      required_sources = c("decision_backtest_bucket"),
      required_columns = list(decision_backtest_bucket = c("Decision", "weighted_survive_score_robust", "weighted_observed_survive4_rate", "weighted_observed_def_ppp")),
      compute = function(src) {
        df <- src$decision_backtest_bucket
        row <- df[toupper(trimws(to_chr(df$Decision))) == "DEF_FLOOR_FAIL", , drop = FALSE]
        if (nrow(row) == 0) return(question_fail("DEF_FLOOR_FAIL row missing in out-of-sample group table."))
        row <- row[1, , drop = FALSE]
        question_success(sprintf(
          "Defense floor fail (DEF_FLOOR_FAIL): robust survive score %s, observed survive-4 rate %s, observed defensive points per possession %s.",
          fmt_q_pct(to_num(row$weighted_survive_score_robust)[[1]], 1),
          fmt_q_pct(to_num(row$weighted_observed_survive4_rate)[[1]], 1),
          fmt_q_num(to_num(row$weighted_observed_def_ppp)[[1]], 3)
        ))
      }
    ),
    spec(
      "Q27", "Decision Quality", 1L,
      "Which decision group has the lowest observed defensive points per possession?",
      required_sources = c("decision_backtest_bucket"),
      required_columns = list(decision_backtest_bucket = c("Decision", "weighted_observed_def_ppp")),
      compute = function(src) {
        df <- src$decision_backtest_bucket
        core_decisions <- toupper(trimws(to_chr(df$Decision))) %in% c(
          "DEF_FLOOR_PASS_UPSIDE_HIGH",
          "DEF_FLOOR_PASS_UPSIDE_MED",
          "DEF_FLOOR_PASS_UPSIDE_LOW",
          "DEF_FLOOR_FAIL"
        )
        cand <- df[core_decisions, , drop = FALSE]
        if (nrow(cand) == 0) cand <- df
        top <- top_row_by(cand, "weighted_observed_def_ppp", decreasing = FALSE)
        if (is.null(top)) return(question_fail("No observed defensive PPP values available."))
        question_success(sprintf(
          "%s has the lowest observed defensive points per possession at %s.",
          display_decision_label(top$Decision),
          fmt_q_num(to_num(top$weighted_observed_def_ppp)[[1]], 3)
        ))
      }
    ),
    spec(
      "Q28", "Decision Quality", 1L,
      "What is the calibration error (ECE)?",
      required_sources = c("decision_calibration_metrics"),
      required_columns = list(decision_calibration_metrics = c("metric", "value")),
      compute = function(src) {
        weighted_ece <- metric_lookup(src$decision_calibration_metrics, "weighted_ece_decile")
        if (!is.finite(weighted_ece)) return(question_fail("weighted_ece_decile metric missing."))
        question_success(sprintf("Calibration error (ECE) is %s.", fmt_q_num(weighted_ece, 3)))
      }
    ),
    spec(
      "Q29", "Decision Quality", 2L,
      "How much out-of-sample evidence feeds calibration?",
      required_sources = c("decision_calibration_metrics"),
      required_columns = list(decision_calibration_metrics = c("metric", "value")),
      compute = function(src) {
        holdout_poss <- metric_lookup(src$decision_calibration_metrics, "sum_holdout_possessions")
        games_n <- metric_lookup(src$decision_calibration_metrics, "n_games_used")
        if (!is.finite(holdout_poss) || !is.finite(games_n)) return(question_fail("Calibration evidence metrics missing."))
        question_success(sprintf(
          "Calibration uses %s out-of-sample possessions across %s games.",
          fmt_q_num(holdout_poss, 2),
          fmt_q_int(games_n)
        ))
      }
    ),
    spec(
      "Q30", "Decision Quality", 2L,
      "How often were lineup stretches eligible for a decision call?",
      required_sources = c("decision_eligibility"),
      required_columns = list(decision_eligibility = c("decision_eligible_prior", "collapse_risk_flag_prior")),
      compute = function(src) {
        df <- src$decision_eligibility
        if (nrow(df) == 0) return(question_fail("No decision eligibility rows available."))
        elig <- as_logical_vec(df$decision_eligible_prior)
        valid <- !is.na(elig)
        if (!any(valid)) return(question_fail("No valid decision_eligible_prior values."))
        elig_n <- sum(elig[valid], na.rm = TRUE)
        total_n <- sum(valid, na.rm = TRUE)
        collapse_n <- sum(as_logical_vec(df$collapse_risk_flag_prior), na.rm = TRUE)
        question_success(sprintf(
          "%s of lineup stretches were decision-eligible (%s of %s); collapse risk flagged %s lineup stretches.",
          fmt_q_pct(elig_n / total_n, 1),
          fmt_q_int(elig_n),
          fmt_q_int(total_n),
          fmt_q_int(collapse_n)
        ))
      }
    ),
    spec(
      "Q31", "Opponent Scouts", 1L,
      "What does the latest opponent meeting summary say?",
      required_sources = c("opp_meeting_summary"),
      required_columns = list(opp_meeting_summary = c("opponent", "game_date", "opponent_fg_pct", "opponent_3p_pct", "opponent_points")),
      compute = function(src) {
        df <- src$opp_meeting_summary
        if (nrow(df) == 0) return(question_fail("Meeting summary row missing."))
        row <- df[1, , drop = FALSE]
        date_val <- parse_any_date(row$game_date)
        date_txt <- if (length(date_val) > 0 && !is.na(date_val[[1]])) as.character(date_val[[1]]) else first_text(row$game_date)
        question_success(sprintf(
          "Latest scout is %s on %s: opponent FG%% %s, 3PT%% %s, points %s.",
          first_text(row$opponent),
          date_txt,
          fmt_q_pct(to_num(row$opponent_fg_pct)[[1]], 1),
          fmt_q_pct(to_num(row$opponent_3p_pct)[[1]], 1),
          fmt_q_int(to_num(row$opponent_points)[[1]])
        ))
      }
    ),
    spec(
      "Q32", "Opponent Scouts", 1L,
      "Which opponent lineup scored the most in the latest scout?",
      required_sources = c("opp_lineup_profile"),
      required_columns = list(opp_lineup_profile = c("offense_lineup_key", "opponent_points", "event_rows")),
      compute = function(src) {
        top <- top_row_by(src$opp_lineup_profile, "opponent_points", decreasing = TRUE)
        if (is.null(top)) return(question_fail("Opponent lineup profile has no scored rows."))
        question_success(sprintf(
          "Top opponent lineup was %s with %s points across %s events.",
          first_text(top$offense_lineup_key),
          fmt_q_int(to_num(top$opponent_points)[[1]]),
          fmt_q_int(to_num(top$event_rows)[[1]])
        ))
      }
    ),
    spec(
      "Q33", "Opponent Scouts", 1L,
      "Which shot zone took the biggest opponent share in the latest scout?",
      required_sources = c("opp_shot_profile"),
      required_columns = list(opp_shot_profile = c("shot_zone_detail", "share_of_fga", "fga")),
      compute = function(src) {
        top <- top_row_by(src$opp_shot_profile, "share_of_fga", decreasing = TRUE)
        if (is.null(top)) return(question_fail("Opponent shot profile has no share values."))
        question_success(sprintf(
          "%s was the top zone at %s of attempts (%s FGA).",
          first_text(top$shot_zone_detail),
          fmt_q_pct(to_num(top$share_of_fga)[[1]], 1),
          fmt_q_int(to_num(top$fga)[[1]])
        ))
      }
    ),
    spec(
      "Q34", "Opponent Scouts", 1L,
      "Who forced the most steals and who committed the most turnovers?",
      required_sources = c("opp_turnover_creators", "opp_turnover_victims"),
      required_columns = list(
        opp_turnover_creators = c("StealPlayer", "steals_forced"),
        opp_turnover_victims = c("UsagePlayer", "turnovers")
      ),
      compute = function(src) {
        creator <- top_row_by(src$opp_turnover_creators, "steals_forced", decreasing = TRUE)
        victim <- top_row_by(src$opp_turnover_victims, "turnovers", decreasing = TRUE)
        if (is.null(creator) || is.null(victim)) return(question_fail("Turnover creator/victim evidence missing."))
        question_success(sprintf(
          "Top steal creator: %s (%s steals). Top turnover victim: %s (%s turnovers).",
          first_text(creator$StealPlayer),
          fmt_q_int(to_num(creator$steals_forced)[[1]]),
          first_text(victim$UsagePlayer),
          fmt_q_int(to_num(victim$turnovers)[[1]])
        ))
      }
    ),
    spec(
      "Q35", "Opponent Scouts", 2L,
      "How many clutch events are logged in the latest scout?",
      required_sources = c("opp_clutch_events"),
      required_columns = list(opp_clutch_events = c("team_context")),
      compute = function(src) {
        n <- nrow(src$opp_clutch_events)
        question_success(sprintf("Latest scout logged %s clutch events.", fmt_q_int(n)))
      }
    ),
    spec(
      "Q36", "Opponent Scouts", 2L,
      "Which matchup pair had the highest event volume?",
      required_sources = c("opp_matchup_matrix"),
      required_columns = list(opp_matchup_matrix = c("team_context", "offense_lineup_key", "defense_lineup_key", "event_rows")),
      compute = function(src) {
        top <- top_row_by(src$opp_matchup_matrix, "event_rows", decreasing = TRUE)
        if (is.null(top)) return(question_fail("Matchup matrix has no event rows."))
        question_success(sprintf(
          "%s: %s vs %s led with %s events.",
          first_text(top$team_context),
          first_text(top$offense_lineup_key),
          first_text(top$defense_lineup_key),
          fmt_q_int(to_num(top$event_rows)[[1]])
        ))
      }
    ),
    spec(
      "Q37", "Pipeline / Method", 1L,
      "What was the sampler warning count (divergent transitions) for the core lineup model?",
      required_sources = c("core_model_diag"),
      required_columns = list(core_model_diag = c("divergent__", "treedepth__")),
      compute = function(src) {
        df <- src$core_model_diag
        if (nrow(df) == 0) return(question_fail("Core model diagnostics table is empty."))
        div_n <- sum(to_num(df$divergent__), na.rm = TRUE)
        max_depth <- max(to_num(df$treedepth__), na.rm = TRUE)
        question_success(sprintf(
          "Core lineup model sampler warning count (divergent transitions): %s (max tree depth %s).",
          fmt_q_int(div_n),
          fmt_q_int(max_depth)
        ))
      }
    ),
    spec(
      "Q38", "Pipeline / Method", 2L,
      "What was the sampler warning count (divergent transitions) for the defense leak model?",
      required_sources = c("def_model_diag"),
      required_columns = list(def_model_diag = c("divergent__", "treedepth__")),
      compute = function(src) {
        df <- src$def_model_diag
        if (nrow(df) == 0) return(question_fail("Defense model diagnostics table is empty."))
        div_n <- sum(to_num(df$divergent__), na.rm = TRUE)
        max_depth <- max(to_num(df$treedepth__), na.rm = TRUE)
        question_success(sprintf(
          "Defense model sampler warning count (divergent transitions): %s (max tree depth %s).",
          fmt_q_int(div_n),
          fmt_q_int(max_depth)
        ))
      }
    ),
    spec(
      "Q39", "Pipeline / Method", 1L,
      "What V4 threshold-tuning status is active in the decision rule?",
      required_sources = c("decision_thresholds"),
      required_columns = list(decision_thresholds = c("metric", "value")),
      compute = function(src) {
        status <- metric_lookup_text(src$decision_thresholds, "TUNING_STATUS")
        feasible <- metric_lookup(src$decision_thresholds, "TUNING_GRID_FEASIBLE")
        explored <- metric_lookup(src$decision_thresholds, "TUNING_GRID_EXPLORED")
        if (!nz(status) || !is.finite(feasible) || !is.finite(explored)) {
          return(question_fail("V4 tuning status metrics missing."))
        }
        question_success(sprintf("V4 threshold tuning status is %s (%s feasible out of %s explored).", status, fmt_q_int(feasible), fmt_q_int(explored)))
      }
    ),
    spec(
      "Q40", "Pipeline / Method", 2L,
      "What V4 weight and floor cutoffs are active?",
      required_sources = c("decision_thresholds"),
      required_columns = list(decision_thresholds = c("metric", "value")),
      compute = function(src) {
        w_def <- metric_lookup(src$decision_thresholds, "W_DEF")
        w_off <- metric_lookup(src$decision_thresholds, "W_OFF")
        w_style <- metric_lookup(src$decision_thresholds, "W_STYLE")
        alpha_opp <- metric_lookup(src$decision_thresholds, "ALPHA_OPP")
        def_floor_q <- metric_lookup(src$decision_thresholds, "DEF_FLOOR_QUANTILE")
        def_floor_t <- metric_lookup(src$decision_thresholds, "DEF_FLOOR_T")
        if (!is.finite(w_def) || !is.finite(w_off) || !is.finite(w_style) ||
            !is.finite(alpha_opp) || !is.finite(def_floor_q) || !is.finite(def_floor_t)) {
          return(question_fail("Decision threshold metrics missing."))
        }
        question_success(sprintf(
          "V4 cutoffs: w_def=%s, w_off=%s, w_style=%s, alpha_opp=%s, def_floor_q=%s, def_floor_t=%s.",
          fmt_q_num(w_def, 3),
          fmt_q_num(w_off, 3),
          fmt_q_num(w_style, 3),
          fmt_q_num(alpha_opp, 3),
          fmt_q_num(def_floor_q, 3),
          fmt_q_num(def_floor_t, 3)
        ))
      }
    )
  )
}

validate_question_registry <- function(specs) {
  errs <- character()

  if (length(specs) != 40) {
    errs <- c(errs, sprintf("Question registry must contain exactly 40 specs (found %s).", length(specs)))
  }

  ids <- vapply(specs, function(s) s$question_id %||% "", character(1))
  if (any(!nz(ids))) {
    errs <- c(errs, "Every question spec must have a non-empty question_id.")
  }
  if (any(duplicated(ids))) {
    dup <- unique(ids[duplicated(ids)])
    errs <- c(errs, sprintf("Duplicate question_id values found: %s", paste(dup, collapse = ", ")))
  }

  for (i in seq_along(specs)) {
    s <- specs[[i]]
    label <- s$question_id %||% paste0("index_", i)

    required_fields <- c("question_id", "section", "priority", "question_text", "required_sources", "required_columns", "compute")
    missing_fields <- setdiff(required_fields, names(s))
    if (length(missing_fields) > 0) {
      errs <- c(errs, sprintf("%s missing fields: %s", label, paste(missing_fields, collapse = ", ")))
    }

    if (!is.function(s$compute)) {
      errs <- c(errs, sprintf("%s compute must be a function.", label))
    }

    required_sources <- s$required_sources %||% character()
    req_cols <- s$required_columns %||% list()
    if (length(setdiff(names(req_cols), required_sources)) > 0) {
      errs <- c(errs, sprintf("%s required_columns keys must match required_sources.", label))
    }
  }

  errs
}

evaluate_single_question <- function(spec, source_catalog) {
  required_sources <- spec$required_sources %||% character()
  req_cols_map <- spec$required_columns %||% list()

  source_paths <- vapply(required_sources, function(sid) {
    p <- source_catalog$paths[[sid]] %||% ""
    first_text(p, default = "")
  }, character(1))
  source_paths <- source_paths[nz(source_paths)]

  missing_sources <- required_sources[!vapply(required_sources, function(sid) {
    isTRUE(source_catalog$present[[sid]]) && nz(source_catalog$paths[[sid]] %||% "")
  }, logical(1))]

  missing_columns <- character()
  for (sid in required_sources) {
    tbl <- source_catalog$tables[[sid]]
    cols_needed <- req_cols_map[[sid]] %||% character()
    if (length(cols_needed) == 0 || is.null(tbl) || !is.data.frame(tbl)) next
    miss <- setdiff(cols_needed, names(tbl))
    if (length(miss) > 0) {
      missing_columns <- c(missing_columns, sprintf("%s -> %s", sid, paste(miss, collapse = ", ")))
    }
  }

  unavailable_reason <- character()
  if (length(missing_sources) > 0) {
    unavailable_reason <- c(unavailable_reason, sprintf("Missing source(s): %s", paste(missing_sources, collapse = ", ")))
  }
  if (length(missing_columns) > 0) {
    unavailable_reason <- c(unavailable_reason, sprintf("Missing required column(s): %s", paste(missing_columns, " | ")))
  }

  result <- NULL
  if (length(unavailable_reason) == 0) {
    result <- tryCatch(
      spec$compute(source_catalog$tables),
      error = function(e) question_fail(conditionMessage(e))
    )

    if (is.null(result$ok) || !isTRUE(result$ok)) {
      unavailable_reason <- c(unavailable_reason, result$reason %||% "Compute returned no answer.")
    } else if (!nz(result$answer_text)) {
      unavailable_reason <- c(unavailable_reason, "Compute returned empty answer text.")
    }
  }

  evidence_counts <- vapply(required_sources, function(sid) {
    tbl <- source_catalog$tables[[sid]]
    if (is.null(tbl) || !is.data.frame(tbl)) return(NA_real_)
    nrow(tbl)
  }, numeric(1))

  evidence_rows_txt <- if (length(required_sources) == 0) {
    ""
  } else {
    parts <- vapply(seq_along(required_sources), function(i) {
      sid <- required_sources[[i]]
      path_txt <- source_catalog$paths[[sid]] %||% sid
      count_txt <- if (is.finite(evidence_counts[[i]])) fmt_q_int(evidence_counts[[i]]) else "NA"
      sprintf("%s:%s", path_txt, count_txt)
    }, character(1))
    paste(parts, collapse = " | ")
  }

  status <- if (length(unavailable_reason) == 0) "AVAILABLE" else "UNAVAILABLE"
  answer_text <- if (status == "AVAILABLE") result$answer_text else "UNAVAILABLE"

  dplyr::tibble(
    question_id = spec$question_id,
    section = spec$section,
    priority = spec$priority,
    question_text = spec$question_text,
    status = status,
    answer_text = answer_text,
    source_paths = paste(unique(source_paths), collapse = " | "),
    evidence_rows = evidence_rows_txt,
    unavailable_reason = paste(unique(unavailable_reason), collapse = " | ")
  )
}

evaluate_question_registry <- function(outputs_by_path) {
  specs <- build_question_specs()
  validation_errors <- validate_question_registry(specs)

  if (length(validation_errors) > 0) {
    all_q <- dplyr::bind_rows(lapply(specs, function(spec) {
      dplyr::tibble(
        question_id = spec$question_id,
        section = spec$section,
        priority = spec$priority,
        question_text = spec$question_text,
        status = "UNAVAILABLE",
        answer_text = "UNAVAILABLE",
        source_paths = "",
        evidence_rows = "",
        unavailable_reason = paste(validation_errors, collapse = " | ")
      )
    }))

    coverage <- all_q %>%
      dplyr::group_by(.data$section) %>%
      dplyr::summarise(
        total_questions = dplyr::n(),
        available_n = sum(.data$status == "AVAILABLE"),
        unavailable_n = sum(.data$status != "AVAILABLE"),
        .groups = "drop"
      ) %>%
      dplyr::arrange(.data$section)

    return(list(
      all = all_q,
      available = all_q[0, , drop = FALSE],
      coverage = coverage,
      errors = validation_errors
    ))
  }

  source_catalog <- build_question_source_catalog(outputs_by_path = outputs_by_path)

  all_q <- dplyr::bind_rows(lapply(specs, function(spec) {
    evaluate_single_question(spec = spec, source_catalog = source_catalog)
  })) %>%
    dplyr::arrange(.data$question_id)

  copy_guard <- coach_copy_guard_findings(all_q)
  if (nrow(copy_guard) > 0) {
    by_qid <- split(to_chr(copy_guard$message), to_chr(copy_guard$question_id))
    guard_msg_for <- function(qid) {
      msgs <- unique(by_qid[[qid]] %||% character())
      if (length(msgs) == 0) return("")
      paste(msgs, collapse = " | ")
    }

    bad_ids <- unique(to_chr(copy_guard$question_id))
    all_q <- all_q %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        status = if (.data$question_id %in% bad_ids) "UNAVAILABLE" else .data$status,
        answer_text = if (.data$question_id %in% bad_ids) "UNAVAILABLE" else .data$answer_text,
        unavailable_reason = if (.data$question_id %in% bad_ids) {
          guard_msg <- guard_msg_for(.data$question_id)
          if (nz(.data$unavailable_reason)) paste(.data$unavailable_reason, guard_msg, sep = " | ") else guard_msg
        } else {
          .data$unavailable_reason
        }
      ) %>%
      dplyr::ungroup()
  }

  available_q <- all_q %>%
    dplyr::filter(.data$status == "AVAILABLE") %>%
    dplyr::arrange(.data$section, .data$priority, .data$question_id)

  coverage <- all_q %>%
    dplyr::group_by(.data$section) %>%
    dplyr::summarise(
      total_questions = dplyr::n(),
      available_n = sum(.data$status == "AVAILABLE"),
      unavailable_n = sum(.data$status != "AVAILABLE"),
      .groups = "drop"
    ) %>%
    dplyr::arrange(.data$section)

  list(
    all = all_q,
    available = available_q,
    coverage = coverage,
    errors = unique(c(
      character(),
      if (nrow(copy_guard) > 0) {
        vapply(seq_len(nrow(copy_guard)), function(i) {
          sprintf(
            "Coach copy guard failed for %s: %s",
            first_text(copy_guard$question_id[[i]], default = "?"),
            first_text(copy_guard$message[[i]], default = "Unknown copy violation.")
          )
        }, character(1))
      } else {
        character()
      }
    ))
  )
}

load_dashboard_data <- function(output_dir = "_outputs") {
  now_ts <- Sys.time()
  errors <- character()
  warnings <- character()

  specs <- required_output_specs()
  required_cols <- required_columns_by_relative_path()

  output_registry <- list_bucketed_output_csvs(output_dir = output_dir)
  outputs_by_path <- list()
  output_status_rows <- list()
  row_idx <- 1L

  if (nrow(output_registry) > 0) {
    output_registry <- output_registry %>%
      dplyr::mutate(required = .data$relative_path %in% specs$relative_path)

    for (i in seq_len(nrow(output_registry))) {
      rel <- output_registry$relative_path[[i]]
      full <- output_registry$full_path[[i]]
      bucket <- output_registry$bucket[[i]]
      required <- isTRUE(output_registry$required[[i]])

      df <- safe_read_csv(full)
      if (inherits(df, "read_error")) {
        msg <- df$error
        if (required) {
          errors <- c(errors, sprintf("Required output failed to read (%s): %s", rel, msg))
        } else {
          warnings <- c(warnings, sprintf("Output read failed (%s): %s", rel, msg))
        }

        output_status_rows[[row_idx]] <- dplyr::tibble(
          source_type = "output_csv",
          bucket = bucket,
          path = rel,
          required = required,
          status = "READ_ERROR",
          message = msg,
          rows = NA_integer_,
          cols = NA_integer_,
          modified_at = file.info(full)$mtime[[1]]
        )
        row_idx <- row_idx + 1L
        next
      }

      missing_cols <- setdiff(required_cols[[rel]] %||% character(), names(df))
      status <- "OK"
      msg <- ""

      if (length(missing_cols) > 0) {
        status <- if (required) "INVALID_SCHEMA" else "WARN_SCHEMA"
        msg <- sprintf("Missing columns: %s", paste(missing_cols, collapse = ", "))

        if (required) {
          errors <- c(errors, sprintf("Required output schema mismatch (%s): %s", rel, msg))
        } else {
          warnings <- c(warnings, sprintf("Output schema warning (%s): %s", rel, msg))
        }
      }

      outputs_by_path[[rel]] <- df
      output_status_rows[[row_idx]] <- dplyr::tibble(
        source_type = "output_csv",
        bucket = bucket,
        path = rel,
        required = required,
        status = status,
        message = msg,
        rows = nrow(df),
        cols = ncol(df),
        modified_at = file.info(full)$mtime[[1]]
      )
      row_idx <- row_idx + 1L
    }
  }

  existing_rel <- names(outputs_by_path)
  missing_required <- setdiff(specs$relative_path, existing_rel)

  if (length(missing_required) > 0) {
    for (rel in missing_required) {
      errors <- c(errors, sprintf("Missing required output CSV: %s", rel))
      output_status_rows[[row_idx]] <- dplyr::tibble(
        source_type = "output_csv",
        bucket = sub("/.*", "", rel),
        path = rel,
        required = TRUE,
        status = "MISSING_REQUIRED",
        message = "Required output CSV missing.",
        rows = NA_integer_,
        cols = NA_integer_,
        modified_at = as.POSIXct(NA)
      )
      row_idx <- row_idx + 1L
    }
  }

  stints_path <- file.path("_data", "01_core_inputs", "uconn_stints_from_pbp.csv")
  stints <- data.frame()
  stints_status <- dplyr::tibble(
    source_type = "core_input",
    bucket = "core_inputs",
    path = stints_path,
    required = TRUE,
    status = "OK",
    message = "",
    rows = NA_integer_,
    cols = NA_integer_,
    modified_at = as.POSIXct(NA)
  )

  if (!file.exists(stints_path)) {
    errors <- c(errors, sprintf("Missing required core input: %s", stints_path))
    stints_status$status <- "MISSING_REQUIRED"
    stints_status$message <- "Required core input missing."
  } else {
    st <- safe_read_csv(stints_path)
    if (inherits(st, "read_error")) {
      errors <- c(errors, sprintf("Failed to read core input (%s): %s", stints_path, st$error))
      stints_status$status <- "READ_ERROR"
      stints_status$message <- st$error
      stints_status$modified_at <- file.info(stints_path)$mtime[[1]]
    } else {
      stints <- st
      required_stint_cols <- c("game_file", "poss_est", "points_for", "points_against")
      miss <- setdiff(required_stint_cols, names(stints))
      if (length(miss) > 0) {
        errors <- c(errors, sprintf("Core input missing columns (%s): %s", stints_path, paste(miss, collapse = ", ")))
        stints_status$status <- "INVALID_SCHEMA"
        stints_status$message <- sprintf("Missing columns: %s", paste(miss, collapse = ", "))
      }
      stints_status$rows <- nrow(stints)
      stints_status$cols <- ncol(stints)
      stints_status$modified_at <- file.info(stints_path)$mtime[[1]]
    }
  }

  manual_loaded <- load_manual_games()
  manual_games <- manual_loaded$games
  if (length(manual_loaded$errors) > 0) {
    errors <- c(errors, manual_loaded$errors)
  }

  scheme_loaded <- load_scheme_tables()
  if (length(scheme_loaded$warnings) > 0) {
    warnings <- c(warnings, scheme_loaded$warnings)
  }

  board_index <- build_board_index(
    manual_games = manual_games,
    outputs_by_path = outputs_by_path
  )

  qc <- build_qc_table(outputs_by_path = outputs_by_path)
  qc_pass <- all(qc$pass)

  question_eval <- evaluate_question_registry(outputs_by_path = outputs_by_path)
  if (length(question_eval$errors) > 0) {
    errors <- c(errors, question_eval$errors)
  }

  outputs_by_bucket <- split(names(outputs_by_path), sub("/.*", "", names(outputs_by_path)))
  outputs_by_bucket <- lapply(outputs_by_bucket, sort)
  for (bucket in expected_output_buckets()) {
    if (is.null(outputs_by_bucket[[bucket]])) {
      outputs_by_bucket[[bucket]] <- character()
    }
  }
  outputs_by_bucket <- outputs_by_bucket[expected_output_buckets()]

  output_status <- if (length(output_status_rows) > 0) dplyr::bind_rows(output_status_rows) else dplyr::tibble()
  status <- dplyr::bind_rows(
    output_status,
    stints_status,
    manual_loaded$status,
    scheme_loaded$status
  )

  existing_times <- status$modified_at[!is.na(status$modified_at)]
  as_of <- if (length(existing_times) > 0) max(existing_times) else as.POSIXct(NA)

  list(
    output_dir = output_dir,
    loaded_at = now_ts,
    as_of = as_of,
    errors = unique(errors),
    warnings = unique(warnings),
    status = status,
    qc = qc,
    qc_pass = qc_pass,
    outputs_by_path = outputs_by_path,
    outputs_by_bucket = outputs_by_bucket,
    stints = stints,
    manual_games = manual_games,
    board_index = board_index,
    scheme_tables = scheme_loaded$tables,
    question_answers_all = question_eval$all,
    question_answers_available = question_eval$available,
    question_coverage_summary = question_eval$coverage
  )
}
