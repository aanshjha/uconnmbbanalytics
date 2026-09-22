combine_reason_codes <- function(...) {
  parts <- c(...)
  parts <- parts[!is.na(parts) & nzchar(parts)]
  if (length(parts) == 0) return("")
  paste(parts, collapse = "|")
}

normalize_lineup_key <- function(lineup) {
  toks <- unlist(stringr::str_split(as.character(lineup), "\\|"))
  toks <- stringr::str_squish(toks)
  toks <- toks[!is.na(toks) & nzchar(toks)]
  if (length(toks) == 0) return(NA_character_)
  paste(sort(unique(toks)), collapse = "|")
}

repair_uconn_stints_core_input <- function(
  stints_path = file.path("_data", "01_core_inputs", "uconn_stints_from_pbp.csv"),
  backup_dir = file.path("_data", "01_core_inputs", "_backup"),
  report_dir = file.path("_outputs", "00_qc"),
  rewrite = FALSE,
  backup = TRUE
) {
  if (!file.exists(stints_path)) {
    stop("Missing required stints file: ", stints_path, call. = FALSE)
  }
  if (isTRUE(rewrite) && !isTRUE(backup)) {
    stop("Rewriting core inputs requires backup=TRUE.", call. = FALSE)
  }

  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%OS6")
  repaired_at_utc <- format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")

  stints <- readr::read_csv(
    stints_path,
    show_col_types = FALSE,
    col_types = readr::cols(
      start_time = readr::col_character(),
      end_time = readr::col_character()
    )
  )

  required_cols <- c(
    "game_file",
    "period",
    "stint_index",
    "start_time",
    "end_time",
    "uconn_lineup",
    "points_for",
    "points_against",
    "poss_est"
  )
  missing_required <- setdiff(required_cols, names(stints))
  if (length(missing_required) > 0) {
    stop(
      "uconn_stints_from_pbp.csv is missing required columns: ",
      paste(missing_required, collapse = ", "),
      call. = FALSE
    )
  }

  if (!("lineup_size" %in% names(stints))) stints$lineup_size <- NA_real_
  if (!("net_pts" %in% names(stints))) stints$net_pts <- NA_real_
  if (!("net_ppp" %in% names(stints))) stints$net_ppp <- NA_real_

  old_uconn_lineup <- as.character(stints$uconn_lineup)
  old_lineup_size <- suppressWarnings(as.numeric(stints$lineup_size))
  old_poss_est <- suppressWarnings(as.numeric(stints$poss_est))
  old_points_for <- suppressWarnings(as.numeric(stints$points_for))
  old_points_against <- suppressWarnings(as.numeric(stints$points_against))
  old_net_pts <- suppressWarnings(as.numeric(stints$net_pts))
  old_net_ppp <- suppressWarnings(as.numeric(stints$net_ppp))

  lineup_norm <- vapply(old_uconn_lineup, normalize_lineup_key, character(1))
  lineup_size_new <- vapply(
    strsplit(ifelse(is.na(lineup_norm), "", lineup_norm), "\\|"),
    function(x) {
      x <- stringr::str_squish(x)
      x <- x[nzchar(x)]
      if (length(x) == 0) NA_integer_ else as.integer(length(x))
    },
    integer(1)
  )

  points_for_new <- old_points_for
  points_against_new <- old_points_against
  poss_est_new <- old_poss_est

  poss_invalid <- !is.finite(poss_est_new) | poss_est_new <= 0
  # Earlier versions filled zero/missing possessions using points or 1. Audit
  # those persisted fills as well; a positive fabricated value is still invalid.
  historical_imputed <- rep(FALSE, nrow(stints))
  identity_cols <- c("game_file", "period", "stint_index", "start_time", "end_time")
  prior_reports <- list.files(report_dir, pattern = "^uconn_stints_core_input_repair_report_.*\\.csv$", full.names = TRUE)
  for (path in prior_reports) {
    prior <- readr::read_csv(path, show_col_types = FALSE, col_types = readr::cols(.default = readr::col_character()))
    if (!all(c(identity_cols, "reason_codes", "new_poss_est") %in% names(prior))) next
    prior <- prior[grepl("poss_est_invalid_(repaired_from_points|defaulted_to_1)", prior$reason_codes), , drop = FALSE]
    if (!nrow(prior)) next
    matches <- dplyr::inner_join(
      dplyr::mutate(stints[identity_cols], row_id = seq_len(nrow(stints))) %>%
        dplyr::mutate(dplyr::across(dplyr::all_of(identity_cols), as.character)),
      prior[c(identity_cols, "new_poss_est")], by = identity_cols
    )
    if (nrow(matches)) {
      still_filled <- is.finite(old_poss_est[matches$row_id]) &
        abs(old_poss_est[matches$row_id] - suppressWarnings(as.numeric(matches$new_poss_est))) < 1e-9
      historical_imputed[matches$row_id[which(still_filled)]] <- TRUE
    }
  }
  if ("poss_source_verified" %in% names(stints)) {
    historical_imputed[stints$poss_source_verified %in% TRUE] <- FALSE
  }
  poss_est_new[poss_invalid | historical_imputed] <- NA_real_
  lineup_invalid <- is.na(lineup_size_new) | lineup_size_new != 5L
  points_invalid <- !is.finite(points_for_new) | !is.finite(points_against_new) |
    points_for_new < 0 | points_against_new < 0
  source_repair_required <- poss_invalid | historical_imputed | lineup_invalid | points_invalid

  net_pts_new <- ifelse(
    is.finite(points_for_new) & is.finite(points_against_new),
    points_for_new - points_against_new,
    NA_real_
  )
  net_ppp_new <- ifelse(
    is.finite(poss_est_new) & poss_est_new > 0 & is.finite(net_pts_new),
    net_pts_new / poss_est_new,
    NA_real_
  )

  lineup_changed <- dplyr::coalesce(stringr::str_squish(old_uconn_lineup), "") !=
    dplyr::coalesce(lineup_norm, "")
  lineup_size_changed <- !(is.na(old_lineup_size) & is.na(lineup_size_new)) &
    (is.na(old_lineup_size) | is.na(lineup_size_new) | old_lineup_size != lineup_size_new)
  poss_changed <- !(is.na(old_poss_est) & is.na(poss_est_new)) &
    (is.na(old_poss_est) | is.na(poss_est_new) | abs(old_poss_est - poss_est_new) > 1e-9)
  net_pts_changed <- !(is.na(old_net_pts) & is.na(net_pts_new)) &
    (is.na(old_net_pts) | is.na(net_pts_new) | abs(old_net_pts - net_pts_new) > 1e-9)
  net_ppp_changed <- !(is.na(old_net_ppp) & is.na(net_ppp_new)) &
    (is.na(old_net_ppp) | is.na(net_ppp_new) | abs(old_net_ppp - net_ppp_new) > 1e-9)

  poss_reason <- ifelse(
    historical_imputed, "historical_imputed_possessions_require_source_repair",
    ifelse(poss_invalid, "invalid_possessions_require_source_repair", NA_character_)
  )
  source_reason <- ifelse(lineup_invalid, "invalid_lineup_requires_source_repair", NA_character_)
  points_reason <- ifelse(points_invalid, "invalid_points_require_source_repair", NA_character_)
  lineup_reason <- ifelse(lineup_changed, "lineup_normalized", NA_character_)
  lineup_size_reason <- ifelse(lineup_size_changed, "lineup_size_recomputed", NA_character_)
  net_reason <- ifelse(net_pts_changed | net_ppp_changed, "net_fields_recomputed", NA_character_)

  reason_codes <- vapply(
    seq_len(nrow(stints)),
    function(i) combine_reason_codes(poss_reason[[i]], source_reason[[i]], points_reason[[i]], lineup_reason[[i]], lineup_size_reason[[i]], net_reason[[i]]),
    character(1)
  )

  repaired <- stints %>%
    dplyr::mutate(
      uconn_lineup = lineup_norm,
      lineup_size = lineup_size_new,
      points_for = points_for_new,
      points_against = points_against_new,
      poss_est = poss_est_new,
      net_pts = net_pts_new,
      net_ppp = net_ppp_new,
      source_repair_required = source_repair_required,
      analysis_eligible = !source_repair_required
    )

  changed_mask <- nzchar(reason_codes)
  changed_n <- sum(changed_mask, na.rm = TRUE)

  repair_report <- tibble::tibble(
    row_id = seq_len(nrow(stints)),
    game_file = as.character(stints$game_file),
    period = as.character(stints$period),
    stint_index = suppressWarnings(as.numeric(stints$stint_index)),
    start_time = as.character(stints$start_time),
    end_time = as.character(stints$end_time),
    old_uconn_lineup = old_uconn_lineup,
    new_uconn_lineup = lineup_norm,
    old_lineup_size = old_lineup_size,
    new_lineup_size = as.numeric(lineup_size_new),
    old_poss_est = old_poss_est,
    new_poss_est = poss_est_new,
    old_net_pts = old_net_pts,
    new_net_pts = net_pts_new,
    old_net_ppp = old_net_ppp,
    new_net_ppp = net_ppp_new,
    source_repair_required = source_repair_required,
    reason_codes = reason_codes,
    repaired_at_utc = repaired_at_utc
  ) %>%
    dplyr::filter(nzchar(reason_codes))

  dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
  report_path <- file.path(
    report_dir,
    sprintf("uconn_stints_core_input_repair_report_%s.csv", timestamp)
  )
  readr::write_csv(repair_report, report_path)
  quarantine_path <- file.path(report_dir, sprintf("uconn_stints_source_repair_quarantine_%s.csv", timestamp))
  quarantine <- stints[source_repair_required, , drop = FALSE]
  quarantine$source_row_id <- which(source_repair_required)
  quarantine$source_repair_reason <- reason_codes[source_repair_required]
  readr::write_csv(quarantine, quarantine_path)

  backup_path <- NA_character_
  if (isTRUE(rewrite)) {
    if (isTRUE(backup)) {
      dir.create(backup_dir, recursive = TRUE, showWarnings = FALSE)
      backup_path <- file.path(
        backup_dir,
        sprintf("uconn_stints_from_pbp_%s.csv", timestamp)
      )
      ok <- file.copy(stints_path, backup_path, overwrite = FALSE)
      if (!isTRUE(ok)) {
        stop("Failed to create stints backup at: ", backup_path, call. = FALSE)
      }
    }
    readr::write_csv(repaired, stints_path)
  }

  list(
    stints_path = stints_path,
    report_path = report_path,
    quarantine_path = quarantine_path,
    backup_path = backup_path,
    changed_rows = changed_n,
    quarantined_rows = sum(source_repair_required),
    historical_imputed_rows = sum(historical_imputed),
    total_rows = nrow(stints),
    rewrite = isTRUE(rewrite)
  )
}

validate_uconn_stints_invariants <- function(
  stints_path = file.path("_data", "01_core_inputs", "uconn_stints_from_pbp.csv"),
  tol = 1e-6
) {
  stints <- readr::read_csv(stints_path, show_col_types = FALSE)
  required <- c("uconn_lineup", "lineup_size", "points_for", "points_against", "poss_est", "net_ppp")
  missing_required <- setdiff(required, names(stints))
  if (length(missing_required) > 0) {
    stop(
      "Invariant check missing columns in stints file: ",
      paste(missing_required, collapse = ", "),
      call. = FALSE
    )
  }

  poss_est <- suppressWarnings(as.numeric(stints$poss_est))
  points_for <- suppressWarnings(as.numeric(stints$points_for))
  points_against <- suppressWarnings(as.numeric(stints$points_against))
  net_ppp <- suppressWarnings(as.numeric(stints$net_ppp))
  lineup_size <- suppressWarnings(as.integer(stints$lineup_size))

  lineup_size_calc <- vapply(
    strsplit(as.character(stints$uconn_lineup), "\\|"),
    function(x) {
      x <- stringr::str_squish(x)
      sum(!is.na(x) & nzchar(x))
    },
    integer(1)
  )

  net_ppp_calc <- ifelse(poss_est > 0, (points_for - points_against) / poss_est, NA_real_)
  net_delta <- abs(net_ppp - net_ppp_calc)

  list(
    rows = nrow(stints),
    poss_est_non_positive = sum(!is.finite(poss_est) | poss_est <= 0, na.rm = TRUE),
    lineup_size_mismatch = sum(!is.na(lineup_size) & lineup_size != lineup_size_calc, na.rm = TRUE),
    net_ppp_mismatch = sum(
      (is.finite(net_ppp_calc) & (!is.finite(net_ppp) | net_delta > tol)) |
        (!is.finite(net_ppp_calc) & !is.na(net_ppp)),
      na.rm = TRUE
    )
  )
}
