manual_csv_root_dir <- function() {
  file.path("_data", "03_manual_game_csv")
}

normalize_competition_bucket <- function(x) {
  x <- trimws(tolower(as.character(x)))
  ifelse(
    x %in% c("conference", "conf", "_conf"),
    "conference",
    ifelse(
      x %in% c("non_conference", "non-conference", "non conference", "nc", "_nc"),
      "non_conference",
      NA_character_
    )
  )
}

manual_csv_bucket_dirs <- function(root_dir = manual_csv_root_dir()) {
  c(
    conference = file.path(root_dir, "_conf"),
    non_conference = file.path(root_dir, "_nc")
  )
}

ensure_manual_csv_dirs <- function(root_dir = manual_csv_root_dir()) {
  dirs <- unname(manual_csv_bucket_dirs(root_dir))
  for (dir_path in dirs) {
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  }
  invisible(dirs)
}

extract_matchup_part <- function(matchup_header, uconn_is_home) {
  parts <- stringr::str_split(as.character(matchup_header), "\\s*-vs-\\s*", n = 2)[[1]]
  if (length(parts) != 2) return(NA_character_)
  # PDF ordering and nominal home/away labels disagree at neutral sites.
  # Select the opponent by identity, never by its position in the header.
  is_uconn <- stringr::str_detect(parts, stringr::regex("^\\s*(UConn|Connecticut)\\b", ignore_case = TRUE))
  if (sum(is_uconn, na.rm = TRUE) != 1L) return(NA_character_)
  parts[!is_uconn][[1]]
}

infer_competition_bucket <- function(game_meta) {
  if ("competition_bucket" %in% names(game_meta)) {
    bucket <- normalize_competition_bucket(game_meta$competition_bucket[[1]])
    if (!is.na(bucket)) return(bucket)
  }

  if (!all(c("matchup_header", "uconn_is_home") %in% names(game_meta))) {
    return(NA_character_)
  }

  opponent_side <- extract_matchup_part(
    matchup_header = game_meta$matchup_header[[1]],
    uconn_is_home = game_meta$uconn_is_home[[1]]
  )
  if (is.na(opponent_side) || opponent_side == "") return(NA_character_)

  # A bare opponent name carries no conference evidence.
  if (!stringr::str_detect(opponent_side, "\\([^)]*[A-Za-z][^)]*\\)")) return(NA_character_)

  if (stringr::str_detect(opponent_side, stringr::regex("Big East", ignore_case = TRUE))) {
    "conference"
  } else {
    "non_conference"
  }
}

manual_csv_dir_for_bucket <- function(bucket, root_dir = manual_csv_root_dir()) {
  dirs <- manual_csv_bucket_dirs(root_dir)
  bucket <- normalize_competition_bucket(bucket)
  if (is.na(bucket) || !(bucket %in% names(dirs))) {
    stop("Unknown competition bucket: ", as.character(bucket), call. = FALSE)
  }
  dirs[[bucket]]
}

manual_csv_output_path <- function(game_meta, root_dir = manual_csv_root_dir()) {
  bucket <- infer_competition_bucket(game_meta)
  if (is.na(bucket)) {
    stop(
      "Could not infer competition_bucket for game_file=",
      as.character(game_meta$game_file[[1]]),
      call. = FALSE
    )
  }

  game_file <- as.character(game_meta$game_file[[1]])
  out_name <- stringr::str_trim(stringr::str_remove(game_file, "\\.pdf$"))
  file.path(manual_csv_dir_for_bucket(bucket, root_dir), paste0(out_name, ".csv"))
}

manual_csv_bucket_from_path <- function(path, root_dir = manual_csv_root_dir()) {
  norm_path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  dirs <- c(manual_csv_bucket_dirs(root_dir), conference_tournament = file.path(root_dir, "_bet"))

  for (bucket in names(dirs)) {
    bucket_dir <- paste0(normalizePath(dirs[[bucket]], winslash = "/", mustWork = FALSE), "/")
    if (startsWith(norm_path, bucket_dir)) return(bucket)
  }

  NA_character_
}

list_manual_game_csv_files <- function(root_dir = manual_csv_root_dir()) {
  dirs <- c(unname(manual_csv_bucket_dirs(root_dir)), file.path(root_dir, "_bet"))
  files <- unlist(
    lapply(dirs, function(dir_path) {
      if (!dir.exists(dir_path)) return(character())
      list.files(dir_path, pattern = "\\.csv$", full.names = TRUE)
    }),
    use.names = FALSE
  )

  files <- files[basename(files) != "_espn_generation_summary.csv"]
  sort(unique(files[file.exists(files)]))
}

# Source IDs are identifiers, not floating point values. An 18-digit ESPN play
# ID cannot be reconstructed after a CSV reader has rounded it to a double.
deduplicate_source_events <- function(
  events,
  keys = c("game_id", "play_id"),
  provenance_cols = c("source_path", "source_file", "source_bucket", "competition_bucket"),
  label = "source events"
) {
  missing <- setdiff(keys, names(events))
  if (length(missing)) stop(label, " missing key columns: ", paste(missing, collapse = ", "), call. = FALSE)
  for (key in keys) {
    if (is.numeric(events[[key]]) && any(abs(events[[key]]) >= 2^53, na.rm = TRUE)) {
      stop(label, " has unsafe numeric ", key, "; reread the original IDs as character.", call. = FALSE)
    }
    events[[key]] <- trimws(as.character(events[[key]]))
    if (any(is.na(events[[key]]) | !nzchar(events[[key]]))) {
      stop(label, " has missing ", key, "; source repair is required.", call. = FALSE)
    }
  }

  duplicate_rows <- duplicated(events[keys]) | duplicated(events[keys], fromLast = TRUE)
  payload_cols <- setdiff(names(events), provenance_cols)
  duplicate_payloads <- unique(as.data.frame(events[duplicate_rows, payload_cols, drop = FALSE]))
  conflicting <- duplicated(duplicate_payloads[keys]) | duplicated(duplicate_payloads[keys], fromLast = TRUE)
  if (any(conflicting)) {
    bad_keys <- unique(duplicate_payloads[conflicting, keys, drop = FALSE])
    display <- apply(utils::head(bad_keys, 5), 1, paste, collapse = "/")
    stop(label, " has conflicting duplicate source keys: ", paste(display, collapse = ", "),
      "; no rows were selected. Reconcile these copies against source events.", call. = FALSE)
  }
  keep <- !duplicated(events[keys])
  out <- events[keep, , drop = FALSE]
  attr(out, "duplicate_rows_removed") <- sum(!keep)
  out
}

load_manual_games <- function(root_dir = manual_csv_root_dir()) {
  manual_files <- list_manual_game_csv_files(root_dir)
  if (length(manual_files) == 0) {
    stop("No manual-game CSV files found under ", root_dir, call. = FALSE)
  }

  manual_games <- dplyr::bind_rows(lapply(manual_files, function(path) {
    tbl <- readr::read_csv(path, show_col_types = FALSE, col_types = readr::cols(
      game_id = readr::col_character(), play_id = readr::col_character(),
      team_id = readr::col_character(), athlete_id_1 = readr::col_character(),
      athlete_id_2 = readr::col_character(), clock_display_value = readr::col_character()
    ))
    dplyr::mutate(
      tbl,
      source_path = path,
      source_file = basename(path),
      source_bucket = manual_csv_bucket_from_path(path, root_dir)
    )
  }))

  char_cols <- names(manual_games)[vapply(manual_games, is.character, logical(1))]
  manual_games[char_cols] <- lapply(manual_games[char_cols], function(col) {
    out <- stringr::str_trim(col)
    out[out == ""] <- NA_character_
    out
  })

  if (!("competition_bucket" %in% names(manual_games))) {
    manual_games$competition_bucket <- NA_character_
  }

  manual_games$competition_bucket <- dplyr::coalesce(
    normalize_competition_bucket(manual_games$competition_bucket),
    manual_games$source_bucket
  )

  # Ignore folder labels when comparing event payloads, but surface every
  # classification disagreement instead of making the first file authoritative.
  provenance <- manual_games %>%
    dplyr::group_by(game_id, play_id) %>%
    dplyr::summarise(
      source_paths = paste(sort(unique(source_path)), collapse = " | "),
      source_buckets = paste(sort(unique(source_bucket)), collapse = " | "),
      source_copy_count = dplyr::n(),
      source_bucket_conflict = dplyr::n_distinct(c(source_bucket, competition_bucket), na.rm = TRUE) > 1,
      .groups = "drop"
    )
  out <- deduplicate_source_events(manual_games, label = "Manual-game CSVs")
  duplicate_rows_removed <- attr(out, "duplicate_rows_removed")
  out <- dplyr::left_join(out, provenance, by = c("game_id", "play_id"))
  out$competition_bucket[out$source_bucket_conflict] <- NA_character_
  attr(out, "duplicate_rows_removed") <- duplicate_rows_removed
  attr(out, "duplicate_report") <- provenance[provenance$source_copy_count > 1, , drop = FALSE]
  out
}
