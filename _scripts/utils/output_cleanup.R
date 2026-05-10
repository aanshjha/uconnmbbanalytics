find_duplicate_suffix_artifacts <- function(root_dir, pattern = " [0-9]+\\.(csv|md|png)$") {
  if (!dir.exists(root_dir)) return(character())
  list.files(root_dir, pattern = pattern, recursive = TRUE, full.names = TRUE)
}

canonical_duplicate_artifact_path <- function(path) {
  sub(" [0-9]+(\\.[^.]+)$", "\\1", path)
}

remove_duplicate_suffix_artifacts <- function(root_dir, pattern = " [0-9]+\\.(csv|md|png)$") {
  dup_paths <- find_duplicate_suffix_artifacts(root_dir, pattern = pattern)
  if (length(dup_paths) == 0) {
    return(data.frame(
      duplicate_path = character(),
      canonical_path = character(),
      canonical_exists = logical(),
      removed = logical(),
      stringsAsFactors = FALSE
    ))
  }

  res <- data.frame(
    duplicate_path = dup_paths,
    canonical_path = vapply(dup_paths, canonical_duplicate_artifact_path, character(1)),
    stringsAsFactors = FALSE
  )
  res$canonical_exists <- file.exists(res$canonical_path)
  res$removed <- FALSE

  for (i in seq_len(nrow(res))) {
    if (!isTRUE(res$canonical_exists[[i]])) next
    res$removed[[i]] <- isTRUE(file.remove(res$duplicate_path[[i]]))
  }

  res
}

collect_run_log_groups <- function(log_dir) {
  if (!dir.exists(log_dir)) {
    return(data.frame(
      timestamp = character(),
      file = character(),
      full_path = character(),
      stringsAsFactors = FALSE
    ))
  }

  files <- list.files(log_dir, full.names = TRUE)
  files <- files[file.info(files)$isdir == FALSE]
  if (length(files) == 0) {
    return(data.frame(
      timestamp = character(),
      file = character(),
      full_path = character(),
      stringsAsFactors = FALSE
    ))
  }

  rel <- basename(files)
  ts <- sub("^([0-9]{8}_[0-9]{6})_.*$", "\\1", rel)
  keep <- grepl("^[0-9]{8}_[0-9]{6}$", ts)
  if (!any(keep)) {
    return(data.frame(
      timestamp = character(),
      file = character(),
      full_path = character(),
      stringsAsFactors = FALSE
    ))
  }

  data.frame(
    timestamp = ts[keep],
    file = rel[keep],
    full_path = files[keep],
    stringsAsFactors = FALSE
  )
}

prune_run_logs <- function(log_dir, keep_recent = 20L, max_age_days = NA_real_) {
  groups <- collect_run_log_groups(log_dir)
  if (nrow(groups) == 0) {
    return(data.frame(
      timestamp = character(),
      file = character(),
      full_path = character(),
      kept = logical(),
      removed = logical(),
      reason = character(),
      stringsAsFactors = FALSE
    ))
  }

  keep_recent <- suppressWarnings(as.integer(keep_recent))
  if (!is.finite(keep_recent) || keep_recent < 0) keep_recent <- 20L

  ts_sorted <- sort(unique(groups$timestamp), decreasing = TRUE)
  keep_ts <- if (keep_recent == 0L) character() else utils::head(ts_sorted, keep_recent)

  cutoff <- NA_real_
  if (is.finite(max_age_days) && max_age_days > 0) {
    cutoff <- as.numeric(Sys.time()) - as.numeric(max_age_days) * 24 * 3600
  }

  groups$kept <- groups$timestamp %in% keep_ts
  groups$removed <- FALSE
  groups$reason <- ifelse(groups$kept, "keep_recent", "trim_recent")

  if (is.finite(cutoff)) {
    ts_time <- suppressWarnings(as.POSIXct(groups$timestamp, format = "%Y%m%d_%H%M%S", tz = "UTC"))
    old <- !is.na(ts_time) & as.numeric(ts_time) < cutoff
    groups$reason[old & !groups$kept] <- "trim_recent_and_age"
    groups$reason[old & groups$kept] <- "keep_recent_even_if_old"
  }

  drop_idx <- !groups$kept
  for (i in which(drop_idx)) {
    groups$removed[[i]] <- isTRUE(file.remove(groups$full_path[[i]]))
  }

  groups
}
