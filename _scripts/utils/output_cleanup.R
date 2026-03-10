find_duplicate_suffix_artifacts <- function(root_dir, pattern = " 2\\.(csv|md|png)$") {
  if (!dir.exists(root_dir)) return(character())
  list.files(root_dir, pattern = pattern, recursive = TRUE, full.names = TRUE)
}

canonical_duplicate_artifact_path <- function(path) {
  sub(" 2(\\.[^.]+)$", "\\1", path)
}

remove_duplicate_suffix_artifacts <- function(root_dir, pattern = " 2\\.(csv|md|png)$") {
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
