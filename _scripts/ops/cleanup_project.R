#!/usr/bin/env Rscript

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
  library(readr)
  library(dplyr)
  library(stringr)
})


cat("=== Project Cleanup ===\n")

# 1) Remove Finder metadata noise.
ds_files <- list.files(".", pattern = "^\\.DS_Store$", recursive = TRUE, full.names = TRUE)
if (length(ds_files) > 0) {
  ok <- file.remove(ds_files)
  cat(sprintf("Removed .DS_Store: %d/%d\n", sum(ok), length(ds_files)))
} else {
  cat("Removed .DS_Store: 0 (none found)\n")
}

# 2) Detect archive duplicate-style filenames.
dup_named <- list.files(
  "_outputs/_archive",
  pattern = " 2\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)
cat(sprintf("Archive files named '* 2.csv': %d\n", length(dup_named)))

# 3) Check game metadata consistency: opponent should appear in matchup_header.
games_path <- "_data/01_core_inputs/uconn_games_meta.csv"
if (!file.exists(games_path)) {
  cat("uconn_games_meta.csv not found; skipped metadata check.\n")
} else {
  games <- read_csv(games_path, show_col_types = FALSE)
  required <- c("game_file", "game_date", "matchup_header", "opponent")
  missing <- setdiff(required, names(games))
  if (length(missing) > 0) {
    cat(sprintf("Skipped metadata check; missing columns: %s\n", paste(missing, collapse = ", ")))
  } else {
    bad <- games %>%
      mutate(
        opponent_key = str_to_lower(str_replace_all(str_trim(opponent), "\\s+", "")),
        header_key = str_to_lower(str_replace_all(coalesce(matchup_header, ""), "\\s+", ""))
      ) %>%
      filter(!is.na(opponent_key), opponent_key != "", !str_detect(header_key, fixed(opponent_key)))

    cat(sprintf("Metadata mismatch rows (opponent not in header): %d\n", nrow(bad)))
    if (nrow(bad) > 0) {
      print(bad %>% select(game_file, game_date, opponent, matchup_header), n = min(10, nrow(bad)))
    }
  }
}

# 4) Report local environment folders if present.
env_dirs <- c(".tmp_pdfenv", ".venv", ".venv_pdf")
present_env <- env_dirs[dir.exists(env_dirs)]
if (length(present_env) > 0) {
  cat(sprintf("Local env dirs present: %s\n", paste(present_env, collapse = ", ")))
  cat("Tip: remove unused env dirs to save space.\n")
} else {
  cat("Local env dirs present: none\n")
}

cat("=== Cleanup Complete ===\n")
