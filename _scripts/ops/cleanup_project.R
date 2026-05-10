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

source("_scripts/utils/output_cleanup.R")
source("_scripts/utils/core_input_repair.R")

as_bool_env <- function(name, default = FALSE) {
  val <- Sys.getenv(name, if (default) "true" else "false")
  tolower(trimws(val)) %in% c("1", "true", "t", "yes", "y")
}

as_num_env <- function(name, default = NA_real_) {
  x <- suppressWarnings(as.numeric(Sys.getenv(name, as.character(default))))
  if (!is.finite(x)) default else x
}


cat("=== Project Cleanup ===\n")

# 1) Remove Finder metadata noise.
ds_files <- list.files(".", pattern = "^\\.DS_Store$", recursive = TRUE, full.names = TRUE)
if (length(ds_files) > 0) {
  ok <- file.remove(ds_files)
  cat(sprintf("Removed .DS_Store: %d/%d\n", sum(ok), length(ds_files)))
} else {
  cat("Removed .DS_Store: 0 (none found)\n")
}

# 2) Remove duplicate-suffix release artifacts when the canonical file exists.
live_outputs_root <- "_outputs"
dup_cleanup <- remove_duplicate_suffix_artifacts(live_outputs_root)
dup_found <- nrow(dup_cleanup)
dup_removed <- sum(dup_cleanup$removed, na.rm = TRUE)
dup_blocked <- sum(!dup_cleanup$removed & dup_cleanup$canonical_exists, na.rm = TRUE)
dup_orphaned <- sum(!dup_cleanup$canonical_exists, na.rm = TRUE)
cat(sprintf("Duplicate-suffix artifacts found under _outputs: %d\n", dup_found))
cat(sprintf("Duplicate-suffix artifacts removed: %d\n", dup_removed))
cat(sprintf("Duplicate-suffix artifacts blocked from removal: %d\n", dup_blocked))
cat(sprintf("Duplicate-suffix artifacts missing canonical original: %d\n", dup_orphaned))

if (dup_orphaned > 0) {
  print(
    dup_cleanup %>%
      filter(!canonical_exists) %>%
      select(duplicate_path, canonical_path) %>%
      head(10)
  )
}

# 3) Repair core stints input with deterministic rules (enabled by default).
repair_core_inputs <- as_bool_env("REPAIR_CORE_INPUTS", default = TRUE)
repair_core_inputs_dry_run <- as_bool_env("REPAIR_CORE_INPUTS_DRY_RUN", default = FALSE)
repair_core_inputs_backup <- as_bool_env("REPAIR_CORE_INPUTS_BACKUP", default = TRUE)

if (!repair_core_inputs) {
  cat("Core stints repair: skipped (REPAIR_CORE_INPUTS=false)\n")
} else {
  repair_res <- repair_uconn_stints_core_input(
    rewrite = !repair_core_inputs_dry_run,
    backup = !repair_core_inputs_dry_run && repair_core_inputs_backup
  )

  cat(sprintf(
    "Core stints repair: changed %d/%d row(s); mode=%s\n",
    repair_res$changed_rows,
    repair_res$total_rows,
    if (repair_res$rewrite) "rewrite" else "dry-run"
  ))
  cat(sprintf("Core stints repair report: %s\n", repair_res$report_path))
  if (!is.na(repair_res$backup_path) && nzchar(repair_res$backup_path)) {
    cat(sprintf("Core stints backup: %s\n", repair_res$backup_path))
  }
}

# 4) Check game metadata consistency: opponent should appear in matchup_header.
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

# 5) Report local environment folders if present.
env_dirs <- c(".tmp_pdfenv", ".venv", ".venv_pdf")
present_env <- env_dirs[dir.exists(env_dirs)]
if (length(present_env) > 0) {
  cat(sprintf("Local env dirs present: %s\n", paste(present_env, collapse = ", ")))
  cat("Tip: remove unused env dirs to save space.\n")
} else {
  cat("Local env dirs present: none\n")
}

# 6) Prune old pipeline run logs.
log_dir <- file.path("_outputs", "_run_logs")
prune_logs <- as_bool_env("PRUNE_RUN_LOGS", default = TRUE)
keep_recent <- suppressWarnings(as.integer(as_num_env("RUN_LOGS_KEEP_RECENT", default = 20)))
if (!is.finite(keep_recent) || keep_recent < 0) keep_recent <- 20L
max_age_days <- as_num_env("RUN_LOGS_MAX_AGE_DAYS", default = NA_real_)

if (!prune_logs) {
  cat("Run log pruning: skipped (PRUNE_RUN_LOGS=false)\n")
} else if (!dir.exists(log_dir)) {
  cat("Run log pruning: skipped (_outputs/_run_logs not found)\n")
} else {
  before_n <- length(list.files(log_dir, full.names = TRUE))
  prune_res <- prune_run_logs(
    log_dir = log_dir,
    keep_recent = keep_recent,
    max_age_days = max_age_days
  )
  removed_n <- sum(prune_res$removed, na.rm = TRUE)
  after_n <- length(list.files(log_dir, full.names = TRUE))

  cat(sprintf(
    "Run log pruning: removed %d file(s); before=%d after=%d; keep_recent=%d",
    removed_n, before_n, after_n, as.integer(keep_recent)
  ))
  if (is.finite(max_age_days) && max_age_days > 0) {
    cat(sprintf("; max_age_days=%.0f", max_age_days))
  }
  cat("\n")
}

cat("=== Cleanup Complete ===\n")
