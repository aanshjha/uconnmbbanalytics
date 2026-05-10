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
})

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(key, default = NULL) {
  hit <- grep(paste0("^--", key, "="), args, value = TRUE)
  if (length(hit) == 0) return(default)
  sub(paste0("^--", key, "="), "", hit[[1]])
}

manifest_path <- arg_value("manifest", "_scripts/ops/runnable_entrypoints_manifest.csv")
if (!file.exists(manifest_path)) {
  stop("Missing manifest file: ", manifest_path, call. = FALSE)
}

manifest <- read_csv(manifest_path, show_col_types = FALSE)
required_cols <- c("entrypoint_id", "run_type", "command")
missing_cols <- setdiff(required_cols, names(manifest))
if (length(missing_cols) > 0) {
  stop(
    "Manifest missing required columns: ",
    paste(missing_cols, collapse = ", "),
    call. = FALSE
  )
}

manifest <- manifest %>%
  mutate(
    entrypoint_id = trimws(as.character(entrypoint_id)),
    run_type = tolower(trimws(as.character(run_type))),
    command = trimws(as.character(command))
  ) %>%
  filter(nzchar(entrypoint_id), nzchar(run_type), nzchar(command))

bad_types <- setdiff(unique(manifest$run_type), c("required", "optional"))
if (length(bad_types) > 0) {
  stop(
    "Manifest has invalid run_type values: ",
    paste(bad_types, collapse = ", "),
    call. = FALSE
  )
}

log_dir <- file.path("_outputs", "_run_logs")
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
summary_path <- file.path(log_dir, sprintf("runnable_entrypoints_%s_summary.csv", timestamp))

run_one <- function(entrypoint_id, command, run_type) {
  log_path <- file.path(
    log_dir,
    sprintf("runnable_entrypoints_%s_%s.log", timestamp, entrypoint_id)
  )
  started <- Sys.time()
  exit_code <- system2(
    "bash",
    c("-l", "-c", shQuote(command)),
    stdout = log_path,
    stderr = log_path
  )
  finished <- Sys.time()
  elapsed <- as.integer(round(difftime(finished, started, units = "secs")))

  status <- if (exit_code == 0) {
    "OK"
  } else if (run_type == "required") {
    "FAIL_REQUIRED"
  } else {
    "FAIL_OPTIONAL"
  }

  tibble(
    entrypoint_id = entrypoint_id,
    run_type = run_type,
    status = status,
    exit_code = as.integer(exit_code),
    elapsed_sec = elapsed,
    started_at_utc = format(as.POSIXct(started, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ"),
    finished_at_utc = format(as.POSIXct(finished, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ"),
    command = command,
    log_path = log_path
  )
}

cat("=== Runnable Entrypoints Runner ===\n")
cat(sprintf("Manifest: %s\n", manifest_path))

results <- list()
required_failed <- FALSE
for (i in seq_len(nrow(manifest))) {
  row <- manifest[i, ]
  cat(sprintf("[%d/%d] %s (%s)\n", i, nrow(manifest), row$entrypoint_id[[1]], row$run_type[[1]]))

  res <- run_one(
    entrypoint_id = row$entrypoint_id[[1]],
    command = row$command[[1]],
    run_type = row$run_type[[1]]
  )
  results[[length(results) + 1]] <- res

  cat(sprintf(
    " -> status=%s exit=%d log=%s\n",
    res$status[[1]],
    res$exit_code[[1]],
    res$log_path[[1]]
  ))

  if (identical(res$status[[1]], "FAIL_REQUIRED")) {
    required_failed <- TRUE
    break
  }
}

summary_df <- bind_rows(results)
write_csv(summary_df, summary_path)
cat(sprintf("Summary: %s\n", summary_path))

if (required_failed) {
  stop("Required runnable entrypoint failed. See summary/logs above.", call. = FALSE)
}

cat("Runnable entrypoints complete.\n")
