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

source("_scripts/utils/core_input_repair.R")

args <- commandArgs(trailingOnly = TRUE)
has_flag <- function(flag) any(args == flag)

rewrite <- TRUE
if (has_flag("--dry-run")) rewrite <- FALSE
if (has_flag("--rewrite")) rewrite <- TRUE
backup <- !has_flag("--no-backup")
if (!rewrite) backup <- FALSE

cat("=== Core Input Repair ===\n")
cat(sprintf("Mode: %s\n", if (rewrite) "rewrite" else "dry-run"))

res <- repair_uconn_stints_core_input(
  rewrite = rewrite,
  backup = backup
)

cat(sprintf("Rows changed: %d / %d\n", res$changed_rows, res$total_rows))
cat(sprintf("Repair report: %s\n", res$report_path))
if (!is.na(res$backup_path) && nzchar(res$backup_path)) {
  cat(sprintf("Backup file: %s\n", res$backup_path))
}

if (rewrite) {
  inv <- validate_uconn_stints_invariants(res$stints_path, tol = 1e-6)
  cat("Post-rewrite invariant check:\n")
  cat(sprintf(" - poss_est <= 0 rows: %d\n", inv$poss_est_non_positive))
  cat(sprintf(" - lineup_size mismatch rows: %d\n", inv$lineup_size_mismatch))
  cat(sprintf(" - net_ppp mismatch rows (>1e-6): %d\n", inv$net_ppp_mismatch))
}

cat("=== Core Input Repair Complete ===\n")
