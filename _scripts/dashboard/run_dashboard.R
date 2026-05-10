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

required_pkgs <- c("shiny", "dplyr", "readr", "DT")
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop(
    "Missing required R package(s): ",
    paste(missing_pkgs, collapse = ", "),
    "\nInstall with: install.packages(c(",
    paste(sprintf('\"%s\"', missing_pkgs), collapse = ", "),
    "))",
    call. = FALSE
  )
}

source("_scripts/dashboard/app.R")

output_dir <- Sys.getenv("DASH_OUTPUT_DIR", unset = "_outputs")
host <- Sys.getenv("DASH_HOST", unset = "127.0.0.1")
port_raw <- Sys.getenv("DASH_PORT", unset = "3838")

if (!grepl("^[0-9]+$", port_raw)) {
  stop("DASH_PORT must be an integer. Received: ", port_raw, call. = FALSE)
}

port <- as.integer(port_raw)
if (!is.finite(port) || port <= 0 || port > 65535) {
  stop("DASH_PORT must be between 1 and 65535. Received: ", port_raw, call. = FALSE)
}

run_dashboard_app(output_dir = output_dir, host = host, port = port)
