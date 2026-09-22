bootstrap_project <- function(script_path = NULL) {
  if (is.null(script_path)) {
    script_arg <- grep("^--file=", commandArgs(), value = TRUE)
    if (length(script_arg) == 0) {
      stop("Could not determine script path. Run this file with Rscript.", call. = FALSE)
    }
    script_path <- sub("^--file=", "", script_arg[[1]])
  }

  script_path <- normalizePath(script_path, winslash = "/", mustWork = TRUE)
  script_folder <- basename(dirname(script_path))
  script_name <- basename(script_path)
  retired_analysis <- script_folder == "analysis" &&
    script_name != "generate_manual_game_csvs_from_espn.R"
  retired_entrypoint <- script_folder %in% c("models", "pipeline") ||
    retired_analysis ||
    (script_folder == "ops" && script_name == "run_runnable_entrypoints.R")
  if (retired_entrypoint) {
    stop(
      "Historical analysis is retired: stint/lineup attribution and earlier validation are not verified. ",
      "This entrypoint cannot regenerate recommendation outputs. ",
      "Use bash run_coaching_pipeline.sh for the current workflow; see docs/RELIABILITY_RESET.md.",
      call. = FALSE
    )
  }
  project_root <- normalizePath(
    file.path(dirname(script_path), "..", ".."),
    winslash = "/",
    mustWork = TRUE
  )

  setwd(project_root)

  if (!dir.exists("_scripts") || !dir.exists("_data")) {
    stop("Could not locate project root. Run via Rscript from this repo.", call. = FALSE)
  }

  invisible(project_root)
}
