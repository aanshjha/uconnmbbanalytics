bootstrap_project <- function(script_path = NULL) {
  if (is.null(script_path)) {
    script_arg <- grep("^--file=", commandArgs(), value = TRUE)
    if (length(script_arg) == 0) {
      stop("Could not determine script path. Run this file with Rscript.", call. = FALSE)
    }
    script_path <- sub("^--file=", "", script_arg[[1]])
  }

  script_path <- normalizePath(script_path, winslash = "/", mustWork = TRUE)
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
