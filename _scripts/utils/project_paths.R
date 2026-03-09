# Shared path helpers for project scripts.
# These keep file discovery aligned with the nested 03_manual_game_csv layout.

manual_csv_search_dirs <- function() {
  c(
    file.path("_data", "03_manual_game_csv"),
    file.path("_data", "03_manual_game_csv", "_games"),
    file.path("_data", "03_manual_game_csv", "_conf"),
    file.path("_data", "03_manual_game_csv", "_nc")
  )
}

recursive_manual_csv_hits <- function(fname) {
  manual_root <- file.path("_data", "03_manual_game_csv")
  if (!dir.exists(manual_root)) return(character())

  manual_files <- list.files(manual_root, recursive = TRUE, full.names = TRUE)
  hits <- manual_files[basename(manual_files) == fname]
  unique(hits[file.exists(hits)])
}

resolve_project_path <- function(fname, extra_dirs = character(), required = TRUE) {
  base_dirs <- c(
    ".",
    file.path("_data", "01_core_inputs"),
    file.path("_data", "02_derived_inputs"),
    manual_csv_search_dirs(),
    file.path("_data", "04_templates"),
    file.path("_data", "05_projects"),
    file.path("_data", "05_projects", "scheme_matchup_project"),
    "_models",
    "_outputs"
  )

  dirs <- unique(c(base_dirs, extra_dirs))
  direct_candidates <- unique(file.path(dirs, fname))
  direct_hits <- direct_candidates[file.exists(direct_candidates)]
  hits <- unique(c(direct_hits, recursive_manual_csv_hits(fname)))

  if (length(hits) == 0) {
    if (!required) return(NULL)
    stop(
      "Missing file: ", fname, "\n",
      "Searched direct paths:\n  - ", paste(direct_candidates, collapse = "\n  - "), "\n",
      "Also searched recursively under:\n  - ", file.path("_data", "03_manual_game_csv")
    )
  }

  hits[[1]]
}

resolve_project_first_path <- function(candidates,
                                       required = TRUE,
                                       search_roots = c(".", "_data", "_models", "_outputs"),
                                       extra_roots = character()) {
  roots <- unique(c(search_roots, extra_roots, manual_csv_search_dirs()))
  expanded <- unique(unlist(lapply(roots, function(root) file.path(root, candidates)), use.names = FALSE))
  direct_hits <- expanded[file.exists(expanded)]
  recursive_hits <- unique(unlist(lapply(candidates, recursive_manual_csv_hits), use.names = FALSE))
  hits <- unique(c(direct_hits, recursive_hits))

  if (length(hits) == 0) {
    if (!required) return(NULL)
    stop(
      "Missing required file. Searched direct paths:\n  - ",
      paste(expanded, collapse = "\n  - "),
      "\nAlso searched recursively under:\n  - ",
      file.path("_data", "03_manual_game_csv")
    )
  }

  hits[[1]]
}
