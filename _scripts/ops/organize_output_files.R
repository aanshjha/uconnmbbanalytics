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

# Organize top-level files in _outputs into fixed subfolders.
# Keeps the output tree readable after full pipeline runs.

suppressPackageStartupMessages({
  library(stringr)
})

ROOT <- "."
OUT_DIR <- file.path(ROOT, "_outputs")
if (!dir.exists(OUT_DIR)) stop("Missing folder: ", OUT_DIR)

buckets <- c(
  "01_lineup_core",
  "02_defense_leaks",
  "03_players",
  "04_games_trends",
  "05_decision_audit",
  "06_scheme_matchups"
)

# Ensure folders exist
for (b in buckets) dir.create(file.path(OUT_DIR, b), recursive = TRUE, showWarnings = FALSE)

# Options
MOVE_FILES  <- TRUE   # TRUE = move, FALSE = copy
DRY_RUN     <- FALSE  # TRUE = print actions only (no changes)
ARCHIVE_OLD <- TRUE   # TRUE = archive bucket folder contents before organizing
DELETE_OLD  <- FALSE  # TRUE = permanently delete bucket folder contents (danger)

# Safety: don't allow archive + delete together
if (ARCHIVE_OLD && DELETE_OLD) stop("Choose ARCHIVE_OLD or DELETE_OLD, not both.")

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
archive_dir <- file.path(OUT_DIR, "_archive", timestamp)

# Safer move/copy helper
safe_copy_or_move <- function(from, to, move = FALSE) {
  if (file.exists(to)) file.remove(to)
  
  ok <- FALSE
  
  if (move) {
    ok <- file.rename(from, to)
    
    # Fallback: rename can fail across filesystems; try copy+delete
    if (!ok) {
      ok2 <- file.copy(from, to, overwrite = TRUE)
      if (ok2) {
        file.remove(from)
        ok <- TRUE
      }
    }
  } else {
    ok <- file.copy(from, to, overwrite = TRUE)
  }
  
  ok
}

# Routing logic (patterns)
pick_bucket <- function(base) {
  # 06_scheme_matchups
  if (str_detect(base, "scheme_matchup|scheme_matchups|scheme_tags|matchup_exploitation")) return("06_scheme_matchups")

  # 05_decision_audit
  if (str_detect(base, "decision_|eligibility_by_stint|rolling_backtest|threshold|pred_pr_net_pos_calibration|synergy_prob_calibration|availability_stress")) {
    return("05_decision_audit")
  }

  # 03_players
  if (str_detect(base, "^uconn_player_.*\\.csv$")) return("03_players")
  
  # 01_lineup_core
  if (str_detect(base, "^uconn_lineup_(usage|synergy_posterior|coach_view|decision_table)\\.csv$")) return("01_lineup_core")
  if (str_detect(base, "^uconn_lineup_core_model_diagnostics\\.csv$")) return("01_lineup_core")
  
  # 02_defense_leaks
  if (str_detect(base, "def_leak|def_leaks|defonly|defense_leaks")) return("02_defense_leaks")
  if (str_detect(base, "model_diagnostics|param_diagnostics|diagnostics")) return("02_defense_leaks")
  
  # 04_games_trends (tighten PNG routing)
  if (str_detect(base, "game_level|trend|bad_games|by_game|game_attribution")) return("04_games_trends")
  if (str_detect(base, "trend.*\\.png$|game_level.*\\.png$|by_game.*\\.png$")) return("04_games_trends")
  
  # Default
  return("01_lineup_core")
}

# Only sort files sitting directly in _outputs (not inside folders)
files <- list.files(OUT_DIR, full.names = TRUE, recursive = FALSE)
files <- files[file.info(files)$isdir == FALSE]

# Exclude top-level manifest files (keep them at top-level)
files <- files[!basename(files) %in% c("organize_manifest.csv")]

if (length(files) == 0) {
  message("No top-level files found in ", OUT_DIR, " (nothing to organize).")
  quit(save = "no")
}

# Archive or delete old bucket contents (only after confirming there is work to do)
archive_log <- data.frame(from = character(), to = character(), ok = logical(), stringsAsFactors = FALSE)

if (ARCHIVE_OLD) {
  if (!DRY_RUN) {
    dir.create(archive_dir, recursive = TRUE, showWarnings = FALSE)
    for (b in buckets) dir.create(file.path(archive_dir, b), recursive = TRUE, showWarnings = FALSE)
  }
  
  for (b in buckets) {
    bdir <- file.path(OUT_DIR, b)
    old_files <- list.files(bdir, full.names = TRUE, recursive = FALSE)
    old_files <- old_files[file.info(old_files)$isdir == FALSE]
    
    if (length(old_files) > 0) {
      for (f in old_files) {
        dest <- file.path(archive_dir, b, basename(f))
        if (DRY_RUN) {
          archive_log <- rbind(archive_log, data.frame(from=f, to=dest, ok=NA, stringsAsFactors = FALSE))
        } else {
          ok <- safe_copy_or_move(f, dest, move = TRUE)
          archive_log <- rbind(archive_log, data.frame(from=f, to=dest, ok=ok, stringsAsFactors = FALSE))
        }
      }
    }
  }
  
  message("ARCHIVE_OLD = TRUE")
  message("Archive location: ", archive_dir)
}

if (DELETE_OLD) {
  for (b in buckets) {
    bdir <- file.path(OUT_DIR, b)
    old_files <- list.files(bdir, full.names = TRUE, recursive = FALSE)
    old_files <- old_files[file.info(old_files)$isdir == FALSE]
    
    if (length(old_files) > 0) {
      if (DRY_RUN) {
        message("[DRY RUN] Would delete ", length(old_files), " files from ", bdir)
      } else {
        file.remove(old_files)
      }
    }
  }
  message("DELETE_OLD = TRUE")
}

manifest <- data.frame(
  file   = basename(files),
  from   = files,
  bucket = vapply(basename(files), pick_bucket, character(1)),
  stringsAsFactors = FALSE
)

# Execute
results <- data.frame(from=character(), to=character(), ok=logical(), stringsAsFactors = FALSE)

for (i in seq_len(nrow(manifest))) {
  src <- manifest$from[i]
  bucket <- manifest$bucket[i]
  dst <- file.path(OUT_DIR, bucket, basename(src))
  
  if (DRY_RUN) {
    results <- rbind(results, data.frame(from=src, to=dst, ok=NA, stringsAsFactors = FALSE))
  } else {
    ok <- safe_copy_or_move(src, dst, move = MOVE_FILES)
    results <- rbind(results, data.frame(from=src, to=dst, ok=ok, stringsAsFactors = FALSE))
  }
}

# Save manifest
manifest_path <- file.path(OUT_DIR, "organize_manifest.csv")
if (!DRY_RUN) {
  write.csv(manifest, manifest_path, row.names = FALSE)
}

# Summary
message("Done organizing _outputs.")
message("Mode: ", ifelse(MOVE_FILES, "MOVE", "COPY"))
message("DRY_RUN: ", DRY_RUN)
message("Manifest: ", manifest_path)

summary_counts <- as.data.frame(table(manifest$bucket))
names(summary_counts) <- c("bucket", "n_files")
print(summary_counts)

if (!DRY_RUN) {
  failed <- results[results$ok == FALSE, , drop = FALSE]
  if (nrow(failed) > 0) {
    warning("Some files failed to move/copy. See below:")
    print(failed)
  }
}
