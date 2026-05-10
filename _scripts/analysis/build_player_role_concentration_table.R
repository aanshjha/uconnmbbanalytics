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

# Build the main player role concentration table from stint usage + player
# posterior impact estimates.

library(dplyr)
library(readr)
library(stringr)
library(tidyr)

source("_scripts/utils/project_paths.R")

# Config
ROOT <- "."
DATA_DIRS <- c(
  file.path(ROOT, "_data", "01_core_inputs"),
  file.path(ROOT, "_data", "02_derived_inputs"),
  file.path(ROOT, "_data", "03_manual_game_csv"),
  file.path(ROOT, "_data", "03_manual_game_csv", "_conf"),
  file.path(ROOT, "_data", "03_manual_game_csv", "_nc"),
  file.path(ROOT, "_data", "04_templates"),
  file.path(ROOT, "_data", "05_projects"),
  file.path(ROOT, "_data", "05_projects", "scheme_matchup_project")
)
OUT_DIR  <- file.path(ROOT, "_outputs")
PLAYER_BUCKET_DIR <- file.path(OUT_DIR, "03_players")

dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(PLAYER_BUCKET_DIR, showWarnings = FALSE, recursive = TRUE)

resolve_path <- function(fname, dirs = c(ROOT, DATA_DIRS, OUT_DIR, file.path(OUT_DIR, "03_players"))) {
  resolve_project_path(fname, extra_dirs = dirs)
}

write_player_output <- function(df, fname) {
  out_paths <- c(
    file.path(OUT_DIR, fname),
    file.path(PLAYER_BUCKET_DIR, fname)
  )
  for (path in unique(out_paths)) {
    write_csv(df, path)
  }
}

stints_path <- resolve_path("uconn_stints_from_pbp.csv")
post_path <- if (file.exists(file.path(OUT_DIR, "uconn_player_net_posterior.csv")) ||
                 file.exists(file.path(OUT_DIR, "03_players", "uconn_player_net_posterior.csv"))) {
  resolve_path("uconn_player_net_posterior.csv")
} else {
  # Backward-compatibility for older runs before net-only patch.
  resolve_path("uconn_player_off_def_net_posterior.csv")
}

# Load inputs
stints <- read_csv(stints_path, show_col_types = FALSE)
player_post <- read_csv(post_path, show_col_types = FALSE)

# Exclude exhibitions
stints <- stints %>%
  filter(!str_detect(game_file, "Exhibition"))

# Canonicalize lineup and expand to player-level rows
segments2 <- stints %>%
  filter(lineup_size == 5, !is.na(poss_est), poss_est > 0, !is.na(net_ppp)) %>%
  mutate(
    lineup_canon = sapply(
      str_split(uconn_lineup, "\\|"),
      function(x) paste(sort(x), collapse = "|")
    )
  )

player_lineup_rows <- segments2 %>%
  select(game_file, lineup_canon, poss_est) %>%
  mutate(player = str_split(lineup_canon, "\\|")) %>%
  unnest(player)

# Per-player usage by lineup
player_usage <- player_lineup_rows %>%
  group_by(player, lineup_canon) %>%
  summarise(
    poss_in_lineup  = sum(poss_est, na.rm = TRUE),
    games_in_lineup = n_distinct(game_file),
    .groups = "drop"
  )

# Role Concentration Index (RCI) diagnostics
# RCI = sum over lineups (share_of_player_possessions_in_lineup^2)
# Higher RCI means the player's usage is concentrated in fewer lineup contexts.
player_rci <- player_usage %>%
  group_by(player) %>%
  mutate(
    total_possessions = sum(poss_in_lineup, na.rm = TRUE),
    share_of_player_possessions = if_else(
      total_possessions > 0,
      poss_in_lineup / total_possessions,
      NA_real_
    )
  ) %>%
  summarise(
    total_possessions     = first(total_possessions),
    unique_lineups        = n_distinct(lineup_canon),
    mean_poss_per_lineup  = mean(poss_in_lineup, na.rm = TRUE),
    RCI                   = sum(share_of_player_possessions^2, na.rm = TRUE),
    lineup_poss_sd        = sd(poss_in_lineup, na.rm = TRUE),
    lineup_poss_cv        = if_else(mean_poss_per_lineup > 0, lineup_poss_sd / mean_poss_per_lineup, NA_real_),
    .groups = "drop"
  ) %>%
  mutate(
    RCI = if_else(total_possessions > 0 & is.finite(RCI), RCI, NA_real_)
  )

write_player_output(player_rci, "uconn_player_rci.csv")

# Merge with model-based player impact and build coach table
player_rci_merged <- player_rci %>%
  left_join(
    player_post %>%
      select(player, net_mean, net_p05, net_p95, net_pr_pos),
    by = "player"
  )

# Thresholds (tune later; these are reasonable defaults)
# Default stable cutoff corresponds to about five equal-share lineup contexts.
RCI_STABLE_EFFECTIVE_LINEUPS_MAX <- suppressWarnings(as.numeric(Sys.getenv(
  "RCI_STABLE_EFFECTIVE_LINEUPS_MAX",
  "5"
)))
if (!is.finite(RCI_STABLE_EFFECTIVE_LINEUPS_MAX) || RCI_STABLE_EFFECTIVE_LINEUPS_MAX <= 0) {
  stop("Invalid RCI_STABLE_EFFECTIVE_LINEUPS_MAX. Expected a positive number.")
}
RCI_STABLE_CUTOFF <- 1 / RCI_STABLE_EFFECTIVE_LINEUPS_MAX
MIN_POSS_FOR_ROLE_LABEL <- suppressWarnings(as.numeric(Sys.getenv("MIN_POSS_FOR_ROLE_LABEL", "50")))
if (!is.finite(MIN_POSS_FOR_ROLE_LABEL) || MIN_POSS_FOR_ROLE_LABEL <= 0) {
  stop("Invalid MIN_POSS_FOR_ROLE_LABEL. Expected a positive number.")
}
PR_HIGH_CUTOFF    <- 0.70
PR_LOW_CUTOFF     <- 0.30

coach_table <- player_rci_merged %>%
  mutate(
    role_rci_display = if_else(total_possessions >= MIN_POSS_FOR_ROLE_LABEL, RCI, NA_real_),
    role_type = case_when(
      is.na(RCI) ~ "UNKNOWN",
      total_possessions < MIN_POSS_FOR_ROLE_LABEL ~ "UNKNOWN",
      RCI >= RCI_STABLE_CUTOFF ~ "STABLE",
      TRUE ~ "FLUID"
    ),
    impact_type = case_when(
      is.na(net_pr_pos) ~ "UNKNOWN",
      net_pr_pos >= PR_HIGH_CUTOFF ~ "HIGH",
      net_pr_pos <= PR_LOW_CUTOFF ~ "LOW",
      TRUE ~ "MID"
    ),
    recommendation = case_when(
      role_type == "UNKNOWN" ~ "HOLD JUDGMENT (sample too small)",
      role_type == "FLUID"  & impact_type == "HIGH" ~ "LOCK ROLE (reduce lineup churn)",
      role_type == "STABLE" & impact_type == "LOW"  ~ "ROLE RE-EVAL (fit/matchups)",
      role_type == "FLUID"  & impact_type == "LOW"  ~ "HOLD JUDGMENT (needs stable reps)",
      role_type == "STABLE" & impact_type == "HIGH" ~ "MAINTAIN ROLE (protect usage)",
      TRUE ~ "MONITOR"
    )
  ) %>%
  transmute(
    player,
    total_possessions = round(total_possessions, 0),
    unique_lineups,
    RCI = round(role_rci_display, 4),
    net_mean = round(net_mean, 3),
    net_p05  = round(net_p05, 3),
    net_p95  = round(net_p95, 3),
    net_pr_pos = round(net_pr_pos, 3),
    role_type,
    recommendation
  ) %>%
  arrange(desc(net_mean), desc(total_possessions))

write_player_output(coach_table, "uconn_player_rci_coach_table.csv")

message("Done. Wrote to: ", OUT_DIR)
message(" - uconn_player_rci.csv")
message(" - uconn_player_rci_coach_table.csv")
