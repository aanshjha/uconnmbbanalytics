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

# Track player role stability over three season windows and pair it with
# a current-form impact check.
# Output: _outputs/03_players/uconn_player_rsi_three_windows.csv

library(dplyr)
library(readr)
library(stringr)
library(tidyr)


# Inputs
STINTS_PATH <- "_data/01_core_inputs/uconn_stints_from_pbp.csv"

# Phase definitions
PHASE1 <- list(name = "phase1", label = "Phase 1 (1-7)",   min_game = 1L,  max_game = 7L)
PHASE2 <- list(name = "phase2", label = "Phase 2 (8-14)",  min_game = 8L,  max_game = 14L)
PHASE3 <- list(name = "phase3", label = "Phase 3 (15-latest)", min_game = 15L, max_game = 19L)

# "Current Phase" definition: last 3 games of Phase 3
CURRENT_LAST_N_GAMES <- 3L

# Minimum possessions per player per phase
MIN_POSS_PER_PHASE <- 50

# Exhibition handling (exclude for all calculations)
EXCLUDE_EXHIBITIONS <- TRUE

# Thresholds for conservative labeling (RSI concentration deltas)
DELTA_SMALL <- 0.02  # within +/- 0.02 treated as "flat"
DELTA_LARGE <- 0.04  # >= 0.04 treated as meaningful shift

# Helpers
normalize_lineup <- function(x) {
  players <- str_split(x, "\\|", simplify = FALSE)[[1]]
  players <- str_trim(players)
  str_c(sort(players), collapse = "|")
}

assign_phase <- function(game_id) {
  if (game_id >= PHASE1$min_game && game_id <= PHASE1$max_game) return(PHASE1$name)
  if (game_id >= PHASE2$min_game && game_id <= PHASE2$max_game) return(PHASE2$name)
  if (game_id >= PHASE3$min_game && game_id <= PHASE3$max_game) return(PHASE3$name)
  return(NA_character_)
}

# Role Stability Index (RSI) per player per phase:
# RSI = sum over lineups (share_of_player_possessions_in_lineup^2)
# Higher RSI -> more concentrated usage across lineups (more stable role)
compute_rsi <- function(df_player_lineup) {
  df_player_lineup %>%
    group_by(player, phase) %>%
    mutate(total_poss = sum(poss)) %>%
    ungroup() %>%
    mutate(share = poss / total_poss) %>%
    group_by(player, phase) %>%
    summarise(
      poss_total = first(total_poss),
      lineups_used = n_distinct(lineup_key),
      rsi = sum(share^2),
      eff_lineups = if_else(sum(share^2) > 0, 1 / sum(share^2), NA_real_),
      .groups = "drop"
    )
}

# Weighted net_ppp by possessions
compute_impact <- function(df_player_stints) {
  df_player_stints %>%
    group_by(player, phase) %>%
    summarise(
      poss_total = sum(poss_est[is.finite(net_ppp)], na.rm = TRUE),
      net_ppp_mean = if_else(
        sum(poss_est[is.finite(net_ppp)], na.rm = TRUE) > 0,
        sum(net_ppp * poss_est, na.rm = TRUE) / sum(poss_est[is.finite(net_ppp)], na.rm = TRUE),
        NA_real_
      ),
      pr_pos_weighted = if_else(
        sum(poss_est[is.finite(net_ppp)], na.rm = TRUE) > 0,
        sum((net_ppp > 0) * poss_est, na.rm = TRUE) / sum(poss_est[is.finite(net_ppp)], na.rm = TRUE),
        NA_real_
      ),
      .groups = "drop"
    )
}

# Two-phase capable role label:
# - Prefer recent (Phase2->Phase3) if both phases meet MIN_POSS_PER_PHASE
# - Else fallback to early (Phase1->Phase2) if both meet MIN
label_role_signal <- function(p1, p2, p3, d12, d23) {
  
  has12 <- !is.na(p1) && !is.na(p2) && !is.na(d12) &&
    p1 >= MIN_POSS_PER_PHASE && p2 >= MIN_POSS_PER_PHASE
  
  has23 <- !is.na(p2) && !is.na(p3) && !is.na(d23) &&
    p2 >= MIN_POSS_PER_PHASE && p3 >= MIN_POSS_PER_PHASE
  
  if (!has12 && !has23) return("insufficient_sample")
  
  # Prefer recent signal
  if (has23) {
    if (abs(d23) >= DELTA_LARGE) {
      if (d23 > 0) return("role_shift_recent_more_stable")
      return("role_shift_recent_less_stable")
    }
    if (abs(d23) < DELTA_SMALL) return("flat_role_recent")
    if (d23 >= DELTA_SMALL) return("stabilizing_role_recent")
    if (d23 <= -DELTA_SMALL) return("unstable_role_recent")
    return("mixed_role_change_recent")
  }
  
  # Fallback to early
  if (has12) {
    if (abs(d12) >= DELTA_LARGE) {
      if (d12 > 0) return("role_shift_early_more_stable")
      return("role_shift_early_less_stable")
    }
    if (abs(d12) < DELTA_SMALL) return("flat_role_early")
    if (d12 >= DELTA_SMALL) return("stabilizing_role_early")
    if (d12 <= -DELTA_SMALL) return("unstable_role_early")
    return("mixed_role_change_early")
  }
  
  "insufficient_sample"
}

impact_confidence <- function(poss_current) {
  if (is.na(poss_current)) return("low")
  if (poss_current >= 120) return("high")
  if (poss_current >= 60)  return("medium")
  "low"
}

impact_label <- function(pr_pos, poss_current) {
  if (is.na(pr_pos) || is.na(poss_current)) return("impact_uncertain")
  if (poss_current < MIN_POSS_PER_PHASE) return("impact_uncertain")
  
  if (pr_pos >= 0.65) return("impact_confirmed_positive")
  if (pr_pos <= 0.35) return("impact_confirmed_negative")
  "impact_neutral_or_uncertain"
}

# Load and validate inputs
stints <- read_csv(STINTS_PATH, show_col_types = FALSE)

required_cols <- c("game_id", "game_file", "uconn_lineup", "points_for", "points_against")
missing <- setdiff(required_cols, names(stints))
if (length(missing) > 0) stop("Missing required columns in stints: ", paste(missing, collapse = ", "))

# Ensure poss_est and net_ppp exist; compute if missing
if (!("poss_est" %in% names(stints))) {
  stints <- stints %>% mutate(poss_est = pmax(1, (points_for + points_against) / 2))
}
if (!("net_ppp" %in% names(stints))) {
  stints <- stints %>% mutate(net_ppp = (points_for - points_against) / poss_est)
}

if (EXCLUDE_EXHIBITIONS) {
  stints <- stints %>%
    filter(!str_detect(game_file, regex("Exhibition", ignore_case = TRUE)))
}

# Extend Phase 3 to the current latest game in the dataset.
max_game_id <- suppressWarnings(max(as.integer(stints$game_id), na.rm = TRUE))
if (!is.finite(max_game_id)) stop("Could not determine max game_id from stints.")
PHASE3$max_game <- as.integer(max_game_id)
if (PHASE3$max_game < PHASE3$min_game) {
  stop("Current dataset has fewer than ", PHASE3$min_game, " games; Phase 3 is not available yet.")
}
PHASE3$label <- sprintf("Phase 3 (%d-%d)", PHASE3$min_game, PHASE3$max_game)

# Add phase + filter to phase range
stints <- stints %>%
  mutate(phase = vapply(game_id, assign_phase, character(1))) %>%
  filter(!is.na(phase))

# Current phase = last N games within Phase 3
phase3_game_ids <- sort(unique(stints$game_id[stints$phase == PHASE3$name]))
if (length(phase3_game_ids) == 0) stop("No stints found for Phase 3. Check game_id ranges.")
current_game_ids <- tail(phase3_game_ids, CURRENT_LAST_N_GAMES)

# Expand players from lineup
players_long <- stints %>%
  mutate(
    lineup_key = vapply(uconn_lineup, normalize_lineup, character(1)),
    player = str_split(uconn_lineup, "\\|")
  ) %>%
  unnest(player) %>%
  mutate(player = str_trim(player)) %>%
  filter(player != "", !is.na(player))

# Role stability (RSI) per phase
player_lineup_poss <- players_long %>%
  group_by(player, phase, lineup_key) %>%
  summarise(poss = sum(poss_est, na.rm = TRUE), .groups = "drop")

rsi_by_phase <- compute_rsi(player_lineup_poss) %>%
  mutate(
    rsi = if_else(poss_total >= MIN_POSS_PER_PHASE, rsi, NA_real_),
    eff_lineups = if_else(poss_total >= MIN_POSS_PER_PHASE, eff_lineups, NA_real_)
  )

rsi_wide <- rsi_by_phase %>%
  select(player, phase, poss_total, lineups_used, rsi, eff_lineups) %>%
  pivot_wider(
    names_from = phase,
    values_from = c(poss_total, lineups_used, rsi, eff_lineups),
    names_sep = "_"
  )

# Impact confirmation per phase (weighted net_ppp)
impact_phase_summary <- players_long %>%
  select(player, phase, poss_est, net_ppp) %>%
  compute_impact()

impact_wide <- impact_phase_summary %>%
  pivot_wider(
    names_from = phase,
    values_from = c(poss_total, net_ppp_mean, pr_pos_weighted),
    names_sep = "_"
  ) %>%
  # Avoid name collisions with RSI phase possession columns (kept as poss_total_*).
  rename_with(
    ~ str_replace(.x, "^poss_total_", "impact_poss_total_"),
    starts_with("poss_total_")
  )

# Current-only impact (Phase 3 last 3 games)
impact_current <- players_long %>%
  filter(game_id %in% current_game_ids) %>%
  group_by(player) %>%
  summarise(
    poss_current = sum(poss_est[is.finite(net_ppp)], na.rm = TRUE),
    net_ppp_current = if_else(
      sum(poss_est[is.finite(net_ppp)], na.rm = TRUE) > 0,
      sum(net_ppp * poss_est, na.rm = TRUE) / sum(poss_est[is.finite(net_ppp)], na.rm = TRUE),
      NA_real_
    ),
    pr_pos_current = if_else(
      sum(poss_est[is.finite(net_ppp)], na.rm = TRUE) > 0,
      sum((net_ppp > 0) * poss_est, na.rm = TRUE) / sum(poss_est[is.finite(net_ppp)], na.rm = TRUE),
      NA_real_
    ),
    .groups = "drop"
  ) %>%
  mutate(
    poss_current = if_else(poss_current >= MIN_POSS_PER_PHASE, poss_current, NA_real_),
    net_ppp_current = if_else(!is.na(poss_current), net_ppp_current, NA_real_),
    pr_pos_current = if_else(!is.na(poss_current), pr_pos_current, NA_real_),
    impact_confidence_current = vapply(poss_current, impact_confidence, character(1)),
    impact_label_current = mapply(impact_label, pr_pos_current, poss_current)
  )

# Combine (robust to missing phase columns)
out <- rsi_wide %>%
  full_join(impact_wide, by = "player") %>%
  full_join(impact_current, by = "player")

# Ensure expected phase columns exist
expected_cols <- c(
  paste0("poss_total_",  PHASE1$name), paste0("poss_total_",  PHASE2$name), paste0("poss_total_",  PHASE3$name),
  paste0("rsi_",         PHASE1$name), paste0("rsi_",         PHASE2$name), paste0("rsi_",         PHASE3$name)
)
for (cc in expected_cols) {
  if (!(cc %in% names(out))) out[[cc]] <- NA_real_
}

# Label role stability (two-phase capable)
out <- out %>%
  mutate(
    rsi_phase1  = .data[[paste0("rsi_", PHASE1$name)]],
    rsi_phase2  = .data[[paste0("rsi_", PHASE2$name)]],
    rsi_phase3  = .data[[paste0("rsi_", PHASE3$name)]],
    
    poss_phase1 = .data[[paste0("poss_total_", PHASE1$name)]],
    poss_phase2 = .data[[paste0("poss_total_", PHASE2$name)]],
    poss_phase3 = .data[[paste0("poss_total_", PHASE3$name)]],
    
    delta_rsi_1_to_2 = rsi_phase2 - rsi_phase1,
    delta_rsi_2_to_3 = rsi_phase3 - rsi_phase2,
    
    role_stability_signal = mapply(
      label_role_signal,
      poss_phase1, poss_phase2, poss_phase3,
      delta_rsi_1_to_2, delta_rsi_2_to_3
    )
  )

# Final output (role stability primary, impact confirmation secondary)
final <- out %>%
  transmute(
    player,
    
    phase1_range = PHASE1$label,
    phase2_range = PHASE2$label,
    phase3_range = PHASE3$label,
    current_phase = paste0(
      "Phase 3 last ", CURRENT_LAST_N_GAMES,
      " games (game_id ", paste(current_game_ids, collapse = ","), ")"
    ),
    
    # Role stability (primary)
    poss_phase1,
    poss_phase2,
    poss_phase3,
    rsi_phase1,
    rsi_phase2,
    rsi_phase3,
    delta_rsi_1_to_2,
    delta_rsi_2_to_3,
    role_stability_signal,
    
    # Impact confirmation (secondary)
    poss_current,
    net_ppp_current,
    pr_pos_current,
    impact_confidence_current,
    impact_label_current
  ) %>%
  arrange(desc(!is.na(poss_current)), desc(poss_current), player)

# Write output
OUT_DIR <- "_outputs/03_players"
OUT_PATH <- file.path(OUT_DIR, "uconn_player_rsi_three_windows.csv")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
write_csv(final, OUT_PATH)

cat("\nWROTE:", OUT_PATH, "\n")
cat("Players:", nrow(final), "\n")
cat("Min possessions per phase:", MIN_POSS_PER_PHASE, "\n")
cat("Current phase last N games:", CURRENT_LAST_N_GAMES, "\n\n")
cat("Role stability signal counts:\n")
print(table(final$role_stability_signal, useNA = "ifany"))
cat("\nImpact label (current) counts:\n")
print(table(final$impact_label_current, useNA = "ifany"))
cat("\n")
