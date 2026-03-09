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

# _scripts/audit_lineup_decision_eligibility.R
# Output: _outputs/05_decision_audit/uconn_decision_eligibility_by_stint.csv
# Label each stint decision as eligible/ineligible using only prior-game information.
# This version sorts output in game-flow order:
# game_id, period, start_time (clock counts down, so descending).

library(dplyr)
library(readr)
library(stringr)
library(tidyr)


# Paths
STINTS_PATH <- "_data/01_core_inputs/uconn_stints_from_pbp.csv"
OUT_DIR     <- "_outputs/05_decision_audit"
OUT_PATH    <- file.path(OUT_DIR, "uconn_decision_eligibility_by_stint.csv")
DEPRECATED_OUT_PATH <- file.path(OUT_DIR, "uconn_decision_validity_by_stint.csv")

# Parameters (must match stabilizer build logic)
G_MIN <- 3
HIGH_POSSESSIONS <- 40
MED_POSSESSIONS  <- 20
K_PRIOR_POSS <- 40

NEGATIVE_NET_PPP_THRESHOLD <- -0.05
EXTREME_DEVIATION_THRESHOLD <- 0.40

# Helpers
normalize_lineup <- function(x) {
  players <- str_split(x, "\\|", simplify = FALSE)[[1]]
  players <- str_trim(players)
  str_c(sort(players), collapse = "|")
}

tier_from_poss <- function(poss) {
  if (poss >= HIGH_POSSESSIONS) return("high")
  if (poss >= MED_POSSESSIONS)  return("medium")
  "low"
}

# Build stabilizers using ONLY stints before a given game_id (no leakage)
build_stabilizers_prior <- function(stints_all, current_game_id) {
  
  prior <- stints_all %>%
    filter(
      game_id < current_game_id,
      !str_detect(game_file, regex("Exhibition", ignore_case = TRUE)), # ignore exhibitions for prior knowledge
      stint_index > 0,
      poss_est > 0
    ) %>%
    mutate(lineup_key = vapply(uconn_lineup, normalize_lineup, character(1)))
  
  if (nrow(prior) == 0) {
    return(tibble(
      lineup_key = character(),
      possessions_prior = numeric(),
      games_played_prior = integer(),
      games_ok_prior = logical(),
      sample_tier_prior = character(),
      net_ppp_prior = numeric(),
      trust_baseline_prior = numeric(),
      collapse_risk_flag_prior = logical()
    ))
  }
  
  games_by_lineup <- prior %>%
    group_by(lineup_key) %>%
    summarise(
      games_played_prior = n_distinct(game_file),
      .groups = "drop"
    ) %>%
    mutate(games_ok_prior = games_played_prior >= G_MIN)
  
  lineups <- prior %>%
    group_by(lineup_key) %>%
    summarise(
      possessions_prior = sum(poss_est),
      pts_for_prior     = sum(points_for),
      pts_against_prior = sum(points_against),
      stints_prior      = n(),
      .groups = "drop"
    ) %>%
    mutate(
      net_pts_prior = pts_for_prior - pts_against_prior,
      net_ppp_prior = net_pts_prior / possessions_prior
    ) %>%
    left_join(games_by_lineup, by = "lineup_key")
  
  team_avg_net_ppp_prior <- sum(lineups$net_pts_prior) / sum(lineups$possessions_prior)
  
  lineups <- lineups %>%
    mutate(
      team_avg_net_ppp_prior = team_avg_net_ppp_prior,
      sample_tier_prior = vapply(possessions_prior, tier_from_poss, character(1)),
      dev_from_team_avg_prior = net_ppp_prior - team_avg_net_ppp_prior,
      trust_baseline_prior =
        (K_PRIOR_POSS * team_avg_net_ppp_prior + possessions_prior * net_ppp_prior) /
        (K_PRIOR_POSS + possessions_prior)
    )
  
  rule_a <- with(lineups,
                 sample_tier_prior == "low" &
                   abs(dev_from_team_avg_prior) >= EXTREME_DEVIATION_THRESHOLD)
  
  rule_b <- with(lineups,
                 sample_tier_prior %in% c("medium", "high") &
                   net_ppp_prior <= NEGATIVE_NET_PPP_THRESHOLD)
  
  lineups %>%
    mutate(collapse_risk_flag_prior = rule_a | rule_b) %>%
    select(
      lineup_key,
      possessions_prior,
      games_played_prior,
      games_ok_prior,
      sample_tier_prior,
      net_ppp_prior,
      trust_baseline_prior,
      collapse_risk_flag_prior
    )
}

# Load stints (requires game_id column already added)
stints_all <- read_csv(STINTS_PATH, show_col_types = FALSE)

if (!("game_id" %in% names(stints_all))) {
  stop("uconn_stints_from_pbp.csv is missing game_id. Add game_id before running this script.")
}

# normalize lineup key once
stints_all <- stints_all %>%
  mutate(lineup_key = vapply(uconn_lineup, normalize_lineup, character(1)))

game_ids <- sort(unique(stints_all$game_id))

# Main loop: label every stint using only prior games
all_labeled <- list()

for (g in game_ids) {
  
  prior_stabilizers <- build_stabilizers_prior(stints_all, current_game_id = g)
  
  current <- stints_all %>%
    filter(game_id == g) %>%
    # decision moment = every stint
    select(
      game_id, game_file, game_date, period, start_time, end_time, stint_index,
      uconn_lineup, lineup_key,
      poss_est, points_for, points_against
    )
  
  labeled <- current %>%
    left_join(prior_stabilizers, by = "lineup_key") %>%
    mutate(
      decision_eligible_prior = if_else(
        !is.na(games_ok_prior) & games_ok_prior & !collapse_risk_flag_prior,
        TRUE, FALSE
      ),
      eligibility_reason = case_when(
        is.na(games_ok_prior) ~ "unknown_lineup_prior",
        games_ok_prior == FALSE ~ "not_games_ok_prior",
        collapse_risk_flag_prior == TRUE ~ "collapse_risk_prior",
        TRUE ~ "eligible"
      )
    )
  
  all_labeled[[as.character(g)]] <- labeled
}

audit <- bind_rows(all_labeled)

# Sort in true game-flow order
# period asc; start_time desc because clock counts down
# start_time is in your stints file as "MM:SS"
audit <- audit %>%
  mutate(
    start_time = as.character(start_time),
    # extract first MM:SS pattern anywhere in the string
    start_time_mmss = str_extract(start_time, "\\b\\d{1,2}:\\d{2}\\b"),
    start_min = as.integer(str_extract(start_time_mmss, "^\\d{1,2}")),
    start_sec = as.integer(str_extract(start_time_mmss, "\\d{2}$")),
    start_clock_sec = start_min * 60 + start_sec,
    # if missing/unparseable, push to end within period
    start_clock_sec = if_else(is.na(start_clock_sec), -1L, start_clock_sec)
  ) %>%
  arrange(
    game_id,
    period,
    desc(start_clock_sec),
    stint_index
  ) %>%
  select(-start_time_mmss, -start_min, -start_sec, -start_clock_sec)

# Write output
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
write_csv(audit, OUT_PATH)
if (file.exists(DEPRECATED_OUT_PATH)) file.remove(DEPRECATED_OUT_PATH)

cat("\nWROTE:", OUT_PATH, "\n")
cat("Rows (stints audited):", nrow(audit), "\n")
cat("Eligible decisions:", sum(audit$decision_eligible_prior, na.rm = TRUE), "\n")
cat("Ineligible decisions:", sum(!audit$decision_eligible_prior, na.rm = TRUE), "\n")
cat("\nReason counts:\n")
print(table(audit$eligibility_reason, useNA = "ifany"))
cat("\n")
