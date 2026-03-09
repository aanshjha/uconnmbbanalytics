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

# _scripts/build_lineup_stability_baselines.R
# Rebuilds: _data/02_derived_inputs/uconn_lineup_stabilizers.csv
# Inputs:   _data/01_core_inputs/uconn_stints_from_pbp.csv
# Notes:    Adds games_played + games_ok filter fields; recomputes Step-2 trust fields.

library(dplyr)
library(readr)
library(stringr)


# Paths
STINTS_PATH <- "_data/01_core_inputs/uconn_stints_from_pbp.csv"
OUT_PATH    <- "_data/02_derived_inputs/uconn_lineup_stabilizers.csv"

# Parameters
# Sample tiers (possessions)
HIGH_POSSESSIONS <- 40
MED_POSSESSIONS  <- 20

# "Games appeared in" threshold
G_MIN <- 3

# Trust baseline shrinkage strength (prior possessions)
K_PRIOR_POSS <- 40

# Collapse risk rules
NEGATIVE_NET_PPP_THRESHOLD <- -0.05  # medium/high: clearly negative
EXTREME_DEVIATION_THRESHOLD <- 0.40  # low sample: extreme deviation from team avg

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

# Load and filter stints
stints <- read_csv(STINTS_PATH, show_col_types = FALSE) %>%
  mutate(lineup_key = vapply(uconn_lineup, normalize_lineup, character(1))) %>%
  filter(
    !str_detect(game_file, regex("Exhibition", ignore_case = TRUE)),
    stint_index > 0,
    poss_est > 0
  )

# Aggregate games_played per lineup
games_by_lineup <- stints %>%
  group_by(lineup_key) %>%
  summarise(games_played = n_distinct(game_file), .groups = "drop") %>%
  mutate(games_ok = games_played >= G_MIN)

# Aggregate core lineup totals
lineups <- stints %>%
  group_by(lineup_key) %>%
  summarise(
    possessions = sum(poss_est),
    pts_for     = sum(points_for),
    pts_against = sum(points_against),
    stints      = n(),
    .groups = "drop"
  ) %>%
  mutate(
    net_pts = pts_for - pts_against,
    net_ppp = net_pts / possessions
  ) %>%
  left_join(games_by_lineup, by = "lineup_key")

# Team baseline net PPP (possession-weighted)
team_avg_net_ppp <- sum(lineups$net_pts) / sum(lineups$possessions)

# Step-2 fields: sample_tier, trust_baseline, collapse_risk_flag
lineups <- lineups %>%
  mutate(
    sample_tier = vapply(possessions, tier_from_poss, character(1)),
    team_avg_net_ppp = team_avg_net_ppp,
    dev_from_team_avg = net_ppp - team_avg_net_ppp,
    trust_baseline =
      (K_PRIOR_POSS * team_avg_net_ppp + possessions * net_ppp) / (K_PRIOR_POSS + possessions)
  )

# Collapse risk rules:
# A) low sample + extreme deviation from team avg
rule_a <- with(lineups, sample_tier == "low" & abs(dev_from_team_avg) >= EXTREME_DEVIATION_THRESHOLD)
# B) medium/high sample + clearly negative net
rule_b <- with(lineups, sample_tier %in% c("medium", "high") & net_ppp <= NEGATIVE_NET_PPP_THRESHOLD)

lineups <- lineups %>%
  mutate(collapse_risk_flag = rule_a | rule_b)

# Sort for convenience (not required)
tier_order <- c(high = 0, medium = 1, low = 2)
lineups <- lineups %>%
  mutate(.tier_order = tier_order[sample_tier]) %>%
  arrange(.tier_order, desc(possessions), desc(trust_baseline)) %>%
  select(-.tier_order)

# Write output
write_csv(lineups, OUT_PATH)

cat("\nWROTE:", OUT_PATH, "\n")
cat("Lineups:", nrow(lineups), "\n")
cat("Team avg net PPP:", round(team_avg_net_ppp, 4), "\n")
cat("Games filter (G_MIN):", G_MIN, "\n")
cat("games_ok TRUE:", sum(lineups$games_ok, na.rm = TRUE), "\n")
cat("sample_tier counts:\n")
print(table(lineups$sample_tier))
cat("collapse_risk_flag TRUE:", sum(lineups$collapse_risk_flag, na.rm = TRUE), "\n\n")
