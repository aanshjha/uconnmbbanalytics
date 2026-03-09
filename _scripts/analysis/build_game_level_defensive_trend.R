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

# Build a game-by-game defensive trend from lineup leak posteriors so
# staff can see whether issues are improving or compounding.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(lubridate)
  library(stringr)
})

source("_scripts/utils/project_paths.R")

resolve_path <- function(fname) {
  resolve_project_path(
    fname,
    extra_dirs = c(
      file.path("_outputs", "01_lineup_core"),
      file.path("_outputs", "02_defense_leaks"),
      file.path("_outputs", "03_players"),
      file.path("_outputs", "04_games_trends")
    )
  )
}

# Load inputs
stints <- read_csv(resolve_path("uconn_stints_from_pbp.csv"), show_col_types = FALSE)

lineup_def <- read_csv(
  resolve_path("uconn_lineup_def_leaks_posterior.csv"),
  show_col_types = FALSE
)

# Pull pace from lineup_usage (no time parsing)
usage <- read_csv(resolve_path("uconn_lineup_usage.csv"), show_col_types = FALSE)

PACE_UCONN <- sum(usage$possessions, na.rm = TRUE) / sum(usage$minutes, na.rm = TRUE)

if (!is.finite(PACE_UCONN) || PACE_UCONN <= 0) {
  stop("PACE_UCONN is invalid from uconn_lineup_usage.csv. Check minutes/possessions.")
}

message(
  "Computed PACE_UCONN (poss/min): ", round(PACE_UCONN, 3),
  " | implied poss per 40: ", round(PACE_UCONN * 40, 1)
)

# Prepare stint-level defense
stints2 <- stints %>%
  filter(
    !str_detect(game_file, regex("Exhibition", ignore_case = TRUE)),
    lineup_size == 5,
    poss_est > 0
  ) %>%
  mutate(
    poss_est = as.numeric(poss_est),
    uconn_lineup_canon = sapply(
      strsplit(uconn_lineup, "\\|"),
      function(x) paste(sort(trimws(x)), collapse = "|")
    )
  ) %>%
  left_join(
    lineup_def %>% select(lineup, u_def_mean, u_def_p05, u_def_p95),
    by = c("uconn_lineup_canon" = "lineup")
  )

joined_missing <- mean(is.na(stints2$u_def_mean))
if (!is.finite(joined_missing) || joined_missing > 0.05) {
  stop(
    "Too many missing defensive posterior joins (",
    round(100 * joined_missing, 1),
    "%). Check lineup canonicalization and input files."
  )
}

# Aggregate to game level
game_def_trend <- stints2 %>%
  group_by(game_file, game_date) %>%
  summarise(
    poss = sum(poss_est, na.rm = TRUE),
    
    def_leak_mean =
      sum(u_def_mean * poss_est, na.rm = TRUE) /
      sum(poss_est, na.rm = TRUE),
    
    def_leak_p05 =
      sum(u_def_p05 * poss_est, na.rm = TRUE) /
      sum(poss_est, na.rm = TRUE),
    
    def_leak_p95 =
      sum(u_def_p95 * poss_est, na.rm = TRUE) /
      sum(poss_est, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(
    game_date = mdy(game_date),
    
    # Convert to points per 40 using UConn pace
    def_pts_40_mean = def_leak_mean * 40 * PACE_UCONN,
    def_pts_40_p05  = def_leak_p05  * 40 * PACE_UCONN,
    def_pts_40_p95  = def_leak_p95  * 40 * PACE_UCONN
  ) %>%
  arrange(game_date)

# Save outputs (top-level _outputs so organizer can bucket it)
out_csv <- file.path("_outputs", "uconn_game_level_def_leak_trend.csv")
out_png <- file.path("_outputs", "uconn_game_level_def_leak_trend.png")

write_csv(game_def_trend, out_csv)

# Plot
p <- ggplot(game_def_trend, aes(x = game_date)) +
  geom_ribbon(aes(ymin = def_pts_40_p05, ymax = def_pts_40_p95),
              fill = "grey80", alpha = 0.5) +
  geom_line(aes(y = def_pts_40_mean), color = "firebrick", linewidth = 1.1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(
    title = "UConn Defense: Game-to-Game Leak Trend",
    subtitle = "Points allowed per 40 minutes vs baseline (posterior mean ± 90% CI)",
    x = "Game Date",
    y = "Defensive Impact (pts / 40)"
  ) +
  theme_minimal(base_size = 13)

ggsave(out_png, p, width = 9, height = 5, dpi = 300)

message("Wrote:")
message(" - ", normalizePath(out_csv))
message(" - ", normalizePath(out_png))
