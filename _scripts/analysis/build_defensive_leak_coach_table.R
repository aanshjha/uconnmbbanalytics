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

# Merge defensive leak posterior + lineup context into a coach-facing
# watchlist table.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
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

out_dir <- if (dir.exists("_outputs")) file.path("_outputs", "02_defense_leaks") else "."
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

post_path  <- resolve_path("uconn_lineup_def_leaks_posterior.csv")
usage_path <- resolve_path("uconn_lineup_usage.csv")

post  <- read_csv(post_path,  show_col_types = FALSE)
usage <- read_csv(usage_path, show_col_types = FALSE)

# UConn pace (possessions per minute) for per-40 conversions
PACE_UCONN <- sum(usage$possessions, na.rm = TRUE) / sum(usage$minutes, na.rm = TRUE)

if (!is.finite(PACE_UCONN) || PACE_UCONN <= 0) {
  stop("PACE_UCONN is invalid. Check uconn_lineup_usage.csv minutes/possessions.")
}

message(
  "Computed PACE_UCONN (poss/min): ", round(PACE_UCONN, 3),
  " | implied poss per 40: ", round(PACE_UCONN * 40, 1)
)

# Coach-facing defense risk bands (tunable).
# We keep high-certainty labels strict but add "LEAN"/"WATCH" bands so the table
# surfaces directional signals instead of collapsing to all-INCONCLUSIVE.
DEF_LEAK_STRONG_PR <- suppressWarnings(as.numeric(Sys.getenv("DEF_LEAK_STRONG_PR", "0.70")))
DEF_LEAK_LEAN_PR   <- suppressWarnings(as.numeric(Sys.getenv("DEF_LEAK_LEAN_PR", "0.60")))
DEF_PLUS_STRONG_PR <- suppressWarnings(as.numeric(Sys.getenv("DEF_PLUS_STRONG_PR", "0.30")))
DEF_PLUS_LEAN_PR   <- suppressWarnings(as.numeric(Sys.getenv("DEF_PLUS_LEAN_PR", "0.40")))
DEF_EFFECTIVE_SAMPLE_HIGH <- suppressWarnings(as.numeric(Sys.getenv("DEF_EFFECTIVE_SAMPLE_HIGH", "120")))
DEF_EFFECTIVE_SAMPLE_MED  <- suppressWarnings(as.numeric(Sys.getenv("DEF_EFFECTIVE_SAMPLE_MED", "60")))

for (nm in c(
  "DEF_LEAK_STRONG_PR",
  "DEF_LEAK_LEAN_PR",
  "DEF_PLUS_STRONG_PR",
  "DEF_PLUS_LEAN_PR",
  "DEF_EFFECTIVE_SAMPLE_HIGH",
  "DEF_EFFECTIVE_SAMPLE_MED"
)) {
  if (!is.finite(get(nm))) stop("Invalid threshold env var: ", nm)
}

# usage has lineup_pretty (pipe-delimited). post has lineup_pretty too.
# join on lineup_id + lineup_pretty (most stable).
df <- post %>%
  left_join(
    usage %>% select(lineup_id, lineup_pretty, possessions, minutes, games, segments, raw_net_ppp),
    by = c("lineup_id", "lineup_pretty")
  )

# Core signal-strength features
df <- df %>%
  mutate(
    # credible interval width (smaller = more certain)
    ci90_width = u_def_p95 - u_def_p05,
    
    # display rounding
    poss_round = round(possessions, 0),
    min_round  = round(minutes, 1),
    
    # Effective sample is raw lineup possessions (no inflation).
    effective_sample = possessions,
    
    # expected added points allowed per 40 minutes (UConn pace scaling)
    expected_pts_allowed_per_40 = round(u_def_mean * 40 * PACE_UCONN, 1),
    
    # Confidence buckets (college thresholds + coach-safe language)
    confidence = case_when(
      effective_sample >= DEF_EFFECTIVE_SAMPLE_HIGH & ci90_width <= 0.18 ~ "HIGH",
      effective_sample >= DEF_EFFECTIVE_SAMPLE_MED  & ci90_width <= 0.22 ~ "MEDIUM",
      TRUE                                         ~ "INSUFFICIENT EVIDENCE"
    ),
    
    # leak labels (use effective sample instead of raw possessions)
    leak_flag = case_when(
      effective_sample >= DEF_EFFECTIVE_SAMPLE_HIGH & pr_leak >= DEF_LEAK_STRONG_PR & u_def_p50 > 0 ~ "LEAK RISK (HIGH CONF)",
      effective_sample >= DEF_EFFECTIVE_SAMPLE_MED  & pr_leak >= DEF_LEAK_LEAN_PR   & u_def_mean > 0 ~ "LEAK RISK (LEAN)",
      effective_sample >= DEF_EFFECTIVE_SAMPLE_HIGH & pr_leak <= DEF_PLUS_STRONG_PR & u_def_p50 < 0 ~ "DEF PLUS (HIGH CONF)",
      effective_sample >= DEF_EFFECTIVE_SAMPLE_HIGH & pr_leak <= DEF_PLUS_LEAN_PR   & u_def_p50 < 0 ~ "DEF PLUS (LEAN)",
      pr_leak >= 0.55 & u_def_mean > 0                                      ~ "WATCH (LEAK SIGNAL)",
      pr_leak <= 0.45 & u_def_mean < 0                                      ~ "WATCH (DEF PLUS SIGNAL)",
      TRUE                                                                  ~ "INCONCLUSIVE"
    ),
    
    # Coach-safety tag
    dont_overreact = case_when(
      effective_sample < DEF_EFFECTIVE_SAMPLE_HIGH ~ "EVALUATION PHASE",
      pr_leak > 0.35 & pr_leak < 0.65             ~ "COIN FLIP",
      ci90_width > 0.25                           ~ "UNCERTAIN SIGNAL",
      TRUE                                        ~ ""
    ),
    
    # Dev vs Win-max framing (update language to match confidence labels)
    lineup_type = case_when(
      effective_sample < DEF_EFFECTIVE_SAMPLE_HIGH ~ "EVALUATION PHASE",
      confidence == "INSUFFICIENT EVIDENCE"       ~ "EVALUATION PHASE",
      str_detect(leak_flag, "^DEF PLUS")          ~ "WIN-MAX (DEFENSE)",
      str_detect(leak_flag, "^LEAK RISK")         ~ "WIN-MAX (AVOID/ADJUST)",
      TRUE                                        ~ "MIXED"
    )
  )

# Rotation necessity tag
# - compute max_poss first, then apply mutate that references it
max_poss <- max(df$possessions, na.rm = TRUE)

df <- df %>%
  mutate(
    rotation_necessary_not_additive = case_when(
      possessions >= 0.30 * max_poss &
        abs(u_def_mean) < 0.02 &
        pr_leak >= 0.35 & pr_leak <= 0.65 ~ "YES",
      TRUE ~ ""
    )
  )

# Coach ordering and output table
df_out <- df %>%
  arrange(
    factor(
      leak_flag,
      levels = c(
        "LEAK RISK (HIGH CONF)",
        "LEAK RISK (LEAN)",
        "WATCH (LEAK SIGNAL)",
        "INCONCLUSIVE",
        "WATCH (DEF PLUS SIGNAL)",
        "DEF PLUS (LEAN)",
        "DEF PLUS (HIGH CONF)"
      )
    ),
    desc(possessions)
  ) %>%
  transmute(
    lineup_pretty,
    possessions = poss_round,
    minutes = min_round,
    games,
    raw_net_ppp,
    
    u_def_mean = round(u_def_mean, 3),
    u_def_p05  = round(u_def_p05, 3),
    u_def_p95  = round(u_def_p95, 3),
    pr_leak    = round(pr_leak, 3),
    
    expected_pts_allowed_per_40,
    confidence,
    leak_flag,
    rotation_necessary_not_additive,
    dont_overreact,
    lineup_type
  )

write_csv(df_out, file.path(out_dir, "uconn_lineup_def_leaks_coach_table.csv"))

message("Done. Wrote to: ", normalizePath(out_dir))
message(" - uconn_lineup_def_leaks_coach_table.csv")
