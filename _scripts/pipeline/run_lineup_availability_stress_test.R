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

# Quick stress test: remove one player and surface the best remaining
# lineup options under current risk thresholds.
library(dplyr)
library(readr)
library(stringr)

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

stab  <- read_csv(resolve_path("uconn_lineup_stabilizers.csv"), show_col_types = FALSE)
leaks <- read_csv(resolve_path("uconn_lineup_def_leaks_posterior.csv"), show_col_types = FALSE)

df <- stab %>%
  left_join(
    leaks %>% select(lineup, pr_leak, lineup_pretty, u_def_mean, u_def_p05, u_def_p95),
    by = c("lineup_key" = "lineup")
  )

required_cols <- c("lineup_key", "sample_tier", "collapse_risk_flag", "trust_baseline", "possessions")
missing_stab <- setdiff(required_cols, names(stab))
if (length(missing_stab) > 0) {
  stop("uconn_lineup_stabilizers.csv is missing columns: ", paste(missing_stab, collapse = ", "))
}

cat("Share missing pr_leak after join:", mean(is.na(df$pr_leak)), "\n")

# Config (env overrides make this script reproducible in automation/CLI)
PLAYER_OUT <- Sys.getenv("STRESS_TEST_PLAYER_OUT", "Mullins,Braylon")
TOP_N <- suppressWarnings(as.integer(Sys.getenv("STRESS_TOP_N", "3")))
MAX_PR_LEAK <- suppressWarnings(as.numeric(Sys.getenv("STRESS_MAX_PR_LEAK", "0.50")))
OUT_DIR <- file.path("_outputs", "05_decision_audit")
OUT_PATH <- file.path(OUT_DIR, "uconn_availability_stress_test_report.csv")

if (!is.finite(TOP_N) || TOP_N < 1) TOP_N <- 3L
if (!is.finite(MAX_PR_LEAK) || MAX_PR_LEAK < 0 || MAX_PR_LEAK > 1) MAX_PR_LEAK <- 0.50
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

eligible_stop_run <- function(data,
                              max_pr_leak = 0.50,
                              min_sample_tier = c("medium","high")) {
  data %>%
    filter(
      sample_tier %in% min_sample_tier,
      collapse_risk_flag == FALSE,
      !is.na(pr_leak),
      pr_leak <= max_pr_leak
    )
}

stress_test_player_out <- function(player_name,
                                   top_n = 3,
                                   max_pr_leak = 0.50) {
  
  remaining <- df %>%
    filter(!str_detect(lineup_key, fixed(player_name)))
  
  eligible_remaining <- remaining %>%
    eligible_stop_run(max_pr_leak = max_pr_leak) %>%
    arrange(pr_leak, desc(trust_baseline), desc(possessions))
  
  top <- eligible_remaining %>%
    select(lineup_key, lineup_pretty, possessions, sample_tier,
           trust_baseline, pr_leak, u_def_mean, u_def_p05, u_def_p95) %>%
    slice_head(n = top_n)
  
  lost <- df %>%
    filter(str_detect(lineup_key, fixed(player_name))) %>%
    eligible_stop_run(max_pr_leak = max_pr_leak) %>%
    arrange(pr_leak, desc(trust_baseline), desc(possessions)) %>%
    select(lineup_key, lineup_pretty, possessions, sample_tier,
           trust_baseline, pr_leak, u_def_mean, u_def_p05, u_def_p95)
  
  list(player_out = player_name,
       survivors_n = nrow(eligible_remaining),
       top = top,
       lost = lost)
}

# Scenario run
res <- stress_test_player_out(PLAYER_OUT, top_n = TOP_N, max_pr_leak = MAX_PR_LEAK)

top_rows <- res$top %>%
  mutate(section = "TOP_SURVIVOR", rank = row_number())

lost_rows <- res$lost %>%
  mutate(section = "LOST_OPTION", rank = row_number())

summary_row <- tibble(
  lineup_key = NA_character_,
  lineup_pretty = NA_character_,
  possessions = NA_real_,
  sample_tier = NA_character_,
  trust_baseline = NA_real_,
  pr_leak = NA_real_,
  u_def_mean = NA_real_,
  u_def_p05 = NA_real_,
  u_def_p95 = NA_real_,
  section = "SUMMARY",
  rank = NA_integer_
)

report <- bind_rows(summary_row, top_rows, lost_rows) %>%
  mutate(
    scenario_player_out = res$player_out,
    top_n_requested = TOP_N,
    max_pr_leak = MAX_PR_LEAK,
    survivors_n = res$survivors_n,
    lost_n = nrow(res$lost),
    generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
  ) %>%
  select(
    generated_at_utc,
    scenario_player_out,
    top_n_requested,
    max_pr_leak,
    survivors_n,
    lost_n,
    section,
    rank,
    lineup_key,
    lineup_pretty,
    possessions,
    sample_tier,
    trust_baseline,
    pr_leak,
    u_def_mean,
    u_def_p05,
    u_def_p95
  )

write_csv(report, OUT_PATH)

cat("\nPLAYER OUT:", res$player_out, "\n")
cat("ELIGIBLE STOP-A-RUN SURVIVORS:", res$survivors_n, "\n\n")
cat("TOP SURVIVORS:\n")
print(res$top)
cat("\nELIGIBLE OPTIONS YOU LOSE:\n")
print(res$lost)
cat("\nWROTE:", OUT_PATH, "\n")
