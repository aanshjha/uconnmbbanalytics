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

# Availability stress test: remove one player at a time and surface the best
# remaining lineup options under current risk thresholds.
library(dplyr)
library(readr)

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
players <- read_csv(resolve_path(file.path("03_players", "uconn_player_rci_coach_table.csv")), show_col_types = FALSE)

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

if (!"player" %in% names(players)) {
  stop("uconn_player_rci_coach_table.csv is missing columns: player")
}

cat("Share missing pr_leak after join:", mean(is.na(df$pr_leak)), "\n")

# Config (env overrides make this script reproducible in automation/CLI)
PLAYER_OUT_OVERRIDE <- trimws(Sys.getenv("STRESS_TEST_PLAYER_OUT", ""))
TOP_N <- suppressWarnings(as.integer(Sys.getenv("STRESS_TOP_N", "3")))
MAX_PR_LEAK <- suppressWarnings(as.numeric(Sys.getenv("STRESS_MAX_PR_LEAK", "0.50")))
OUT_DIR <- file.path("_outputs", "05_decision_audit")
OUT_PATH <- file.path(OUT_DIR, "uconn_availability_stress_test_report.csv")

if (!is.finite(TOP_N) || TOP_N < 1) TOP_N <- 3L
if (!is.finite(MAX_PR_LEAK) || MAX_PR_LEAK < 0 || MAX_PR_LEAK > 1) MAX_PR_LEAK <- 0.50
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

normalize_player_token <- function(x) {
  x <- trimws(as.character(x))
  x <- tolower(gsub("[^[:alnum:]]", "", x, perl = TRUE))
  x
}

lineup_has_player <- function(lineup_keys, player_name) {
  player_norm <- normalize_player_token(player_name)
  if (!nzchar(player_norm)) return(rep(FALSE, length(lineup_keys)))

  vapply(
    lineup_keys,
    function(key) {
      key <- as.character(key)
      if (is.na(key) || !nzchar(key)) return(FALSE)
      tokens <- trimws(unlist(strsplit(key, "\\|", perl = TRUE)))
      any(normalize_player_token(tokens) == player_norm)
    },
    logical(1)
  )
}

player_pool <- players %>%
  transmute(player = trimws(as.character(.data$player))) %>%
  filter(!is.na(.data$player), .data$player != "") %>%
  distinct() %>%
  arrange(.data$player) %>%
  pull(.data$player)

if (length(player_pool) == 0) {
  stop("No players available in uconn_player_rci_coach_table.csv for scenario generation.")
}

scenario_players <- if (nzchar(PLAYER_OUT_OVERRIDE)) PLAYER_OUT_OVERRIDE else player_pool

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
  contains_player <- lineup_has_player(df$lineup_key, player_name)

  remaining <- df %>%
    filter(!contains_player)
  
  eligible_remaining <- remaining %>%
    eligible_stop_run(max_pr_leak = max_pr_leak) %>%
    arrange(pr_leak, desc(trust_baseline), desc(possessions))
  
  top <- eligible_remaining %>%
    select(lineup_key, lineup_pretty, possessions, sample_tier,
           trust_baseline, pr_leak, u_def_mean, u_def_p05, u_def_p95) %>%
    slice_head(n = top_n)
  
  lost <- df %>%
    filter(contains_player) %>%
    eligible_stop_run(max_pr_leak = max_pr_leak) %>%
    arrange(pr_leak, desc(trust_baseline), desc(possessions)) %>%
    select(lineup_key, lineup_pretty, possessions, sample_tier,
           trust_baseline, pr_leak, u_def_mean, u_def_p05, u_def_p95)
  
  list(player_out = player_name,
       survivors_n = nrow(eligible_remaining),
       top = top,
       lost = lost)
}

# Scenario run (all players by default, one player when override is supplied)
results <- lapply(scenario_players, function(player_name) {
  stress_test_player_out(player_name, top_n = TOP_N, max_pr_leak = MAX_PR_LEAK)
})

generated_at_utc <- format(Sys.time(), tz = "UTC", usetz = TRUE)

build_report_rows <- function(res) {
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

  bind_rows(summary_row, top_rows, lost_rows) %>%
    mutate(
      generated_at_utc = generated_at_utc,
      scenario_player_out = res$player_out,
      top_n_requested = TOP_N,
      max_pr_leak = MAX_PR_LEAK,
      survivors_n = res$survivors_n,
      lost_n = nrow(res$lost)
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
}

report <- bind_rows(lapply(results, build_report_rows)) %>%
  mutate(
    section_order = dplyr::case_when(
      .data$section == "SUMMARY" ~ 1L,
      .data$section == "TOP_SURVIVOR" ~ 2L,
      .data$section == "LOST_OPTION" ~ 3L,
      TRUE ~ 99L
    )
  ) %>%
  arrange(.data$scenario_player_out, .data$section_order, .data$rank) %>%
  select(-section_order)

write_csv(report, OUT_PATH)

summary_tbl <- tibble(
  scenario_player_out = vapply(results, `[[`, character(1), "player_out"),
  survivors_n = vapply(results, `[[`, numeric(1), "survivors_n"),
  lost_n = vapply(results, function(x) nrow(x$lost), numeric(1))
)

cat("\nSCENARIOS RUN:", length(results), "\n")
print(summary_tbl)
cat("\nWROTE:", OUT_PATH, "\n")
