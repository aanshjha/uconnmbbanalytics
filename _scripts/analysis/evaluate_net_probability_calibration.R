stop(
  "Legacy calibration is not releasable: the scored target and future-informed decision score were incompatible. ",
  "Run python3 _scripts/analysis/evaluate_pregame_defense.py; see docs/PREGAME_EVALUATION.md.",
  call. = FALSE
)

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

# Probability reliability diagnostics for lineup holdout predictions.
# Uses rolling holdout rows and evaluates V3 survive score vs observed_survive4.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
})


resolve_path <- function(fname) {
  candidates <- c(
    file.path(".", fname),
    file.path("_outputs", fname),
    file.path("_outputs", "05_decision_audit", fname)
  )
  hit <- candidates[file.exists(candidates)]
  if (length(hit) == 0) {
    stop(
      "Missing file: ", fname, "\n",
      "Searched:\n  - ", paste(candidates, collapse = "\n  - ")
    )
  }
  hit[[1]]
}

weighted_mean_safe <- function(x, w) {
  x <- as.numeric(x)
  w <- as.numeric(w)
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

clamp_prob <- function(p, eps = 1e-6) {
  p <- as.numeric(p)
  p[!is.finite(p)] <- NA_real_
  pmax(eps, pmin(1 - eps, p))
}

coerce_bool_num <- function(x) {
  if (is.logical(x)) return(as.numeric(x))
  y <- tolower(trimws(as.character(x)))
  out <- rep(NA_real_, length(y))
  out[y %in% c("true", "t", "1", "yes", "y")] <- 1
  out[y %in% c("false", "f", "0", "no", "n")] <- 0
  out
}

rows_path <- resolve_path("uconn_lineup_decision_rolling_backtest_rows.csv")
out_dir <- file.path("_outputs", "05_decision_audit")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

out_deciles <- file.path(out_dir, "uconn_pred_pr_net_pos_calibration_deciles.csv")
out_metrics <- file.path(out_dir, "uconn_pred_pr_net_pos_calibration_metrics.csv")
out_plot <- file.path(out_dir, "uconn_pred_pr_net_pos_calibration_curve.png")

rows <- read_csv(rows_path, show_col_types = FALSE)

required_cols <- c(
  "holdout_game_id",
  "holdout_game_file",
  "observed_survive4",
  "holdout_possessions"
)
missing_cols <- setdiff(required_cols, names(rows))
if (length(missing_cols) > 0) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
}

prob_col <- if ("decision_survive_score_robust" %in% names(rows)) {
  "decision_survive_score_robust"
} else if ("decision_survive_score_raw" %in% names(rows)) {
  "decision_survive_score_raw"
} else {
  "pred_pr_net_pos_for_decision"
}
if (!(prob_col %in% names(rows))) {
  stop("Missing required probability column. Expected `decision_survive_score_robust` or `decision_survive_score_raw`.")
}

rows2 <- rows %>%
  mutate(
    pred_pr_net_pos = as.numeric(.data[[prob_col]]),
    observed_net_positive_num = as.numeric(observed_survive4),
    holdout_possessions = as.numeric(holdout_possessions)
  )

n_total_rows <- nrow(rows2)

dat <- rows2 %>%
  filter(
    is.finite(pred_pr_net_pos),
    is.finite(observed_net_positive_num),
    pred_pr_net_pos >= 0,
    pred_pr_net_pos <= 1
  ) %>%
  mutate(
    w = ifelse(is.finite(holdout_possessions) & holdout_possessions > 0, holdout_possessions, 1),
    p_clamped = clamp_prob(pred_pr_net_pos)
  )

if (nrow(dat) < 20) {
  stop("Too few usable rows for calibration diagnostics (N = ", nrow(dat), ").")
}

n_bins <- min(10L, nrow(dat))

dat <- dat %>%
  arrange(pred_pr_net_pos, holdout_game_id, holdout_game_file) %>%
  mutate(decile = dplyr::ntile(pred_pr_net_pos, n_bins))

deciles <- dat %>%
  group_by(decile) %>%
  summarise(
    n_rows = n(),
    n_games = n_distinct(holdout_game_id),
    total_holdout_possessions = sum(w, na.rm = TRUE),
    pred_min = min(pred_pr_net_pos, na.rm = TRUE),
    pred_max = max(pred_pr_net_pos, na.rm = TRUE),
    mean_pred_pr_net_pos = mean(pred_pr_net_pos, na.rm = TRUE),
    weighted_pred_pr_net_pos = weighted_mean_safe(pred_pr_net_pos, w),
    observed_positive_rate = mean(observed_net_positive_num, na.rm = TRUE),
    weighted_observed_positive_rate = weighted_mean_safe(observed_net_positive_num, w),
    calibration_gap = observed_positive_rate - mean_pred_pr_net_pos,
    weighted_calibration_gap = weighted_observed_positive_rate - weighted_pred_pr_net_pos,
    .groups = "drop"
  ) %>%
  arrange(decile)

metrics <- tibble(
  metric = c(
    "source_file",
    "probability_column",
    "outcome_column",
    "n_total_rows_input",
    "n_rows_used",
    "n_rows_dropped",
    "n_games_used",
    "sum_holdout_possessions",
    "mean_pred_pr_net_pos",
    "weighted_mean_pred_pr_net_pos",
    "observed_positive_rate",
    "weighted_observed_positive_rate",
    "brier_score",
    "weighted_brier_score",
    "log_loss",
    "weighted_log_loss",
    "ece_decile",
    "weighted_ece_decile",
    "max_abs_decile_gap",
    "max_abs_weighted_decile_gap"
  ),
  value = c(
    basename(rows_path),
    prob_col,
    "observed_survive4",
    as.character(n_total_rows),
    as.character(nrow(dat)),
    as.character(n_total_rows - nrow(dat)),
    as.character(n_distinct(dat$holdout_game_id)),
    as.character(sum(dat$w, na.rm = TRUE)),
    as.character(mean(dat$pred_pr_net_pos, na.rm = TRUE)),
    as.character(weighted_mean_safe(dat$pred_pr_net_pos, dat$w)),
    as.character(mean(dat$observed_net_positive_num, na.rm = TRUE)),
    as.character(weighted_mean_safe(dat$observed_net_positive_num, dat$w)),
    as.character(mean((dat$observed_net_positive_num - dat$pred_pr_net_pos)^2, na.rm = TRUE)),
    as.character(weighted_mean_safe((dat$observed_net_positive_num - dat$pred_pr_net_pos)^2, dat$w)),
    as.character(-mean(
      dat$observed_net_positive_num * log(dat$p_clamped) +
      (1 - dat$observed_net_positive_num) * log(1 - dat$p_clamped),
      na.rm = TRUE
    )),
    as.character(weighted_mean_safe(
      -(
        dat$observed_net_positive_num * log(dat$p_clamped) +
        (1 - dat$observed_net_positive_num) * log(1 - dat$p_clamped)
      ),
      dat$w
    )),
    as.character(
      sum(abs(deciles$calibration_gap) * deciles$n_rows, na.rm = TRUE) /
      sum(deciles$n_rows, na.rm = TRUE)
    ),
    as.character(
      sum(abs(deciles$weighted_calibration_gap) * deciles$total_holdout_possessions, na.rm = TRUE) /
      sum(deciles$total_holdout_possessions, na.rm = TRUE)
    ),
    as.character(max(abs(deciles$calibration_gap), na.rm = TRUE)),
    as.character(max(abs(deciles$weighted_calibration_gap), na.rm = TRUE))
  )
)

write_csv(deciles, out_deciles)
write_csv(metrics, out_metrics)

plot_df <- deciles %>%
  mutate(
    decile_label = paste0("D", decile),
    point_size = pmax(total_holdout_possessions, 1)
  )

p <- ggplot(plot_df, aes(x = weighted_pred_pr_net_pos, y = weighted_observed_positive_rate)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40") +
  geom_line(color = "steelblue4", linewidth = 0.9, alpha = 0.9) +
  geom_point(aes(size = point_size), color = "steelblue4", alpha = 0.9) +
  geom_text(aes(label = decile_label), nudge_y = 0.02, size = 3, check_overlap = TRUE) +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_size_continuous(name = "Holdout possessions") +
  coord_equal() +
  labs(
    title = "UConn Lineup Reliability: Net-Positive Probability Calibration",
    subtitle = "Rolling holdout backtest deciles (weighted by holdout possessions)",
    x = paste0("Predicted probability (`", prob_col, "`)"),
    y = "Observed net-positive rate"
  ) +
  theme_minimal(base_size = 13)

ggsave(out_plot, p, width = 7.5, height = 6, dpi = 300)

message("Wrote:")
message(" - ", normalizePath(out_deciles))
message(" - ", normalizePath(out_metrics))
message(" - ", normalizePath(out_plot))
