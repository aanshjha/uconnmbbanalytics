#!/usr/bin/env bash
set -euo pipefail

# Current workflow: reconcile sources, evaluate pregame forecasts, build a review.
for argument in "$@"; do
  case "$argument" in
    -h|--help)
      cat <<'USAGE'
Usage: bash run_coaching_pipeline.sh [--fetch] [--refresh]

Run the current source-checked workflow:
  1. Reconcile game totals and events against cached ESPN source snapshots.
  2. Evaluate pregame forecasts against simple historical baselines.
  3. Build the UConn–Florida defensive review (game 401812793).
  4. Summarize recorded human pilot sessions.

Options:
  --fetch    Retrieve missing source snapshots.
  --refresh  Replace cached source snapshots explicitly.
  -h, --help Show this help without running the workflow.

The default run uses local data. It rebuilds generated outputs; it does not
start a human trial, record human results, or publish the dashboard.
See docs/RELIABILITY_RESET.md and docs/STAFF_PILOT.md.
USAGE
      exit 0
      ;;
  esac
done

# Source reconciliation is required before evaluation or staff review.
cd "$(dirname "$0")"
python3 _scripts/ops/reconcile_source_data.py "$@"
python3 _scripts/analysis/evaluate_pregame_defense.py
python3 _scripts/analysis/postgame_defensive_review.py build --game-id 401812793
python3 _scripts/analysis/postgame_defensive_review.py summary
