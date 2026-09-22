#!/usr/bin/env bash
set -euo pipefail

case "${1:-}" in
  -h|--help)
    cat <<'USAGE'
Usage: bash run_dashboard.sh

Open the historical R Shiny dashboard using existing analytical outputs.
Its lineup and model views are retained for audit, not current validated
staff recommendations. This is separate from the current defensive review.

Environment options:
  DASH_OUTPUT_DIR  Output folder to read (default: _outputs).
  DASH_HOST        Interface to bind (default: 127.0.0.1).
  DASH_PORT        Port to serve (default: 3838).

Requires R and the shiny, dplyr, readr, and DT packages.
See docs/STAFF_DASHBOARD.md for current dashboard status.
USAGE
    exit 0
    ;;
esac

cd "$(dirname "$0")"
Rscript --vanilla _scripts/dashboard/run_dashboard.R "$@"
