#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"
Rscript --vanilla _scripts/dashboard/run_dashboard.R "$@"
