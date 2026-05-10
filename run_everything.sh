#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"
Rscript --vanilla _scripts/ops/run_runnable_entrypoints.R "$@"
