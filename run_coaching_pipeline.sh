#!/usr/bin/env bash
set -euo pipefail

# Simple entry point for the full coaching workflow.
cd "$(dirname "$0")"
Rscript --vanilla _scripts/pipeline/run_coaching_pipeline.R "$@"
