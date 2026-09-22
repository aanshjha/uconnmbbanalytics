#!/usr/bin/env bash
set -euo pipefail

# Compatibility entrypoint: the current workflow owns its options and help.
cd "$(dirname "$0")"
bash run_coaching_pipeline.sh "$@"
