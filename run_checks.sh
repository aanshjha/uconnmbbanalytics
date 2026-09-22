#!/usr/bin/env bash
set -euo pipefail

real_data_mode=auto
case "${1:-}" in
  -h|--help)
    cat <<'USAGE'
Usage: bash run_checks.sh [--fixtures-only | --require-real-data]

Check shell, Python, R, Stan, and dashboard JavaScript syntax; run the Python
and R regression suites. If private source inputs are present, rebuild the
current workflow in a temporary directory and verify its reconciliation and
evaluation outputs without changing the project's saved data or human ledger.

  --fixtures-only     Run syntax and fixture checks without the real-data rebuild.
  --require-real-data Fail if the private inputs needed for the rebuild are absent.

Requires Python 3.9+, R with dplyr/readr/stringr/rstan, and Node.js.
See tests/README.md for input requirements and interpretation.
USAGE
    exit 0 ;;
  --fixtures-only) real_data_mode=skip ;;
  --require-real-data) real_data_mode=require ;;
  "") ;;
  *) printf 'Unknown option: %s\nUse bash run_checks.sh --help.\n' "$1" >&2; exit 2 ;;
esac
if (( $# > 1 )); then
  printf 'Only one option is supported. Use bash run_checks.sh --help.\n' >&2
  exit 2
fi

cd "$(dirname "$0")"
for required_command in python3 Rscript node; do
  if ! command -v "$required_command" >/dev/null 2>&1; then
    printf 'Required command not found: %s\nSee tests/README.md.\n' "$required_command" >&2
    exit 1
  fi
done

printf 'Checking source syntax...\n'
for script_path in ./*.sh; do bash -n "$script_path"; done
python3 - <<'PY'
import ast
from pathlib import Path
paths = sorted(Path('_scripts').rglob('*.py')) + sorted(Path('tests').rglob('*.py'))
for path in paths:
    ast.parse(path.read_text(encoding='utf-8'), filename=str(path))
print(f'Parsed {len(paths)} Python files')
PY
Rscript --vanilla -e 'paths <- list.files("_scripts", pattern="[.]R$", recursive=TRUE, full.names=TRUE); for (path in paths) parse(path); cat("Parsed", length(paths), "R files\n"); for (path in list.files("_models", pattern="[.]stan$", full.names=TRUE)) { result <- rstan::stanc(file=path); if (!isTRUE(result$status)) stop("Invalid Stan model: ", path) }; cat("Parsed Stan models\n")'
node --check staff-dashboard/dist/app.js

printf '\nRunning Python regression tests...\n'
python3 -m unittest discover -s tests -p 'test_*.py' -v
printf '\nRunning R source-input guards...\n'
Rscript --vanilla _scripts/tests/test_source_input_guards.R

if [[ "$real_data_mode" == skip ]]; then
  printf '\nReal-data rebuild skipped by --fixtures-only.\n'
else
  required_inputs=(
    _data/00_source/espn
    _data/01_core_inputs/uconn_games_meta.csv
    _data/01_core_inputs/uconn_stints_from_pbp.csv
    _data/03_manual_game_csv
  )
  missing_inputs=()
  for input_path in "${required_inputs[@]}"; do
    if [[ ! -e "$input_path" ]]; then missing_inputs+=("$input_path"); fi
  done
  if (( ${#missing_inputs[@]} > 0 )); then
    if [[ "$real_data_mode" == require ]]; then
      printf 'Private inputs required for real-data rebuild are missing:\n' >&2
      printf '  %s\n' "${missing_inputs[@]}" >&2
      exit 1
    fi
    printf '\nReal-data rebuild skipped: private inputs are absent.\n'
    printf '  Missing: %s\n' "${missing_inputs[*]}"
  else
    scratch_dir="$(mktemp -d "${TMPDIR:-/tmp}/uconn-checks.XXXXXX")"
    trap 'rm -rf "$scratch_dir"' EXIT
    mkdir -p "$scratch_dir/_data/00_source" "$scratch_dir/_data/01_core_inputs" "$scratch_dir/_data/03_manual_game_csv"
    cp -R _scripts "$scratch_dir/_scripts"
    cp run_coaching_pipeline.sh "$scratch_dir/"
    cp -R _data/00_source/espn "$scratch_dir/_data/00_source/"
    cp _data/01_core_inputs/uconn_games_meta.csv _data/01_core_inputs/uconn_stints_from_pbp.csv "$scratch_dir/_data/01_core_inputs/"
    cp -R _data/03_manual_game_csv/. "$scratch_dir/_data/03_manual_game_csv/"
    printf '\nRebuilding from cached private source data in an isolated directory...\n'
    (cd "$scratch_dir" && bash run_coaching_pipeline.sh)
    python3 - "$scratch_dir" <<'PY'
import csv
import json
import math
from pathlib import Path
import sys

root = Path(sys.argv[1])
summary = json.loads((root / '_outputs/00_qc/reconciliation_summary.json').read_text())
evaluation = json.loads((root / '_outputs/08_reconciled_evaluation/pregame_manifest.json').read_text())
pilot = json.loads((root / '_outputs/08_staff_pilot/human_pilot_summary.json').read_text())
assert summary['games'] > 0 and summary['canonical_source_events'] > 0
assert summary['score_reconciled_games'] == summary['games']
assert summary['event_stats_reconciled_games'] == summary['games']
assert summary['stat_checks'] == 20 * summary['games']
assert summary['stat_mismatches'] == 0
assert evaluation['test_games'] > 0
assert all(math.isfinite(row['mae_points']) for row in evaluation['metrics'])
assert pilot['completed_sessions'] == 0  # The isolated run must not invent human results.
with (root / '_data/02_derived_inputs/reconciled_games.csv').open(newline='') as handle:
    assert len(list(csv.DictReader(handle))) == summary['games']
print(f"Real-data rebuild verified: {summary['games']} games, {summary['stat_checks']} stat checks, {evaluation['test_games']} forecast tests")
PY
  fi
fi

printf '\nAll requested checks passed. Syntax/tests validate software; lineup attribution and staff usefulness remain unverified.\n'
