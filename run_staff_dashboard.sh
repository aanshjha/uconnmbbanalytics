#!/usr/bin/env bash
set -euo pipefail

case "${1:-}" in
  -h|--help)
    cat <<'USAGE'
Usage: bash run_staff_dashboard.sh [port]

Historical staff dashboard launcher (currently suspended).
The exporter deliberately stops because legacy lineup recommendations are
not source-verified. This command does not bypass that release gate.

If that exporter is replaced by a verified workflow in the future, the
launcher serves its output locally at 127.0.0.1 (default port: 4173).
For the current defensive review, see docs/STAFF_PILOT.md.
For dashboard status, see docs/STAFF_DASHBOARD.md.
USAGE
    exit 0
    ;;
esac

cd "$(dirname "$0")"
dashboard_port="${1:-4173}"
python3 _scripts/dashboard/export_staff_dashboard.py
python3 -m http.server "$dashboard_port" --bind 127.0.0.1 --directory staff-dashboard/dist
