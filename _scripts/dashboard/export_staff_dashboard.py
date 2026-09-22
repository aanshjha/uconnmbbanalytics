#!/usr/bin/env python3
"""Export selected coaching outputs to an atomic, self-contained website snapshot.

No third-party packages or model fitting required. Run from any directory.
"""
import argparse
import csv
from datetime import datetime, timezone
import hashlib
import json
import math
from pathlib import Path
import re
import tempfile

ROOT = Path(__file__).resolve().parents[2]
SOURCES = {
    'lineups': ('01_lineup_core/uconn_lineup_coach_view.csv', ['lineup_pretty', 'possessions', 'games', 'raw_net_ppp', 'sample_flag', 'decision_label']),
    'defense': ('02_defense_leaks/uconn_lineup_def_leaks_coach_table.csv', ['lineup_pretty', 'pr_leak', 'confidence', 'leak_flag']),
    'trend': ('02_defense_leaks/uconn_game_level_def_leak_trend.csv', ['game_file', 'game_date', 'def_pts_40_mean', 'def_pts_40_p05', 'def_pts_40_p95']),
    'players': ('03_players/uconn_player_rci_coach_table.csv', ['player', 'total_possessions', 'RCI', 'net_mean', 'net_p05', 'net_p95']),
    'creation': ('03_players/uconn_player_creation_profile.csv', ['player', 'fga', 'assists_recorded']),
    'roles': ('03_players/uconn_player_rci_three_windows.csv', ['player', 'rci_phase1', 'rci_phase2', 'rci_phase3']),
    'availability': ('05_decision_audit/uconn_availability_stress_test_report.csv', ['scenario_player_out', 'section', 'lineup_pretty', 'max_pr_leak']),
    'calibration': ('05_decision_audit/uconn_pred_pr_net_pos_calibration_metrics.csv', ['metric', 'value']),
    'calibrationModel': ('05_decision_audit/uconn_pred_pr_net_pos_calibration_model.csv', ['mode', 'status']),
    'thresholds': ('05_decision_audit/uconn_lineup_decision_rule_v2_thresholds.csv', ['metric', 'value']),
    'defenseValidation': ('02_defense_leaks/uconn_def_leaks_holdout_validation_meta.csv', ['metric', 'value']),
    'scouts': ('07_opps/manual_game_scouts/manual_game_scout_manifest.csv', ['game_file', 'game_date', 'opponent', 'meeting_site', 'report_summary_path']),
}


def utc(timestamp):
    return datetime.fromtimestamp(timestamp, timezone.utc).isoformat()


def scalar(value):
    if value is None or value.strip() in ('', 'NA', 'NaN', 'Inf', '-Inf'):
        return None
    if value in ('TRUE', 'FALSE'):
        return value == 'TRUE'
    if re.fullmatch(r'-?\d+(\.\d+)?([eE][+-]?\d+)?', value):
        number = float(value)
        return (int(number) if number.is_integer() else number) if math.isfinite(number) else None
    return value


def key(name):
    return ''.join(sorted(re.findall(r'[a-z0-9]+', name.lower())))


def export(destination):
    # A schema-valid historical export is not a source-validated recommendation.
    raise RuntimeError(
        'Historical lineup/dashboard publication is suspended: source stint attribution '
        'and the legacy backtest are unverified. Run bash run_coaching_pipeline.sh and '
        'use the reconciled postgame review under _outputs/08_staff_pilot. '
        'See docs/RELIABILITY_RESET.md.'
    )


def export_historical_snapshot_unreleased(destination):
    """Preserved implementation for audit; intentionally not exposed by the CLI."""
    data, sources = {}, []
    for name, (relative, required) in SOURCES.items():
        path = ROOT / '_outputs' / relative
        raw = path.read_bytes()
        with path.open(encoding='utf-8-sig', newline='') as handle:
            reader = csv.DictReader(handle)
            missing = set(required) - set(reader.fieldnames or [])
            if missing:
                raise ValueError(f'{path.name}: missing columns {sorted(missing)}')
            data[name] = [{k: scalar(v) for k, v in row.items()} for row in reader]
        if not data[name]:
            raise ValueError(f'{path.name}: no data rows; previous snapshot retained')
        sources.append({'dataset': name, 'path': '_outputs/' + relative, 'rows': len(data[name]),
                        'updatedAt': utc(path.stat().st_mtime), 'sha256': hashlib.sha256(raw).hexdigest()})

    names = {key(row['player']): row['player'] for row in data['creation']}
    def display(name):
        return names.get(key(name), name)
    for dataset in ['lineups', 'defense', 'availability']:
        for row in data[dataset]:
            lineup = row.get('lineup_pretty')
            row['players'] = [display(p.strip()) for p in lineup.split('|')] if lineup else []
            if row.get('scenario_player_out'):
                row['playerOut'] = display(row['scenario_player_out'])
    for dataset in ['players', 'roles', 'creation']:
        for row in data[dataset]:
            row['name'] = display(row['player'])
    for dataset in ['calibration', 'thresholds', 'defenseValidation']:
        data[dataset] = {row['metric']: scalar(str(row['value'])) for row in data[dataset]}

    metadata = {}
    with (ROOT / '_data/01_core_inputs/uconn_games_meta.csv').open(newline='') as handle:
        for row in csv.DictReader(handle):
            metadata[row['game_file']] = row
    for row in data['trend']:
        row['opponent'] = metadata.get(row['game_file'], {}).get('opponent') or row['game_file'].removesuffix('.pdf')
    data['trend'].sort(key=lambda row: row['game_date'])

    # Reports are a library, not official box scores. Keep summary prose but omit
    # local artifact paths and expose duplicate game-key counts for review.
    for row in data['scouts']:
        summary = (ROOT / row['report_summary_path']).resolve()
        if not summary.is_relative_to((ROOT / '_outputs').resolve()):
            raise ValueError('Scout report path escaped _outputs')
        row['summary'] = summary.read_text().split('## Files')[0].strip()
        row.pop('output_dir', None)
    data['scouts'].sort(key=lambda row: (row['game_date'], row['opponent']), reverse=True)

    qc_files = sorted((ROOT / '_outputs/_run_logs').glob('*_qc_postflight.txt'))
    qc = {}
    if qc_files:
        qc = dict(line.split('=', 1) for line in qc_files[-1].read_text().splitlines() if '=' in line)
        qc = {k: qc[k] for k in ['timestamp', 'result', 'tuning_status', 'v4_safe_fallback', 'manual_scout_release_qc'] if k in qc}
    data['meta'] = {'schemaVersion': 1, 'exportedAt': datetime.now(timezone.utc).isoformat(),
                    'analysisUpdatedAt': max(source['updatedAt'] for source in sources),
                    'startDate': data['trend'][0]['game_date'], 'throughDate': data['trend'][-1]['game_date'],
                    'qc': qc, 'sources': sources}
    destination.parent.mkdir(parents=True, exist_ok=True)
    payload = json.dumps(data, allow_nan=False, ensure_ascii=False, separators=(',', ':'))
    with tempfile.NamedTemporaryFile(mode='w', encoding='utf-8', dir=destination.parent, delete=False) as handle:
        handle.write(payload)
        temporary = Path(handle.name)
    temporary.replace(destination)
    print(f'Dashboard snapshot refreshed: {len(data["lineups"])} lineups, {len(data["players"])} players; games through {data["meta"]["throughDate"]}.')


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--output', type=Path, default=ROOT / 'staff-dashboard/dist/data.json')
    export(parser.parse_args().output.resolve())
