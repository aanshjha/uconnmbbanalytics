#!/usr/bin/env python3
"""Audit source-linked lineups and bounded possessions without repairing old stints.

This is an evidence inventory, not a lineup-model input or recommendation file.
Events at substitution clocks and states after invalid substitutions are excluded.
Only possessions with explicit control changes and stable lineups are retained.
"""

from __future__ import annotations

import argparse
import collections
import json
from pathlib import Path
import re

import reconcile_source_data as source

ROOT = Path(__file__).resolve().parents[2]
OUTPUT = ROOT / "_outputs/09_lineup_source_audit"
IDENTITY = ("game_file", "period", "stint_index", "start_time", "end_time")
IMPUTATION_REASONS = ("poss_est_invalid_repaired_from_points", "poss_est_invalid_defaulted_to_1")


def seconds(clock):
    minute, second = clock.split(":")
    return 60 * int(minute) + float(second)


def period_number(label):
    if label.startswith("1st Half"):
        return 1
    if label.startswith("2nd Half"):
        return 2
    match = re.match(r"OT\s+(\d+)", label)
    if match:
        return 2 + int(match.group(1))
    raise ValueError(f"Unrecognized core stint period: {label!r}")


def starters_and_names(source_data):
    starters, names = {}, {}
    for team in source_data["boxscore"]["players"]:
        team_id = str(team["team"]["id"])
        athletes = [athlete for group in team["statistics"] for athlete in group["athletes"]]
        starters[team_id] = {
            str(row["athlete"]["id"]) for row in athletes if row.get("starter")
        }
        for row in athletes:
            athlete_id = str(row["athlete"]["id"])
            names[(team_id, athlete_id)] = row["athlete"].get("displayName", "")
    return starters, names


def participant_id(play):
    participants = play.get("participants") or []
    if len(participants) != 1:
        return ""
    return str(participants[0].get("athlete", {}).get("id", ""))


def substitution_direction(play):
    text = play.get("text", "").lower()
    if "subbing out" in text:
        return "out"
    if "subbing in" in text:
        return "in"
    return ""


def lineup_state_rows(game_id, source_data, canonical_events):
    """Track exact player IDs, invalidating a team after an unexplained change."""
    starters, names = starters_and_names(source_data)
    team_ids = {str(team["id"]) for team in source_data["header"]["competitions"][0]["competitors"]}
    opponent_id = next(team_id for team_id in team_ids if team_id != "41")
    if team_ids != set(starters) or any(len(starters[team_id]) != 5 for team_id in team_ids):
        raise ValueError(f"Missing or non-five starters for {game_id}")
    active = {team_id: set(starters[team_id]) for team_id in team_ids}
    raw_by_id = {str(play["id"]): play for play in source_data["plays"]}
    groups = collections.defaultdict(list)
    for event in canonical_events:
        groups[(event["period_number"], event["clock_display_value"])].append(event)
    state_rows, issues = [], []
    for (period, clock), events in groups.items():
        substitutions = [event for event in events if event["type_text"] == "Substitution"]
        for event in events:
            raw = raw_by_id[event["play_id"]]
            scorer = participant_id(raw) if event["points"] else ""
            team_id = event["team_id"]
            if not substitutions and scorer and team_id in active and active[team_id] is not None:
                if scorer not in active[team_id]:
                    issues.append({"game_id": game_id, "period_number": period, "clock": clock,
                                   "team_id": team_id, "play_ids": event["play_id"],
                                   "reason": "scorer_not_in_tracked_five"})
                    active[team_id] = None
            if substitutions:
                status = "same_clock_substitution"
            elif any(active[team_id] is None for team_id in team_ids):
                status = "unresolved_player_state"
            else:
                status = "source_consistent_five_player_state"
            state_rows.append(dict(event,
                lineup_state_status=status,
                uconn_lineup_ids="|".join(sorted(active["41"])) if status == "source_consistent_five_player_state" else "",
                opponent_lineup_ids="|".join(sorted(active[opponent_id])) if status == "source_consistent_five_player_state" else "",
            ))
        by_team = collections.defaultdict(list)
        for event in substitutions:
            by_team[event["team_id"]].append(event)
        for team_id, team_subs in by_team.items():
            if team_id not in active or active[team_id] is None:
                continue
            pairs = [(substitution_direction(raw_by_id[event["play_id"]]),
                      participant_id(raw_by_id[event["play_id"]])) for event in team_subs]
            outs = [athlete for direction, athlete in pairs if direction == "out"]
            ins = [athlete for direction, athlete in pairs if direction == "in"]
            remaining = active[team_id] - set(outs)
            if (any(not direction or not athlete for direction, athlete in pairs)
                    or len(outs) != len(set(outs)) or len(ins) != len(set(ins))
                    or not set(outs) <= active[team_id]
                    or bool(set(ins) & remaining)
                    or len(remaining | set(ins)) != 5):
                issues.append({"game_id": game_id, "period_number": period, "clock": clock,
                               "team_id": team_id, "play_ids": "|".join(event["play_id"] for event in team_subs),
                               "reason": "invalid_substitution_group"})
                active[team_id] = None
            else:
                active[team_id] = remaining | set(ins)
    players = [{"game_id": game_id, "team_id": team_id, "athlete_id": athlete_id,
                "display_name": name, "starter": athlete_id in starters[team_id]}
               for (team_id, athlete_id), name in sorted(names.items())]
    return state_rows, issues, players


def is_action(event):
    return any(int(event[field]) for field in ("FGA", "FTA", "TOV", "OREB", "DREB"))


def is_foul_or_jump(event):
    return bool(re.search(r"foul|jump\s*ball", event["type_text"] + " " + event["text"], re.I))


def made_shot_has_control_change(events, index, offense_id):
    shot = events[index]
    for later in events[index + 1:]:
        if later["period_number"] != shot["period_number"]:
            return False
        if is_foul_or_jump(later) or int(later["FTA"]):
            return False
        if is_action(later):
            return later["team_id"] != offense_id and bool(int(later["FGA"]) or int(later["TOV"]))
    return False


def bounded_possessions(events, opponent_id):
    """Keep only explicit, single-lineup, no-foul/FT control sequences."""
    results, exclusions = [], []
    team_ids = ("41", opponent_id)
    for index, start in enumerate(events):
        for offense_id in team_ids:
            start_reason = ("offense_defensive_rebound" if start["team_id"] == offense_id and int(start["DREB"])
                            else "defense_turnover" if start["team_id"] != offense_id and int(start["TOV"])
                            else "")
            if not start_reason:
                continue
            reason, end_reason, sequence = "", "", [start]
            if start["lineup_state_status"] != "source_consistent_five_player_state":
                reason = "unresolved_start_lineup"
            else:
                for offset in range(index + 1, len(events)):
                    event = events[offset]
                    if event["period_number"] != start["period_number"]:
                        reason = "period_change"; break
                    elapsed = seconds(start["clock_display_value"]) - seconds(event["clock_display_value"])
                    if not 0 <= elapsed <= 120:
                        reason = "clock_or_duration"; break
                    if (event["lineup_state_status"] != "source_consistent_five_player_state"
                            or any(event[key] != start[key] for key in ("uconn_lineup_ids", "opponent_lineup_ids"))):
                        reason = "unresolved_or_changed_lineup"; break
                    if is_foul_or_jump(event) or int(event["FTA"]):
                        reason = "foul_free_throw_or_jump"; break
                    previous_actions = [row for row in sequence[1:] if is_action(row)]
                    pending_miss = bool(previous_actions and int(previous_actions[-1]["FGA"])
                                        and not int(previous_actions[-1]["FGM"]))
                    if pending_miss and is_action(event) and not (
                            event["team_id"] == offense_id and int(event["OREB"])
                            or event["team_id"] != offense_id and int(event["DREB"])):
                        reason = "missing_rebound_after_miss"; break
                    sequence.append(event)
                    if event["team_id"] == offense_id:
                        if int(event["TOV"]):
                            end_reason = "offense_turnover"; break
                        if int(event["OREB"]):
                            if not pending_miss:
                                reason = "unexpected_offensive_rebound"; break
                        elif int(event["FGM"]):
                            if made_shot_has_control_change(events, offset, offense_id):
                                end_reason = "made_field_goal_confirmed"; break
                            reason = "made_shot_control_unconfirmed"; break
                        elif int(event["DREB"]):
                            reason = "unexpected_defensive_rebound"; break
                    elif int(event["DREB"]):
                        if pending_miss:
                            end_reason = "defense_rebound_after_miss"; break
                        reason = "unexpected_defense_rebound"; break
                    elif int(event["FGA"]) or int(event["TOV"]) or int(event["OREB"]):
                        reason = "opponent_action_before_boundary"; break
                if not reason and not end_reason:
                    reason = "no_explicit_end"
            record = {"game_id": start["game_id"], "offense_team_id": offense_id,
                      "start_play_id": start["play_id"], "start_period": start["period_number"],
                      "start_clock": start["clock_display_value"], "start_reason": start_reason,
                      "end_play_id": sequence[-1]["play_id"] if end_reason else "",
                      "end_reason": end_reason, "event_ids": "|".join(row["play_id"] for row in sequence),
                      "uconn_lineup_ids": start["uconn_lineup_ids"] if end_reason else "",
                      "opponent_lineup_ids": start["opponent_lineup_ids"] if end_reason else "",
                      "points_scored": sum(int(row["PTS"]) for row in sequence if row["team_id"] == offense_id) if end_reason else "",
                      "source_sha256": start["source_sha256"],
                      "status": "source_bounded_not_film_verified" if end_reason else "excluded",
                      "exclusion_reason": reason}
            (results if end_reason else exclusions).append(record)
    return results, exclusions


def imputed_possession_keys(report_dir, stints):
    """Match old repair reports to the *current* denominator, as the R audit does."""
    by_identity = {tuple(row[field] for field in IDENTITY): row for row in stints}
    marked = set()
    reports = sorted(Path(report_dir).glob("uconn_stints_core_input_repair_report_*.csv"))
    for path in reports:
        for row in source.read_csv(path):
            if not any(reason in row.get("reason_codes", "") for reason in IMPUTATION_REASONS):
                continue
            identity = tuple(row.get(field, "") for field in IDENTITY)
            current = by_identity.get(identity)
            if current and row.get("new_poss_est") and current.get("poss_est"):
                if abs(float(row["new_poss_est"]) - float(current["poss_est"])) < 1e-9:
                    marked.add(identity)
    return marked, len(reports)


def audit_core_stints(stints, games_by_file, events_by_game, imputed_keys, imputation_known):
    rows = []
    for row in stints:
        identity = tuple(row[field] for field in IDENTITY)
        game = games_by_file.get(row["game_file"])
        start, end = seconds(row["start_time"]), seconds(row["end_time"])
        if start < end:
            raise ValueError(f"Reverse core stint clock: {identity}")
        matching = ([event for event in events_by_game[game["game_id"]]
                     if int(event["PTS"]) and event["period_number"] == period_number(row["period"])
                     and end <= seconds(event["clock_display_value"]) <= start] if game else [])
        record = {"game_file": row["game_file"], "game_id": game["game_id"] if game else "",
                  "period": row["period"], "stint_index": row["stint_index"],
                  "start_time": row["start_time"], "end_time": row["end_time"],
                  "legacy_poss_est": row["poss_est"],
                  "historical_imputed_possessions": identity in imputed_keys if imputation_known else "unknown",
                  "source_snapshot_available": bool(game)}
        for side in ("for", "against"):
            relevant = [event for event in matching if (event["team_id"] == "41") == (side == "for")]
            interior = sum(int(event["PTS"]) for event in relevant
                           if end < seconds(event["clock_display_value"]) < start)
            boundary = sum(int(event["PTS"]) for event in relevant
                           if seconds(event["clock_display_value"]) in (start, end))
            legacy = float(row["points_" + side])
            if not legacy.is_integer():
                raise ValueError(f"Noninteger core stint points: {identity}")
            legacy = int(legacy)
            record.update({"legacy_points_" + side: legacy,
                           "source_interior_points_" + side: interior if game else "",
                           "source_boundary_candidate_points_" + side: boundary if game else "",
                           "definite_points_mismatch_" + side: bool(game and not interior <= legacy <= interior + boundary)})
        record["status"] = ("no_source_snapshot" if not game else "definite_points_mismatch" if
                            record["definite_points_mismatch_for"] or record["definite_points_mismatch_against"]
                            else "within_bounds_only_not_lineup_verified")
        rows.append(record)
    return rows


def audit_core_games(stint_rows, games_by_file, events_by_game):
    by_file = collections.defaultdict(list)
    for row in stint_rows:
        by_file[row["game_file"]].append(row)
    results = []
    for game_file, rows in sorted(by_file.items()):
        game = games_by_file.get(game_file)
        scoring_events = ([event for event in events_by_game[game["game_id"]] if int(event["PTS"])]
                          if game else [])
        uncovered = [event for event in scoring_events if not any(
            period_number(stint["period"]) == event["period_number"] and
            seconds(stint["end_time"]) <= seconds(event["clock_display_value"]) <= seconds(stint["start_time"])
            for stint in rows)]
        legacy_for = sum(row["legacy_points_for"] for row in rows)
        legacy_against = sum(row["legacy_points_against"] for row in rows)
        source_for = int(game["uconn_points"]) if game else None
        source_against = int(game["opponent_points"]) if game else None
        results.append({"game_file": game_file, "game_id": game["game_id"] if game else "",
                        "stint_rows": len(rows), "legacy_points_for": legacy_for,
                        "legacy_points_against": legacy_against,
                        "source_points_for": source_for if game else "",
                        "source_points_against": source_against if game else "",
                        "legacy_minus_source_for": legacy_for - source_for if game else "",
                        "legacy_minus_source_against": legacy_against - source_against if game else "",
                        "score_totals_match": (legacy_for == source_for and legacy_against == source_against)
                        if game else "unknown",
                        "scoring_events_outside_all_stint_clocks": len(uncovered) if game else "unknown",
                        "definite_mismatch_stint_rows": sum(row["status"] == "definite_points_mismatch" for row in rows),
                        "historical_imputed_possession_rows": sum(row["historical_imputed_possessions"] is True
                                                               for row in rows) if rows[0]["historical_imputed_possessions"] != "unknown" else "unknown",
                        "status": "no_source_snapshot" if not game else
                                  "legacy_score_mismatch" if legacy_for != source_for or legacy_against != source_against
                                  else "legacy_score_totals_match_only"})
    return results


def write_rows(path, rows, fields):
    source.write_csv(path, rows, fields)


def run(root=ROOT, output_dir=OUTPUT):
    root, output_dir = Path(root), Path(output_dir)
    games = source.read_csv(root / "_data/02_derived_inputs/reconciled_games.csv")
    stints = source.read_csv(root / "_data/01_core_inputs/uconn_stints_from_pbp.csv")
    imputed_keys, report_count = imputed_possession_keys(root / "_outputs/00_qc", stints)
    games_by_file = {game["game_file"]: game for game in games}
    all_states, all_issues, all_players, all_possessions, all_exclusions = [], [], [], [], []
    events_by_game, game_rows = {}, []
    for game in games:
        game_id = game["game_id"]
        source_data, metadata = source.get_source(game_id, root / "_data/00_source/espn")
        rebuilt_game, events, comparisons, timeline_issues = source.canonical_game(
            game_id, game["game_file"], source_data, metadata)
        if (metadata["sha256"] != game["source_sha256"] or
                not rebuilt_game["score_reconciled"] or not rebuilt_game["event_stats_reconciled"] or
                str(rebuilt_game["score_timeline_reconciled"]) != game["score_timeline_reconciled"] or
                int(game["uconn_points"]) != rebuilt_game["uconn_points"] or
                int(game["opponent_points"]) != rebuilt_game["opponent_points"] or
                any(not comparison["matched"] for comparison in comparisons)):
            raise ValueError(f"Reconciled game differs from cached source: {game_id}")
        states, issues, players = lineup_state_rows(game_id, source_data, events)
        opponent_id = next(str(team["id"]) for team in source_data["header"]["competitions"][0]["competitors"]
                           if str(team["id"]) != "41")
        possessions, exclusions = bounded_possessions(states, opponent_id)
        for collection, values in ((all_states, states), (all_issues, issues), (all_players, players),
                                   (all_possessions, possessions), (all_exclusions, exclusions)):
            collection.extend(values)
        events_by_game[game_id] = events
        attributed = {team_id: sum(int(row["PTS"]) for row in states
                                   if row["team_id"] == team_id and row["lineup_state_status"] == "source_consistent_five_player_state")
                      for team_id in ("41", opponent_id)}
        excluded = {team_id: sum(int(row["PTS"]) for row in states
                                 if row["team_id"] == team_id and row["lineup_state_status"] != "source_consistent_five_player_state")
                    for team_id in ("41", opponent_id)}
        if (attributed["41"] + excluded["41"] != rebuilt_game["uconn_points"] or
                attributed[opponent_id] + excluded[opponent_id] != rebuilt_game["opponent_points"]):
            raise ValueError(f"Lineup accounting lost source points: {game_id}")
        game_rows.append({"game_id": game_id, "game_file": game["game_file"],
                          "uconn_final_points": rebuilt_game["uconn_points"],
                          "opponent_final_points": rebuilt_game["opponent_points"],
                          "uconn_points_with_source_consistent_lineups": attributed["41"],
                          "opponent_points_with_source_consistent_lineups": attributed[opponent_id],
                          "uconn_points_excluded": excluded["41"],
                          "opponent_points_excluded": excluded[opponent_id],
                          "substitution_issues": len(issues),
                          "source_bounded_possessions": len(possessions),
                          "possession_candidates_excluded": len(exclusions),
                          "source_score_reconciled": True,
                          "source_score_timeline_reconciled": rebuilt_game["score_timeline_reconciled"],
                          "score_timeline_issues": len(timeline_issues),
                          "lineup_model_eligible": False,
                          "reason": "Full possession coverage and same-clock lineup attribution remain unverified"})
    stint_rows = audit_core_stints(stints, games_by_file, events_by_game, imputed_keys, report_count > 0)
    core_game_rows = audit_core_games(stint_rows, games_by_file, events_by_game)
    output_dir.mkdir(parents=True, exist_ok=True)
    write_rows(output_dir / "source_lineup_events.csv", all_states,
               list(all_states[0]) if all_states else [])
    write_rows(output_dir / "source_players.csv", all_players,
               ["game_id", "team_id", "athlete_id", "display_name", "starter"])
    write_rows(output_dir / "substitution_issues.csv", all_issues,
               ["game_id", "period_number", "clock", "team_id", "play_ids", "reason"])
    possession_fields = ["game_id", "offense_team_id", "start_play_id", "start_period", "start_clock",
                         "start_reason", "end_play_id", "end_reason", "event_ids", "uconn_lineup_ids",
                         "opponent_lineup_ids", "points_scored", "source_sha256", "status", "exclusion_reason"]
    write_rows(output_dir / "bounded_possessions.csv", all_possessions, possession_fields)
    write_rows(output_dir / "excluded_possession_candidates.csv", all_exclusions, possession_fields)
    write_rows(output_dir / "core_stint_event_bounds.csv", stint_rows, list(stint_rows[0]))
    write_rows(output_dir / "core_game_discrepancies.csv", core_game_rows, list(core_game_rows[0]))
    write_rows(output_dir / "game_lineup_audit.csv", game_rows, list(game_rows[0]))
    summary = {"status": "source_evidence_audit_only_no_lineup_release",
               "games": len(games), "source_events": len(all_states),
               "source_bounded_possessions": len(all_possessions),
               "excluded_possession_candidates": len(all_exclusions),
               "substitution_issues": len(all_issues),
               "core_stints": len(stints), "definite_core_stint_point_mismatches": sum(
                   row["status"] == "definite_points_mismatch" for row in stint_rows),
               "core_games_with_score_mismatches": sum(row["status"] == "legacy_score_mismatch" for row in core_game_rows),
               "source_scoring_events_outside_stint_clocks": sum(
                   row["scoring_events_outside_all_stint_clocks"] for row in core_game_rows
                   if row["status"] != "no_source_snapshot"),
               "historically_imputed_possession_rows": len(imputed_keys) if report_count else None,
               "historical_repair_reports_read": report_count,
               "total_source_points": sum(row["uconn_final_points"] + row["opponent_final_points"] for row in game_rows),
               "points_with_source_consistent_lineups": sum(
                   row["uconn_points_with_source_consistent_lineups"] +
                   row["opponent_points_with_source_consistent_lineups"] for row in game_rows),
               "points_excluded_from_lineups": sum(row["uconn_points_excluded"] + row["opponent_points_excluded"] for row in game_rows),
               "games_with_score_timeline_issues": sum(not row["source_score_timeline_reconciled"] for row in game_rows),
               "games_with_zero_excluded_lineup_points": sum(
                   row["uconn_points_excluded"] == row["opponent_points_excluded"] == 0 for row in game_rows),
               "lineup_models_released": False,
               "limitations": [
                   "Source scores reconcile, but excluded points are not assigned to a lineup.",
                   "Any scoring event sharing a clock with a substitution is excluded from lineup attribution.",
                   "After an invalid substitution group, that team's lineup remains unknown; no player is guessed.",
                   "Bounded possessions exclude fouls, free throws, jumps, lineup changes and unclear control.",
                   "Bounded sequences are incomplete coverage, not a valid possessions denominator for lineup models.",
                   "Core stint clock bounds do not certify the five players or exact same-clock scoring attribution.",
                   "Some source score timelines have intermediate discrepancies despite matching final and box-score totals.",
                   "Historical imputation detection depends on locally retained repair reports."
               ]}
    (output_dir / "audit_summary.json").write_text(json.dumps(summary, indent=2) + "\n")
    return summary


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=ROOT)
    parser.add_argument("--output-dir", type=Path, default=OUTPUT)
    args = parser.parse_args()
    print(json.dumps(run(args.root, args.output_dir), indent=2))
