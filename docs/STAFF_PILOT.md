# Staff Defensive Review Pilot

[Project home](../README.md) / [Documentation](README.md) / Staff pilot

The narrow workflow is preparing **at most three defensive film-review items** after one game: opponent threes, logged layup/dunk/tip shots, and turnovers. Each item needs a specific possession and source play ID. Zero useful items is a valid outcome. The generated packet organizes source events for review; it does not grade defense, diagnose coverage, assign responsibility or recommend rotations.

The demonstration packet covers UConn–Florida, December 9, 2025 (ESPN game `401812793`), final 77–73. No staff participant has completed a trial. Source-event counts and machine generation timing are measured; staff time saved, film confirmation and usefulness remain unmeasured.

## Assigned Trial

The project user volunteered to run the first comparison. The assignment is saved in `_outputs/08_staff_pilot/trial_assignment.json`; its status is **prepared, not started**.

| Order | Condition | Game | Packet |
|---|---|---|---|
| 1 | Baseline: chronological source log | Butler home, `401822868` | `_outputs/08_staff_pilot/401822868/baseline_packet.html` |
| 2 | Report: prepared defensive review | DePaul home, `401822893` | Open the assigned packet only when the report condition starts |

Both games are reconciled home games with 60 opponent points. Allow up to five minutes per condition to identify at most three possessions worth reviewing. If the reviewer reaches the cap, compare correct/useful items at the fixed time; do not claim a completion-time saving. Game difficulty and baseline-first order remain confounders.

The timer begins only when the user says **“start baseline.”** Keep the assigned packets unchanged before and during the trial, and show only the current condition. Florida is a demonstration because the user has already seen its report.

## Build and Inspect Packets

From the repository root, using Python 3 and the standard library:

```bash
python3 _scripts/ops/reconcile_source_data.py
python3 _scripts/analysis/postgame_defensive_review.py build --game-id 401812793
python3 _scripts/analysis/postgame_defensive_review.py summary
python3 -m unittest discover -s tests -p 'test_staff_pilot.py' -v
```

Reconciliation uses the existing cached source snapshots. If a cache is missing, the reconciler's explicit `--fetch` option retrieves it. The pilot itself never fetches or replaces source data. `build` without `--game-id` selects the latest score- and event-stat-reconciled game. `--output` belongs before the subcommand; `build` also accepts `--events`, `--games` and `--source-cache` for an alternate reconciled dataset.

The packet is in `_outputs/08_staff_pilot/401812793/`:

- `defensive_review.html`: portable, printable short report; `defensive_review.md` contains the same review queue.
- `possession_evidence.csv`: complete event rows for each selected sequence, including boundary reasons and source provenance.
- `defensive_events.csv`: all events used in the three aggregate counts.
- `baseline_packet.html`: readable full chronological source log, with period, clock, event text and exact string play IDs; no selected observations or intermediate scoreboard values. `baseline_packet.md` supplies the baseline instructions and `baseline_events.csv` contains the full event data.
- `review.json`: aggregate counts, selection rule, exact supporting IDs and source/input hashes.
- `build_metrics.json`: machine generation time, with human outcomes explicitly null.
- `packet_manifest.json`: hashes of the generated artifacts; checked when starting and finishing a session.

Both reconciliation flags must pass. The loader also regenerates the chosen game's canonical rows from its hash-verified cached ESPN response and compares every generated field. Modified text, clocks, ordering, scores, play IDs or ownership cannot pass by merely preserving aggregate totals or a claimed source hash. This validates consistency with that cached source, not the accuracy of the source relative to film.

## Qualifying Possession Examples

A sequence starts at an opponent defensive rebound or UConn turnover. It ends at an opponent turnover, a UConn defensive rebound after a logged missed shot, or an opponent made field goal with subsequent UConn shot/turnover control confirmed. A missed attempt requires a logged rebound before another attempt. Fouls, free throws, jump balls, period changes, backward clocks, gaps beyond 120 seconds, inconsistent control and incomplete endings exclude the sequence. Fractional source clocks are supported.

Examples use the earliest qualifying make and miss for each shot theme, then the earliest two turnovers. This rule is fixed and does not select the most dramatic plays. Counts include all relevant source events, including those excluded from reconstruction. The selected sequences have incomplete coverage and must never serve as possession denominators. Layup/dunk/tip labels establish neither precise location nor a defensive error. Turnover text does not establish that defensive pressure caused the turnover. All such judgments require authorized film review.

## Trial Protocol

Have a staff reviewer compare the source packet with the short report, using the same film access and the same deliverable: at most three evidence-backed items. Preassign an anonymized reviewer ID and a pair ID before either condition. Start the timer immediately before opening the assigned packet; finish after the findings are recorded. Do not rebuild or edit a packet during a session.

Open `baseline_packet.html` in the browser for the baseline condition and `defensive_review.html` for the report condition. The full baseline log permits normal browser text search. Both conditions require the same task and timing limit; record an item as film-confirmed only when authorized film was actually checked.

Use two comparable reconciled games per reviewer when possible, one per condition. Alternate baseline-first and report-first order across reviewers or pairs. Avoid showing the report before that game's baseline review. A same-game repeat is allowed but its familiarity bias is explicitly flagged. Different-game pairs can differ in difficulty; this small pilot supplies descriptive observations, not a causal effect estimate.

The Butler/DePaul comparison above is the current assignment. A single report session can measure actual usefulness and elapsed time, but **cannot establish time saved**. A baseline session and report session with the same reviewer/pair IDs are required for a paired time contrast.

The Florida report has already been shown to the project user. If that person becomes the reviewer, use another game for the baseline; a later Florida baseline would be affected by familiarity. A staff reviewer who has not seen the report can receive only the assigned packet. Hand off the packet, the common three-item task, access to authorized film and these recording instructions together. An actual human operator must start and finish the trial; preparing or previewing the packet does not create a staff timing result.

## Start an Actual Session

The following command prompts for actual identifiers and the chosen condition, then prints the session ID. It does not create sample results:

```bash
python3 -c '
import subprocess, sys
script = "_scripts/analysis/postgame_defensive_review.py"
game = input("Built game ID: ").strip()
condition = input("Condition (baseline or report): ").strip()
reviewer = input("Actual anonymized reviewer ID: ").strip()
pair = input("Preassigned pair ID: ").strip()
subprocess.run([sys.executable, script, "start", "--game-id", game,
               "--condition", condition, "--reviewer", reviewer, "--pair-id", pair], check=True)
'
```

## Record Findings

Record the actual findings in a JSON array, with one object per item reviewed, including rejected items. An empty array `[]` is correct only when the actual session reviewed zero items. Each object has these fields:

| Field | Required value |
|---|---|
| `observation` | The actual proposed review observation |
| `period`, `clock`, `play_id` | A locator matching an event in this game's `baseline_events.csv`; preserve play IDs as strings and the exact displayed clock |
| `assessment` | What the reviewer checked and concluded, including why an item was rejected |
| `source_correct` | Boolean: the logged evidence supports the stated factual observation |
| `worth_reviewing` | Boolean: the reviewer considers it worth staff/film review |
| `film_confirmed` | Boolean: the reviewer actually checked authorized film and confirmed the observation; false when no film check occurred |
| `actionable` | Boolean: a specific coaching/review action is supported and recorded in the assessment |

`actionable` requires `worth_reviewing`; `film_confirmed` requires `source_correct`. These remain staff judgments, even though the program validates source locators and count consistency. Do not equate reading play-by-play with film confirmation.

## Finish and Summarize

After recording the findings, this command prompts for the returned session ID and the actual findings file, derives the aggregate counts from its boolean flags, and stops the timer:

```bash
python3 -c '
import json, subprocess, sys
from pathlib import Path
session = input("Started session ID: ").strip()
path = Path(input("Path to actual findings JSON: ").strip())
findings = json.loads(path.read_text())
command = [sys.executable, "_scripts/analysis/postgame_defensive_review.py", "finish",
           "--session-id", session, "--findings", str(path),
           "--items-reviewed", str(len(findings))]
for field in ("source_correct", "worth_reviewing", "film_confirmed", "actionable"):
    command += ["--" + field.replace("_", "-"), str(sum(item[field] is True for item in findings))]
subprocess.run(command, check=True)
'
python3 _scripts/analysis/postgame_defensive_review.py summary
```

Session times use wall-clock timestamps across CLI invocations, so they include interruptions and recording time. Record interruptions with the `finish --notes` option when using the CLI directly. A backward system-clock change is rejected; other clock adjustments cannot be detected. Keep the machine clock stable. Duplicate completions, overlapping sessions for one reviewer, duplicate conditions within a reviewer/pair, invalid locators and changed packet artifacts are rejected.

The append-only `_outputs/08_staff_pilot/human_sessions.jsonl` contains actual starts and completions. `human_pilot_summary.json` reports completed sessions, per-condition median time and item counts, and paired baseline-minus-report seconds. A positive paired value indicates less elapsed time in the report condition for that pair. It also reports usefulness differences, order and same-game reuse. Machine generation seconds are never presented as staff time saved.

## Decide Whether to Continue

Retain each reviewer assessment and the source/film locator. Consider another pilot if the report produces a new item the reviewer wants to inspect or lowers preparation time without reducing correct/useful items. Investigate incorrect observations or failed possession boundaries before expansion. With no completed staff sessions, the correct conclusion is **ready for an actual staff trial; impact unknown**. Tests use synthetic data and mocked clocks in temporary directories and never populate the real human ledger.
