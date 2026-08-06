#!/usr/bin/env bash
# scripts/earnest_notify.sh -- send Boxscore Prophet delivery to Telegram via OpenClaw.
# Usage: earnest_notify.sh [--preview]

set -euo pipefail
cd "$(dirname "$0")/.."

PREVIEW_ONLY=0
if [ "${1:-}" = "--dry-run" ] || [ "${1:-}" = "--preview" ]; then
  PREVIEW_ONLY=1
fi

ENV_FILE="${EARNEST_DELIVERY_ENV_FILE:-scripts/earnest_delivery.env}"
if [ -f "$ENV_FILE" ]; then
  # shellcheck disable=SC1090
  source "$ENV_FILE"
fi

CHANNEL="${OPENCLAW_DELIVERY_CHANNEL:-telegram}"
TARGET="${OPENCLAW_DELIVERY_TARGET:-}"
ACCOUNT="${OPENCLAW_DELIVERY_ACCOUNT:-default}"
SILENT="${OPENCLAW_DELIVERY_SILENT:-false}"
FORCE_DOCUMENT="${OPENCLAW_DELIVERY_FORCE_DOCUMENT:-false}"
MANIFEST="output/latest/run_manifest.json"

[ -f "$MANIFEST" ] || { echo "[earnest_notify] missing manifest: $MANIFEST" >&2; exit 1; }
[ -n "$TARGET" ] || { echo "[earnest_notify] OPENCLAW_DELIVERY_TARGET is not set" >&2; exit 1; }
command -v openclaw >/dev/null || { echo "[earnest_notify] openclaw CLI not found" >&2; exit 1; }

SUMMARY_FILE=$(mktemp)
MEDIA_FILE=$(mktemp)
python3 - "$MANIFEST" "$SUMMARY_FILE" "$MEDIA_FILE" <<'PY'
import csv, json, os, subprocess, sys

path = sys.argv[1]
summary_path = sys.argv[2]
media_path = sys.argv[3]
with open(path) as fh:
    m = json.load(fh)

commit = subprocess.check_output(["git", "rev-parse", "--short", "HEAD"], text=True).strip()
season = m["season"]
week = m["week"]
mode = m.get("mode", "run")
counts = m.get("counts", {})
warnings = m.get("warnings", [])
ecr = counts.get("ecr", {})
status = "completed with warnings" if warnings else "finished clean"
artifacts = m.get("artifacts", {})
summary = [
    f"Boxscore Prophet update, {season} W{week:02d} {mode} {status}.",
    f"{counts.get('games', 0)} games in slate, {counts.get('scored_players', 0)} scored players.",
]
if ecr:
    ordered = [pos for pos in ("QB", "RB", "WR", "TE") if pos in ecr]
    if ordered:
        summary.append("ECR loaded: " + ", ".join(f"{pos} {ecr[pos]}" for pos in ordered) + ".")
summary.append(f"Repo commit pushed to main ({commit}).")
if warnings:
    summary.append("Warnings: " + "; ".join(warnings[:2]) + ".")
summary.append("Latest handoff refreshed at output/latest/boards.md")

if mode == "full":
    rookie_tracker = artifacts.get("rookie_tracker_csv")
    rookie_leaders = artifacts.get("rookie_leaders_csv")
    rookie_role = artifacts.get("rookie_role_earners_csv")
    rookie_watch = artifacts.get("rookie_watch_csv")

    def read_rows(path):
        if not path or not os.path.exists(path):
            return []
        with open(path, newline="") as fh:
            return list(csv.DictReader(fh))

    tracker_rows = read_rows(rookie_tracker)
    leaders_rows = read_rows(rookie_leaders)
    role_rows = read_rows(rookie_role)
    watch_rows = read_rows(rookie_watch)

    if tracker_rows:
        status_counts = {}
        for row in tracker_rows:
            key = row.get("tracker_status") or "unknown"
            status_counts[key] = status_counts.get(key, 0) + 1
        active = status_counts.get("active", 0)
        limited = status_counts.get("limited_role", 0)
        waiting = status_counts.get("no_nfl_data_yet", 0)
        pending = status_counts.get("id_pending", 0)
        summary.append(
            "Rookie tracker: "
            f"{active} active, {limited} limited-role, {waiting} waiting on NFL data"
            + (f", {pending} id-pending." if pending else ".")
        )

        if leaders_rows:
            top = leaders_rows[:3]
            summary.append(
                "Top rookie production: " + ", ".join(
                    f"{row['nfl_name']} ({row['fp_pg']} FP/G)" for row in top
                ) + "."
            )
        elif watch_rows:
            top = watch_rows[:3]
            summary.append(
                "Rookie watchlist: " + ", ".join(
                    f"{row['nfl_name']} ({row['draft_team']})" for row in top
                ) + "."
            )

        if role_rows:
            top = role_rows[:3]
            summary.append(
                "Role earners: " + ", ".join(
                    f"{row['nfl_name']} ({row['latest_snap_share'] or '--'} snap, {row['latest_opportunities'] or '--'} opps)"
                    for row in top
                ) + "."
            )

# Movers -- both modes (10g refreshes on rescores, where injury movers appear).
def read_rows(path):
    if not path or not os.path.exists(path):
        return []
    with open(path, newline="") as fh:
        return list(csv.DictReader(fh))

mover_rows = read_rows(artifacts.get("movers_csv"))
if mover_rows:
    def cap_pct(p):
        return round(100 * min(max(float(p), 0.02), 0.95))

    def fmt_mover(row):
        tag = " (inj)" if row.get("injury_flag") == "TRUE" else ""
        delta = round(float(row["delta_start_pp"]))
        return (
            f"{row['player_name']} {row['position']} {delta:+d} "
            f"({cap_pct(row['p_start'])}% vs {cap_pct(row['p_start_base'])}%){tag}"
        )

    ranked = sorted(mover_rows, key=lambda r: float(r["delta_start_pp"]))
    ups = [r for r in reversed(ranked) if float(r["delta_start_pp"]) > 0][:3]
    downs = [r for r in ranked if float(r["delta_start_pp"]) < 0][:3]
    if ups:
        summary.append("Movers up: " + ", ".join(fmt_mover(r) for r in ups) + ".")
    if downs:
        summary.append("Movers down: " + ", ".join(fmt_mover(r) for r in downs) + ".")

with open(summary_path, "w") as fh:
    fh.write("\n".join(summary))
with open(media_path, "w") as fh:
    for key in ("start_rb", "start_wr", "start_te", "start_qb", "boom_flex"):
        fh.write(m["artifacts"]["images"][key] + "\n")
PY
SUMMARY=$(cat "$SUMMARY_FILE")
REPO=$(pwd)
MEDIA=()
while IFS= read -r line; do
  [ -z "$line" ] && continue
  case "$line" in
    /*) MEDIA+=("$line") ;;
    *)  MEDIA+=("$REPO/$line") ;;
  esac
done < "$MEDIA_FILE"
rm -f "$SUMMARY_FILE" "$MEDIA_FILE"

summary_cmd=(openclaw message send --account "$ACCOUNT" --channel "$CHANNEL" --target "$TARGET" --message "$SUMMARY")
media_cmd=(openclaw message send --account "$ACCOUNT" --channel "$CHANNEL" --target "$TARGET")
[ "$SILENT" = "true" ] && summary_cmd+=(--silent) && media_cmd+=(--silent)
[ "$FORCE_DOCUMENT" = "true" ] && media_cmd+=(--force-document)
for path in "${MEDIA[@]}"; do
  media_cmd+=(--media "$path")
done
printf '[earnest_notify] summary\n%s\n' "$SUMMARY"
if [ "$PREVIEW_ONLY" -eq 1 ]; then
  printf '[earnest_notify] media group\n'
  printf '%s\n' "${MEDIA[@]}"
  exit 0
fi

"${summary_cmd[@]}"
"${media_cmd[@]}"

echo "[earnest_notify] delivered ${#MEDIA[@]} media attachments to $CHANNEL:$TARGET"
