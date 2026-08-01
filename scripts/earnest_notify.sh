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
import json, subprocess, sys

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
