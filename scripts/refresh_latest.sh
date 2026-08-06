#!/usr/bin/env bash
# scripts/refresh_latest.sh -- refresh stable local handoff files for content work.
# Usage: refresh_latest.sh <season> <week> [mode] [log_path]

set -euo pipefail
cd "$(dirname "$0")/.."

SEASON="${1:?usage: refresh_latest.sh <season> <week> [mode] [log_path]}"
WEEK="${2:?usage: refresh_latest.sh <season> <week> [mode] [log_path]}"
MODE="${3:-unknown}"
LOG_PATH="${4:-}"
WTAG=$(printf "%s_w%02d" "$SEASON" "$WEEK")
LATEST_DIR="output/latest"
IMG_DIR="$LATEST_DIR/img"
mkdir -p "$IMG_DIR"

need_file() {
  local path="$1"
  [ -f "$path" ] || { echo "[refresh_latest] missing required file: $path" >&2; exit 1; }
}

copy_required() {
  local src="$1" dst="$2"
  need_file "$src"
  cp "$src" "$dst"
}

copy_optional() {
  local src="$1" dst="$2"
  if [ -f "$src" ]; then
    cp "$src" "$dst"
  else
    rm -f "$dst"
  fi
}

copy_required "output/10d_boards_${WTAG}.md"          "$LATEST_DIR/boards.md"
copy_required "output/10d_content_board_${WTAG}.csv"   "$LATEST_DIR/content_board.csv"
copy_required "output/10d_start_board_${WTAG}.csv"     "$LATEST_DIR/start_board.csv"
copy_required "output/10d_boom_board_${WTAG}.csv"      "$LATEST_DIR/boom_board.csv"
copy_required "output/10d_streamer_board_${WTAG}.csv"  "$LATEST_DIR/streamer_board.csv"
copy_required "output/10c_scored_slate_${WTAG}.csv"    "$LATEST_DIR/scored_slate.csv"
copy_required "output/10b_game_slate_${WTAG}.csv"      "$LATEST_DIR/game_slate.csv"
copy_optional "output/10d_ecr_gap_${WTAG}.csv"         "$LATEST_DIR/ecr_gap.csv"
copy_optional "output/10d_receipts_${WTAG}.md"         "$LATEST_DIR/receipts.md"
copy_optional "output/10d_receipts_${WTAG}.csv"        "$LATEST_DIR/receipts.csv"
copy_optional "output/10d_receipt_bands_${WTAG}.csv"   "$LATEST_DIR/receipt_bands.csv"
copy_optional "output/10g_movers_${WTAG}.csv"          "$LATEST_DIR/movers.csv"
copy_optional "output/10e_rookie_content_${SEASON}.md"      "$LATEST_DIR/rookies.md"
copy_optional "output/10e_rookie_tracker_${SEASON}.csv"     "$LATEST_DIR/rookie_tracker.csv"
copy_optional "output/10e_rookie_weekly_${SEASON}.csv"      "$LATEST_DIR/rookie_weekly.csv"
copy_optional "output/10e_rookie_leaders_${SEASON}.csv"     "$LATEST_DIR/rookie_leaders.csv"
copy_optional "output/10e_rookie_role_earners_${SEASON}.csv" "$LATEST_DIR/rookie_role_earners.csv"
copy_optional "output/10e_rookie_efficiency_${SEASON}.csv"  "$LATEST_DIR/rookie_efficiency.csv"
copy_optional "output/10e_rookie_watch_${SEASON}.csv"       "$LATEST_DIR/rookie_watch.csv"

copy_required "output/img/10d_start_rb_${WTAG}.png"   "$IMG_DIR/start_rb.png"
copy_required "output/img/10d_start_wr_${WTAG}.png"   "$IMG_DIR/start_wr.png"
copy_required "output/img/10d_start_te_${WTAG}.png"   "$IMG_DIR/start_te.png"
copy_required "output/img/10d_start_qb_${WTAG}.png"   "$IMG_DIR/start_qb.png"
copy_required "output/img/10d_boom_flex_${WTAG}.png"  "$IMG_DIR/boom_flex.png"

python3 - "$SEASON" "$WEEK" "$MODE" "$WTAG" "$LOG_PATH" "$LATEST_DIR" <<'PY'
import csv, datetime as dt, json, os, sys

season = int(sys.argv[1])
week = int(sys.argv[2])
mode = sys.argv[3]
wtag = sys.argv[4]
log_path = sys.argv[5]
latest_dir = sys.argv[6]


def row_count(path):
    if not os.path.exists(path):
        return None
    with open(path, newline="") as fh:
        return max(sum(1 for _ in fh) - 1, 0)


def ecr_counts(path):
    if not os.path.exists(path):
        return {}
    counts = {}
    with open(path, newline="") as fh:
        reader = csv.DictReader(fh)
        for row in reader:
            pos = row.get("position")
            if pos:
                counts[pos] = counts.get(pos, 0) + 1
    return counts


def warning_lines(path):
    if not path or not os.path.exists(path):
        return []
    out = []
    seen = set()
    with open(path) as fh:
        for raw in fh:
            line = raw.strip()
            if not line.startswith("! "):
                continue
            msg = line[2:]
            if "No backtest rows for" in msg:
                continue
            if msg not in seen:
                seen.add(msg)
                out.append(msg)
    return out

counts = {
    "games": row_count(os.path.join(latest_dir, "game_slate.csv")),
    "scored_players": row_count(os.path.join(latest_dir, "scored_slate.csv")),
    "content_board_rows": row_count(os.path.join(latest_dir, "content_board.csv")),
    "start_board_rows": row_count(os.path.join(latest_dir, "start_board.csv")),
    "boom_board_rows": row_count(os.path.join(latest_dir, "boom_board.csv")),
    "streamer_board_rows": row_count(os.path.join(latest_dir, "streamer_board.csv")),
    "ecr_gap_rows": row_count(os.path.join(latest_dir, "ecr_gap.csv")),
    "rookie_tracker_rows": row_count(os.path.join(latest_dir, "rookie_tracker.csv")),
    "rookie_leaders_rows": row_count(os.path.join(latest_dir, "rookie_leaders.csv")),
    "rookie_role_rows": row_count(os.path.join(latest_dir, "rookie_role_earners.csv")),
    "rookie_efficiency_rows": row_count(os.path.join(latest_dir, "rookie_efficiency.csv")),
    "rookie_watch_rows": row_count(os.path.join(latest_dir, "rookie_watch.csv")),
    "movers_rows": row_count(os.path.join(latest_dir, "movers.csv")),
    "ecr": ecr_counts(os.path.join("data", "ecr", f"ecr_{wtag}.csv")),
}

manifest = {
    "generated_at": dt.datetime.now().astimezone().isoformat(),
    "mode": mode,
    "season": season,
    "week": week,
    "wtag": wtag,
    "log_path": log_path if log_path and os.path.exists(log_path) else None,
    "counts": counts,
    "warnings": warning_lines(log_path),
    "artifacts": {
        "boards_md": os.path.join(latest_dir, "boards.md"),
        "content_board_csv": os.path.join(latest_dir, "content_board.csv"),
        "start_board_csv": os.path.join(latest_dir, "start_board.csv"),
        "boom_board_csv": os.path.join(latest_dir, "boom_board.csv"),
        "streamer_board_csv": os.path.join(latest_dir, "streamer_board.csv"),
        "game_slate_csv": os.path.join(latest_dir, "game_slate.csv"),
        "scored_slate_csv": os.path.join(latest_dir, "scored_slate.csv"),
        "ecr_gap_csv": os.path.join(latest_dir, "ecr_gap.csv"),
        "receipts_md": os.path.join(latest_dir, "receipts.md"),
        "rookies_md": os.path.join(latest_dir, "rookies.md"),
        "rookie_tracker_csv": os.path.join(latest_dir, "rookie_tracker.csv"),
        "rookie_weekly_csv": os.path.join(latest_dir, "rookie_weekly.csv"),
        "rookie_leaders_csv": os.path.join(latest_dir, "rookie_leaders.csv"),
        "rookie_role_earners_csv": os.path.join(latest_dir, "rookie_role_earners.csv"),
        "rookie_efficiency_csv": os.path.join(latest_dir, "rookie_efficiency.csv"),
        "rookie_watch_csv": os.path.join(latest_dir, "rookie_watch.csv"),
        "movers_csv": os.path.join(latest_dir, "movers.csv"),
        "images": {
            "start_rb": os.path.join(latest_dir, "img", "start_rb.png"),
            "start_wr": os.path.join(latest_dir, "img", "start_wr.png"),
            "start_te": os.path.join(latest_dir, "img", "start_te.png"),
            "start_qb": os.path.join(latest_dir, "img", "start_qb.png"),
            "boom_flex": os.path.join(latest_dir, "img", "boom_flex.png"),
        },
    },
    "source_files": {
        "boards_md": f"output/10d_boards_{wtag}.md",
        "content_board_csv": f"output/10d_content_board_{wtag}.csv",
        "start_board_csv": f"output/10d_start_board_{wtag}.csv",
        "boom_board_csv": f"output/10d_boom_board_{wtag}.csv",
        "streamer_board_csv": f"output/10d_streamer_board_{wtag}.csv",
        "scored_slate_csv": f"output/10c_scored_slate_{wtag}.csv",
        "game_slate_csv": f"output/10b_game_slate_{wtag}.csv",
        "ecr_gap_csv": f"output/10d_ecr_gap_{wtag}.csv",
        "rookies_md": f"output/10e_rookie_content_{season}.md",
        "rookie_tracker_csv": f"output/10e_rookie_tracker_{season}.csv",
        "rookie_weekly_csv": f"output/10e_rookie_weekly_{season}.csv",
        "rookie_leaders_csv": f"output/10e_rookie_leaders_{season}.csv",
        "rookie_role_earners_csv": f"output/10e_rookie_role_earners_{season}.csv",
        "rookie_efficiency_csv": f"output/10e_rookie_efficiency_{season}.csv",
        "rookie_watch_csv": f"output/10e_rookie_watch_{season}.csv",
        "movers_csv": f"output/10g_movers_{wtag}.csv",
    },
}

with open(os.path.join(latest_dir, "run_manifest.json"), "w") as fh:
    json.dump(manifest, fh, indent=2)
    fh.write("\n")
PY

echo "[refresh_latest] updated $LATEST_DIR for $WTAG"
