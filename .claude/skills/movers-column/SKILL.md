---
name: movers-column
description: Draft the weekly start/sit movers column for Substack from the 10g movers table, receipts, and boards. Use when Steve asks for the weekly column, start/sit write-up, or movers article.
---

# Weekly Movers Column

Draft the start 'em / sit 'em column for Substack from the week's movers
table. The output is a DRAFT for Steve to edit -- his editing pass is part
of the published workflow (the git diff is his public receipt), so never
polish away room for his voice, and never commit.

## Inputs (in priority order)

1. `output/10g_movers_<season>_w<week>.csv` -- the full movers table.
   If missing, run `Rscript R/10g_movers_table.R <season> <week>` first.
   Columns: p_start/p_boom now vs `*_base` (trailing published baseline),
   `delta_start_pp`, context (`pred_vol`, `opp_def_adj_*`, `implied_total_*`,
   `team_spread_*`, `report_status`, `injury_flag`).
2. `output/10d_receipts_<season>_w<prevweek>.md` -- last week's graded
   calls. The column ALWAYS opens with receipts when they exist.
3. `output/10d_boards_<season>_w<week>.md` -- for cross-checking ranks
   and display values. Do NOT reproduce full boards in the column; the
   column shares names and reasoning, the boards stay scarce (paid tier).
4. Voice reference: read 2-3 recent pieces in `~/content/published/`
   before writing, every time. Do not imitate structure verbatim; absorb
   tone.

## Column contract

- 600-900 words, ASCII only (no unicode dashes/quotes).
- Structure: short cold open -> receipts from last week (one short
  section, wins AND misses, numbers stated plainly) -> 3 starts ->
  3 sits -> one-line close. Section headers in Steve's style (pop-culture
  riffs welcome, never forced).
- Every start/sit is a MOVER: picked from the top risers/fallers in the
  CSV, not from the top of the board. Obvious names moving in obvious
  directions are not content.
- Each pick states: this week's probability, the player's own baseline,
  and the WHY in NFL terms from the context columns (front quality,
  projected volume shift, game environment, injury status). Never invent
  a reason the context columns don't support; if the driver is unclear,
  say the model moved and the why is muddy -- honesty is the brand.
- Percentages: use the display-capped values as shown in the 10g md
  (already clamped 2-95). Round, no decimals on probabilities.

## Voice guardrails (Steve's standing feedback)

- NO gambling language: no "odds", "fade", "price", "sharp", "the house",
  "lock". Probabilities are "chances". Fantasy vernacular (boom, flex,
  waiver wire) is fine.
- Numbers get translated on the spot ("43% -- a coin-flip minus a nickel"
  style is too gambly; "he clears a startable week two times in five" is
  right).
- Probabilities are not promises: when a pick can miss, the copy owns it
  up front. Never claim the delta framing as validated model skill --
  deltas are arithmetic on published numbers.
- Injury-driven movers are labeled as report-driven, not matchup-driven.
- Descriptive stats (season-to-date leaderboards) stay visually and
  verbally separate from model probabilities.

## Output

Write to `~/content/draft/w<NN>_movers_column.md` (zero-padded week, no
season prefix, e.g. `~/content/draft/w01_movers_column.md`). This is
Steve's content folder, OUTSIDE this repo -- the column is not a repo
artifact and must not be written into `content/` here. Report word count.
Surface proposed revisions in chat for approval before editing the file
on any subsequent pass. Never commit or push; Steve handles git.
