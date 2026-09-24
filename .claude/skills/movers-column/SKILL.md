---
name: movers-column
description: Draft the Sunday-morning start/sit movers Substack Note (150-200 words) from the freshest (Sunday 08:00 rescore) 10g movers table. Use when Steve asks for the weekly movers note, start/sit note, or movers column.
---

# Start 'Em, Sit 'Em: The Movers (Sunday AM Note)

Draft the start 'em / sit 'em piece as a short Substack NOTE, not a
full article. This is the forward-looking, actionable half of the weekly
pair -- see `CONTENT_GUIDE.md` for voice rules and how this relates to
Wednesday's `/on-the-record` grading note (which owns the receipts
recap; this note never opens with it). The output is a DRAFT for Steve
to edit -- his editing pass is part of the published workflow, so never
polish away room for his voice, and never commit.

## Timing matters here

Run this from the SUNDAY 08:00 rescore, the last run before kickoff.
Practice reports and final injury designations land Thursday through
Saturday; a Sunday-morning note reflects all of it. Confirm the
`output/10g_movers_<season>_w<week>.csv` being read was written by the
Sunday 08:00 rescore (check the file's mtime / the run log) before
drafting. If only an older version exists (Saturday 15:00 or Tuesday),
say so and ask Steve whether to wait or draft off it -- do not silently
draft off stale numbers.

## Inputs (in priority order)

1. `output/10g_movers_<season>_w<week>.csv` -- the full movers table,
   Sunday-rescore version (see above). If missing, run
   `Rscript R/10g_movers_table.R <season> <week>` first, after confirming
   the Sunday rescore itself has completed. Columns: p_start/p_boom now
   vs `*_base` (trailing published baseline), `delta_start_pp`, context
   (`pred_vol`, `opp_def_adj_*`, `implied_total_*`, `team_spread_*`,
   `report_status`, `injury_flag`).
2. `output/10d_boards_<season>_w<week>.md` -- for cross-checking ranks
   and display values. Do NOT reproduce boards in the note; the note
   shares names and reasoning, the boards stay scarce (paid tier).
3. Voice reference: read 2-3 recent pieces in `~/content/published/`
   (and any prior movers notes in `~/content/draft/`) before writing,
   every time. Absorb tone, do not imitate structure verbatim.

## Note contract

- 150-200 words, hard range. ASCII only. No em-dashes in any form (not
  the unicode character, not "--" as a stand-in); use a period, comma,
  colon, or parentheses instead.
- Substack Notes render as plain short-form text: no section headers,
  no tables, no bullet-heavy layout. Bold a player name at most.
- Structure: one-line hook -> 2-4 movers total (at least one start and
  one sit; 2+2 is the default, go to 1+1 if the reasoning needs the
  room) -> one-line close. An optional pointer to Wednesday's grading
  ("graded Wednesday, as always") can be the close.
- Every pick is a MOVER: picked from the top risers/fallers in the CSV,
  not from the top of the board. Per `CONTENT_GUIDE.md`'s
  Decision-Relevant Tiers: RB/WR picks from roughly rank 20-39, QB/TE
  from roughly rank 10-19. A big delta above that band is not a real
  start/sit dilemma -- skip it for a mover further down the board even
  if its delta is smaller.
- Each pick states, in one or two sentences: this week's chance, the
  player's own baseline, and the single strongest WHY in NFL terms from
  the context columns (front quality, projected volume shift, game
  environment, injury status). With this little room, pick the one
  driver that matters most rather than listing all of them. Never invent
  a reason the context columns don't support; if the driver is unclear,
  say the model moved and the why is muddy.
- Percentages: use the display-capped values as shown in the 10g md
  (already clamped 2-95). Whole numbers.

## Voice guardrails (Steve's standing feedback)

- NO gambling language: no "odds", "fade", "price", "sharp", "the house",
  "lock". Probabilities are "chances". Fantasy vernacular (boom, flex,
  waiver wire) is fine.
- Numbers get translated on the spot ("he clears a startable week two
  times in five").
- Probabilities are not promises. Never claim the delta framing as
  validated model skill -- deltas are arithmetic on published numbers.
- Injury-driven movers are labeled as report-driven, not matchup-driven.

## Output

Print the note in chat for copy/paste AND write it to
`~/content/draft/w<NN>_movers_note.md` (zero-padded week, e.g.
`~/content/draft/w04_movers_note.md`). Steve's content folder, OUTSIDE
this repo -- never write into `content/` here. Report word count.
Surface proposed revisions in chat for approval before editing the file
on any subsequent pass. Never commit or push; Steve handles git.
