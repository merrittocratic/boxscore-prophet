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
   before writing, every time. Absorb tone, do not imitate structure verbatim.
4. Name log (added 2026-10-04): check the movers name log in memory
   (`project_movers_name_log`) before picking. Skip anyone named in
   the prior 2 weeks unless the reason is genuinely new, and mix
   positions (WR/TE, not just RBs). Steve can override with a name he
   wants in. After he posts, update the log with the final names.

## Note contract

- 150-200 words, hard range. ASCII only. No em-dashes in any form (not
  the unicode character, not "--" as a stand-in); use a period, comma,
  colon, or parentheses instead.
- Substack Notes render as plain short-form text: no markdown headers,
  no tables, no bullet-heavy layout. Bold a player name at most.
- Structure: one-line hook -> start group -> sit group -> one-line
  close. 2+2 is the default (at least one start and one sit; 1+1 if the
  reasoning needs the room). If Steve asks for more names (e.g. 2 per
  position) draft them all and let him pare down; the word range then
  applies to his final cut, not the draft.
  - Hook: names the week's theme in Steve's voice, not a generic intro
    ("Injuries, not matchups, drive this week's movers."; "The Jets
    backfield has a clear #1 this week..."). A light pop-culture or
    fan-voice line is welcome.
  - Group labels are plain text lines, not headers: "Who I like:" /
    "Who should you start?" and "Who should you sit?" / "Who might be on
    the struggle bus:". Lead each pick with the player's name.
  - Close: a Wednesday pointer ("See you on Wednesday for the grades!").
- Benchmark line (Steve, 2026-10-25): every note must say explicitly,
  at least once, what the chance MEANS, so a new reader can't read
  "35% chance" as "start him in 35% of lineups". Name the bar for the
  positions in the note: a startable week is 15+ PPR points for RB/WR,
  12+ PPR for TE, 20+ standard-scoring points for QB. Phrase it as the
  model's chance he clears that bar, e.g. "a 1-in-3 chance at a
  startable week (15+ PPR points)". Once per position per note is
  enough; don't repeat it on every pick.
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
- Frame chances as "1 in N" fractions, not raw percentages (Steve,
  2026-10-25): "moves from a 1-in-5 chance to a 1-in-3 chance at a
  startable week". Start from the display-capped values in the 10g md
  (already clamped 2-95) and round to the nearest plain fraction:
  ~10% 1 in 10, ~17% 1 in 6, ~20% 1 in 5, ~25% 1 in 4, ~33% 1 in 3,
  ~40% 2 in 5, ~50% even / 1 in 2, ~60% 3 in 5, ~67% 2 in 3, ~75% 3
  in 4. Use "nearly"/"better than" when a value sits between two
  fractions. When both ends round to the same fraction, the move is too
  small to frame this way, which usually means it isn't a mover worth
  naming. A raw percentage may appear in parentheses sparingly when
  Steve wants the precision.
- Consensus contrast: check `output/10d_ecr_lock_<season>_w<week>.csv`
  (`ecr_rank`) for each pick. When the model rank and consensus rank
  differ meaningfully, say so plainly ("The model has him as its QB11.
  Consensus (FantasyPros ECR) has him 25th."). A pick where consensus
  already agrees with the move is weaker; prefer one with a gap when
  candidates are otherwise close.
- Team scoring: say "projected total" / "projected scoring" (e.g. "the
  Chargers' projected total drops from 26 to 21"), not "implied total".
- First-person asides in Steve's voice are fine and wanted when the
  data supports them ("I need to see it from the Chargers offense
  before trusting anyone on their roster").

## Voice guardrails (Steve's standing feedback)

- Market language is allowed (Steve, 2026-10-25): "odds", "price",
  "market", "implied probability" are fine ("moves from 1-in-5 to
  1-in-3 startable odds"). Fantasy vernacular (boom, flex, waiver wire)
  is still fine. No certainty words ("lock"), and never claim the model
  beats the market until proven (CLAUDE.md).
- Numbers get translated on the spot ("he clears a startable week two
  times in five").
- Probabilities are not promises. Never claim the delta framing as
  validated model skill -- deltas are arithmetic on published numbers.
- Injury-driven movers are labeled as report-driven, not matchup-driven.

## Output

CHAT OUTPUT ONLY (Steve, 2026-09-24). Print the note in chat for
copy/paste and report the word count. Do not write a file anywhere --
not `~/content/draft/`, not this repo's `content/`. Revisions are
likewise printed in chat as a full updated note. Never commit or push.
