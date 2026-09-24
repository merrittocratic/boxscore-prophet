---
name: on-the-record
description: Draft the weekly "On the Record" grading Substack Note (150-200 words) from last week's flex-tier receipts -- the start/sit calls that actually get made, and where the model disagreed with FantasyPros ECR. Use when Steve asks for the Wednesday recap, the grading note, or "On the Record."
---

# On the Record (Wednesday Note)

Draft the weekly public grading piece as a short Substack NOTE, not a
full article: what the model said last week about the players readers
were actually deciding on, graded against what happened. This is the
accountability piece -- see `CONTENT_GUIDE.md` for the full voice
contract. The output is a DRAFT for Steve to edit; never commit or push.

## What this note is about (Steve, 2026-09-24)

The FLEX TIER, not the whole board. Nobody benches De'Von Achane, and
nobody outside the deepest leagues starts an 8%-chance WR, so neither
one is a grade that matters to a reader even when it's the week's
biggest miss or longest shot. The note answers two questions:

1. **Who is the model saying should be in lineups that consensus has on
   the bench?** Flex-tier players the model ranked above ECR -- did they
   deliver?
2. **Where did the model miss flex players?** Players it backed who
   flopped, and players it doubted (lower than consensus, or a low
   stated chance) who went off.

Model-vs-ECR disagreement is the best content in the note. Agreement
is not a story.

## Why Wednesday

The Tuesday 23:30 full run finalizes the prior week's receipts (it
re-runs 10d for the prior week after Monday Night Football). A
Wednesday note grades a fully played week.

## Inputs

1. `output/10d_receipts_<season>_w<prevweek>.md` -- REQUIRED. Read the
   **Flex-tier receipts** section first: the model-higher / model-lower /
   agree tallies, and the four tables (model higher than consensus,
   model lower than consensus, flex misses the model backed, flex hits
   the model doubted). The calibration-by-band table at the top is
   secondary. Check that "Still on the board" is empty. If games are
   still pending or the file is missing, run
   `Rscript R/10d_content_tables.R <season> <prevweek>` (after the week
   has fully played out) and use the result.
2. `output/10d_flex_receipts_<season>_w<prevweek>.csv` -- the full flex
   pool behind those tables (model_rank, ecr_rank, rank_gap, view,
   start_pct, fp_actual, hit_start). Use it to pick names and to check
   position-level patterns (e.g. "zero flex RBs cleared the bar").
3. Voice reference: read 2-3 recent pieces in `~/content/published/`
   (and any prior `on_the_record` drafts in `~/content/draft/`) before
   writing. Absorb tone, do not imitate structure verbatim.

If the flex section says there was no ECR lock for the week, the
consensus comparison is unavailable: grade the flex tier on model rank
alone, and say nothing about consensus rather than inventing it.

## Week 1 exception

There is no prior week to grade in Week 1 -- do not attempt to draft this
note for Week 1. Flag this to Steve and stop.

## Who can be named

Only players in the flex receipts. Never name a consensus auto-start
(the pool already excludes them) or a deep-roster longshot outside the
pool, no matter how big the hit or miss.

## Note contract

- 150-200 words, hard range. ASCII only. No em-dashes in any form (not
  the unicode character, not "--" as a stand-in); use a period, comma,
  colon, or parentheses instead.
- Substack Notes render as plain short-form text: no section headers,
  no tables, no bullet-heavy layout.
- Structure:
  1. One-sentence headline grade on the flex tier.
  2. The best model-over-consensus call that HIT: player, where the
     model had him vs where consensus had him, in roster terms ("the
     model had him as a top-6 TE; consensus had him 12th, a streamer"),
     his chance, what he scored.
  3. The miss (MANDATORY): the most notable model-backed flex player who
     flopped, preferably one the model ranked above consensus. If room
     allows, add the most notable flex player the model doubted who
     went off.
  4. The head-to-head in one line: how the model's above-consensus
     flex calls did vs its below-consensus ones (hits of total, each).
  5. One sentence of overall calibration (the band table, players who
     played only) -- keep it, especially while bands run off.
  6. Optional one-line teaser to Thursday's preview.
  Cut 6, then the second half of 3, for space. Never cut the miss.
- A week where consensus did better than the model on flex
  disagreements is stated as plainly as one where the model did better.
  The short format is not an excuse to drop it.
- One week of flex disagreements is a small sample (usually 20-30 calls
  per side). Report the tally; never present one week as proof the model
  beats (or trails) consensus. Per `CLAUDE.md`, no public claim of an
  edge over the market ships until it's real and reproducible.
- When the whole flex tier had a low-scoring week, say so: if both
  sides hit about equally rarely, the honest read is "a wash," not a win.
- Every number stated must trace to a row in the receipts md or flex
  CSV. Never round a miss into looking closer than it was.
- Percentages: whole numbers. Translate chances into plain English
  ("about a coin flip," "one in four").

## Voice guardrails

Full list in `CONTENT_GUIDE.md`. The ones that bite hardest here:

- No gambling language ("odds," "fade," "price," "sharp," "lock").
  Probabilities are "chances." ECR is "consensus" or "the experts,"
  attributed as FantasyPros ECR on first use.
- The model is "the model," third person -- not "our model."
- Never claim more skill than the receipts show. A doubted player who
  went off is a model miss, not "variance" -- say it plainly.

## Output

Print the note in chat for copy/paste AND write it to
`~/content/draft/w<NN>_on_the_record.md` (zero-padded week, e.g.
`~/content/draft/w03_on_the_record.md`). Steve's content folder,
OUTSIDE this repo -- never write into `content/` here. Report word
count. Surface proposed revisions in chat for approval before editing
the file on any subsequent pass. Never commit or push; Steve handles git.
