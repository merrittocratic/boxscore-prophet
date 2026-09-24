# Boxscore Prophet -- Content Guide

Written 2026-08-23. Companion to `HANDOFF.md` (orientation) and
`MODEL_BRIDGE.md` (cross-project translation). This is the voice and
format contract for what gets published under the Merrittocracy brand
from this model. Read it before drafting any column; the `/movers-column`
and `/on-the-record` skills both point back here.

Core identity is shared with shadow-leaderboard and nfl-draft-model: the
narrative-checker. "Here's what everyone is saying -- now let's look at
what the data actually shows." Conversational and confident, contrarian
takes earned by the data. See `MODEL_BRIDGE.md` for how the push-back
target differs by project (mock-draft consensus / broadcast booth / ECR
consensus here).

---

## The Weekly Lineup

Updated 2026-09-24 (Steve's call). Three recurring model-driven pieces
across the week, plus a Monday college football wrap-up that is outside
this model and this repo (listed here only so the week reads whole):

| Day | Piece | Form | Skill |
|---|---|---|---|
| Wed | On the Record | Substack Note, 150-200 words | `/on-the-record` |
| Thu | Next-week preview | Full article | `/storyline-data-packet` feeds it |
| Sun AM | Start 'Em, Sit 'Em: The Movers | Substack Note, 150-200 words | `/movers-column` |
| Mon | College football wrap-up | Full article | none (not this model) |

The two grading/start-sit pieces are deliberately short Notes; the
Thursday preview is the one long-form piece built from this model.

### On the Record (Wednesdays, Note)

**Job:** grade what we said in public last week. This is the
differentiator -- most fantasy content never reckons with its own misses.
Format: "the model was right here, wrong here, here's the receipt for
both." Modeled on the sports-media right-or-wrong recap format, but with
an actual stated probability on record instead of a vibe.

**Why Wednesday:** the Tuesday 23:30 full production build is what
writes the prior week's receipts, and by then Monday Night Football is
final. Wednesday is the first day the full week's receipts are both
honest and fresh.

**Shape:** headline grade -> one-sentence calibration read -> one named
miss -> one named longshot hit -> optional ECR line -> optional teaser
to Thursday. The miss is mandatory; the ECR line and teaser are what get
cut for space.

**Data:** `output/10d_receipts_<season>_w<prevweek>.md` (calibration by
stated band, worst misses, longshots that hit) is the spine. Pull
`output/10d_ecr_gap_<season>_w<prevweek>.csv` when it exists for the
"here's how that compared to consensus" beat -- restate one comparison
in prose, do not reproduce the table (boards stay scarce).

**Do not skip a bad week.** A week with more misses than hits is a
BETTER On the Record note than a clean week, not a worse one -- it's
the proof the grading is real. See Voice Guardrails below.

### Next-Week Preview (Thursdays, Article)

**Job:** take the storylines of the moment ("are the Raiders for
real?") and test them against the data: first the season so far (every
game, who it came against, whether the efficiency backs the record),
then the upcoming opponent (where the matchup plays to or against that
profile, and what the model gives the key players this week).

**Data:** the `/storyline-data-packet` skill builds the packet
(retrospective first, then look-ahead) as chat output for Steve or
Cousin Claude to write from. The article itself is never drafted into
this repo. Game-level expectations come only from the market line
(spread/total -> implied points), attributed as the market's; the model
is player-level and never produces a win probability or a projected
score.

### Start 'Em, Sit 'Em: The Movers (Sunday AM, Note)

**Job:** the actionable pick, timed to be the most current start/sit
information available before the Sunday slate. Never leads with the top
of the board -- leads with players whose own number moved most against
their own trailing baseline.

**Why Sunday morning:** the Sunday 08:00 rescore is the last run before
kickoff and reflects every practice report and final injury
designation from Thursday through Saturday. Thursday Night Football is
already final, so this note is implicitly about the Sunday/Monday
slate.

**Shape:** one-line hook -> 2-4 movers (at least one start, one sit;
2+2 default) with the single strongest why for each -> one-line close.

**Data:** `output/10g_movers_<season>_w<week>.csv` from the Sunday 08:00
rescore (NOT the Tuesday or Saturday version -- the whole point is the
freshest number). Cross-check against `output/10d_boards_<season>_w<week>.md`
for ranks and display values without reproducing them.

All three pieces share the voice guardrails below and all are
Claude-drafted (or Claude-researched, for Thursday), Steve-edited --
the edit pass is part of the published product, not a formality.

---

## Decision-Relevant Tiers

A start/sit example is only content if a real reader would actually be
choosing between two names. Confirmed 2026-09-05: examples and movers
should be drawn from the tier where the roster decision is genuinely
live, not from the auto-start tier -- a big probability move on a
player everyone starts regardless is not a dilemma, no matter how large
the delta is.

- **RB/WR:** roughly ranks 20-39 (flex/streaming range -- two flex-caliber
  names competing for one spot). Avoid the top ~15-19 at either position;
  those are auto-starts in standard lineups regardless of weekly movement.
- **QB/TE:** roughly ranks 10-19 ("teens" -- single-starter streaming
  range). Most standard leagues roster one QB and one TE with no flex
  depth at either, so the real decision zone starts much earlier than
  RB/WR's -- avoid the top ~9 at either position for the same reason.

This applies to the movers column's pick selection (see its skill
contract) and to any one-off illustrative example drawn from a board in
other content. When checking a candidate example/mover against this,
confirm against the actual board's rank column -- don't estimate from
name recognition, since perceived star power and model rank can diverge.

---

## Content Autonomy Levels

- **CSVs, boards, receipts artifacts (10d/10f/10g outputs):** autonomous.
  Templated, numeric, generated directly from model output. No review
  needed before they land in `output/`.
- **On the Record draft:** Claude drafts full first pass via the skill,
  Steve edits for voice before publishing. Never commit or push from the
  skill.
- **Movers column draft:** same -- Claude drafts, Steve edits, never
  committed by Claude.
- **X posts pulled from either column:** draft only, heavier editing
  expected than the long-form draft.

---

## Uncertainty Rule (stricter than the sister projects)

nfl-draft-model and shadow-leaderboard both publish a probability RANGE
wrapped around an underlying point estimate or residual. Boxscore
Prophet does not have a point estimate to wrap -- the published number
IS a probability, and that's the whole pitch (see `MODEL_BRIDGE.md`,
"What's actually different"). Practical rules that follow from that:

- Never publish a point projection, ever, in any format. There is no
  "he'll score 14.2 points" sentence available to write here -- if a
  draft contains one, it's a mistake, not a style choice.
- Displayed probabilities are editorially capped at 2-95%: the model
  never claims certainty in either direction. Raw values in the CSVs
  can exceed the cap; display copy uses the capped figure.
- Translate every percentage into plain English on first use in a
  section: "clears a startable week two times in five," not "43%"
  standing alone. Round to whole percent in copy.
- A probability is a chance, never a promise. When a pick can miss, the
  copy owns that up front, every time, not just in a disclaimer footer.

---

## Voice Guardrails

These are standing feedback, not per-column judgment calls.

- **No gambling language, ever.** No "odds," "fade," "price," "sharp,"
  "the house," "lock." Fantasy vernacular (boom, flex, waiver wire,
  streamer) is fine and encouraged. Probabilities are "chances."
- **The model is "the model," third person.** Established voice from the
  first published piece (`content/2025_w15_movers_column.md`) refers to
  "the model" doing things ("the model does not care how his name
  sounds," "the model already told you") rather than first-person-plural
  "our model." Keep this consistent -- it reads as more clinical/honest
  than a brand-voice "we," which fits the grading-in-public identity.
- **Direct address to the reader is fine and used often** ("you did not
  need me... to arrive at 'play your studs'"). This is a smart-friend-
  at-the-bar voice, not a report.
- **Never invent a reason the context columns don't support.** If the
  driver behind a mover is unclear, say so plainly -- "the model moved,
  the why is muddy" is an honest sentence and part of the brand.
- **Injury-driven movers are labeled as report-driven, not
  matchup-driven.** Don't dress up a designation-status move as a
  schedule read.
- **Descriptive stats (season-to-date leaderboards, box scores) stay
  visually and verbally separate from model probabilities.** A reader
  should never have to guess which number is a model output and which
  is just what already happened.
- **Never claim the delta framing (movers) as validated model skill.**
  A week-over-week delta is arithmetic on two published numbers, not a
  separately-tested claim. Don't oversell it as a discovered signal.
- **A bad grading week is content, not a problem to minimize.** On the
  Record exists specifically to survive a bad week in public. Do not
  let the copy hedge, bury, or reframe a real miss.

---

## Vocabulary

- **"On the Record"** -- branded term for the Wednesday grading Note.
  Capitalize when referring to the franchise.
- **"Movers"** -- players whose P(start) or P(boom) moved most against
  their own trailing published baseline. Not a synonym for "top of the
  board."
- **"Receipts"** -- the underlying graded-calibration artifact (10d).
  Used both as the internal file name and in reader-facing copy ("we
  grade every one of these in public").
- **"Startable" / "boom"** -- always paired with "week," always tied to
  the position-specific published threshold (RB/WR 15/20, TE 12/17, QB
  20/25 standard scoring). Never used as a standalone adjective for a
  player ("he's startable" is fine in narrative voice; don't invent
  "start score" or "boom rating" as branded metric names).
- **"Chances," never "odds."** See Voice Guardrails.
- **"Board" / "boards"** -- the full per-position ranked tables. STAY
  SCARCE. Never published free in full; the paid tier is the future home
  for full boards. Columns reference specific rows from a board; they do
  not reproduce it.
- **"ECR"** -- FantasyPros Expert Consensus Rankings, always attributed
  on first use in a piece that cites it ("Data: FantasyPros ECR").

---

## Formatting

- ASCII only -- no unicode dashes/quotes/arrows in any published draft
  (matches the global output-encoding rule; use "->" for arrows if
  needed in methodology asides).
- No em-dashes, period -- not the unicode character, and not "--" as
  an ASCII stand-in either. Confirmed 2026-09-21: Steve doesn't want
  the em-dash construction (aside/interruption) at all, in any
  encoding. Rewrite as two sentences, a comma, a colon, or parentheses
  instead. A hyphen for ranges or compound words ("600-900 words,"
  "week-two") is unaffected -- this only bans the dash-as-aside move.
- Percentages: whole numbers in copy, no decimals ("43%" not "43.2%").
- On the Record and Movers are Substack Notes: 150-200 words each,
  plain short-form text -- no section headers, no tables, no
  bullet-heavy layout (bolding a player name is fine). Changed
  2026-09-24 from 400-700 / 600-900 word articles.
- Thursday preview article: long-form, length at Steve's discretion.
  Section headers there may use pop-culture riffs in Steve's style;
  never forced if nothing fits that week.

---

## What Claude Should Never Generate as Content Here

- A point fantasy-point projection in any form ("he'll score 14 points").
  See Uncertainty Rule above -- this is a harder line here than in the
  sister projects.
- Betting picks, value plays, or anything readable as odds/wagering
  content. FantasyPros and sportsbooks are not this brand's lane.
- A clean-week-only On the Record column that quietly omits a bad miss.
- Full board reproductions in either column -- boards are the scarce/paid
  asset.
- Player character claims (work ethic, "wants it more," locker-room
  intangibles). The model sees usage and matchup, not people.
- A delta (movers) framing presented as a validated, separately-tested
  signal rather than arithmetic on two published numbers.
- Injury-status moves narrated as if they were matchup or schedule
  insight.
