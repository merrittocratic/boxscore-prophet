# Boxscore Prophet -- Standing Rules

Auto-loaded every session in this repo, so kept short on purpose --
these are the rules that must never depend on an agent happening to
open the right file. Narrative/architecture context (how the model
works, project history, current state, arming checklists) lives in
`HANDOFF.md`; voice/format contract in `CONTENT_GUIDE.md`; cross-project
translation in `MODEL_BRIDGE.md`. Read those on demand -- this file is
the one that's always loaded.

## Authority

Steve (owner) decides all bars, ships, and spends. Nothing publishes
without his explicit yes.

## Content artifacts

- Written prose meant for Substack/X (movers columns, On the Record
  columns, lede-in posts, fact sheets) is NEVER a repo artifact. It
  goes to `~/content/draft/` (Steve's personal workspace, outside this
  repo), never into this repo's `content/` folder. Established
  2026-08-23 after a cleanup found loose drafts sitting in `content/`.
- Material prepared for "Cousin Claude" (Steve's external writing-help
  Claude instance, no repo access) defaults to CHAT OUTPUT ONLY -- do
  not write a file for it, not even to `~/content/draft/`, unless Steve
  explicitly asks for a real draft artifact. Confirmed 2026-09-05: this
  repo has hooks/sub-agents that fire when a new file lands, so writing
  a file here is never a free/neutral action -- default to surfacing
  text in the terminal for copy/paste instead.
- What DOES stay in this repo's `content/`: chart-generating CODE and
  already-published/committed brand assets. Finished artifacts or
  code, never drafts-in-progress.

## Data/model discipline

- Single-writer rule: `data/deployment_params.rds` and
  `data/deploy_models/` are committed ONLY by Earnest, in-season.
  Manfred: checkout before commit; exception only for coordinated ship
  passes.
- Nothing enters a trained feature set unless it's point-in-time
  reconstructable as of Friday lock. Beat-reporter/text signal is
  banned from training for this reason -- it lives in a live override
  layer instead, graded in-season, not trained on.
- Every experiment is pre-registered: expectation, decision rule, and
  pass/fail bars stated before code runs. Bars never move after data
  arrives; overrides are signed, not laundered.
- Nulls get published with receipts, not buried.

## The bar the model has to clear

Calibration (a stated probability happens about that often) is
necessary but not sufficient -- it's the entry price for a probability
to mean anything, not the product. The real bar: does the model's
prediction carry real information beyond what a market/consensus
baseline (FantasyPros ECR, Vegas-implied lines) already gives a reader
for free, today. As of 2026-09-05 this has not been measured -- no
historical ECR archive exists to backtest against (FantasyPros' API
serves live-week rankings only, no history), and the live weekly-drop
accumulator (`R/10f_weekly_eval.R`'s `ecr_baseline` column) needs 4+
in-season weeks before it's estimable. Proving genuine edge over a
market/consensus baseline -- not just calibration -- is the top
modeling priority right now, ahead of further content work. No public
differentiation claim ships until it's real and reproducible.
