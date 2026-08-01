# R/17a_map_policy_experiment.R
# Deployment-procedure experiment (NOT a ladder rung): should the recal maps
# refit weekly in-season, or stay frozen at the ship-pass fit?
#
# ==========================================================================
# PRE-REGISTRATION (drafted 2026-08-01; LOCKED upon Steve sign-off --
# the body below refuses to run until APPROVED is flipped)
# ==========================================================================
#
# THE SEAM (two legs, from the 2026-08-01 review):
#   Leg 1: deployed maps are frozen at the ship fit (through 2025) while the
#     backtest that validated them refit walk-forward each week -- by late
#     season, production maps have never seen a current-season outcome.
#   Leg 2: maps were fit on backtest FOLD-model predictions but score the
#     weekly-retrained DEPLOYED model's predictions, which drifts away from
#     the fold models as the season accrues (pred-vol seam class).
#
# WHAT THIS EXPERIMENT CAN AND CANNOT MEASURE: the 2025 pretend-deploy
# sizes LEG 1 cleanly. Leg 2 cannot be measured until a live season's
# ledger exists (fold preds ARE the walk-forward preds here, so both
# policies see the same prediction distribution). Leg 2 argues
# directionally for refit; it is stated here so the decision is made with
# eyes open, not silently folded into the result.
#
# DESIGN (pretend-deploy on the 2025 season; fold preds = production sim,
# per "deployment is one more fold"):
#   POLICY A (frozen):   maps fit ONCE on fold predictions through 2024,
#                        applied unchanged to every 2025 week.
#   POLICY B (refit):    maps fit for each 2025 week W on fold predictions
#                        through W-1 (2025 weeks included as they accrue).
#                        This is EXACTLY the shipped 6c/12e/9b walk-forward
#                        -- Policy B's numbers already exist in the recal
#                        probability files; only Policy A is new compute.
#   HELD FIXED in both arms: the method PER CELL (the terminal-round
#     judge's picks -- platt_vegas etc.). 17a varies the fitting WINDOW
#     only; it does NOT re-open method selection.
#
# METRICS (2025 season, per position x threshold = 8 pools):
#   M1 pooled |stated - empirical| (calibration, pp)
#   M2 Brier score
#   SECONDARY (report, not decision): same metrics on weeks 10+ only,
#     where the frozen-map gap is largest; and per-cell deltas under the
#     ladder cell machinery (4pp / n>=400).
#
# PRE-COMMITTED DECISION RULE:
#   POLICY B (weekly refit) ships for 2026 IFF
#     (a) mean |stated-empirical| across the 8 pools improves (B < A) by
#         >= 0.5pp, AND
#     (b) no single pool worsens under B by >= 1.0pp, AND
#     (c) mean Brier does not worsen.
#   OTHERWISE POLICY A (frozen) ships -- pre-stated tiebreak: one season
#   cannot resolve fine gradations, and the less-adaptive procedure is the
#   safer default for a first live season. Either way the shipped policy
#   is documented as a D-entry and 10f monitors it live (its drift alarms
#   are the in-season check on whichever policy wins).
#   IF B ships: production refit uses fold-era rows + the accumulating
#   2026 LEDGER rows (the deployment's own pre-kickoff predictions vs
#   outcomes), which is what addresses Leg 2; the weekly refit slots into
#   Tuesday full mode between 10a and the slates.
#
# EXPECTATION (stated before running): mostly a wash on full-season pooled
# metrics (one season of extra data on a 10-season window is ~5% of rows);
# any real separation should appear in the W10+ secondary cut. Prior:
# tiebreak fires, A ships, and the decision value is having TESTED the
# frozen procedure the backtest never ran.
# ==========================================================================

APPROVED <- FALSE   # flip to TRUE only on Steve sign-off of the rule above

if (!APPROVED) {
  cli::cli_abort("17a pre-registration awaiting sign-off -- body does not run until APPROVED <- TRUE (and the header is locked verbatim at that point).")
}

# Implementation lands after sign-off:
#   1. Fit Policy-A maps: per deployed cell, the judged method fit on fold
#      preds with season <= 2024 (reusing the 6c/12e/9b fitting helpers).
#   2. Apply to 2025 fold preds; Policy-B columns read straight from
#      output/06c_recal_probabilities.csv / 12e / 09b.
#   3. Emit output/17a_policy_comparison.csv + verdict block per the rule.
