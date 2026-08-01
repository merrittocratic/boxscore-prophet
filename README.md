# boxscore-prophet

Calibrated weekly threshold probabilities for fantasy football, built on EPA
interval models. The published numbers are **P(FP >= 15)** (the start/sit
decision) and **P(FP >= 20)** (the boom indicator), PPR scoring, season-long
audience first.

The EPA models are infrastructure. The probabilities are the product.

## Architecture at a glance

```
  play-by-play (nflreadr)
        |
        v
  [1] Feature layer (frozen)          rb_feature_table v2.0 / wr_feature_table v1.0
        |                             efficiency and volume kept separate
        v
  [2] Rubric (frozen)                 204 walk-forward folds, pre-committed
        |                             decision rule, low-usage veto
        v
  [3] Bake-off  ---------------->     WINNER: 3A-v2 (tuned LightGBM +
        |                             power-law conformal intervals)
        v
  [4] WR clone of 3A-v2               same construction, WR features
        |
        v                             RB + WR = the projection spine
  [6] EPA -> PPR translation          spline regression + empirical simulation
        |                             (06 normal approx superseded by 06b)
        v
  [6c] Walk-forward recalibration     isotonic maps, fit on past weeks only
        |
        v
  P(FP >= 15), P(FP >= 20)            per player per week, honest out-of-time
```

Parked branches: 3C hierarchical Bayes as an RB upside model (see two-product
architecture below); stadium/weather features (deferred after content pivot).

## The modeling problem

A player's weekly fantasy outcome decomposes into **volume** (opportunities:
carries + targets) times **efficiency** (EPA per opportunity). These are
modeled separately and never collapsed, because they have different dynamics:
volume is role-driven and relatively stable; per-touch efficiency is noisy
and its variance depends on usage tier. The models predict *intervals*, not
points, because the product is a probability -- and an honest probability
requires an honest distribution, not just an accurate center.

## Decision log

Every consequential decision, with the reason it was made. Ordered roughly by
pipeline stage.

### D1. Efficiency and volume are separate models; grouping keys never collapsed

`player_id / defteam / season / week` are preserved intact everywhere, and
`epa_per_opp_obs` is never pre-multiplied into totals in the feature layer.

**Why:** Collapsing the nesting would foreclose hierarchical models (random
effects need the keys), and pre-combining eff x vol would hide which component
a calibration failure comes from. Every diagnosis in this project (committee-
back undercoverage, WR sigma problems) depended on being able to look at the
components separately.

### D2. Freeze discipline: feature table and rubric locked before any contender runs

`output/rb_feature_table_v2.0.csv`, `data/fold_map.rds`, and `R/metrics.R`
were frozen before step 3. All bake-off contenders ran against identical
inputs, splits, and metrics.

**Why:** Model comparisons are meaningless if the playing field moves between
contenders. Any post-hoc change to data or metric would confound "better
model" with "different conditions."

### D3. The rubric: coverage first, sharpness as tiebreak, low-usage veto

Pre-committed before results existed:
- PRIMARY: pooled combined 80% coverage closest to nominal
- TIEBREAK: narrowest mean 80% width among models within +-2pp of nominal
- VETO: low-usage-bucket combined 80% delta beyond +-10pp disqualifies
  regardless of pooled performance

**Why coverage first:** a sharp interval that misses is worse than a wide one
that covers -- the product is honest probabilities. **Why the veto:** pooled
metrics are dominated by high-volume players; a model can look calibrated
overall while quietly abandoning committee backs. The veto forces honesty for
the 5-8 touch tier. This proved decisive twice: it caught 3C (v1) and
disqualified 3E, whose winning pooled sharpness was bought with -9pp
undercoverage on workhorses.

### D4. Tuning objective is RMSE on inner holdout -- never coverage or sharpness

**Why:** Tuning to the decision criterion is gameable. A model tuned to look
calibrated is not the same as a model that is calibrated. RMSE is a neutral
objective; calibration is measured afterward, out-of-sample, by the frozen
rubric.

### D5. Interval construction: per-fold power-law conformal (Mechanism A)

Combined intervals use conformal quantiles on residuals normalized by
`opportunities^alpha`, with alpha fitted per fold by OLS on
log(|resid|) ~ log(opp), clamped to [0.20, 0.90]. Median fitted alpha ~ 0.51.

**Why:** total-EPA variance scales sub-linearly with opportunities --
workhorses are more consistent per touch than committee backs. Naive pooled
conformal (alpha=0) overcovered low-usage players by +12pp; linear
normalization (alpha=1) undercovered them by -11pp. The fitted power law
(~sqrt(opp), consistent with theory) resolved both. This construction is
frozen and shared by every contender so that learner quality is the only
thing being compared.

### D6. Bake-off verdict: 3A-v2 (nested-CV-tuned LightGBM) is the projection engine

Five paradigms competed on 11,918 RB player-games (2014-2025, 204 folds):
default LightGBM (3A), tuned LightGBM (3A-v2), RF + conformal (3B),
hierarchical Bayes Student-t (3C), TabPFN (3D), direct quantile LightGBM (3E).

Final v2 table (pooled combined 80% delta / width / low-usage veto):

| Model | Pooled | Width | Low-usage | Verdict |
|---|---|---|---|---|
| 3A default | -0.3pp | 11.17 | -15.1pp | vetoed |
| **3A-v2 tuned** | **-0.3pp** | **9.24** | **-0.8pp** | **winner** |
| 3B RF | -0.1pp | 9.63 | +0.8pp | clean, wider |
| 3C HierBayes | +0.6pp | 10.32 | +13.4pp | vetoed |
| 3D TabPFN | crashed (reticulate segfault) | | | not blocking |
| 3E quantile | -1.8pp | 8.77 | +8.6pp | disqualified (stratified) |

**Why 3A-v2:** within 1pp of nominal across every touch stratum AND the
sharpest surviving model. **Why 3E lost despite winning pooled sharpness:**
its width was bought with -9.0pp undercoverage on 14+ touch players -- the
primary audience of a projection product. Stratified honesty outranks pooled
sharpness. **Why the 3C veto was not "fixed":** re-running 3C with a
different combined construction to pass would have confounded the paradigm
comparison. Its veto stands; its insight is used elsewhere (D7).

### D7. Two-product architecture: two rosters, not two numbers per player

The bake-off's deepest finding: every point-estimator + conformal approach
missed the committee-back efficiency tail by 9-12pp regardless of learner;
only 3C's Student-t with volume-conditional sigma calibrated it. No single
model was honest for both workhorses and dart throws.

Resolution -- a product decision, not a modeling patch:
- **Projection model (3A-v2):** high-touch roster. "What should I expect
  from the players I'm starting?"
- **Upside model (3C):** low-touch roster. "Which fringe guy has a tail
  worth streaming?" (RB only for now -- see D9.)

Players are routed to exactly one product by predicted role/volume.

**Why two rosters instead of running both models on everyone:** picking which
number to show per player recreates the switching-leaderboard seam the
architecture exists to avoid. A player moving between products when his role
changes reads as news, not as a modeling artifact.

### D8. WR clone (04a/04b): same construction, wider intervals accepted

The WR projection model is a direct clone of 3A-v2 -- identical tuning grid,
split protocol, and interval construction, WR-specific features. Passed the
rubric: pooled -0.5pp, all strata within +-3pp, width 10.10 (vs RB 9.24).

**Why the wider width was accepted:** weekly target volume is genuinely more
volatile than carry volume. Forcing RB-level sharpness would cost coverage.

### D9. WR 3C parked; no prior widening

The WR upside model failed twice (sigma ~ log_opp, then + air yards): WR
per-target EPA is broadly more dispersed than the Student-t allows at the
fitted sigma level. The failure is the sigma *level*, not the sigma *driver*.

**Why parked rather than patched:** widening the sigma prior until the veto
passes is tuning toward the decision criterion (violates D4's spirit). WR
per-target EPA has a different distributional character (deep-shot right
tail) than RB per-carry EPA. The two-product architecture may be RB-only;
that is an acceptable outcome. Revisit only with a fundamentally different
distributional spec.

*Resolved 2026-07-17: retired, not revisited -- see D14. The failure lives
at the efficiency-component level, which the product never ships; the
combined-level chain is honest on the WR streamer roster without a second
engine.*

### D10. The product is P(threshold), not a point projection

Published numbers: P(FP >= 15) for start/sit, P(FP >= 20) for booms. PPR.
Season-long players first, DFS later if warranted.

**Why:** a calibrated probability is one number per player per week that
moves as news breaks and resolves cleanly on Monday -- the same mechanics
that make win probability work as golf content. Every platform publishes
point projections; almost nobody publishes calibrated threshold
probabilities. The EPA interval machinery exists precisely to make these
probabilities honest. Corollary: all modeling decisions are judged by whether
they improve P(15+)/P(20+) calibration, not EPA metrics for their own sake.

### D11. Translation layer: regression bridge, empirical simulation, no parametric tails

`06_fp_translation.R` fits per-position OLS `fp_ppr ~ total_epa +
opportunities` (b_epa ~ 1.0 for both positions: one point of EPA is one PPR
point; the opportunities term carries the reception bonus). Its v1 normal
approximation failed its own calibration check exactly where a normal should
fail on right-skewed scoring: overconfident about must-starts at 15+,
understated booms at 20+ (residual skewness RB 0.90, WR 1.19).

`06b_fp_simulation.R` (v2) replaces the normal with simulation:
- EPA and volume drawn from each player-week's own conformal quantile points
  (piecewise-linear inverse CDF -- no distributional assumption)
- eff/vol error dependence via Gaussian copula, rho estimated from fold
  errors (RB 0.03, WR 0.12)
- translation noise resampled from actual OLS residuals, binned by volume
  tier (low-tier skew 1.4-1.8 vs ~0.7 for workhorses -- dart throws are
  boom-or-bust in translation too)
- P(20+) <= P(15+) coherent by construction (same draws)

**Why simulation over skew-t or a direct classifier:** it adds zero new
parametric assumptions -- every distortion in real scoring (TD lumpiness,
the floor near zero) is replayed from data. The EPA spine stays
scoring-format-agnostic: half-PPR or DFS scoring is a new cheap regression,
not a new model. A direct P(threshold) classifier was considered and
deliberately deferred: 3E already demonstrated where direct target-learning
cuts corners, and a classifier forfeits the interval infrastructure and
multi-threshold coherence. It remains the fallback experiment if the
translation route stalls (bake it off on Brier + stratified deciles).

**Data note:** the v1 run surfaced an NA-player_id cartesian join (dplyr
matches NA to NA) that inflated WR output 6x and polluted the v1 WR
regression fit. Fixed by filtering NA ids before all joins.

### D12. Current calibration status and the open WR gap

n-weighted mean |calibration delta| after v2 (2026-07-05):

| Position | 15+ | 20+ | Status |
|---|---|---|---|
| RB | 1.9pp | 1.3pp | publishable |
| WR | 2.3pp | 2.2pp | boom bins still understate +6-7pp |

The gap was chased hard (2026-07-05 session) and the negative results are
as informative as the wins:
- **Asymmetric conformal (04c)**: signed-residual upper/lower quantiles,
  04b learner unchanged. Passes the rubric, slightly sharper than 04b, and
  fixes the WR volume upper tail (target eruptions: above-90% arm exceedance
  8.2% -> 5.7%). Kept as the WR interval source. Did NOT move FP calibration.
- **Spline translation**: the linear EPA->FP fit had U-shaped residuals by
  EPA decile (TD convexity: a TD is ~6 FP but little EPA). ns(total_epa, 4)
  flattens them and FIXES the must-start overconfidence -- but worsens boom
  bins, revealing that the linear model's mid-range inflation had been
  accidentally compensating the boom miss (two canceling errors).
- **Ruled out by direct diagnostics**: EPA interval tails (marginal and
  conditional on prediction level), volume tails (after 04c), EPA-error x
  translation-residual dependence (~0), joint tail dependence (mild). An
  oracle test (true EPA/volume through the translation layer) is nearly
  calibrated, so no single structural culprit remains; the residual +5-7pp
  boom understatement is several ~1-2pp effects compounding.

Resolution: D13.

### D13. Walk-forward recalibration layer (6c) -- the last inch

`06c_recalibration.R` maps stated probability to empirical rate with a thin
final recalibration: four maps (position x threshold), refit weekly on all
PRIOR weeks only and applied forward (2014-15 burn-in, evaluated 2016+).
Isotonic and Platt competed under a pre-committed rule (n-weighted mean
|bin delta| primary, Brier-must-not-degrade sanity); **isotonic won all
four**, with Brier improving everywhere.

**Extended 2026-07-17 to volume-conditional maps.** The pooled maps were
honest on average but blind to who the probability belongs to: stratifying
the 07-05 run by EX-ANTE predicted volume (pred_vol -- deployable
pre-kickoff; observed volume would condition on game script, the QB 08b
false-STOP lesson) showed the WR streamer stratum (<= 5 projected targets)
understating P(15+) by +3.8pp and the WR high-volume stratum overstating
P(20+) by -6.7pp. Three volume-aware candidates entered the bake-off
(stratified Platt, stratified isotonic with pooled fallback under 300 train
rows, and Platt-with-volume-covariate), judged by an updated pre-committed
rule: n-weighted mean |delta| over (stratum x bin) cells -- stratified
because the pooled metric is exactly what the incumbents saturate -- with
the same Brier-must-not-degrade sanity check. **Stratified isotonic won all
four maps**, beating the incumbent pooled isotonic out-of-time.

Judge metric (stratified weighted |delta|), raw -> pooled iso -> strat_iso:

| Map | Raw | Pooled iso | Strat iso |
|---|---|---|---|
| RB 15+ | 4.13pp | 2.50pp | 1.20pp |
| RB 20+ | 2.63pp | 1.84pp | 0.79pp |
| WR 15+ | 3.21pp | 1.41pp | 0.69pp |
| WR 20+ | 3.14pp | 1.24pp | 0.76pp |

Stratum-level honesty after the fix: every position x threshold x stratum
cell within +-1.3pp (WR streamer P(15+) +3.8pp -> +1.1pp; WR high-volume
P(20+) -6.7pp -> -1.3pp). Note: the Platt-with-covariate candidate posted
the best Brier scores (its volume term adds resolution) but worse
calibration cells; the pre-committed rule is calibration-first because
resolution belongs to the upstream models -- this layer's only job is
honesty.

**Why this is not a fudge:** it is graded exclusively on weeks it never saw,
the same walk-forward discipline as every model stage. And it was only
built AFTER every structural layer was individually verified honest -- a
recalibration slapped on top of broken structure would mask defects; on top
of verified structure it corrects the compounding of small residual leaks.
**Why isotonic:** the miss was nonlinear in stated probability (small at the
bottom, growing through the middle bins); a two-parameter Platt curve
underfit it.

Known blemish: the extreme top tail (stated 15+ probability above ~0.7, or
20+ above ~0.5) is a few player-weeks per season and stays noisy. Editorial
guidance: cap displayed probabilities (publish ">70%") rather than quote
precise numbers there.

Deployment artifacts: `data/fp_recal_maps.rds` (maps refit on all history;
every map has uniform signature `function(p, pred_vol)` and is
self-contained -- closures bind everything locally, no globalenv
dependencies, so the production runner can readRDS in a fresh session) and
`output/06c_recal_map_grid.csv` (maps evaluated at each stratum's median
pred_vol). Weekly flow: 06b simulation probabilities -> apply maps ->
publish.

### D14. WR 3C retired: the upside question was a calibration question

The revisit of D9 (2026-07-17) reframed it: WR 3C failed at the efficiency
component, but the product ships combined-level FP probabilities, and at
that level the deployed 04c chain was already honest on the streamer
roster (80% interval covers 80.5% on 3-5 target WRs) -- the efficiency
miss washes out against the volume component. The only real defect was the
conditional probability miscalibration fixed by the D13 extension. So the
RB rationale for a second engine (projection models abandoned the
committee-back tail at the COMBINED level) never applied to WR.

**Decision:** WR two-product = one engine (04c), two rosters at the
presentation layer, routed by ex-ante predicted targets. `05_wr_3c_*` is
retired history, not parked work. RB keeps its 3C upside engine (its
low-touch roster genuinely needs one), but RB 3C is not yet wired through
FP translation -- that belongs to deployment scoping.

### D15. QB translation layer (9a/9b): components stay separate, raw mostly wins

`09a_qb_fp_simulation.R` is the 06b analog on the 08c hybrid spine, with
the differences pre-specified in the 08c wrap-up: standard scoring (4pt
pass TD, no PPR), thresholds 20/25, and the regression keeps pass and rush
separate -- `fp ~ ns(pass_epa) + dropbacks + rush_epa + carries` -- because
the FP exchange rate differs by component (rush TDs 6pt vs pass 4pt).
The simulation draws all four 08c component intervals under a 4x4 Gaussian
copula from standardized fold errors (rush_epa x carries came in at +0.23,
the one dependence the design notes predicted would matter) and derives
pass_epa = pass_eff x dropbacks per draw. Residual pools are binned by
carries tier (statue/mover/scrambler, 4/8): the scrambler pool is
fatter-right-tailed (skew 0.52 vs 0.27), which is rush-TD lumpiness.

**The functional-form gate earned its keep:** the first fit omitted
dropbacks (pass volume "already inside" pass_epa) and showed a monotone
residual trend from -4.4 FP to +4.5 FP across dropback deciles. FP pays
for yardage ACCRUAL that EPA does not credit -- a 40-dropback zero-EPA
game still banks ~10 FP of passing yards. With dropbacks as a term the
trend flattens to +-0.5.

Raw simulation calibration: 1.2pp weighted error at 20+, 1.1pp at 25+,
beating the normal baseline at 20+ (2.3pp) -- QB skew is milder than
RB/WR, as feasibility predicted.

`09b_qb_recalibration.R` runs the same conditional bake-off as 6c with
strata on ex-ante pred_carry. Verdict, per the pre-committed judge:
**raw wins at 20+** (every recal method degraded Brier -- the simulation
is already honest there; the deployed map is the identity) and **pooled
Platt wins at 25+** (1.07pp -> 0.24pp). No conditional method survived --
the QB probability surface is not conditionally mispriced the way WR was.
Watch items, documented not patched: movers +3.4pp cold at 20+ (n=809,
borderline); scramblers -4 to -5pp hot (n=132, inside noise, and
opposite-signed to the same check under prior_carries_pg conditioning --
the signature of noise, not structure). Deployment:
`data/qb_fp_recal_maps.rds`, same `function(p, pred_vol)` signature as
RB/WR with pred_vol = pred_carry.

## Repository map

```
R/
  build_rb_feature_layer.R      [1] RB feature table (frozen v2.0)
  build_bakeoff_rubric.R        [2] fold map + decision rule (frozen)
  metrics.R                     [2] shared metrics (frozen)
  03a_lgbm_control.R            [3] contender: LightGBM defaults
  03a_interval_construction.R   [3] Mechanism A power-law conformal (shared)
  03a_v2_lgbm_tuned.R           [3] WINNER: nested-CV-tuned LightGBM
  03b_rf_conformal.R            [3] contender: random forest
  03c_hierarchical_bayes.R      [3] contender: Student-t HierBayes (upside engine)
  03d_tabpfn.R                  [3] contender: TabPFN (crashed in v2)
  03e_quantile_lgbm.R           [3] contender: direct quantile (disqualified)
  04_bakeoff_results.R          [3] rubric adjudication
  04a_wr_feature_layer.R        [4] WR feature table (frozen v1.0)
  04b_wr_lgbm_tuned.R           [4] WR projection model (3A-v2 clone)
  05_wr_3c_hierarchical_bayes.R [5] WR upside attempt (RETIRED, see D14)
  06_fp_translation.R           [6] EPA -> PPR regression + v1 normal approx
  06b_fp_simulation.R           [6] simulation probabilities (spline + 04c)
  06c_recalibration.R           [6] walk-forward recalibration, volume-
                                    conditional stratified isotonic (final)
  07_qb_feasibility.R           [7] QB feasibility: 4pt TD, thresholds,
                                    two-component requirement
  08a_qb_feature_layer.R        [8] QB feature table (frozen v1.0)
  08b_qb_interval_construction.R[8] hybrid vs symmetric bake-off; const
                                    mechanism locked; ex-ante judge lesson
  08b2_qb_scrambler_check.R     [8] scrambler deep-dive (cleared)
  08c_qb_lgbm_tuned.R           [8] QB spine: tuned hybrid model, veto passed
  09a_qb_fp_simulation.R        [9] QB FP translation + 4-component copula
                                    simulation (thresholds 20/25)
  09b_qb_recalibration.R        [9] QB walk-forward recalibration (raw at
                                    20+, Platt at 25+)
  12_te_feasibility.R           [12] TE feasibility: thresholds 12/17,
                                    blocking state, aDOT, elite concentration
  12a_te_feature_layer.R        [12] TE feature table (frozen v1.0; WR clone
                                    + wt_tgt_per_snap role feature, 7-air-yard
                                    defensive split, NA-receiver filter)
  12b_te_lgbm_tuned.R           [12] TE projection model (04b clone)
  12c_te_asymmetric_conformal.R [12] TE asymmetric intervals (04c clone)
  12d0_te_predvol_rescale.R     [12] pred-vol rescale (06b0 clone)
  12d_te_fp_simulation.R        [12] TE FP simulation at 12/17 (06b clone,
                                    single position, predvol input default)
  12e_te_recalibration.R        [12] TE walk-forward recalibration (06c
                                    clone; platt_vol at 12+, strat_platt 17+)
  read_stathead.R               data utility

data/    RDS intermediates (feature tables, outcomes, fold map)
output/  frozen CSV artifacts: feature tables, fold predictions, coverage
         tables, calibration tables, deployment params (06b_sim_params,
         06b_resid_pools)
logs/    run logs for reproducibility
```

Deployment artifact dependency note: the recalibration maps
(`fp_recal_maps.rds`, `qb_fp_recal_maps.rds`) are base-R self-contained,
but the translation fits (`fp_translation_fits.rds`,
`qb_fp_translation_fit.rds`) use `ns()` -- the production runner must
`library(splines)` before calling `predict()` on them.

### D16. RB 3C wiring veto FAILED: single-engine RB, streamer cut moved to 10

The 3C upside engine was wired through translation (06e: shared translation
fit + pools, engine-independent; 3C intervals + own copula rho) and
recalibration (06f: same conditional bake-off, strat_iso won both maps),
then graded by a veto pre-committed BEFORE the run: ship for the streamer
roster (3C pred_vol < 10) only if honest there (|delta| <= 2pp) AND Brier
no worse than the shipped 3A-v2 chain on the same player-weeks.

Result: honest yes (-0.9pp / -0.5pp), Brier NO -- and against the FINAL
incumbent (strat_iso after the stratum re-cut below), the incumbent is
both honest on the roster (+1.3pp / +0.7pp) and clearly sharper (Brier
0.0829 vs 0.0982 at 15+; 0.0338 vs 0.0383 at 20+). The Bayes intervals
are wide, which pushes probabilities toward the base rate: calibrated in
the mean but weaker at separating the 4%-tail streamer from the 15%-tail
one -- and discrimination is what a streamer board is for. **Both
positions therefore run single-engine (the D7 two-product split survives
as a roster/presentation split), and 3C is retired for deployment at both
positions.** 06e/06f artifacts kept as the receipts.

The veto's real payload was a defect in the SHIPPED chain: the 6c RB low
stratum cut at pred_vol <= 8 held only 57 rows (volume-model shrinkage --
observed 10th pctile is 6 opportunities, predicted is 9), so the streamer
band always hit the pooled fallback and ran +3.25pp cold where the router
actually cuts (< 10). Fix: RB low boundary moved 8 -> 10 in 6c and the
bake-off re-run under the unchanged judge.

### D17. TE ships as a WR-spine derivative: role feature, position-calibrated thresholds (2026-07-19)

TE was the last uncovered position. Feasibility first (12_te_feasibility.R,
house discipline): the WR 15/20 cuts hit at only 17.4%/7.5% of TE starter
games (vs the 26.9%/13.4% reference rates), 26.5% of high-snap TE weeks
fall under the 3-target floor (vs 10.7% WR -- the blocking state; snap
share does not imply targets for TEs), TE median aDOT is 6.9 vs WR 10.4,
and the top 12 TEs take 41.8% of positional FP (vs 21.1% WR). One stated
expectation FAILED honestly: TD-gating of booms is NOT worse than WR once
thresholds are rate-matched (boom w/o TD 3.0% vs 2.8%) -- no extra TD
machinery built.

Build = the frozen WR chain with three receipt-backed changes:
  1. Thresholds 12 (start) / 17 (boom), rate-matched to RB/WR hit rates.
  2. wt_tgt_per_snap role feature in the volume model, built on the SNAPS
     table with zero-target blocking games filled as 0 (the outcome table
     never sees those weeks).
  3. Defensive short/deep split at 7 air yards (WR's 10 starves the deep
     component for TEs).
Everything else frozen: same fold map, grids, Mechanism A, asym conformal,
spline + simulation translation, conditional recal bake-off.

Results: 12b passes the rubric (pooled 80% -0.4pp, low-usage veto -0.4pp);
12c passes identically (TE EPA intervals are nearly symmetric -- the FP
right-skew lives in the TD residual pools, low-tier skew 1.78, captured by
12d); simulation beats normal 3.9->1.5pp (12+) and 2.6->0.9pp (17+)
n-weighted |delta|; 12e picks platt_vol (12+) and strat_platt (17+) under
the unchanged judge, fixing the high-volume stratum from +2.1pp to +0.2pp.
Watch item: exante_low (pred_vol <= 4) holds only 315 rows -- volume-model
shrinkage, the same signature the RB 8->10 re-cut fixed; re-cut if the
streamer board runs thin. The TE feature table is built with the
NA-receiver filter, so the WR ghost-row flag does not apply to TE.

RNG lesson (found by the recon gate): the TE simulation runs LAST in 10c.
Inserting it before QB shifted the shared draw stream and jittered a
published QB row 2.6pp over the 10pp reconciliation flag. Position sim
order is now part of the published-number contract.

### D18. Rung 2 (Vegas lines): "expect flat" FAILED -- signal at all four positions (2026-07-19)

The pre-registered expectation was wrong, and the receipt is the story.
The 13a diagnostic (pre-committed proceed rule, upper-bound probe with
nflverse CLOSING lines) found the volume models flat vs Vegas (the
decomposition's "volume is solved" held everywhere) but a monotone
TOTAL-EPA residual gradient in implied team total at every position --
QB -2.67 -> +2.42 EPA across implied buckets, RB -0.35 -> +1.16, TE
-0.42 -> +0.78, WR -0.51 -> +0.76 -- which propagates to +-4-12pp
conditional dishonesty in the published FP tails (32 trigger cells; QB
start in high-implied games understated by 11.6pp). The efficiency
models see opponent defense but not the Vegas-priced environment.

A/B (13b RB/WR/TE, 13c QB; arm B = c(team_spread, implied_total) added
to the EFFICIENCY feature set only, volume/other components REFIT from
shipped tune logs and verified to reproduce shipped predictions at
~1e-15). Pre-committed acceptance: rubric intact, gradient shrink >=
50%, sharpness within +2%. ALL FOUR POSITIONS PASS:

  pos  gradient (EPA)     shrink   RMSE     80% width
  QB   5.09 -> 1.53       70%      -1.9%    27.13 -> 26.71
  RB   1.51 -> 0.47       69%      -0.9%     9.23 -> 9.17
  WR   1.27 -> 0.26       80%      -0.6%    10.31 -> 10.24
  TE   1.20 -> 0.47       61%      -0.7%     8.70 -> 8.63

Intervals NARROW while honesty improves -- Vegas is genuinely
informative for per-touch efficiency, not width-buying.

LINE-SOURCE RESOLUTION (2026-07-19, same day): OPENING lines -- posted
Sunday night/Monday, unambiguously available at Tuesday build and
Friday lock -- were tested as the free point-in-time-honest variant
before buying anything. Source: aussportsbetting historical file
(openers + closers, 2006-2024; fetched via Wayback, Cloudflare gates
the live site; data/vegas/ gitignored like data/ecr). Join validated
at 99.86% match with closer-fingerprint r=0.997 vs nflverse (13d0).
Open->close movement: spread sd 1.9 pts, p90 = 3.

Opener A/B, same pre-committed bar (closer-arm shrink in parens):

  pos  opener shrink   verdict
  WR   85.6% (80%)     PASS -- opener BEATS the closer arm
  QB   69.1% (70%)     PASS -- opener costs ~nothing
  RB   64.0% (69%)     PASS
  TE   49.3% (61%)     FAIL by 0.7 points on the 50% bar

TE DECISION (Steve, 2026-07-19, recorded as an owner's call -- the
50% bar itself is NOT widened): TE ships with the opener feature
despite the near-miss. Rationale: rubric and sharpness pass, RMSE
improves (-0.36%), gradient still halves (1.17 -> 0.60 EPA), and the
alternative was ~$100 of timestamped-archive data whose estimated
effect on TE is under one point of shrink -- paying for a pass mark,
not product quality. Conditional on the other three passing; they did.

NO ODDS PURCHASE. Recurring line-data cost: $0. Ship-step notes:
production needs a weekly line fetch at Tuesday build (train-on-opener
vs serve-on-Tuesday-line skew is 1-2 days of movement, smaller than
open->close -- size it in the ship step per the 06b0 lesson); the 2025
season has no openers in the archive (trained-feature NAs there;
backfill options exist if it matters).

Harness finds along the way: (1) the TE feature table carries ONE
duplicated player-week (Conklin 2021-W18, a 12a snap-crosswalk dup; two
pfr_ids -> one gsis_id duplicates the row and the second copy's rolling
window sees the first). Sized: 1 row of 6,765, immaterial; 12a/10b5
dedupe queued for the next feature-table version. (2) The WR arm
initially failed reproduction because the harness filtered the
NA-player ghost rows the shipped chain trained with -- reproducing a
frozen procedure means reproducing its quirks.

### D19. Rung 2 SHIPPED: three-layer Vegas integration, published floor (2026-07-26)

The ship pass that landed the D18 ablation, with two defects found and
fixed on the way -- the closure gate (13f, pre-committed: zero cells
with |stated-empirical| >= 3pp at n >= 500) refused the first two
attempts and each refusal located a real mechanism:

LAYER 1 (efficiency, from D18): opener team_spread + implied_total in
the EFF models (pass_eff for QB). 13e promotes the opener arms to the
canonical fold predictions (untouched components verified to reproduce
shipped at ~1e-15; QB pass_eff arms materialized via logged params).
LAYER 2 (translation, found by 13f round 1): conditional on the SAME
EPA and volume, FP ran +0.05 per implied point -- TD-per-EPA is
environment-dependent, invisible to any EPA-layer feature. Fix: opener
implied total (centered; NA 2025 -> neutral zero; center stored as
attr(fit, "it_center")) in the 06b/12d/09a translation regressions.
Post-fix translation residuals flat (+-0.04 FP) by environment.
LAYER 3 (recal, found by 13f round 2): 3-7pp game-script cells
remained -- compounded SUB-TRIGGER biases (e.g. big-dog RBs +0.8
carries over-projected), no single layer owning them. Fix: Vegas-aware
recal candidates (platt_vegas, platt_vol_vegas: logit(p) + spread +
|spread| + implied, smooth only) under an EXTENDED pre-committed judge
(union of volume/spread/implied cells; Brier sanity unchanged).
TERMINAL-ROUND declaration capped the adaptive iteration: the |spread|
term (round 2 of 2) was declared the last in-rung candidate change
BEFORE it ran. Picks: RB15/20 platt_vegas, WR15 strat_platt, WR20
strat_iso, TE12 platt_vol_vegas, TE17 platt_vegas, QB20
platt_vol_vegas, QB25 platt -- six of eight maps Vegas-aware.

RESULT (the published rung-2 floor): conditional-dishonesty trigger
cells 32 -> 2 on ex-ante (opener) buckets: QB starts in projected-close
games -3.9pp, TE starts on big underdogs -3.8pp. Closer-bucket view: 5
cells <= 4.9pp, mostly the open->close information gap (the measured
cost of $0 lines). The two floor cells are KNOWN LIMITATIONS, published
-- adjacent to the pre-registered game-script/opponent-front ladder
family, not patched further in-rung by declaration.

DEPLOYMENT: 10a joins the opener sidecar (data/vegas_open_lines.rds,
same pattern as injury states) + Vegas features in EFF lists; slates
carry team_spread/implied_total via vegas_slate_lines() (hindcast =
sidecar, live = schedules-at-build with sidecar fallback; train-on-
opener vs serve-at-build skew bounded by open->close movement, spread
sd 1.9 pts, absorbed by the recal layer per the pred-vol seam
precedent); deployed recal map signature widened uniformly to
function(p, pred_vol, team_spread, implied_total) with centers sealed
in the closures; 10c feeds the slate Vegas columns through (sim it_c
from attr(fit, "it_center") -- the first recon run caught the missing
term; regression fixed). Slate gates |diff|=0 x 12 (4 positions x 3
weeks); recon W13/W14 all bounds pass; W15 aggregates pass (r .963-.997,
|mean| <= .76pp) with ONE explained row-flag (Robinson WR boom -10.9pp:
~5pp normal all-data-vs-fold raw divergence amplified by a steep
high-stratum strat_iso step -- the documented iso-cliff class, same as
the accepted Egbuka flag in rung 1; 1 of 404 comparisons). WR20
strat_iso cliff behavior stays on the watch list. 2025-season openers
remain NA (neutral) pending backfill at rollover.

### D20. Rung 3 (weather): PUBLISHED NULL -- the market already prices it (2026-07-26)

The pre-registration expected a thin post-prediction adjustment; rung 2
changed the null before rung 3 ran (opener totals price weather), and
the diagnostic confirmed the absorption. 14a0 fetched kickoff-hour
HISTORICAL FORECASTS (as-of-lock discipline, never observed; Open-Meteo
archive floor = 2021, so 5 seasons, outdoor games only; 1,359/1,359
games, 0 failures; data by Open-Meteo, CC-BY 4.0). 14a measured fold
residuals (13e Vegas-era arms) and shipped-probability honesty by
wind / temp / precip bucket under a pre-committed rule (residual >= 0.8
EPA at n >= 300, QB >= 2.0; calibration >= 4pp at n >= 400 -- bars
wider than rung 2's for the half-length window).

NO TRIGGER. Verdict: NULL, published. No 14b, no weather features
anywhere in the trained chain; slate weather remains content flags.

WATCH ITEM (bar NOT widened post hoc): WR starts in 15+ mph wind read
-6.6pp at n=265 -- the exact cell the expectation flagged, but below
the pre-committed 400-row floor on only 52 windy games in the archive.
Re-examine as seasons accrue (~10 windy games/yr); becomes testable at
the pre-committed floor around 2027-2028.

### D21. Rung 4 (OL/opponent front): PUBLISHED NULL -- trenches priced too (2026-08-01)

Same market-conditioned posture as rung 3: the chain already carries
opponent EPA-adjustments (def_prior, def_*_epa_adj) and opener lines,
and Vegas moves on OL news, so the question was whether TRENCH-SPECIFIC
states add anything beyond both. 15a0 built team-week axes -- ex-ante
own-OL sack/hit/stuff rates allowed and defense front rates generated
(pbp, 2014-2025; prior-season fallback under 100 plays), OBSERVED
starting-5 continuity from snap counts (11a sizing precedent; 2020 and
2025 required OL/OT/OG label variants -- generic-label drift), and FTN
box/blitz tendencies (2022+ floor; short-era axis, weather precedent).
Coverage gates 97-99%; FTN join 96.6% on the REG denominator.

15a measured 13e fold residuals + shipped-probability honesty across 7
axes x tercile/state cells under the rung-3 rule (residual >= 0.8 EPA at
n >= 300, QB >= 2.0; calibration >= 4pp at n >= 400; locked 2026-08-01
pre-run). NO TRIGGER. Verdict: NULL, published. No 15b; no trench
features anywhere in the chain.

The pre-stated live candidates split: heavy-box vs RB is DEAD (flat to
slightly positive, +0.2..+0.6 across terciles). The QB TRENCH FAMILY is
the watch item (bar NOT moved post hoc): four correlated cells all
negative -- continuity-broken -1.37 EPA (n=592, se 0.45), own sack-rate
hi -0.96 (n=2040), opp sack-rate hi -0.84, own stuff hi -0.82 -- same
story (QBs behind compromised lines slightly overpredicted even after
market conditioning), every cell under the 2.0 QB bar, and the cells
share population so they are one lean, not four confirmations. Worst
calibration cell: RB starts vs blitz-hi -3.1pp (n=1262), under the 4pp
bar. If a future QB-specific rung picks this up, the continuity axis
must first be rebuilt EX-ANTE (Friday-lock reconstruction from injury
designations + prior-week line) -- 15a's version is observed-state
sizing only.

### D22. Rung 5 (rookie priors): PUBLISHED NULL -- ladder complete (2026-08-01)

The chain's existing rookie machinery -- draft-tier median cold-start
baselines + the volume-conditional walk-forward recal -- was the null
hypothesis, and it held. 16a measured 13e fold residuals and shipped-
probability honesty across cohort (veteran / rookie), draft-capital
tier (R1 / day2 / day3+UDFA), and season phase (w1-4 / w5+) cells under
the ladder rule (residual >= 0.8 EPA at n >= 300, QB >= 2.0;
calibration >= 4pp at n >= 400; locked pre-run). NO TRIGGER. No 16b;
no rookie features change.

Pre-stated candidates resolved: rookie w1-4 flat (RB +0.31, WR -0.06);
the day-3/UDFA volume hypothesis was wrong in DIRECTION (vol residual
mildly negative -- tier medians slightly over-credit the breakout
class); R1 rookie QBs unremarkable. WATCH (bars not moved): rookie-QB
family -- rookie_all QB -1.03 EPA (n=642 vs 2.0 bar) with loud-but-tiny
below-floor cells (day2 -1.86 n=95, day3+UDFA -2.65 n=130); at ~50
rookie-QB player-weeks/yr these reach the floor ~2028-29. Worst
calibration cell: RB day3+UDFA boom -3.0pp (n=945), under the bar.

Cross-rung note: rungs 4 and 5 lean the same way -- the chain's softest
cells are QB-context cells (compromised trenches, non-premium rookies),
every one below the pre-committed bars. If a QB-specific rung is ever
pre-registered, it starts from those two watch items and an EX-ANTE
continuity rebuild (D21).

THE PRE-REGISTERED LADDER IS COMPLETE: rung 1 injury states SHIPPED,
rung 2 Vegas SHIPPED (3 layers), rung 3 weather NULL, rung 4 OL/front
NULL, rung 5 rookie priors NULL (rest/travel was batched with rung 2).
Two signals entered the chain; three nulls published with receipts.

## Deployment runner (10-series) -- design, in progress 2026-07-17

The backtest chain trains a model per fold; deployment is ONE MORE FOLD:
train on all history through the current week using the same pre-committed
tuning grids and conformal machinery, score the upcoming slate, push
through the saved translation + recalibration artifacts. Weekly retrain
(that is exactly what the walk-forward folds simulate). Stages:

```
10a_deployment_models.R   BUILT 2026-07-18. Deployment = one more fold of
                          the frozen backtest procedure per position: all
                          rows, last-20% season-week cal split, frozen
                          32-combo grids, conformal from cal residuals.
                          Saves data/deploy_models/*.txt (8 lgb models) +
                          data/deployment_params.rds. Trained through
                          2025-W18; reruns Tuesdays in-season. Deployment
                          seam: RB/WR combined intervals scale by
                          pred_vol^alpha at scoring time (observed volume
                          does not exist pre-kickoff); re-scored backtest
                          coverage under pred scaling holds at 80/90
                          (+1.2/+0.3pp RB, +1.4/+0.1pp WR), +2.5pp
                          conservative at 50. QB const mechanism: no seam.
10b_weekly_slate.R        PART 1 BUILT 2026-07-18: game slate + kickoff-
                          hour weather at stadium coords (data/
                          stadium_coords.csv; Open-Meteo forecast API
                          live, HISTORICAL FORECAST archive for hindcast
                          -- never ERA5/observed; content flags at 15mph
                          wind / 20F). Ablation ladder for MODELED
                          features pre-registered in
                          building_in_public_log.md
10b2_player_slate.R       PART 2, RB BUILT 2026-07-18: ex-ante feature
                          rows for a target week, standalone next-value
                          carry-forward of the frozen rolling logic
                          (frozen scripts untouched). VALIDATION GATE:
                          hindcast rows must reproduce the frozen
                          table's ex-ante rows EXACTLY -- passed at
                          max |diff| = 0 for 2025-W15 AND 2025-W02
                          (fallback branch), 58 players x 12 features
                          each. Priors recomputed from saved raw plays
                          (table-lift shortcut left NAs for sub-floor
                          players -- first gate run caught it). Includes
                          injury-report join + depth-chart override hook
                          (data/overrides/depth_overrides.csv)
10b3_wr_slate.R           WR clone, gate passed |diff|=0 (2025-W15: 75
                          players x 12 features; W02 fallback branch ok)
10b5_te_slate.R           TE clone (2026-07-19), gate passed |diff|=0
                          for 2025-W13/14/15 (33/30/37 players x 13
                          features incl. wt_tgt_per_snap, rebuilt on the
                          snaps table with zero-target fill exactly as
                          12a). TE table has no NA pseudo-rows (12a
                          filters NA receivers).
10b4_qb_slate.R           QB clone, gate passed |diff|=0 (2025-W15: 32
                          QBs x 19 features; W02 ok). Requires
                          data/qb_def_adj.rds -- additive save added to
                          08a (rush def component measures ALL rushers,
                          whose raw plays were not otherwise persisted).
                          08a rerun verified byte-identical artifacts:
                          feature regeneration is deterministic, safe
                          for the weekly cadence.
10b_roster_helpers.R      EX-ANTE ROSTER HARDENED 2026-07-18: future-
                          mode roster = played-this-season UNION point-
                          in-time weekly roster (load_rosters_weekly,
                          archived -- hindcast-honest) incl. position
                          converts via known_ids (frozen-layer "ever at
                          pos" definition); current roster team wins on
                          trades; def fallback now covers no-history
                          teams (week-1 bug fixed). Validation: recall
                          100% all positions at W15 AND W01 (2025);
                          forced-future runs ALSO pass the exact-match
                          gate on intersections incl. the all-cold-start
                          week-1 regime; zero NA defense/baseline
                          features; fallback rates exactly 100%/0% at
                          W01/W15. NEXT: 10c scoring.

KNOWN DATA-QUALITY FLAG (found by the 10b3 gate, 2026-07-18): the frozen
WR feature table carries one NA-player pseudo-row per team-game (3,734
rows, ~17% of the table) -- unattributed targets pass the play filter
because wr_ids includes an NA gsis_id, so `NA %in% wr_ids` is TRUE.
These rows were part of WR model TRAINING and conformal calibration
(04b/04c/10a do not filter player_id); the published FP chain is clean
(06b filters them before scoring). Slate builders exclude them.

SIZED 2026-07-18 (04d A/B, pre-committed rule, receipt in
output/04d_wr_nona_ab_table.csv): IMMATERIAL. Filtered refit vs shipped
on identical real-player rows: 80% coverage +0.53pp pooled / +0.15pp
low-usage; width +1.84% / +0.38%; median prediction movement 0.12 EPA;
RMSE a wash. The NA rows acted as mildly stabilizing calibration mass
(removing them WIDENS intervals slightly). Decision: shipped chain
stands; the wr_ids NA filter goes into the next feature-table version
rather than an emergency rebuild.
10c_weekly_score.R        BUILT 2026-07-18: slate -> deploy models ->
                          conformal intervals (RB sym power-law, WR
                          asym qsets, QB const; pred_vol floored at 1
                          for scaling only) -> cloned 06b/09a sim
                          (saved pools + rhos) -> recal maps
                          (library(splines); function(p, pred_vol),
                          pred_carry for QB) -> coherence-capped
                          probabilities + reconciliation vs backtest
                          chain under PRE-COMMITTED bounds (r >= 0.95,
                          |mean| <= 2pp, rows > 10pp flagged).
                          RECONCILIATION FINDING (2026-07-18): first
                          run breached RB/WR -- backtest intervals were
                          scaled by OBSERVED volume (game script),
                          deployment by predicted; recal maps were fit
                          on obs-vol probabilities but applied to
                          pred-vol ones (train/serve skew), amplified
                          by strat_iso step cliffs. Fix: 06b0 rescales
                          fold tot-intervals to pred-vol scaling
                          ((pred/obs)^alpha_fold arm rescale); 06b/06c
                          rerun on deployment-consistent inputs (env
                          seams RB_PRED_FILE/WR_PRED_FILE). Frozen 6c
                          judge re-ran: smooth maps beat the step maps
                          (RB15 raw, RB20 platt, WR strat_platt x2) --
                          no more 20-30pp cliffs. Rerun reconciliation
                          PASSES 18/18 bound cells over W13/W14/W15
                          hindcasts (r 0.97-0.99, |mean| < 2pp); one
                          explainable rookie row at 10.05pp (fold-vs-
                          all-data refit, Egbuka W13). QB chain
                          untouched (const mechanism, no seam).
10d_content_tables.R      BUILT 2026-07-18: from the 10c scored slate --
                          (1) flagship start boards per position (P(15+
                          PPR) RB/WR, P(20+ std) QB), (2) flex boom
                          board (RB+WR by P(20+)) + QB boom, (3)
                          streamer/waiver board (exante_low volume
                          stratum: RB pred_vol < 10, WR < 5), (4)
                          receipts when the week has been played
                          (stated-band calibration + worst-miss /
                          longshot-hit callouts), (5) ECR gap via
                          data/ecr/ecr_<wtag>.csv, fed by
                          10d0_ecr_fetch.R (FantasyPros public API v2;
                          key in macOS keychain as fantasypros-api-key;
                          skips itself while API approval is pending;
                          manual CSV drop also works). Names joined
                          through the shared normalizer
                          (10d_name_helpers.R: suffix/punctuation
                          stripping + alias table; unmatched in-depth
                          players logged as alias candidates each
                          week). data/ecr/ is GITIGNORED: licensed
                          rankings data is never committed; the
                          published gap piece carries FantasyPros
                          attribution per their terms. Free-tier
                          truncation depth is logged per fetch -- HOF
                          upgrade only if it cuts the streamer tier.
                          Outputs: CSVs +
                          markdown boards + 4 rendered X board PNGs
                          (output/img/, 1280x1080, validated palette,
                          image order = raw-probability order matching
                          the CSVs). For operations/content handoff,
                          scripts/refresh_latest.sh also copies the
                          current week into output/latest/ (stable
                          names + run_manifest.json) so downstream
                          content work never has to chase week-tagged
                          filenames. EDITORIAL CAPS pre-committed:
                          displayed probabilities clamped to [2%, 95%]
                          (never publish a certainty); raw values stay
                          in the CSVs. Hindcast demo: 2025-W15.
```

Laptop = stage, MacMini (Earnest) = production; same scripts, cron on the
MacMini via scripts/weekly_run.sh (modes: full | rescore; season/week
auto-detect = week of the earliest future REG kickoff).

ANNUAL OFFSEASON CHORES (Manfred, before Week 1):
  1. Season-constant rollover: bump the layer/slate season constants and
     CLI defaults (see the 2026 rollover commit for the full file list;
     the 06/06b/09a/12d ranges are outcome-fitting -- extend those only
     AFTER the completed season, never at rollover).
  2. Opener backfill (Feb/Mar, after the Super Bowl): download a fresh
     https://www.aussportsbetting.com/historical_data/nfl.xlsx in a real
     browser (Cloudflare blocks every scripted route) to
     data/vegas/nfl_odds_aussports.xlsx, bump COVERED_THROUGH in
     R/13d0_opener_lines.R, run it -- gates verify the join. Live weeks
     never depend on this; slates pull lines from nflverse schedules.
  3. Early-September pass: re-run R/10e0_rookie_crosswalk.R (rookie
     gsis ids firm up late August), check the ECR name-alias report on
     the first real W1 slate, then arm the crontab.

IN-SEASON CADENCE (decided 2026-07-18, kickoff-aware). 10c only scores
games whose kickoff is still in the future (AS_OF env overrides the clock
for tests/replays; a fully-past week auto-runs as hindcast) and appends
every run to output/10c_ledger_<wtag>.csv. A played game is NEVER
re-scored: receipts grade the LATEST pre-kickoff ledger row per player.

    # Earnest crontab (times ET; MacMini local tz). Installed/managed by
    # scripts/earnest_setup.sh --arm; entrypoint is earnest_cron.sh, which
    # wraps weekly_run.sh with the git cadence (clean-tree guard -> ff-only
    # pull -> run -> commit outputs -> push), then refreshes output/latest/
    # and sends a Telegram summary + 5-image media group via OpenClaw when
    # scripts/earnest_delivery.env is configured. Preflight first:
    # earnest_setup.sh with no args checks git/R/deps/keychain/artifacts and
    # refuses to arm dirty.
    30 23 * * 2  earnest_cron.sh full      # Tue: retrain + W+1 slate; models freeze
    0  15 * * 4  earnest_cron.sh rescore   # Thu: TNF lock (Wed injury desigs + weather)
    0  15 * * 6  earnest_cron.sh rescore   # Sat: late-season Sat slates (no-op refresh otherwise)
    0  8  * * 0  earnest_cron.sh rescore   # Sun: main lock, before 9:30am ET internationals
    0  15 * * 1  earnest_cron.sh rescore   # Mon: MNF lock + Monday receipts

RECEIPTS TIMING (Steve 2026-07-18): Monday-with-pending -- the Monday run
publishes receipts with MNF statements listed as "still on the board";
the Tuesday full run finalizes the week's receipt CSVs.

SINGLE-WRITER RULE FOR DEPLOY ARTIFACTS (Steve 2026-07-26). The
regenerable training artifacts -- data/deployment_params.rds and
data/deploy_models/*.txt -- are binaries that every 10a run rewrites,
and git cannot merge binaries. Convention:
  - IN-SEASON: only EARNEST commits them (his Tuesday full run is the
    production trainer, so the committed artifacts always equal what
    production scores with -- the artifacts-as-receipts property).
  - MANFRED may run 10a any time for testing but treats the local
    outputs as scratch: `git checkout -- data/deployment_params.rds
    data/deploy_models/` before committing anything else.
  - EXCEPTION: a coordinated model-change ship pass (a new position, a
    ladder rung landing) retrains and commits from the laptop; Earnest
    must hold a clean working tree when it lands (his next weekly run
    then takes over as writer).
  - CONFLICT RECOVERY (either machine): the local binary is never worth
    merging -- `git stash` (or checkout) the local copy, pull, drop the
    stash; rerun 10a if a fresh artifact is needed. First observed
    2026-07-26 when a MacMini test run's artifact collided with the
    TE-ship commit on pull.
  - RUN OUTPUTS (ledgers, receipts, boards, run logs): production-
    cadence runs COMMIT their outputs -- they are the published record.
    TEST runs clean up after themselves (preview first):
      git clean -n -- 'output/*_<season>_w<wk>*' 'logs/'
      git clean -f -- 'output/*_<season>_w<wk>*' 'logs/'
    A test-run ledger left lying around and later swept into a commit
    would masquerade as a published statement -- never `git add -A`
    on the MacMini outside the production cadence.

## Ablation ladder rung 1 SHIPPED (2026-07-18): RB injury state machine

11a diagnostic -> 11b ex-ante layer (Friday-lock masked; shared core in
11b_injury_state_fns.R) -> 11c/11d A/B under the frozen rubric. RB
SHIPS: fresh-shock bias +3.83 -> +0.96 touches, vol RMSE -14% on those
weeks, rubric clean. WR = published null; QB = skipped w/ diagnostic
receipt (~1 dropback vs 9 RMSE). INTEGRATION: 06b0 RB source = 11c
injury-arm fold predictions; 06b/06c rebuilt (new picks: RB15 platt,
RB20 platt_vol, WR strat_platt x2 -- all smooth); 10a trains
RB_INJURY_FEATURES into the RB volume model (injury_states_rb.rds
join); 10b2 computes slate-side states with the SAME shared core and
gates them exact-match vs the training layer (|diff| = 0 at W15/W14/
W13/W02); 10c reconciliation passes all bounds on three hindcast
weeks. Cadence semantics: Tuesday slates score as no-designations
(= the trained "not listed Friday" healthy state); the injury signal
enters at Thu/Sat/Sun rescores as reports land. 2025 injury rows have
no date_modified (unmaskable, ~7% approximation documented in 11b).
Next rungs: 2 Vegas lines (cheap control), 3 weather adjustment layer.

## Reproducibility

- Data: nflreadr play-by-play and player stats, seasons 2014-2025, REG only
- Evaluation: 204 walk-forward folds (train on past, test on next week);
  fold map frozen in `data/fold_map.rds`
- Seeds: 42 everywhere randomness exists (simulation, tuning)
- Every model stage writes its coverage/calibration tables to `output/` and
  its console log to `logs/`

## Status update (2026-07-19): TE joins the product

All FOUR positions now content-ready and wired through deployment. TE
chain (12-series, D17): thresholds 12/17, rubric passed (-0.4pp pooled and
low-usage), sim beats normal 3.9->1.5pp / 2.6->0.9pp, conditional recal
picks platt_vol / strat_platt, slate gate |diff|=0 x3 weeks, 10c recon all
bounds passed x3 weeks with RB/WR/QB published numbers byte-stable (TE
simulates last -- RNG order is part of the published-number contract).
10a now trains 10 deploy models; weekly_run.sh runs 12a + 10b5 in cadence.
TE editorial (Steve 2026-07-19): start board + streamer board (cut:
pred_vol < 4) + flex boom board (RB/WR/TE combined; bars are position-
calibrated 20/20/17 -- equal rarity by construction -- and disclosed per
row and in the footnote). Watch items:
TE exante_low stratum thin (315 rows, RB-recut signature); TE injury-state
layer untested (11-family A/B is the natural next rung); star shrinkage
applies (top-12 TEs take 41.8% of positional FP).

## Status (2026-07-17, end of session)

- **P(15+) and P(20+) are content-ready for both positions**, now honest
  conditionally as well as on average: after the 6c volume-conditional
  extension, every position x threshold x ex-ante-volume stratum is within
  +-1.3pp out-of-time
- Full product stack: 3A-v2 RB intervals + 04c WR asymmetric intervals ->
  spline translation + empirical simulation (06b) -> stratified isotonic
  recalibration (06c, deployed maps take (p, pred_vol))
- WR 3C retired (D14): WR upside is a roster split over the single 04c
  engine; RB keeps its 3C upside engine (not yet wired through translation)
- **QB is content-ready** (D15): P(20+)/P(25+) from the 9a simulation
  layer, raw honest at 20+ (identity map), Platt at 25+; known
  limitations: star shrinkage (elite tier reads conservative) and the
  mover/scrambler watch items
- Editorial note: cap displayed probabilities in the sparse extreme tail
- Next: weekly deployment wiring (Earnest) and content formats -- all
  three positions now emit calibrated probabilities through deployed
  artifacts (translation fits + recal maps, all self-contained rds)
