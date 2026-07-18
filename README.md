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
10b4_qb_slate.R           QB clone, gate passed |diff|=0 (2025-W15: 32
                          QBs x 19 features; W02 ok). Requires
                          data/qb_def_adj.rds -- additive save added to
                          08a (rush def component measures ALL rushers,
                          whose raw plays were not otherwise persisted).
                          08a rerun verified byte-identical artifacts:
                          feature regeneration is deterministic, safe
                          for the weekly cadence.
                          NEXT: ex-ante roster hardening for true future
                          weeks (cold-start additions from rosters),
                          then 10c scoring.

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
10c_weekly_score.R        point preds + intervals for the slate ->
                          simulation translation (saved resid pools +
                          copula rhos) -> recal maps (library(splines);
                          function(p, pred_vol)) -> calibrated
                          probabilities per player
10d_content_tables.R      emit content products: forward boards
                          (leaderboards by threshold) + backward receipts
                          (last week's stated p vs outcomes); editorial
                          caps applied in the extreme tails
```

Laptop = stage, MacMini (Earnest) = production; same scripts, cron on the
MacMini. In-season cadence: Tuesday night data pull + retrain + score;
re-score Sunday morning on inactives (10b/10c only, models frozen for the
week).

## Reproducibility

- Data: nflreadr play-by-play and player stats, seasons 2014-2025, REG only
- Evaluation: 204 walk-forward folds (train on past, test on next week);
  fold map frozen in `data/fold_map.rds`
- Seeds: 42 everywhere randomness exists (simulation, tuning)
- Every model stage writes its coverage/calibration tables to `output/` and
  its console log to `logs/`

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
