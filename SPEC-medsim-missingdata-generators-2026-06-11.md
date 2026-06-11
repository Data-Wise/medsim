# SPEC (DEFERRED) — `medsim` missing-data + nonnormality DGM generators

**Status:** 🟡 Spec only — **do not build yet** (prototype-first; build after the Missing Effect
`SPEC-simulation-design` freezes at v1.0 and the standalone prototype validates).
**Author:** Davood Tofighi
**Date:** 2026-06-11
**Consumer:** `~/projects/research/Missing Effect` — the inference-led study (MBCO-MI vs Monte-Carlo CI
for the indirect effect under missingness × nonnormality). Also reusable by `sensitivity` and
`measurement error`.
**Depends on:** (1) Missing Effect `SPEC-simulation-design-2026-06.md` v1.0; (2) the validated
`Missing Effect/code/prototype-d4-mbco.R` (D4-MBCO exact-match vs `mitml`, per its `.STATUS` phase2).
**Priority when unblocked:** P2 (infrastructure; **not** on the manuscript critical path — the
prototype already produces results).

---

## TL;DR
The Missing Effect study needs reusable **data-generating utilities** that `medsim` does not yet
provide: (1) **nonnormality** generators (target skew/kurtosis) and (2) **missingness amputation**
under MCAR / MAR / MNAR. Unlike the differential-misclassification case (whose domain logic rightly
lives in `medrobust` — see `PROPOSAL-medsim-dm-integration.md`), these are **domain-agnostic
simulation utilities** (cf. `mice::ampute`), so their natural home is `medsim` itself. This spec
registers the generator API + a missing-data scenario constructor + MBCO-MI / MC-CI method adapters.
**Capture now; prototype in `Missing Effect/code/` first; promote here once the sim design freezes.**

## Decisions recorded (2026-06-11, author)
- **DGM home = `medsim` API, prototype-first.** Generators are generic/reusable → they belong in
  `medsim`, but are prototyped in `Missing Effect/code/` and promoted after validation (follows the
  DM proposal's "capture now, build after validation" timing rule).
- **IPW = thin wrapper → robustness appendix.** Consistent with the inference-led (MBCO-MI) headline
  and with `missingmed` having no IPW until Phase 1. A thin `method()` wrapper makes IPW empirically
  present without becoming a co-equal main-results arm.

---

## Why these belong in `medsim` (vs the DM case)
- **Generic, not domain-specific.** MCAR/MAR/MNAR amputation and skew/kurtosis draws are not
  mediation-specific; `sensitivity`, `measurement error`, and Missing Effect all need them.
- **DRY.** Otherwise each research repo re-implements `ampute`-style logic in a bespoke script —
  exactly the tech debt the DM proposal flags for `runner.R`/`parallel.R`/`tables.R`.
- **Estimator-agnostic boundary preserved.** These are *data_generator* helpers; the estimators
  (MI via `missingmed`, inference via `rmediation`) stay outside `medsim`, kept in `Suggests`.

## Proposed change (against the REAL `medsim` API, verified 2026-06-11)
`medsim` already exposes the hooks (per `PROPOSAL-medsim-dm-integration.md` §"Proposed change"):
`medsim_scenario(name, description, data_generator, params)` (generator accepts `n`);
`medsim_run(method, scenarios, config, compute_truth = NULL, …)` (`method`/`compute_truth` are
functions); `medsim_analyze_coverage()` / `medsim_table_coverage()`.
> ⚠️ Verify the exact `method` second-arg name (`params` vs `scenario`) against `R/runner.R` before
> building — the survey saw `method(data, params)`; the DM proposal wrote `method(data, scenario=NULL)`.

### 1. Nonnormality generator — `R/dgm_nonnormal.R`
```r
# Draw n values with a target marginal skew/kurtosis (Fleishman / Vale–Maurelli power method).
medsim_rnonnormal <- function(n, mean = 0, sd = 1, skew = 0, kurtosis = 0) { ... }
# Vectorized residual draws for a path model; returns standardized then rescaled errors.
```
- Pure base R + a small Fleishman-coefficient solver; **no new hard dependency**.
- Validity guard: warn/stop on infeasible (skew, kurtosis) outside the Fleishman region.

### 2. Missingness amputation — `R/dgm_amputate.R`
```r
medsim_amputate <- function(data, target, mechanism = c("MCAR","MAR","MNAR"),
                            prop = 0.2, predictors = NULL, weights = NULL,
                            type = c("RIGHT","LEFT","MID","TAIL")) {
  mechanism <- match.arg(mechanism)
  # MCAR: prob constant; MAR: logistic on observed `predictors`; MNAR: logistic incl. `target` itself.
  # Thin, documented wrapper aligning mice::ampute semantics to the medsim DGM contract.
}
```
- `target` = column(s) to set `NA` (mediator `M`, outcome `Y`, or both per the estimand×mechanism
  matrix in `ESTIMANDS-2026-06.md`).
- MNAR self-mechanism = logistic on the to-be-missing variable; MNAR-other = on a downstream var.
- `mice` in **Suggests** (used if present); otherwise the built-in logistic amputer.

### 3. Missing-data scenario constructor — add to `R/scenarios.R` (or `R/scenarios_missing.R`)
```r
medsim_scenario_missing <- function(name, true_params,            # a, b, cp, residual sd's
                                    mechanism, prop = 0.2, target = "M",
                                    nonnormal = NULL) {            # list(skew=, kurtosis=) or NULL
  medsim_scenario(
    name = name,
    description = sprintf("%s missing on %s (prop=%.2f)%s",
                          mechanism, paste(target, collapse="+"), prop,
                          if (is.null(nonnormal)) "" else ", nonnormal"),
    data_generator = function(n) {
      d <- .gen_complete_med(n, true_params, nonnormal)     # X→M→Y with optional nonnormal residuals
      medsim_amputate(d, target = target, mechanism = mechanism, prop = prop,
                      predictors = setdiff(names(d), target))
    },
    params = list(true_params = true_params, mechanism = mechanism,
                  prop = prop, target = target, nonnormal = nonnormal)
  )
}
```

### 4. Method adapters (the MBCO-MI vs MC-CI comparison)
```r
# Headline pair — each returns the medsim coverage/power contract: {est, est_ci_lower/_upper, est_p}.
medsim_method_mbco_mi <- function(model, m = 20, ...) {        # MI (missingmed) + rmediation::mbco()
  function(data, params) {
    list(indirect = ..., indirect_ci_lower = ..., indirect_ci_upper = ..., indirect_p = ...,
         branch_switch = ...)   # MEMO union-null diagnostic (ab=0); summarized by medsim_analyze
  }
}
medsim_method_mc_ci  <- function(model, m = 20, ...) { ... }   # MI + rmediation::medci()
medsim_method_ipw    <- function(model, ...) { ... }           # THIN — robustness appendix only
```
- `missingmed` and `rmediation` stay in **Suggests** (loaded inside the adapter); during prototyping
  the adapter wraps `Missing Effect/code/prototype-d4-mbco.R` directly.

### 5. Coverage/metrics — reuse as-is (no internal gap)
Unlike the DM bounds (interval estimands needing a new coverage branch), MBCO-MI / MC-CI return a
**point estimate + CI**, which `medsim_analyze_coverage()` / `medsim_analyze_power()` already handle.
Optional add: surface the MBCO `branch_switch` rate as a column via `medsim_analyze` (new summary, not
a new metric engine).

---

## Acceptance criteria (when built)
- [ ] `medsim_rnonnormal()` hits target skew/kurtosis within Monte-Carlo tolerance (≥3 (skew,kurt) cells).
- [ ] `medsim_amputate()` reproduces target missingness rate and mechanism (MCAR independence; MAR
      depends on observed only; MNAR depends on the missing var) — unit tests per mechanism.
- [ ] `medsim_scenario_missing()` + the adapters reproduce the standalone prototype's coverage/Type-I
      numbers to Monte-Carlo error on ≥3 cells.
- [ ] `medsim_table_coverage()` emits the Missing Effect Study-2/3 tables (LaTeX) directly.
- [ ] `mice`, `missingmed`, `rmediation` remain in **Suggests** (no hard deps; avoid CRAN dep-cycle).

## Out of scope
- No estimator math in `medsim` (MI lives in `missingmed`; MBCO/MC-CI in `rmediation`).
- No MNAR *sensitivity/identification* workflow (that is the Missing Effect G3 deliverable, not a DGM).
- No vignette here; the worked missing-data example belongs in the Missing Effect manuscript / a
  `missingmed` vignette.

## Pointer
- Consumer spec: `~/projects/research/Missing Effect/SPEC-simulation-design-2026-06.md`,
  `ESTIMANDS-2026-06.md` (estimand×mechanism matrix), `MEMO-MBCO-MI-derivation-2026-06.md`
  (branch-switching / union-null), `docs/PACKAGE-FIT-2026-06.md` (the 3-package architecture).
- Prototype to wrap/validate against: `~/projects/research/Missing Effect/code/prototype-d4-mbco.R`.
- Sibling pattern: `PROPOSAL-medsim-dm-integration.md` (same capture-now/build-later philosophy).

---

# Augmentation (2026-06-11) — built for parallel implementation

> Designed so several sessions/agents can build concurrently with **no file collisions**. The rule:
> each workstream **owns new files**; shared files (`DESCRIPTION`, `NAMESPACE`, `analyze.R`) are touched
> only in the serial integration step. Build against the frozen interfaces below, not each other's code.

## Step 0 — Interface freeze (do this FIRST, single commit, then fan out)
Before any parallel work, land a tiny commit that fixes the **contracts** so workstreams can stub each
other. Nothing here is logic — just signatures + return shapes.

### Shared return contract for `method()` adapters (the medsim coverage/power API)
Every estimator adapter returns a flat named list of numeric scalars:
```r
list(
  indirect          = <num>,   # point estimate of the indirect effect (a*b / TNIE)
  indirect_ci_lower = <num>,   # consumed by medsim_analyze_coverage()
  indirect_ci_upper = <num>,
  indirect_p        = <num>,   # consumed by medsim_analyze_power()
  branch_switch     = <0/1>,   # MBCO union-null diagnostic (ab=0); NA for MC-CI/IPW
  converged         = <0/1>    # estimator convergence flag (drop non-converged in analyze)
)
```
### Shared DGM data contract
`data_generator(n)` returns a `data.frame` with columns **`X, M, Y`** (+ optional covariates `C*`);
`medsim_amputate()` returns the same frame with `NA`s inserted in `target` column(s); column names and
order are preserved.

### ⚠️ Resolve-first blocker (gates C/D)
Verify the real `method` second-arg name in `R/runner.R` (`params` vs `scenario`). Pin it in Step 0 so
C and D agree. *This is the only cross-workstream ambiguity.*

## Parallel implementation plan (6 workstreams)

| WS | Owns (NEW files) | Depends on | Can start | Parallel-safe? |
|----|------------------|-----------|-----------|----------------|
| **A. nonnormal** | `R/dgm_nonnormal.R`, `tests/testthat/test-dgm-nonnormal.R` | — | immediately | ✅ pure, isolated |
| **B. amputate** | `R/dgm_amputate.R`, `tests/testthat/test-dgm-amputate.R` | — | immediately | ✅ pure, isolated |
| **C. scenarios** | `R/scenarios_missing.R`, `tests/testthat/test-scenarios-missing.R` | A,B **interfaces** | after Step 0 (stub A/B) | ✅ new file (not `scenarios.R`) |
| **D. methods** | `R/methods_missing.R`, `tests/testthat/test-methods-missing.R` | prototype + rmediation/missingmed (Suggests) | after Step 0 | ✅ independent of A/B/C |
| **E. analyze** | `R/analyze_missing.R`, `tests/testthat/test-analyze-missing.R` | D **return contract** (`branch_switch`) | after Step 0 | ✅ new file (NOT editing `analyze.R`) |
| **F. integrate** | edits `DESCRIPTION` (Suggests), roxygenize `NAMESPACE`/`man/`, vignette stub, `R CMD check` | A–E merged | last (serial) | ❌ serial by design |

**Dependency graph:** `Step0 → {A, B, D} (fully parallel) → C (needs A,B) ‖ E (needs D) → F`.
A, B, and D can run simultaneously from the start; C joins once A/B land; E joins once D lands; F is the
single serial seam.

### Why new files, not edits
- `scenarios_missing.R` not `scenarios.R`; `methods_missing.R` new; `analyze_missing.R` new (export a
  helper `medsim_summarize_branch_switch()` rather than editing `medsim_analyze`). This keeps every
  workstream on disjoint paths → clean merges. `roxygen2` regenerates `NAMESPACE` in F, so no manual
  NAMESPACE edits collide.
- Only `DESCRIPTION` Suggests (`mice`, `missingmed`, `rmediation`) is a genuine shared edit → done once
  in F.

## Per-module test matrix
| WS | Key tests |
|----|-----------|
| A | target skew/kurtosis recovered within MC tolerance (≥3 cells); infeasible (skew,kurt) errors; mean/sd preserved |
| B | realized missing rate ≈ `prop`; MCAR ⟂ all vars; MAR depends on observed only; MNAR depends on target; `target` multi-column (M+Y) |
| C | scenario validates via `medsim_validate_scenario()`; generator returns X/M/Y with NAs in `target`; nonnormal flag wired |
| D | adapter return list has all 6 contract fields; MBCO vs MC-CI both runnable on one toy dataset; `branch_switch ∈ {0,1}`; reproduces prototype on ≥3 cells |
| E | `branch_switch` rate summarized per scenario; non-converged rows dropped; merges into a coverage table column |
| F | `R CMD check` clean (no new NOTEs); Suggests-only (package loads without mice/missingmed/rmediation present) |

## Branch / worktree plan (parallel sessions)
- Integration branch: `dev`. One feature branch + worktree per workstream off `dev`:
  `feature/dgm-nonnormal`, `feature/dgm-amputate`, `feature/scenarios-missing`,
  `feature/methods-missing`, `feature/analyze-missing`.
- Each worktree is its own `claude` session (per the worktree rules). Merge each to `dev` via PR after
  its module's tests pass; do **F (integration)** only once A–E are on `dev`.
- Step 0 (interface freeze) lands directly on `dev` first so all worktrees branch from it.

## Coordination with `PROPOSAL-medsim-dm-integration.md`
Both this spec (WS-E `branch_switch` summary) and the DM proposal (interval-coverage branch) extend the
analysis layer. **Keep them on disjoint files** — DM edits `medsim_analyze_coverage()` for intervals;
this spec adds a *separate* `analyze_missing.R` helper. If both build concurrently, the only shared
surface is `DESCRIPTION` Suggests — reconcile in each one's integration step.

## Optional adds surfaced while augmenting (decide later)
- `medsim_scenario_missing_grid(true_params_list, mechanisms, props, ns, nonnormal_list)` — build the
  full factorial from the `SPEC-simulation-design` cells in one call (WS-C, additive).
- `medsim_rnonnormal()` multivariate variant (Vale–Maurelli) if joint X/M/Y nonnormality is needed
  beyond independent marginal residuals (WS-A, additive).
- Surface realized (skew, kurtosis, missing-rate) as scenario *diagnostics* in the results object so the
  manuscript can report achieved vs target (small WS-E add).

---

# Execution decisions (2026-06-11) — full build authorized

> The 🟡 deferral was **overridden** by the author: build all 6 workstreams now. Recorded here so the
> spec stays the single source of truth for what was actually built and why.

- **Step-0 blocker RESOLVED.** `R/runner.R:329` calls `method(data, scenario$params)` → adapters are
  `function(data, params)` (the spec was right; the DM proposal's `function(data, scenario=NULL)` was
  wrong). Coverage engine (`R/analyze.R`) keys on `<param>_ci_lower`/`_ci_upper`/`<param>_truth` with
  default `ci_suffix = "_ci"`, so the `indirect_ci_*` contract flows in with no engine change.
- **Scaffold already existed.** Commits `355c7fe` (Step-0 interface freeze: 5 stub R files) and
  `2a03d00` (red `testthat` skeletons + `WORKSTREAM-KICKOFFS.md`) on `feature/dgm-interface`. This run
  is the TDD **green phase**: fill the stubs to satisfy the existing red tests.
- **Orchestration = single worktree + `Workflow` tool** (not the 5-worktree/5-session plan). File
  ownership is provably disjoint, so one `feature/dgm-interface` worktree has zero collision risk.
  Because authoring only needs the *frozen signatures* (not sibling implementations), all five modules
  are written in **one parallel barrier**; only execution is ordered. Collapsed DAG:
  `parallel-write(A–E) → serial integrate+test(F) → repair-to-green → R CMD check`.
- **No `document()` mid-build.** Concurrent `devtools::document()` would race on the shared
  `NAMESPACE`/`man/`; all doc regen is deferred to the serial F phase. Implement agents are
  **write-only** + single-file `parse()` self-check (race-free); F runs the first real `load_all`/test.
- **WS-D = graceful-degrade** (overrides the kickoff's "wrap the prototype"): `requireNamespace()`
  guards on `missingmed`/`rmediation`; documented base-R fallback estimator so adapters run end-to-end
  with neither installed. Invariant: package loads + tests pass with the Suggests **absent**.

## Deferred #3 — corrected scope (decided 2026-06-11; do NOT re-assume `missingmed`/`rmediation`)

The WS-D adapters currently ship a **Sobel/listwise placeholder** (graceful-degrade). Promoting them to
the real method was investigated and **deferred** — but the original framing ("wire `missingmed` +
`rmediation::mbco()`/`medci()`") is **wrong** and should not be revived as-is:

- **The validated method does not use either sibling.** `~/projects/research/Missing Effect/code/prototype-d4-mbco.R`
  (phase-2 VALIDATED 2026-06-10, exact match vs `mitml`) has **zero** references to `missingmed`/`rmediation`.
  It runs on **`mice`** (already in Suggests, installed in CI) + hand-coded **D4 pooling** + **MBCO LRT**.
  So wiring the siblings would implement an *unvalidated* path, not promote the proven one.
- **`missingmed`** is mid **S7 migration** (active `feature/s7-migration`; exports `set_sem/run_sem/pool_sem`)
  → API in flux; coupling now = rework.
- **`rmediation` (dev, Data-Wise)** exports `ci()` / `MBCOResult` / `ci_mediation_data` — **not** the
  `mbco()`/`medci()` the spec assumed (that was CRAN `RMediation::medci`, which *is* installed and is
  already used by `medsim_method_mc_ci()` when present).
- **Consumer sim-design not frozen** (`Missing Effect/SPEC-simulation-design-2026-06.md` = `draft-for-review`;
  "freeze v1.0" still an open gate). No urgency: the manuscript runs on the prototype directly meanwhile.

**Real upgrade path when chosen (own PR):** port the validated prototype (`mice` MI + D4 pooling + MBCO LRT
branch diagnostic) into `medsim_method_mbco_mi()` — **no new dependencies, fully CI-testable**. Acceptance:
reproduce the prototype's coverage/Type-I on ≥3 cells. Best done *after* the sim-design freezes (stable
scenario grid). Gated on the decision to invest, **not** on any missing package.
