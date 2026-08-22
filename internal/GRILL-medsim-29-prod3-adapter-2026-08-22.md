# GRILL — medsim #29: medsim_method_prod3() adapter for the trivariate normal product

**Date**: 2026-08-22 · **Target**: issue #29 · **Base**: dev @ `1150e45`
**Status**: **DEFERRED (2026-08-22 grill, GD3)** — design is complete and verified;
no work scheduled. Build only when a consumer appears. §0's scope binary is
answered by the deferral: if it is ever built, build **(a) adapter-only** first.

> **Why deferred — no consumer exists.** Verified read-only against
> `~/projects/research/product of three` (the CSDA paper that motivated this
> issue): its `.STATUS` states verbatim **"(prod3 doesn't use medsim)"**, its
> medsim deployment is archived at `code/R/archive-medsim/`, and the paper is
> at 99% / author-review / submission-ready with its simulation study already
> complete (Hopper job 4297837, 60/60 array tasks, results published in the
> manuscript). Nothing in medsim or the mediationverse currently calls a
> three-path estimand.
>
> An exported adapter is permanent surface — CRAN checks, roxygen, a
> version-gated `RMediation` dependency, and a test suite to keep green — so
> it is not worth carrying for speculative value. This document stays as the
> finished design: picking it up later is a half-day, not a restart.
>
> **Revisit trigger:** CSDA reviewers requesting additional simulations, or
> any new study needing a sequential `a1*a2*b` indirect effect.
**Location**: canonical design doc for #29.

**Method**: solo grounding pass against the installed `RMediation 1.6.1`
(actual exports, help pages, live calls — not the issue's assumed API) +
one adversarial pass via `advisor()`.

---

## 0. Scope decision — BLOCKING, pick (a) or (b)

The issue's acceptance criteria assume `medsim_method_prod3()` "drops into
the MBCO-MI / MC-CI comparison grid unchanged." **That grid does not exist
for a three-path estimand.** `medsim_method_mc_ci()` (`R/methods_missing.R:145`)
is built for the missing-data study's **two-path** design (`a`, `b` only —
simple `X → M → Y` mediation with a single indirect effect `a*b`). The
trivariate product `a1*a2*b` requires a **serial** `X → M1 → M2 → Y` design.
There is no `medsim_scenario_serial()` or equivalent DGP anywhere in
`R/` — grepped, confirmed absent. The acceptance criterion as written is
not achievable; the issue needs a scope choice before any code:

| | (a) Adapter only | (b) Adapter + serial scenario/DGP |
|---|---|---|
| Ships | `R/methods_prod3.R`: `medsim_method_prod3()` taking raw `mu`/`Sigma` (or a caller-supplied fitting closure) | (a) **plus** `medsim_scenario_serial()` (X, M1, M2, Y, C data generator) and a `.medsim_fit_serial_ab()` internal that fits the three paths and returns `(mu, Sigma)` |
| Grid consumer | None yet — documented as "adapter is usable standalone; no medsim scenario currently drives it" | The new serial scenario itself, used exactly like `medsim_scenario_missing_grid()` drives `medsim_method_mbco_mi()`/`medsim_method_mc_ci()` today |
| Effort | Small (one estimator function + fixture-based tests) | Roughly 3x — a full new DGP family, its own test suite, vignette section |
| Risk | Low — narrow, testable in isolation | Higher — new scenario design decisions (what M1→M2 confounding structure, missingness interaction with the existing missing-data study, etc.) |

**Recommendation: (a) first.** The issue's own "Follow-on (not this issue)"
section already defers "a `medsim_scenario`-based benchmark vignette" —
that's effectively (b), already scoped out by the issue author. Ship the
adapter standalone, correct the acceptance criteria to drop the
"unchanged grid" claim, and open a **separate** follow-up issue for (b) if
a real simulation study needs it. Building (b) speculatively, with no named
consumer, is the same failure mode already avoided in #41 D2 — infra
without a driver.

Everything below assumes (a). If (b) is chosen instead, this doc's §1–§3
still apply verbatim to the adapter; §0's scenario-design questions would
need their own grill round.

---

## 1. Contract — two gaps the issue left unspecified, now resolved

### Gap 1: `indirect`'s point estimate is NOT `prod(mu)` under correlation

Verified numerically (`RMediation::ProductNormal3` has no `mean()`/point
accessor — checked `S7::props()`, `methods(class=...)`, tried `mean()`,
`summary()`; none exist):

```r
mu    <- c(0.5, 0.3, 0.4)
Sigma <- matrix(c(0.02, 0.01, -0.005,
                  0.01, 0.02,  0.008,
                 -0.005,0.008, 0.015), 3, 3)
# naive product:      0.060000
# MC E[XYZ] (2e6 draws): 0.066497
# closed-form E[XYZ]:    0.066500   <- matches MC to 3 decimals
```

For three jointly normal variables `(X,Y,Z) ~ N(mu, Sigma)`:

```
E[XYZ] = mu_x*mu_y*mu_z + mu_x*Cov(Y,Z) + mu_y*Cov(X,Z) + mu_z*Cov(X,Y)
```

A real fitted `path_cov` from sequential regression is **not diagonal**
(`a1`/`a2`/`b` share the same data), so the naive product is systematically
off-center relative to the exact CI `ProductNormal3` reports — an ~11%
relative gap in the example above, not a rounding artifact. **Decision:
`indirect` uses the closed-form third-moment formula**, not `prod(mu)`. One
line of arithmetic, no extra integration cost.

### Gap 2: `indirect_p` — convention specified

`cdf(obj, q = 0)` (equivalently `pprodnormal3(q = 0, mean = mu, cov = Sigma,
method = "hcubature")`) gives `P(a1*a2*b <= 0)`. **Decision**: two-sided
p-value via `2 * min(F(0), 1 - F(0))` — the same "double the smaller tail"
convention as every other medsim adapter's `indirect_p` (matches
`RMediation::medci`'s own p-value construction, used by
`medsim_method_mc_ci()`).

### Confirmed real API (installed, verified live — corrects the issue's
draft signature only in the `level=` argument name)

```r
obj <- RMediation::ProductNormal3(mu = c(a1, a2, b), Sigma = path_cov,
                                  method = "hcubature")
bounds <- RMediation::ci(obj, level = ci_level)   # named c(lower=, upper=); level defaults 0.95
p0     <- RMediation::cdf(obj, q = 0)             # scalar in [0,1]
```

`ci()` and `confint()` are aliases at the S7 level (verified: identical
output at matching `level=`). Use `ci()` per the issue's original choice —
no functional difference, `ci()` reads better next to `cdf()`.

---

## 2. Design

```r
medsim_method_prod3 <- function(data, params = list(),
                                 mu = NULL, Sigma = NULL,
                                 fit = NULL,
                                 ci_level = 0.95) {
  if (!requireNamespace("RMediation", quietly = TRUE) ||
      utils::packageVersion("RMediation") < "1.6.1") {
    stop("medsim_method_prod3() requires RMediation >= 1.6.1 (ProductNormal3/ci/cdf). ",
         "Install via install.packages('RMediation').", call. = FALSE)
  }
  ests <- if (!is.null(fit)) fit(data, params) else list(mu = mu, Sigma = Sigma)
  # ests$mu: c(a1_hat, a2_hat, b_hat); ests$Sigma: 3x3 asymptotic cov
  obj <- RMediation::ProductNormal3(mu = ests$mu, Sigma = ests$Sigma, method = "hcubature")
  bounds <- tryCatch(RMediation::ci(obj, level = ci_level), error = function(e) NULL)
  if (is.null(bounds)) {
    return(list(indirect = NA_real_, indirect_ci_lower = NA_real_,
                indirect_ci_upper = NA_real_, indirect_p = NA_real_,
                branch_switch = NA, converged = 0))
  }
  m <- ests$mu; S <- ests$Sigma
  indirect <- m[1]*m[2]*m[3] + m[1]*S[2,3] + m[2]*S[1,3] + m[3]*S[1,2]
  p0 <- RMediation::cdf(obj, q = 0)
  list(
    indirect = indirect,
    indirect_ci_lower = unname(bounds["lower"]),
    indirect_ci_upper = unname(bounds["upper"]),
    indirect_p = 2 * min(p0, 1 - p0),
    branch_switch = NA,
    converged = 1
  )
}
```

`data`/`params` are accepted for signature parity with every other medsim
adapter (`method(data, params)`) but are only consulted through `fit` — a
caller-supplied closure that extracts `(mu, Sigma)` from `data`/`params`
however their scenario defines the three paths. Passing `mu`/`Sigma`
directly (bypassing `fit`) supports the fixture-only test mode from §0(a)
where no real scenario exists yet.

---

## 3. Non-Goals

- No `medsim_scenario_serial()` (§0, deferred to (b) as a separate issue if
  a study needs it).
- No cross-check against `medsim_method_mc_ci()` — wrong estimand shape
  (2-path vs 3-path), per §0. The negative control instead is a hand-rolled
  Monte Carlo of the *same* trivariate product (draw `(a1,a2,b) ~ N(mu,
  Sigma)`, average `a1*a2*b`, empirical quantiles) — apples to apples.
- No vendoring of RMediation; version-gated `requireNamespace()` per every
  other optional-dependency adapter in this package.

---

## 4. API Summary

| Function | Change |
|---|---|
| `medsim_method_prod3()` | NEW, `R/methods_prod3.R` |
| `DESCRIPTION` | `RMediation (>= 1.6.1)` moves from a bare `Suggests` entry to a versioned one if not already |

---

## 5. Verification (planted-defect matrix)

| Case | Check |
|---|---|
| 1 | Diagonal `Sigma` (independent paths): `indirect` == `prod(mu)` exactly (closed-form reduces to naive product when off-diagonals are 0) |
| 2 | Non-diagonal `Sigma`: `indirect` matches the closed-form formula, NOT `prod(mu)` (regression guard for Gap 1) |
| 3 | `indirect_ci_lower/upper` matches `RMediation::ci(obj, level=ci_level)` to floating-point tolerance, for `ci_level` != 0.95 too (catches a hardcoded-0.95 mistake) |
| 4 | `indirect_p` matches `2*min(cdf(0), 1-cdf(0))` computed independently in the test |
| 5 | MC cross-check: 200k-draw Monte Carlo of `a1*a2*b` at fixed `(mu, Sigma)` — sample mean within MC error of closed-form `indirect`; empirical 2.5/97.5 percentiles within MC error of `ci()` bounds |
| 6 | `RMediation` version guard: mock `packageVersion` < 1.6.1 → informative `stop()`, not a cryptic downstream error |
| 7 | `fit =` closure receives `(data, params)` and its returned `mu`/`Sigma` are the ones passed to `ProductNormal3` (wiring test, not a real fit) |
| 8 | hcubature integration failure (malformed `Sigma`, e.g. non-PD) → `converged = 0`, all fields `NA`, no error escapes to the caller |

Gates: `devtools::test()`, `R CMD check --as-cran` with `RMediation` present
(coverage of the real code path) — CI's R-CMD-check matrix should already
carry `RMediation` via `Suggests`, confirm before merge. Skip-on-missing
test for the version guard, so the suite stays green on a machine without
`RMediation` installed.

---

## 6. Branch / Worktree Plan (descriptive — not an action item)

Single feature branch off `dev`, e.g. `feature/prod3-adapter`. Independent
of #41 and #28 — no shared files. **Not scheduled** (see Status / GD3).

---

## 7. Grill ledger (interactive, 2026-08-22)

| # | Question | Resolution |
|---|---|---|
| GD3 | #29 has no consumer — build it anyway? | **Defer.** Keep the issue open with this doc attached as its design; build nothing now. Evidence: the motivating CSDA paper explicitly does not use medsim (`.STATUS`: "(prod3 doesn't use medsim)"), has archived its medsim deployment, and has a completed simulation study. Rejected alternatives: build adapter-only now (adds a maintained export with no caller), close outright (discards a verified design and the `prod(mu)` bias finding), build adapter + serial DGP (~3x work, no study asking). |

### Findings preserved for whoever picks this up

The two contract corrections in §1 are the load-bearing content of this doc —
both were found by running the real `RMediation 1.6.1`, not by reading the
issue, and both would be silently wrong in a naive implementation:

1. **`indirect` is not `prod(mu)`.** Under a realistic non-diagonal `path_cov`,
   `E[XYZ] = μ₁μ₂μ₃ + μ₁Cov(Y,Z) + μ₂Cov(X,Z) + μ₃Cov(X,Y)` — an ~11% relative
   gap in the worked example, verified against a 2e6-draw Monte Carlo. Using
   `prod(mu)` beside an exactly-computed CI puts the point estimate off-center.
2. **`indirect_p` had no specified convention.** `cdf(obj, q = 0)` gives
   `P(a1·a2·b ≤ 0)`; the adapter contract needs `2 * min(F(0), 1 - F(0))` to
   match every other medsim adapter.

Also settled: `ProductNormal3` exposes no point-estimate accessor at all
(checked `S7::props()`, `methods()`, `mean()`, `summary()`) — hence finding 1
is a decision the implementer must make, not an API lookup.
