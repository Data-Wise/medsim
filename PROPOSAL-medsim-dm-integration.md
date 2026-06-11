# PROPOSAL (DEFERRED) — `medsim` integration for differential-misclassification studies

**Status:** 🟡 Deferred / spec only — **do not build yet**
**Author:** Davood Tofighi
**Date:** 2026-06-11
**Depends on:** (1) M2a/M2b simulations validated via the standalone `run_simulations.R`;
(2) `medrobust` on CRAN (currently P0, 65%, at the CRAN gate).
**Priority when unblocked:** P2 (infrastructure polish, not on the manuscript critical path).

---

## TL;DR
The differential-misclassification (DM) bounds papers (M2a mediator, M2b exposure) currently run
their simulation study from a **bespoke script** (`~/projects/research/me-*/code/run_simulations.R`)
that hand-rolls the factorial grid, the parallel driver, and the coverage/width aggregation —
**all of which `medsim` already provides**. This proposal registers a `differential_misclassification`
scenario + method adapter so the DM study runs through `medsim`'s `medsim_run()` /
`medsim_analyze_coverage()` / `medsim_table_coverage()` pipeline instead. **Capture now, build after
the papers' sims validate and `medrobust` ships.**

---

## Why this is worth doing (eventually)
- **DRY / single source of truth.** The standalone script re-implements `medsim`'s `runner.R`,
  `parallel.R`, `tables.R`, `analyze.R`. Two simulation engines in one ecosystem is tech debt.
- **Consistent outputs.** `medsim_table_coverage()` already emits LaTeX tables; routing DM sims
  through it gives M2a/M2b publication tables in the same format as the rest of the mediationverse.
- **Reuse the truth/coverage machinery.** `medsim_run(compute_truth=)` +
  `medsim_analyze_coverage()` already separate ground-truth from estimates and compute empirical
  coverage — exactly the DM study's primary metric.

## Why NOT now (timing rationale)
1. **Validate the science first.** The DM sims have not been run once. Field-name or
   naive-decomposition fixes should surface in a throwaway script, not in a package's test surface.
2. **`medrobust` is P0 at the CRAN gate.** Adding cross-package features now risks scope creep on
   the package being shipped (CRAN compliance is a standing priority).
3. **The papers don't need it.** They need *results*; the standalone script produces them.

---

## Proposed change (against the REAL `medsim` API, verified 2026-06-11)

`medsim` already exposes the three hooks we need:

- `medsim_scenario(name, description, data_generator, params)` — `data_generator` must accept `n`.
- `medsim_run(method, scenarios, config, compute_truth = NULL, parallel, verbose)` — `method` and
  `compute_truth` are **functions**.
- `medsim_analyze_coverage()` / `medsim_table_coverage()` — coverage metric + LaTeX table.

### 1. New scenario constructor (no new infra, just a wrapper)
Add to `R/scenarios.R` (or a new `R/scenarios_dm.R`):

```r
medsim_scenario_dm <- function(name, n, true_params, dm_params,
                               misclass_type = c("mediator", "exposure")) {
  misclass_type <- match.arg(misclass_type)
  medsim_scenario(
    name = name,
    description = sprintf("Differential misclassification of the %s", misclass_type),
    data_generator = function(n) {
      medrobust::simulate_dm_data(
        n = n, true_params = true_params, dm_params = dm_params,
        misclass_type = misclass_type, return_truth = TRUE
      )@observed                      # medsim expects a data frame back
    },
    params = list(true_params = true_params, dm_params = dm_params,
                  misclass_type = misclass_type)
  )
}
```

### 2. Method adapter (wraps `bound_ne` to the `medsim_run` method contract)
```r
medsim_method_bounds <- function(misclass_type, sensitivity_region, effect_scale = "OR") {
  function(data, scenario = NULL, ...) {
    b <- medrobust::bound_ne(
      data = data,
      exposure = if (misclass_type == "exposure") "A_star" else "A",
      mediator = if (misclass_type == "mediator") "M_star" else "M",
      outcome  = "Y", confounders = "C1",
      misclassified_variable = misclass_type,
      sensitivity_region = sensitivity_region,
      effect_scale = effect_scale, verbose = FALSE
    )
    list(NDE_lower = b@NDE_lower, NDE_upper = b@NDE_upper,
         NIE_lower = b@NIE_lower, NIE_upper = b@NIE_upper)
  }
}
```

### 3. Truth function
`compute_truth = function(scenario) medrobust::simulate_dm_data(...)@true_effects`
returning `NDE_OR` / `NIE_OR`, fed to `medsim_run(compute_truth = )`.

### 4. Coverage for *interval* estimands (the one genuine gap)
`medsim_analyze_coverage()` is built for point estimates + SE. Bounds are **intervals**, so coverage
is "is truth inside [lower, upper]?" — either (a) add an `estimand = "interval"` branch to
`medsim_analyze_coverage()`, or (b) add a small `medsim_analyze_bound_coverage()`. **(a) preferred**
for consistency. This is the only change touching existing `medsim` internals.

### 5. Falsification metric (currently missing everywhere)
Draft §6.4 item 4 ("proportion falsified") is not implemented in the standalone script either.
Wire `medrobust::check_compatibility()` / `falsification_summary()` into the method adapter's return
list so `medsim_analyze` can summarize it. Adds a new column to the coverage table.

---

## Acceptance criteria (when built)
- [ ] `medsim_scenario_dm()` + `medsim_method_bounds()` reproduce the standalone script's coverage
      numbers to Monte Carlo error (cross-check on ≥3 cells).
- [ ] `medsim_table_coverage()` emits the M2a/M2b §6 tables (LaTeX) directly.
- [ ] Interval-coverage path added to `medsim_analyze_coverage()` with a unit test.
- [ ] Falsification proportion reported per cell.
- [ ] `medsim` keeps `medrobust` in **Suggests** (not Imports) — DM is one of many scenario types;
      avoid a hard dependency and the resulting CRAN dependency-cycle risk.

## Out of scope
- No change to `medrobust`'s `simulate_dm_data()` / `bound_ne()` — they already do their part.
- No vignette here; the worked DM example belongs in `medrobust`'s vignette.

## Pointer
Standalone scripts this would replace:
`~/projects/research/me-mediator-bounds/code/run_simulations.R` (STRAND="mediator"),
`~/projects/research/me-exposure-recall/code/run_simulations.R` (STRAND="exposure").
Split context: those projects' `02_Notes/MEMO-manuscript-split-2026-06.md`.
