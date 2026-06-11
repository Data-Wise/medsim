# CLAUDE.md for medsim Package

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

---

## About This Package

**medsim** provides standardized infrastructure for conducting Monte Carlo simulation studies in mediation analysis. It is part of the mediationverse ecosystem.

### Core Mission

Provide a complete, reusable simulation framework that eliminates the need to repeatedly implement parallel processing, progress reporting, result analysis, and visualization across different mediation research projects.

### Key Features

- Environment-aware execution (local vs HPC cluster)
- Three execution modes (test, local, cluster)
- Parallel processing with progress bars
- Ground truth caching
- Automated analysis and visualization
- Publication-ready output (figures and LaTeX tables)
- Missing-data + nonnormality DGM generators + missing-data mediation estimator adapters
  (*in progress* on `feature/dgm-interface`; see Code Architecture below)

---

## Common Development Commands

```r
# Install dependencies and check package
remotes::install_deps(dependencies = TRUE)
rcmdcheck::rcmdcheck(args = "--no-manual", error_on = "error")

# Development workflow
devtools::load_all()
devtools::document()
devtools::test()
```

---

## Code Architecture

### Core Functions

| Function | Purpose |
|----------|---------|
| `medsim_config()` | Environment-aware configuration |
| `medsim_run()` | Execute simulations with parallel processing |
| `medsim_scenario()` | Define custom simulation scenarios |
| `medsim_scenarios_mediation()` | Standard mediation scenarios |
| `medsim_analyze()` | Summarize simulation results |
| `medsim_figures()` | Generate publication-ready figures |
| `medsim_tables()` | Generate LaTeX tables |
| `medsim_workflow()` | Complete simulation-to-manuscript pipeline |

### Execution Modes

| Mode | Replications | Use Case |
|------|--------------|----------|
| `test` | 20 | Quick validation |
| `local` | 100 | Development |
| `cluster` | 1000+ | Production |

### Missing-data DGM generators (in progress — `feature/dgm-interface`)

Reusable data-generating utilities + missing-data mediation estimator adapters, added for the
**Missing Effect** study (MBCO-MI vs Monte-Carlo CI under missingness × nonnormality) and reusable by
`sensitivity` / `measurement error`. Spec: `SPEC-medsim-missingdata-generators-2026-06-11.md`.

| Function | Purpose |
|----------|---------|
| `medsim_rnonnormal()` | Draw values with target skew/kurtosis (Fleishman power method) |
| `medsim_amputate()` | Insert `NA`s under MCAR / MAR / MNAR (logistic amputer, rate-calibrated) |
| `medsim_scenario_missing()` / `medsim_scenario_missing_grid()` | Missing-data mediation scenarios (factorial) |
| `medsim_method_mbco_mi()` / `medsim_method_mc_ci()` / `medsim_method_ipw()` | Estimator adapters (the headline MBCO-MI vs MC-CI pair; thin IPW) |
| `medsim_summarize_branch_switch()` | Summarize the MBCO union-null branch-switch rate per scenario |

Design rules: estimator-agnostic boundary preserved (these are `data_generator`/`method` helpers);
heavy deps (`mice`, `missingmed`, `rmediation`) stay in **Suggests** (loaded via `requireNamespace`);
the `method()` contract returns the 6-field list `{indirect, indirect_ci_lower/_upper, indirect_p,
branch_switch, converged}`. Status: WS-A..E implemented; **WS-F integration pending** (DESCRIPTION
Suggests + roxygen + `R CMD check`). See `WORKSTREAM-KICKOFFS.md` on the branch.

---

## Ecosystem Coordination

medsim is a **support package** in the mediationverse ecosystem.

### Role in Ecosystem

medsim tests and validates methods from other mediationverse packages:

| Package | What medsim tests |
|---------|-------------------|
| medfit | Model extraction, bootstrap inference |
| probmed | P_med coverage, accuracy |
| RMediation | DOP/MBCO CI coverage |
| medrobust | Sensitivity bounds |

### Central Planning

Ecosystem coordination managed in `/Users/dt/mediation-planning/`:
- `ECOSYSTEM-COORDINATION.md` - Version matrix, change propagation
- `MONTHLY-CHECKLIST.md` - Recurring ecosystem health checks

| Document | Purpose |
|----------|---------|
| `ECOSYSTEM-COORDINATION.md` | Version matrix, change propagation |
| `MONTHLY-CHECKLIST.md` | Recurring ecosystem health checks |
| `templates/README-template.md` | Standardized README structure |
| `templates/NEWS-template.md` | Standardized NEWS.md format |

### Testing Other Packages

When simulating methods from medfit or other packages:

```r
library(medsim)
library(medfit)
library(probmed)

pmed_method <- function(data, params) {
  fit_m <- lm(M ~ X, data = data)
  fit_y <- lm(Y ~ X + M, data = data)

  med_data <- medfit::extract_mediation(
    fit_m, model_y = fit_y,
    treatment = "X", mediator = "M"
  )

  p_med <- probmed::compute_pmed(med_data)

  list(estimate = p_med, truth = params$true_pmed)
}

results <- medsim_run(pmed_method, scenarios, config)
```

### Compatibility

- Requires: R >= 4.1.0
- Suggested: medfit, probmed, RMediation, medrobust (for method testing)

### GitHub-only Sibling Dependencies

Sibling packages medfit, medrobust, and probmed are NOT on CRAN. DESCRIPTION
must include a `Remotes:` field so pak can resolve them during R-CMD-check:

```
Remotes:
    Data-Wise/medfit,
    Data-Wise/medrobust,
    Data-Wise/probmed
```

RMediation IS on CRAN — do not list it under Remotes. Without this hint, pak
treats these as missing packages and R-CMD-check fails. Added in PR #1
(2026-05-09).

---

## Related Packages

| Package | Repository | Purpose |
|---------|-----------|---------|
| medfit | https://github.com/data-wise/medfit | Foundation (model fitting, extraction) |
| probmed | https://github.com/data-wise/probmed | P_med effect size |
| RMediation | https://github.com/data-wise/rmediation | Confidence intervals |
| medrobust | https://github.com/data-wise/medrobust | Sensitivity analysis |

---

## Development Guidelines

### Naming Conventions

- Functions: `medsim_*()` prefix for all exported functions
- Internal functions: `.medsim_*()` with dot prefix
- Arguments: `snake_case`

### Testing

- Target: >80% coverage
- Test all execution modes (test, local)
- Test HPC detection logic
- Test output generation (figures, tables)

---

## CI / GitHub Actions Notes

- **Workflow auto-disable**: GitHub auto-disables scheduled workflows that
  fail for 60+ days without a manual successful run. Re-enable via:
  `gh api -X PUT repos/Data-Wise/medsim/actions/workflows/<id>/enable`
- **`pull_request.branches:` filter is base-branch-matched**: GitHub
  Actions matches the PR's *target* branch, not the head. R-CMD-check
  silently skipped every PR to `dev` until PR #2 added `dev` to the
  filter (2026-05-09). When introducing a new integration branch, audit
  every workflow's `pull_request.branches:` list — they don't auto-update.
- **Branch protection on `main`**: PR required, no force-push, no deletions.
  R-CMD-check is now a candidate to add as a required status check (it
  reliably fires on PRs to `dev` post-fix, and passes when DESCRIPTION's
  `Remotes:` field is in place).
- **R-devel runs on a separate weekly schedule** (PR #3, 2026-05-09).
  PR-time `R-CMD-check.yaml` covers macOS, Windows, Ubuntu release +
  oldrel-1 (~4 min). Weekly `R-CMD-check-devel.yaml` covers Ubuntu
  R-devel via cron (Mon 08:00 UTC) + `workflow_dispatch`. Re-add
  `ubuntu-devel` to `R-CMD-check.yaml`'s matrix before any CRAN
  submission. Note: `schedule:` triggers only fire from the *default*
  branch (main), so the cron won't activate until the workflow file
  lands on main via a dev → main release.
- **Concurrency cancel-in-progress** is set on `R-CMD-check.yaml` and
  `test-coverage.yaml` (PR #3). Rapid pushes cancel stale runs.
  `pkgdown.yaml` keeps its own job-level concurrency design (unique
  group per PR run); leave it alone.
- **`R-hub` workflow** (`.github/workflows/rhub.yaml`, restored from main
  via the dev/main reconciliation) is registered with GitHub but
  scheduled-only / dispatch-only. Use for multi-platform pre-CRAN testing.

---

**Last Updated**: 2026-05-10
