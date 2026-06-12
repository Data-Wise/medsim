# CLAUDE.md for medsim Package

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository.

------------------------------------------------------------------------

## About This Package

**medsim** is the mediationverse’s standardized infrastructure for Monte
Carlo simulation studies in mediation analysis — a reusable framework so
parallel processing, progress reporting, result analysis, and
visualization don’t get reimplemented per project.

### Key Features

- Environment-aware execution (local vs HPC cluster)
- Three execution modes (test, local, cluster)
- Parallel processing with progress bars
- Ground truth caching
- Automated analysis and visualization
- Publication-ready output (figures and LaTeX tables)
- Missing-data + nonnormality DGM generators + missing-data mediation
  estimator adapters (shipped in **v0.2.0**; see Code Architecture
  below)

------------------------------------------------------------------------

## Common Development Commands

``` r

# Install dependencies and check package
remotes::install_deps(dependencies = TRUE)
rcmdcheck::rcmdcheck(args = "--no-manual", error_on = "error")

# Development workflow
devtools::load_all()
devtools::document()
devtools::test()
```

------------------------------------------------------------------------

## Code Architecture

### Core Functions

| Function | Purpose |
|----|----|
| [`medsim_config()`](https://data-wise.github.io/medsim/dev/reference/medsim_config.md) | Environment-aware configuration |
| [`medsim_run()`](https://data-wise.github.io/medsim/dev/reference/medsim_run.md) | Execute simulations with parallel processing |
| [`medsim_scenario()`](https://data-wise.github.io/medsim/dev/reference/medsim_scenario.md) | Define custom simulation scenarios |
| [`medsim_scenarios_mediation()`](https://data-wise.github.io/medsim/dev/reference/medsim_scenarios_mediation.md) | Standard mediation scenarios |
| [`medsim_analyze()`](https://data-wise.github.io/medsim/dev/reference/medsim_analyze.md) | Summarize simulation results |
| [`medsim_figures()`](https://data-wise.github.io/medsim/dev/reference/medsim_figures.md) | Generate publication-ready figures |
| [`medsim_tables()`](https://data-wise.github.io/medsim/dev/reference/medsim_tables.md) | Generate LaTeX tables |
| [`medsim_workflow()`](https://data-wise.github.io/medsim/dev/reference/medsim_workflow.md) | Complete simulation-to-manuscript pipeline |

### Execution Modes

| Mode      | Replications | Use Case         |
|-----------|--------------|------------------|
| `test`    | 20           | Quick validation |
| `local`   | 100          | Development      |
| `cluster` | 1000+        | Production       |

### Missing-data DGM generators (shipped in v0.2.0)

Reusable data-generating utilities + missing-data mediation estimator
adapters, added for the **Missing Effect** study (MBCO-MI vs Monte-Carlo
CI under missingness × nonnormality) and reusable by `sensitivity` /
`measurement error`. Spec:
`SPEC-medsim-missingdata-generators-2026-06-11.md`.

| Function | Purpose |
|----|----|
| [`medsim_rnonnormal()`](https://data-wise.github.io/medsim/dev/reference/medsim_rnonnormal.md) | Draw values with target skew/kurtosis (Fleishman power method) |
| [`medsim_amputate()`](https://data-wise.github.io/medsim/dev/reference/medsim_amputate.md) | Insert `NA`s under MCAR / MAR / MNAR (logistic amputer, rate-calibrated) |
| [`medsim_scenario_missing()`](https://data-wise.github.io/medsim/dev/reference/medsim_scenario_missing.md) / [`medsim_scenario_missing_grid()`](https://data-wise.github.io/medsim/dev/reference/medsim_scenario_missing_grid.md) | Missing-data mediation scenarios (factorial) |
| [`medsim_method_mbco_mi()`](https://data-wise.github.io/medsim/dev/reference/medsim_method_mbco_mi.md) / [`medsim_method_mc_ci()`](https://data-wise.github.io/medsim/dev/reference/medsim_method_mc_ci.md) / [`medsim_method_ipw()`](https://data-wise.github.io/medsim/dev/reference/medsim_method_ipw.md) | Estimator adapters (the headline MBCO-MI vs MC-CI pair; thin IPW) |
| [`medsim_summarize_branch_switch()`](https://data-wise.github.io/medsim/dev/reference/medsim_summarize_branch_switch.md) | Summarize the MBCO union-null branch-switch rate per scenario |

Design rules: estimator-agnostic boundary preserved (these are
`data_generator`/`method` helpers); the `method()` contract returns the
6-field list
`{indirect, indirect_ci_lower/_upper, indirect_p, branch_switch, converged}`.
[`medsim_method_mbco_mi()`](https://data-wise.github.io/medsim/dev/reference/medsim_method_mbco_mi.md)
implements the validated **D4-stacked MBCO** (`mice` multiple
imputation + Reiter/Chan–Meng D4 pooling + union-null LRT; reproduces
`mitml::testModels(method = "D4")` exactly), degrading to a
complete-case MBCO chi-square test without `mice`. Suggests used: `mice`
(MI), `RMediation` (MC CI via `medci`), `mitml` (D4 validation). Status:
**complete**, released in **v0.2.0** — PR \#17 (generators) + PR \#18
(D4-MBCO port, which dropped the now-unused `missingmed`/`rmediation`
from Suggests/Remotes — the validated method uses neither). All 8
functions exported, documented, and in the pkgdown reference index.

------------------------------------------------------------------------

## Ecosystem Coordination

medsim is a **support package** in the mediationverse ecosystem.

### Role in Ecosystem

medsim tests and validates methods from other mediationverse packages:

| Package    | What medsim tests                     |
|------------|---------------------------------------|
| medfit     | Model extraction, bootstrap inference |
| probmed    | P_med coverage, accuracy              |
| RMediation | DOP/MBCO CI coverage                  |
| medrobust  | Sensitivity bounds                    |

### Central Planning

Ecosystem coordination lives in `/Users/dt/mediation-planning/`:

| Document                       | Purpose                            |
|--------------------------------|------------------------------------|
| `ECOSYSTEM-COORDINATION.md`    | Version matrix, change propagation |
| `MONTHLY-CHECKLIST.md`         | Recurring ecosystem health checks  |
| `templates/README-template.md` | Standardized README structure      |
| `templates/NEWS-template.md`   | Standardized NEWS.md format        |

### Testing Other Packages

When simulating methods from medfit or other packages:

``` r

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

- Requires: R \>= 4.1.0
- Suggested: medfit, probmed, RMediation, medrobust (for method testing)

### GitHub-only Sibling Dependencies

medsim’s GitHub-only dependencies need a `Remotes:` field so pak can
resolve them during R-CMD-check (without it, pak treats them as missing
and the check fails). Keep it in lockstep with `Suggests` — as of v0.2.0
the only GitHub-only dep is `medfit` (PR \#18’s validated D4-MBCO uses
`mice`/`mitml`/`RMediation`, all on CRAN, so `missingmed`/`rmediation`
were dropped):

    Remotes:
        Data-Wise/medfit

RMediation is on CRAN — never list it under Remotes. `Remotes:` first
added in PR \#1 (2026-05-09).

------------------------------------------------------------------------

## Related Packages

| Package | Repository | Purpose |
|----|----|----|
| medfit | <https://github.com/data-wise/medfit> | Foundation (model fitting, extraction) |
| probmed | <https://github.com/data-wise/probmed> | P_med effect size |
| RMediation | <https://github.com/data-wise/rmediation> | Confidence intervals |
| medrobust | <https://github.com/data-wise/medrobust> | Sensitivity analysis |

------------------------------------------------------------------------

## Development Guidelines

### Naming Conventions

- Functions: `medsim_*()` prefix for all exported functions
- Internal functions: `.medsim_*()` with dot prefix
- Arguments: `snake_case`

### Testing

- Target: \>80% coverage
- Test all execution modes (test, local)
- Test HPC detection logic
- Test output generation (figures, tables)

------------------------------------------------------------------------

## CI / GitHub Actions Notes

- **Workflow auto-disable**: GitHub auto-disables scheduled workflows
  that fail for 60+ days without a manual successful run. Re-enable via:
  `gh api -X PUT repos/Data-Wise/medsim/actions/workflows/<id>/enable`
- **`pull_request.branches:` filter is base-branch-matched**: GitHub
  Actions matches the PR’s *target* branch, not the head. R-CMD-check
  silently skipped every PR to `dev` until PR \#2 added `dev` to the
  filter (2026-05-09). When introducing a new integration branch, audit
  every workflow’s `pull_request.branches:` list — they don’t
  auto-update.
- **Branch protection on `main`**: PR required, no force-push, no
  deletions. R-CMD-check is now a candidate to add as a required status
  check (it reliably fires on PRs to `dev` post-fix, and passes when
  DESCRIPTION’s `Remotes:` field is in place).
- **R-devel runs on a separate weekly schedule** (PR \#3, 2026-05-09).
  PR-time `R-CMD-check.yaml` covers macOS, Windows, Ubuntu release +
  oldrel-1 (~4 min). Weekly `R-CMD-check-devel.yaml` covers Ubuntu
  R-devel via cron (Mon 08:00 UTC) + `workflow_dispatch`. Re-add
  `ubuntu-devel` to `R-CMD-check.yaml`’s matrix before any CRAN
  submission. Note: `schedule:` triggers only fire from the *default*
  branch (main), so the cron won’t activate until the workflow file
  lands on main via a dev → main release.
- **Concurrency cancel-in-progress** is set on `R-CMD-check.yaml` and
  `test-coverage.yaml` (PR \#3). Rapid pushes cancel stale runs.
  `pkgdown.yaml` keeps its own job-level concurrency design (unique
  group per PR run); leave it alone.
- **`R-hub` workflow** (`.github/workflows/rhub.yaml`, restored from
  main via the dev/main reconciliation) is registered with GitHub but
  scheduled-only / dispatch-only. Use for multi-platform pre-CRAN
  testing.
- **r-universe distribution**: medsim is published (green, all
  platforms) at the live `data-wise` universe —
  `install.packages("medsim", repos = "https://data-wise.r-universe.dev")`.
  This is a separate service from GitHub CI (rebuilds on its own ~hourly
  cron, not on push). Registry repo:
  `Data-Wise/data-wise.r-universe.dev` (`packages.json`) — the registry
  key MUST match the DESCRIPTION `Package:` field case-sensitively (the
  `rmediation`→`RMediation` mismatch fail-stopped the whole org sync).
  Full philosophy + readiness checklist: `R-UNIVERSE-STANDARDS.md` at
  repo root.
  - **No r-universe step belongs in any GitHub workflow** — it’s
    external; you don’t create CI for it. The universe rebuilds the
    **default branch** (`main`), so a release reaches it only via the
    dev→main merge (not pushes to `dev`, not tags).
  - **Release checklist — add a post-merge VERIFY step**: after dev→main
    merges, the universe lags ~hours behind `main`. Confirm it actually
    rebuilt before calling the release done:
    `curl -s https://data-wise.r-universe.dev/api/packages/medsim | python3 -c "import sys,json;print(json.load(sys.stdin)['Version'])"`
    — poll until it matches the released version. Same lag gate applies
    when a dependency must land first (e.g. medfit before missingmed’s
    IPW build).

------------------------------------------------------------------------

**Last Updated**: 2026-06-11
