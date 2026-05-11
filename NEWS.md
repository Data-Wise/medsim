# medsim (development version)

## New functionality

* End-to-end output wrappers (`medsim_workflow()`, `medsim_figures()`,
  `medsim_tables()`) that previous documentation referenced but had not
  been implemented. They wrap the existing granular `medsim_plot_*` and
  `medsim_table_*` functions for one-call output generation.

## Bug fixes

* `medsim_run_parallel(packages = ...)` on PSOCK clusters used to crash
  with `object 'packages' not found`. The `packages` argument was a free
  variable inside a `clusterEvalQ` call and never reached the workers.
  Switched to `clusterCall`, which serializes the argument to workers.
  Regression test added.

## Testing

* Added unit tests for `R/runner.R`, `R/parallel.R`, and `R/visualize.R`,
  which were previously untested or only partially tested. Test suite
  grew substantially; key core paths now exercised.
* Silenced known-legitimate warnings in test-analyze.R and test-workflow.R
  (no-ground-truth-for-CI warnings and ggplot2 NA-removal noise) so real
  warnings remain visible.

## Infrastructure

* CI overhaul: PR-time R-CMD-check now ~4 min (was ~30 min) by moving
  R-devel to a weekly schedule (`R-CMD-check-devel.yaml`). Concurrency
  cancel-in-progress added on `R-CMD-check.yaml` and `test-coverage.yaml`.
* `R-hub` workflow now installs Quarto before running checks, so vignette
  re-build succeeds on r-hub containers.
* DESCRIPTION: added `Remotes:` for GitHub-only sibling deps
  (medfit, medrobust, probmed) so `pak::pkg_install(".")` resolves them
  during R-CMD-check.
* `.Rbuildignore` expanded to exclude local-only artifacts (`cache/`,
  `simulation_results/`, `BRAINSTORM-*.md`, `.STATUS*`, `.claude/`) and
  qmd build directories (`vignettes/.+_files`).

## medsim 0.1.0 (planned, not yet released)

**Planned scope of the 0.1.0 release.** Marked here as a forward-looking
summary of the work happening in the 0.0.0.9000 dev cycle. Will become
the canonical 0.1.0 entry once a release is tagged.

### New Features

* **Core Infrastructure** - Complete simulation framework
  - `medsim_config()` for environment-aware configuration (test/local/cluster modes)
  - `medsim_run()` for parallel simulation execution with progress tracking
  - `medsim_scenario()` for defining custom simulation scenarios
  - `medsim_scenarios_mediation()` for standard mediation scenarios

* **Analysis Functions**
  - `medsim_analyze()` for summarizing simulation results
  - `medsim_analyze_coverage()` for coverage rate computation
  - `medsim_compare_methods()` for multi-method comparison

* **Visualization**
  - `medsim_figures()` for publication-ready figure generation
  - `medsim_tables()` for LaTeX table generation

* **Complete Workflow**
  - `medsim_workflow()` - single function from simulation to manuscript-ready output

* **HPC Support**
  - Automatic SLURM/PBS/LSF environment detection
  - Parallel processing with automatic core detection
  - Ground truth caching for expensive computations

### Documentation

* Comprehensive pkgdown website at https://data-wise.github.io/medsim/
* Vignette: `getting-started.qmd` (custom-scenarios and HPC vignettes
  planned for a future release)

### Ecosystem Notes

* Part of the mediationverse ecosystem for mediation analysis
* Designed to test methods from medfit, probmed, RMediation, and medrobust
* Tested with R 4.1.0+
* See [Ecosystem Coordination](https://github.com/data-wise/medfit/blob/main/planning/ECOSYSTEM.md) for guidelines

### Internal

* GitHub Actions CI/CD with multi-platform testing (macOS, Windows,
  Ubuntu release + oldrel-1; Ubuntu R-devel weekly via cron)
* Codecov integration for coverage tracking

---

*This is a development version. Breaking changes may occur.*
