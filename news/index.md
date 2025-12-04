# Changelog

## medsim (development version)

### medsim 0.1.0

**Initial development release**

#### New Features

- **Core Infrastructure** - Complete simulation framework
  - [`medsim_config()`](https://data-wise.github.io/medsim/reference/medsim_config.md)
    for environment-aware configuration (test/local/cluster modes)
  - [`medsim_run()`](https://data-wise.github.io/medsim/reference/medsim_run.md)
    for parallel simulation execution with progress tracking
  - [`medsim_scenario()`](https://data-wise.github.io/medsim/reference/medsim_scenario.md)
    for defining custom simulation scenarios
  - [`medsim_scenarios_mediation()`](https://data-wise.github.io/medsim/reference/medsim_scenarios_mediation.md)
    for standard mediation scenarios
- **Analysis Functions**
  - [`medsim_analyze()`](https://data-wise.github.io/medsim/reference/medsim_analyze.md)
    for summarizing simulation results
  - [`medsim_analyze_coverage()`](https://data-wise.github.io/medsim/reference/medsim_analyze_coverage.md)
    for coverage rate computation
  - [`medsim_compare_methods()`](https://data-wise.github.io/medsim/reference/medsim_compare_methods.md)
    for multi-method comparison
- **Visualization**
  - `medsim_figures()` for publication-ready figure generation
  - `medsim_tables()` for LaTeX table generation
- **Complete Workflow**
  - `medsim_workflow()` - single function from simulation to
    manuscript-ready output
- **HPC Support**
  - Automatic SLURM/PBS/LSF environment detection
  - Parallel processing with automatic core detection
  - Ground truth caching for expensive computations

#### Documentation

- Comprehensive pkgdown website at <https://data-wise.github.io/medsim/>
- Vignettes for getting started, custom scenarios, and HPC usage

#### Ecosystem Notes

- Part of the mediationverse ecosystem for mediation analysis
- Designed to test methods from medfit, probmed, RMediation, and
  medrobust
- Tested with R 4.1.0+
- See [Ecosystem
  Coordination](https://github.com/data-wise/medfit/blob/main/planning/ECOSYSTEM.md)
  for guidelines

#### Internal

- 313+ tests passing
- GitHub Actions CI/CD with multi-platform testing
- Codecov integration for coverage tracking

------------------------------------------------------------------------

*This is a development version. Breaking changes may occur.*
