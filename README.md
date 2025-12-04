# medsim <img src="man/figures/logo.png" align="right" height="139" alt="medsim website" />

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Repo Status](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![R-CMD-check](https://github.com/data-wise/medsim/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/data-wise/medsim/actions/workflows/R-CMD-check.yaml)
[![Website Status](https://github.com/data-wise/medsim/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/data-wise/medsim/actions/workflows/pkgdown.yaml)
[![R-hub](https://github.com/data-wise/medsim/actions/workflows/rhub.yaml/badge.svg)](https://github.com/data-wise/medsim/actions/workflows/rhub.yaml)
[![Codecov](https://codecov.io/gh/data-wise/medsim/graph/badge.svg)](https://codecov.io/gh/data-wise/medsim)
<!-- badges: end -->

**medsim** provides standardized infrastructure for conducting Monte Carlo simulation studies in mediation analysis. Eliminate the need to repeatedly implement parallel processing, progress reporting, result analysis, and visualization across different research projects.

## Features

- **Environment-Aware Execution**: Automatically detects local machine vs HPC cluster
- **Three Execution Modes**: test (~30s), local (~15min), cluster (hours)
- **Parallel Processing**: Built-in parallelization with progress bars
- **Ground Truth Caching**: Avoid expensive recomputation
- **Automated Analysis**: Summary statistics, accuracy metrics, coverage rates
- **Publication-Ready Output**: Figures and LaTeX tables with one function call

## Mediationverse Ecosystem

**medsim** is part of the **mediationverse** ecosystem for mediation analysis in R:

| Package | Purpose | Role |
|---------|---------|------|
| [medfit](https://data-wise.github.io/medfit/) | Model fitting, extraction, bootstrap | Foundation |
| [probmed](https://data-wise.github.io/probmed/) | Probabilistic effect size (P_med) | Application |
| [RMediation](https://cran.r-project.org/package=RMediation) | Confidence intervals (DOP, MBCO) | Application |
| [medrobust](https://data-wise.github.io/medrobust/) | Sensitivity analysis | Application |
| **medsim** (this) | Simulation infrastructure | Support |

See [Ecosystem Coordination](https://github.com/data-wise/medfit/blob/main/planning/ECOSYSTEM.md) for version compatibility and development guidelines.

## Installation

Install the development version from GitHub:

``` r
# install.packages("pak")
pak::pak("data-wise/medsim")
```

## Quick Start

```r
library(medsim)

# 1. Define your estimation method
my_method <- function(data, params) {
  fit_m <- lm(M ~ X, data = data)
  fit_y <- lm(Y ~ X + M, data = data)

  a <- coef(fit_m)["X"]
  b <- coef(fit_y)["M"]

  list(indirect = a * b, a = a, b = b)
}

# 2. Configure and run simulation
config <- medsim_config("local")
scenarios <- medsim_scenarios_mediation()

results <- medsim_run(
  method = my_method,
  scenarios = scenarios,
  config = config
)

# 3. Generate analysis and output
medsim_workflow(results, output_dir = "results")
```

**Output**: Complete simulation results with figures and tables in `results/`

## Execution Modes

Three modes optimized for different use cases:

| Mode | Replications | Runtime | Use Case |
|------|--------------|---------|----------|
| `test` | 20 | ~30s | Quick validation |
| `local` | 100 | ~15m | Development |
| `cluster` | 1000+ | hours | Production runs |

```r
# Auto-detect environment
config <- medsim_config(mode = "auto")

# Or specify explicitly
config_test <- medsim_config(mode = "test")      # Quick check
config_local <- medsim_config(mode = "local")    # Development
config_cluster <- medsim_config(mode = "cluster")  # Production
```

## Standard Scenarios

Six standard mediation scenarios covering common data-generating patterns:

```r
scenarios <- medsim_scenarios_mediation()

# 1. Independent paths (ρ = 0)
# 2. Moderate correlation (ρ = 0.3)
# 3. High correlation (ρ = 0.7)
# 4. Suppression (mixed signs)
# 5. Non-zero direct effect
# 6. Unequal variances

# View scenario details
print(scenarios[[1]])

# Generate data from a scenario
data <- scenarios[[1]]$data_generator(n = 200)
```

## Custom Scenarios

Define custom data-generating processes:

```r
my_scenario <- medsim_scenario(
  name = "Large Effects",
  description = "Both paths have large effects (a = b = 0.7)",
  data_generator = function(n = 200) {
    X <- rnorm(n)
    M <- 0.7 * X + rnorm(n)
    Y <- 0.7 * M + rnorm(n)
    data.frame(X = X, M = M, Y = Y)
  },
  params = list(
    a = 0.7,
    b = 0.7,
    indirect = 0.49
  )
)

# Combine with standard scenarios
all_scenarios <- c(medsim_scenarios_mediation(), list(my_scenario))
```

## Complete Workflow

Full pipeline from simulation to manuscript-ready output:

```r
# Run simulation
results <- medsim_run(
  method = my_method,
  scenarios = scenarios,
  config = medsim_config("local")
)

# Analyze results
analysis <- medsim_analyze(results)
coverage <- medsim_analyze_coverage(results)
power <- medsim_analyze_power(results)

# Create publication-ready figures
medsim_plot_coverage(coverage, output_file = "figures/coverage.pdf")
medsim_plot_error_boxplot(results, output_file = "figures/errors.pdf")

# Generate LaTeX tables
medsim_tables_workflow(results, output_dir = "tables")
```

**Generates**:
- Summary statistics and accuracy metrics
- Publication-ready PDF figures
- LaTeX tables ready for manuscript inclusion

## HPC Cluster Support

Automatic detection of SLURM, PBS, and LSF job schedulers:

```bash
#!/bin/bash
#SBATCH --job-name=medsim
#SBATCH --nodes=1
#SBATCH --cpus-per-task=20
#SBATCH --time=24:00:00

module load R/4.3.0
Rscript run_simulation.R
```

```r
# run_simulation.R
library(medsim)

# Auto-detects cluster and uses all allocated cores
config <- medsim_config(mode = "auto")

results <- medsim_run(
  method = my_method,
  scenarios = scenarios,
  config = config
)
```

## Integration with Mediation Ecosystem

Designed to work seamlessly with other mediationverse packages:

```r
library(medsim)
library(medfit)
library(probmed)

# Simulate P_med coverage
pmed_method <- function(data, params) {
  fit_m <- lm(M ~ X, data = data)
  fit_y <- lm(Y ~ X + M, data = data)

  med_data <- medfit::extract_mediation(
    fit_m, model_y = fit_y,
    treatment = "X", mediator = "M"
  )

  p_med <- probmed::compute_pmed(med_data)

  # Bootstrap CI
  boot <- medfit::bootstrap_mediation(
    med_data,
    statistic = probmed::compute_pmed,
    n_boot = 1000
  )

  list(
    estimate = p_med,
    ci_lower = boot@ci_lower,
    ci_upper = boot@ci_upper,
    truth = params$true_pmed
  )
}

results <- medsim_run(pmed_method, scenarios, config)
coverage <- medsim_analyze_coverage(results)
```

## Method Comparison

Compare performance across different estimation approaches:

```r
methods <- list(
  pmed = function(data, params) {
    # Compute P_med with bootstrap CI
  },

  dop = function(data, params) {
    # Distribution of Product CI
  },

  bounds = function(data, params) {
    # Sensitivity analysis bounds
  }
)

results <- medsim_compare_methods(
  methods = methods,
  scenarios = scenarios,
  config = config
)

# Generate comparison table
medsim_comparison_table(results)
#>           Accuracy  Coverage  Power  Runtime
#> P_med     0.95      0.948     0.82   3.2s
#> DOP CI    0.94      0.952     0.81   0.1s
#> Bounds    0.93      0.945     0.78   1.5s
```

## Design Philosophy

Inspired by simulation infrastructure in successful R packages and academic projects:

1. **Configuration Over Repetition**: Write simulation logic once, run in multiple modes
2. **Environment Awareness**: Seamlessly scale from laptop to HPC cluster
3. **Reproducibility by Design**: Automatic seed management, session tracking
4. **Publication Ready**: One function generates manuscript-ready output

## Documentation

- [Getting Started](https://data-wise.github.io/medsim/articles/getting-started.html) - Step-by-step tutorial
- [Function Reference](https://data-wise.github.io/medsim/reference/index.html) - Complete API documentation
- [Package Website](https://data-wise.github.io/medsim/) - Comprehensive guides and examples

## Citation

If you use **medsim** in your research, please cite:

```r
citation("medsim")
```

## Getting Help

- [GitHub Issues](https://github.com/data-wise/medsim/issues) - Bug reports and feature requests
- [Documentation](https://data-wise.github.io/medsim/) - Comprehensive guides
- [Discussions](https://github.com/data-wise/medsim/discussions) - Questions and community support

## Code of Conduct

This project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html). By contributing, you agree to abide by its terms.

## License

GPL (>= 3)

## Contact

**Maintainer**: Davood Tofighi (dtofighi@gmail.com)
**ORCID**: [0000-0001-8523-7776](https://orcid.org/0000-0001-8523-7776)
