# medsim: Simulation Infrastructure for Mediation Analysis

medsim provides standardized infrastructure for conducting Monte Carlo
simulation studies in mediation analysis. The package is designed to
work seamlessly with the mediation analysis ecosystem.

## Details

### Core Features

- **Environment-aware execution**: Automatically detects local vs HPC
  cluster

- **Parallel processing**: Built-in parallelization with progress
  reporting

- **Three execution modes**: test (\<1 min), local (~15 min), cluster
  (hours)

- **Ground truth caching**: Avoid expensive recomputation

- **Automated analysis**: Generate summary statistics automatically

- **Publication-ready output**: Figures and LaTeX tables with one
  function

### Key Functions

**Configuration**:

- [`medsim_config()`](https://data-wise.github.io/medsim/reference/medsim_config.md) -
  Create simulation configuration

- [`medsim_detect_environment()`](https://data-wise.github.io/medsim/reference/medsim_detect_environment.md) -
  Detect local vs cluster

**Scenarios**:

- [`medsim_scenarios_mediation()`](https://data-wise.github.io/medsim/reference/medsim_scenarios_mediation.md) -
  Standard mediation scenarios

- [`medsim_scenario()`](https://data-wise.github.io/medsim/reference/medsim_scenario.md) -
  Define custom scenario

**Execution**:

- [`medsim_run()`](https://data-wise.github.io/medsim/reference/medsim_run.md) -
  Run simulation study

- [`medsim_run_parallel()`](https://data-wise.github.io/medsim/reference/medsim_run_parallel.md) -
  Parallel execution

**Analysis**:

- [`medsim_analyze()`](https://data-wise.github.io/medsim/reference/medsim_analyze.md) -
  Analyze simulation results

- [`medsim_analyze_coverage()`](https://data-wise.github.io/medsim/reference/medsim_analyze_coverage.md) -
  Analyze coverage rates

- [`medsim_analyze_power()`](https://data-wise.github.io/medsim/reference/medsim_analyze_power.md) -
  Analyze statistical power

- [`medsim_table_accuracy()`](https://data-wise.github.io/medsim/reference/medsim_table_accuracy.md) -
  Create accuracy tables

- [`medsim_plot_error_boxplot()`](https://data-wise.github.io/medsim/reference/medsim_plot_error_boxplot.md) -
  Generate error boxplots

### Quick Start

Basic simulation workflow:

    library(medsim)

    # Define your method
    my_method <- function(data, params) {
      # ... your implementation ...
      list(estimate = ..., se = ...)
    }

    # Configure and run
    config <- medsim_config("local")
    scenarios <- medsim_scenarios_mediation()

    results <- medsim_run(
      method = my_method,
      scenarios = scenarios,
      config = config
    )

    # Analyze results
    analysis <- medsim_analyze(results)

### Execution Modes

Three modes for different use cases:

|         |              |         |                  |
|---------|--------------|---------|------------------|
| Mode    | Replications | Runtime | Use Case         |
| test    | 20           | ~30s    | Quick validation |
| local   | 100          | ~15m    | Development      |
| cluster | 1000         | hours   | Production       |

### Integration with Mediation Ecosystem

medsim is designed to work with:

- **medfit**: Model infrastructure and extraction

- **probmed**: Probabilistic effect size (P_med)

- **RMediation**: Confidence intervals (DOP, MBCO, MC)

- **medrobust**: Sensitivity analysis

### Learn More

- Getting Started:
  [`vignette("getting-started", package = "medsim")`](https://data-wise.github.io/medsim/articles/getting-started.md)

- Custom Scenarios: `vignette("custom-scenarios", package = "medsim")`

- HPC Clusters: `vignette("parallel-computing", package = "medsim")`

- GitHub: <https://github.com/data-wise/medsim>

- Website: <https://data-wise.github.io/medsim/>

## See also

Useful links:

- <https://github.com/data-wise/medsim>

- <https://data-wise.github.io/medsim/>

- Report bugs at <https://github.com/data-wise/medsim/issues>

## Author

**Maintainer**: Davood Tofighi <dtofighi@gmail.com>
([ORCID](https://orcid.org/0000-0001-8523-7776))
