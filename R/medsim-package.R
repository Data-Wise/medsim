#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom stats aggregate median quantile rnorm sd
#' @importFrom utils glob2rx write.csv
## usethis namespace: end
NULL

#' medsim: Simulation Infrastructure for Mediation Analysis
#'
#' @description
#' medsim provides standardized infrastructure for conducting Monte Carlo
#' simulation studies in mediation analysis. The package is designed to work
#' seamlessly with the mediation analysis ecosystem.
#'
#' @details
#' ## Core Features
#'
#' - **Environment-aware execution**: Automatically detects local vs HPC cluster
#' - **Parallel processing**: Built-in parallelization with progress reporting
#' - **Three execution modes**: test (<1 min), local (~15 min), cluster (hours)
#' - **Ground truth caching**: Avoid expensive recomputation
#' - **Automated analysis**: Generate summary statistics automatically
#' - **Publication-ready output**: Figures and LaTeX tables with one function
#'
#' ## Key Functions
#'
#' **Configuration**:
#' - [medsim_config()] - Create simulation configuration
#' - [medsim_detect_environment()] - Detect local vs cluster
#'
#' **Scenarios**:
#' - [medsim_scenarios_mediation()] - Standard mediation scenarios
#' - [medsim_scenario()] - Define custom scenario
#'
#' **Execution**:
#' - [medsim_run()] - Run simulation study
#' - [medsim_run_parallel()] - Parallel execution
#'
#' **Analysis**:
#' - [medsim_analyze()] - Analyze simulation results
#' - [medsim_analyze_coverage()] - Analyze coverage rates
#' - [medsim_analyze_power()] - Analyze statistical power
#' - [medsim_table_accuracy()] - Create accuracy tables
#' - [medsim_plot_error_boxplot()] - Generate error boxplots
#'
#' ## Quick Start
#'
#' Basic simulation workflow:
#'
#' ```r
#' library(medsim)
#'
#' # Define your method
#' my_method <- function(data, params) {
#'   # ... your implementation ...
#'   list(estimate = ..., se = ...)
#' }
#'
#' # Configure and run
#' config <- medsim_config("local")
#' scenarios <- medsim_scenarios_mediation()
#'
#' results <- medsim_run(
#'   method = my_method,
#'   scenarios = scenarios,
#'   config = config
#' )
#'
#' # Analyze results
#' analysis <- medsim_analyze(results)
#' ```
#'
#' ## Execution Modes
#'
#' Three modes for different use cases:
#'
#' | Mode | Replications | Runtime | Use Case |
#' |------|--------------|---------|----------|
#' | test | 20 | ~30s | Quick validation |
#' | local | 100 | ~15m | Development |
#' | cluster | 1000 | hours | Production |
#'
#' ## Integration with Mediation Ecosystem
#'
#' medsim is designed to work with:
#'
#' - **medfit**: Model infrastructure and extraction
#' - **probmed**: Probabilistic effect size (P_med)
#' - **RMediation**: Confidence intervals (DOP, MBCO, MC)
#' - **medrobust**: Sensitivity analysis
#'
#' ## Learn More
#'
#' - Getting Started: `vignette("getting-started", package = "medsim")`
#' - Custom Scenarios: `vignette("custom-scenarios", package = "medsim")`
#' - HPC Clusters: `vignette("parallel-computing", package = "medsim")`
#' - GitHub: <https://github.com/data-wise/medsim>
#' - Website: <https://data-wise.github.io/medsim/>
NULL
