# Package index

## Configuration & Environment

Functions for configuring simulation execution and detecting the runtime
environment (local vs HPC cluster).

- [`medsim_config()`](https://data-wise.github.io/medsim/reference/medsim_config.md)
  : Create Simulation Configuration
- [`medsim_detect_environment()`](https://data-wise.github.io/medsim/reference/medsim_detect_environment.md)
  : Detect Computing Environment
- [`medsim_detect_cores()`](https://data-wise.github.io/medsim/reference/medsim_detect_cores.md)
  : Detect Number of Available Cores
- [`medsim_get_optimal_cores()`](https://data-wise.github.io/medsim/reference/medsim_get_optimal_cores.md)
  : Get Optimal Number of Cores
- [`medsim_estimate_speedup()`](https://data-wise.github.io/medsim/reference/medsim_estimate_speedup.md)
  : Estimate Parallel Speedup
- [`medsim_compare_configs()`](https://data-wise.github.io/medsim/reference/medsim_compare_configs.md)
  : Compare Multiple Configurations

## Simulation Scenarios

Functions for defining and validating simulation scenarios with ground
truth parameters.

- [`medsim_scenario()`](https://data-wise.github.io/medsim/reference/medsim_scenario.md)
  : Create Custom Simulation Scenario
- [`medsim_scenarios_mediation()`](https://data-wise.github.io/medsim/reference/medsim_scenarios_mediation.md)
  : Create Standard Mediation Scenarios
- [`medsim_validate_scenario()`](https://data-wise.github.io/medsim/reference/medsim_validate_scenario.md)
  : Validate Scenario

## Running Simulations

Core functions for executing Monte Carlo simulations with progress
tracking and parallel processing.

- [`medsim_run()`](https://data-wise.github.io/medsim/reference/medsim_run.md)
  : Run Simulation Study
- [`medsim_run_parallel()`](https://data-wise.github.io/medsim/reference/medsim_run_parallel.md)
  : Run Tasks in Parallel

## Result Analysis

Functions for analyzing simulation results including coverage, power,
and method comparisons.

- [`medsim_analyze()`](https://data-wise.github.io/medsim/reference/medsim_analyze.md)
  : Analyze Simulation Results
- [`medsim_analyze_coverage()`](https://data-wise.github.io/medsim/reference/medsim_analyze_coverage.md)
  : Analyze Coverage Rates
- [`medsim_analyze_power()`](https://data-wise.github.io/medsim/reference/medsim_analyze_power.md)
  : Analyze Statistical Power
- [`medsim_compare_methods()`](https://data-wise.github.io/medsim/reference/medsim_compare_methods.md)
  : Compare Multiple Methods
- [`medsim_check_results()`](https://data-wise.github.io/medsim/reference/medsim_check_results.md)
  : Check Results for Errors

## Caching

Functions for caching ground truth values and simulation results to
avoid redundant computation.

- [`medsim_cache_init()`](https://data-wise.github.io/medsim/reference/medsim_cache_init.md)
  : Initialize Cache Directory
- [`medsim_cache_save()`](https://data-wise.github.io/medsim/reference/medsim_cache_save.md)
  : Save Object to Cache
- [`medsim_cache_load()`](https://data-wise.github.io/medsim/reference/medsim_cache_load.md)
  : Load Object from Cache
- [`medsim_cache_exists()`](https://data-wise.github.io/medsim/reference/medsim_cache_exists.md)
  : Check if Cache Exists
- [`medsim_cache_list()`](https://data-wise.github.io/medsim/reference/medsim_cache_list.md)
  : List Cache Files
- [`medsim_cache_info()`](https://data-wise.github.io/medsim/reference/medsim_cache_info.md)
  : Get Cache Info
- [`medsim_cache_clear()`](https://data-wise.github.io/medsim/reference/medsim_cache_clear.md)
  : Clear Cache

## Visualization

Functions for creating publication-ready figures from simulation
results.

- [`medsim_plot_coverage()`](https://data-wise.github.io/medsim/reference/medsim_plot_coverage.md)
  : Plot Coverage Rates
- [`medsim_plot_error_boxplot()`](https://data-wise.github.io/medsim/reference/medsim_plot_error_boxplot.md)
  : Plot Error Distribution Boxplots
- [`medsim_plot_timing()`](https://data-wise.github.io/medsim/reference/medsim_plot_timing.md)
  : Plot Timing Comparison
- [`medsim_plot_combined_panel()`](https://data-wise.github.io/medsim/reference/medsim_plot_combined_panel.md)
  : Create Combined Multi-Panel Figure

## LaTeX Tables

Functions for generating publication-ready LaTeX tables from simulation
results.

- [`medsim_table_accuracy()`](https://data-wise.github.io/medsim/reference/medsim_table_accuracy.md)
  : Generate Accuracy Table
- [`medsim_table_comparison()`](https://data-wise.github.io/medsim/reference/medsim_table_comparison.md)
  : Generate Method Comparison Table
- [`medsim_table_coverage()`](https://data-wise.github.io/medsim/reference/medsim_table_coverage.md)
  : Generate Coverage Table
- [`medsim_table_power()`](https://data-wise.github.io/medsim/reference/medsim_table_power.md)
  : Generate Power Table
- [`medsim_table_timing()`](https://data-wise.github.io/medsim/reference/medsim_table_timing.md)
  : Generate Timing Comparison Table
- [`medsim_tables_workflow()`](https://data-wise.github.io/medsim/reference/medsim_tables_workflow.md)
  : Generate All Tables
- [`medsim_write_table()`](https://data-wise.github.io/medsim/reference/medsim_write_table.md)
  : Write Table to File

## Print & Summary Methods

S3 methods for printing and summarizing medsim objects.

- [`print(`*`<medsim_analysis>`*`)`](https://data-wise.github.io/medsim/reference/print.medsim_analysis.md)
  : Print Analysis Results
- [`print(`*`<medsim_comparison>`*`)`](https://data-wise.github.io/medsim/reference/print.medsim_comparison.md)
  : Print Method Comparison
- [`print(`*`<medsim_config>`*`)`](https://data-wise.github.io/medsim/reference/print.medsim_config.md)
  : Print Configuration Summary
- [`print(`*`<medsim_coverage>`*`)`](https://data-wise.github.io/medsim/reference/print.medsim_coverage.md)
  : Print Coverage Results
- [`print(`*`<medsim_power>`*`)`](https://data-wise.github.io/medsim/reference/print.medsim_power.md)
  : Print Power Results
- [`print(`*`<medsim_results>`*`)`](https://data-wise.github.io/medsim/reference/print.medsim_results.md)
  : Print Simulation Results
- [`print(`*`<medsim_scenario>`*`)`](https://data-wise.github.io/medsim/reference/print.medsim_scenario.md)
  : Print Scenario Summary
- [`print(`*`<medsim_table>`*`)`](https://data-wise.github.io/medsim/reference/print.medsim_table.md)
  : Print medsim_table
- [`summary(`*`<medsim_results>`*`)`](https://data-wise.github.io/medsim/reference/summary.medsim_results.md)
  : Summarize Simulation Results
