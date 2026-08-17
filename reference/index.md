# Package index

## End-to-End Workflow

Convenience wrappers that produce all standard outputs (analysis,
figures, tables) from simulation results in one call.

- [`medsim_workflow()`](https://data-wise.github.io/medsim/reference/medsim_workflow.md)
  : Generate analysis, figures, and tables from simulation results
- [`medsim_figures()`](https://data-wise.github.io/medsim/reference/medsim_figures.md)
  : Generate all standard figures from simulation results
- [`medsim_tables()`](https://data-wise.github.io/medsim/reference/medsim_tables.md)
  : Generate all standard LaTeX tables from simulation results

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
truth parameters, and the estimand-kind descriptor used by downstream
analysis commands.

- [`medsim_estimand()`](https://data-wise.github.io/medsim/reference/medsim_estimand.md)
  : Declare the estimand kind for a simulation scenario
- [`medsim_scenario()`](https://data-wise.github.io/medsim/reference/medsim_scenario.md)
  : Create Custom Simulation Scenario
- [`medsim_scenarios_mediation()`](https://data-wise.github.io/medsim/reference/medsim_scenarios_mediation.md)
  : Create Standard Mediation Scenarios
- [`medsim_validate_scenario()`](https://data-wise.github.io/medsim/reference/medsim_validate_scenario.md)
  : Validate Scenario

## Numeric Accuracy Scenarios

Scenario factory for numerical accuracy and timing studies (e.g., the
product-of-three CI comparison). Uses `estimand$kind = "numeric"` — no
DGM, no coverage/power analysis; only `error`, `abs_error`,
`elapsed_sec` result columns and timing tables.

- [`medsim_scenario_numeric()`](https://data-wise.github.io/medsim/reference/medsim_scenario_numeric.md)
  : Create a numeric accuracy simulation scenario

## P_med Probabilistic Mediation (pmed)

Scenario factory and MBCO estimator adapter for the P_med probabilistic
estimand `P(Y1 > Y0) + 0.5·P(Y1 == Y0)`. Uses
`estimand$kind = "probabilistic"` so
[`medsim_analyze_coverage()`](https://data-wise.github.io/medsim/reference/medsim_analyze_coverage.md)
dispatches the MBCO-CI coverage branch. Ground truth is computed via a
cross-world potential-outcome draw at scenario construction time.

- [`medsim_scenario_pmed()`](https://data-wise.github.io/medsim/reference/medsim_scenario_pmed.md)
  : Create a P_med simulation scenario
- [`medsim_method_pmed_mbco()`](https://data-wise.github.io/medsim/reference/medsim_method_pmed_mbco.md)
  : MBCO confidence interval for P_med (two-branch method)

## Differential-Misclassification Bounds (DM)

Scenario factory and estimator adapter for partial-identification bounds
studies (me-mediator-bounds / me-exposure-recall). Uses the
`interval`-kind estimand so that
[`medsim_analyze_coverage()`](https://data-wise.github.io/medsim/reference/medsim_analyze_coverage.md)
dispatches the Imbens-Manski / partial-ID coverage branch automatically.
Requires **medrobust** (Suggests); synthetic fallback used when absent.

- [`medsim_scenario_dm()`](https://data-wise.github.io/medsim/reference/medsim_scenario_dm.md)
  : Construct a differential-misclassification (DM) mediation scenario
- [`medsim_method_bounds()`](https://data-wise.github.io/medsim/reference/medsim_method_bounds.md)
  : Bounds estimator adapter for differential-misclassification (DM)
  studies

## Sobol / Functional-ANOVA Variance Share

Scenario factory and estimator adapter for the Sobol variance-share
proportion mediated (P_med^sigma2 = V_med/V_T). Uses the
`variance_share`-kind estimand with a closed-form ground truth; the
estimator is injected, not vendored.

- [`medsim_scenario_sobol()`](https://data-wise.github.io/medsim/reference/medsim_scenario_sobol.md)
  : Create a Sobol / functional-ANOVA variance-share simulation scenario
- [`medsim_method_sobol()`](https://data-wise.github.io/medsim/reference/medsim_method_sobol.md)
  : Sobol / functional-ANOVA variance-share estimator wrapper

## Gauge Residual (P_med + W)

Scenario factory and estimator adapter for the gauge-residual estimand
(`P_med = IIE/OE`, `W = R/OE`). Uses `variance_share`-kind with exact
closed-form corner-mean ground truth; wraps `probmed::ward_residual()`
(analytic or bootstrap percentile CI); estimator is injected, not
vendored.

- [`medsim_scenario_gauge()`](https://data-wise.github.io/medsim/reference/medsim_scenario_gauge.md)
  : Create a gauge-residual (P_med + W) simulation scenario
- [`medsim_method_gauge()`](https://data-wise.github.io/medsim/reference/medsim_method_gauge.md)
  : Gauge-residual (P_med + W) estimator wrapper

## Running Simulations

Core functions for executing Monte Carlo simulations with progress
tracking and parallel processing.

- [`medsim_run()`](https://data-wise.github.io/medsim/reference/medsim_run.md)
  : Run Simulation Study
- [`medsim_run_parallel()`](https://data-wise.github.io/medsim/reference/medsim_run_parallel.md)
  : Run Tasks in Parallel

## Hopper / SLURM Cluster Harness

Pattern for chunked SLURM array jobs on UNM CARC Hopper. Write a submit
script, run one chunk per array task, then combine the resulting RDS
files – under an integrity audit (rep-id contiguity, collapse signature,
seed collisions) – into a single `medsim_results` object for analysis.

- [`medsim_write_submit_script()`](https://data-wise.github.io/medsim/reference/medsim_write_submit_script.md)
  : Write a SLURM array submit script for chunked simulation jobs
- [`medsim_run_chunk()`](https://data-wise.github.io/medsim/reference/medsim_run_chunk.md)
  : Run one chunk of a chunked SLURM array simulation
- [`medsim_combine_chunks()`](https://data-wise.github.io/medsim/reference/medsim_combine_chunks.md)
  : Combine chunk RDS files produced by medsim_run_chunk()
- [`medsim_audit_results()`](https://data-wise.github.io/medsim/reference/medsim_audit_results.md)
  : Audit a combined results object for chunked-run integrity violations

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

## ADEMP / Performance Reporting

Functions for ADEMP-compliant performance reporting: sizing replications
to a target coverage Monte Carlo SE, SE-vs-estimate diagnostics, and
full performance summaries (bias, empirical SE, model SE, RMSE + MCSEs).
Coverage output from
[`medsim_analyze_coverage()`](https://data-wise.github.io/medsim/reference/medsim_analyze_coverage.md)
also gains `coverage_mcse`, `n_failed`, and `failure_rate` columns in
v0.4.0.

- [`medsim_nsim_for_mcse()`](https://data-wise.github.io/medsim/reference/medsim_nsim_for_mcse.md)
  : Replications needed for a target coverage Monte Carlo SE
- [`medsim_plot_se_vs_estimate()`](https://data-wise.github.io/medsim/reference/medsim_plot_se_vs_estimate.md)
  : Diagnostic scatter of model SE against the point estimate
- [`medsim_analyze_performance()`](https://data-wise.github.io/medsim/reference/medsim_analyze_performance.md)
  : ADEMP performance summary (bias, SEs, RMSE) with Monte Carlo SEs
- [`medsim_table_performance()`](https://data-wise.github.io/medsim/reference/medsim_table_performance.md)
  : Render a performance summary as a LaTeX table

## Missing-Data & Nonnormality (DGM)

Data-generating utilities and missing-data mediation estimator adapters
for simulations under missingness and nonnormality (the Missing Effect
study).
[`medsim_method_mbco_mi()`](https://data-wise.github.io/medsim/reference/medsim_method_mbco_mi.md)
offers own- or fixed-branch ARIV pooling (`ariv =`) and always emits
branch-mixing diagnostics.

- [`medsim_rnonnormal()`](https://data-wise.github.io/medsim/reference/medsim_rnonnormal.md)
  : Draw nonnormal values with a target marginal skew/kurtosis
- [`medsim_amputate()`](https://data-wise.github.io/medsim/reference/medsim_amputate.md)
  : Insert missing values under MCAR / MAR / MNAR
- [`medsim_scenario_missing()`](https://data-wise.github.io/medsim/reference/medsim_scenario_missing.md)
  : Construct a missing-data mediation scenario
- [`medsim_scenario_missing_grid()`](https://data-wise.github.io/medsim/reference/medsim_scenario_missing_grid.md)
  : Build the full factorial of missing-data scenarios
- [`medsim_method_mbco_mi()`](https://data-wise.github.io/medsim/reference/medsim_method_mbco_mi.md)
  : MBCO-MI estimator adapter
- [`medsim_method_mc_ci()`](https://data-wise.github.io/medsim/reference/medsim_method_mc_ci.md)
  : Monte-Carlo CI estimator adapter
- [`medsim_method_ipw()`](https://data-wise.github.io/medsim/reference/medsim_method_ipw.md)
  : Thin IPW estimator adapter (robustness appendix)
- [`medsim_summarize_branch_switch()`](https://data-wise.github.io/medsim/reference/medsim_summarize_branch_switch.md)
  : Summarize the MBCO branch-switch rate per scenario

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
