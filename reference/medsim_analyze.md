# Analyze Simulation Results

Computes comprehensive accuracy metrics for simulation results including
bias, mean absolute error (MAE), root mean squared error (RMSE), and
relative efficiency.

## Usage

``` r
medsim_analyze(results, metrics = "all", by_scenario = TRUE)
```

## Arguments

- results:

  A medsim_results object from
  [`medsim_run()`](https://data-wise.github.io/medsim/reference/medsim_run.md)

- metrics:

  Character vector: which metrics to compute. Options:

  - "bias": Mean estimation error

  - "mae": Mean absolute error

  - "rmse": Root mean squared error

  - "median_ae": Median absolute error

  - "max_ae": Maximum absolute error

  - "relative_bias": Bias as percentage of truth

  - "all": Compute all metrics (default)

- by_scenario:

  Logical: Compute metrics separately for each scenario (default TRUE)

## Value

A list with class "medsim_analysis" containing:

- `accuracy`: data.frame with accuracy metrics

- `by_scenario`: data.frame with metrics by scenario (if
  by_scenario=TRUE)

- `summary`: Overall summary statistics

## Details

### Metrics Computed

For each estimated parameter with known ground truth:

- **Bias**: Mean estimation error (estimate - truth)

- **MAE**: Mean absolute error, robust to outliers

- **RMSE**: Root mean squared error, penalizes large errors

- **Median AE**: Median absolute error, very robust

- **Max AE**: Maximum absolute error, identifies worst case

- **Relative Bias**: Bias as percentage of true value

### Interpretation

- Bias close to 0: unbiased estimator

- MAE/RMSE small: accurate estimator

- RMSE \> MAE: presence of outliers/large errors

- Relative bias: standardized bias metric

## See also

[`medsim_analyze_coverage()`](https://data-wise.github.io/medsim/reference/medsim_analyze_coverage.md),
[`medsim_compare_methods()`](https://data-wise.github.io/medsim/reference/medsim_compare_methods.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Run simulation
results <- medsim_run(method, scenarios, config)

# Analyze accuracy
analysis <- medsim_analyze(results)

# View overall metrics
print(analysis$accuracy)

# View by-scenario metrics
print(analysis$by_scenario)

# Custom metrics
analysis <- medsim_analyze(
  results,
  metrics = c("bias", "mae", "rmse"),
  by_scenario = FALSE
)
} # }
```
