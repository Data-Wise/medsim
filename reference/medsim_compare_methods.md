# Compare Multiple Methods

Compares performance of multiple estimation methods across scenarios.
Combines results from multiple simulation runs and computes relative
performance metrics.

## Usage

``` r
medsim_compare_methods(..., metrics = "all")
```

## Arguments

- ...:

  Named medsim_results objects (e.g., method1 = results1, method2 =
  results2)

- metrics:

  Character vector: which comparison metrics to compute. Options:
  "accuracy", "timing", "coverage", "all" (default)

## Value

A list with class "medsim_comparison" containing:

- `accuracy_comparison`: data.frame comparing accuracy metrics

- `timing_comparison`: data.frame comparing timing

- `coverage_comparison`: data.frame comparing coverage (if applicable)

- `summary`: Overall comparison summary

## Details

### Comparison Metrics

- **Accuracy**: MAE, RMSE, bias across methods

- **Timing**: Mean/median execution time

- **Coverage**: CI coverage rates (if CIs available)

- **Relative Efficiency**: Ratio of MSE (lower = better)

### Notes

All results must:

- Use the same scenarios (matched by name)

- Use the same configuration (n_replications, seed)

- Have ground truth available (for accuracy comparison)

## See also

[`medsim_analyze()`](https://data-wise.github.io/medsim/reference/medsim_analyze.md),
[`medsim_analyze_coverage()`](https://data-wise.github.io/medsim/reference/medsim_analyze_coverage.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Run multiple methods
results_proposed <- medsim_run(method_proposed, scenarios, config)
results_delta <- medsim_run(method_delta, scenarios, config)
results_boot <- medsim_run(method_boot, scenarios, config)

# Compare
comparison <- medsim_compare_methods(
  proposed = results_proposed,
  delta = results_delta,
  bootstrap = results_boot
)

# View accuracy comparison
print(comparison$accuracy_comparison)

# View timing comparison
print(comparison$timing_comparison)
} # }
```
