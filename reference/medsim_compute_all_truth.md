# Compute Ground Truth for All Scenarios

Internal function to compute ground truth across all scenarios using
caching to avoid recomputation.

## Usage

``` r
medsim_compute_all_truth(
  scenarios,
  truth_function,
  config,
  parallel = TRUE,
  verbose = TRUE,
  truth_on_failure = c("stop", "warn")
)
```

## Arguments

- scenarios:

  List of scenario objects

- truth_function:

  Function to compute ground truth

- config:

  Configuration object

- parallel:

  Use parallel processing

- verbose:

  Print progress messages

- truth_on_failure:

  `"warn"` or `"stop"`; see
  [`medsim_run()`](https://data-wise.github.io/medsim/reference/medsim_run.md).

## Value

data.frame with ground truth for each scenario whose truth succeeded
(failed scenarios are absent), or `NULL` if none did.
