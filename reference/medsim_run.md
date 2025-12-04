# Run Simulation Study

Executes a complete simulation study by running a user-defined method
across multiple scenarios and replications. Supports parallel processing
and automatic result aggregation.

## Usage

``` r
medsim_run(
  method,
  scenarios,
  config,
  compute_truth = NULL,
  parallel = TRUE,
  verbose = TRUE
)
```

## Arguments

- method:

  Function: User-defined simulation method. Must accept two arguments:
  `data` (data.frame) and `params` (list). Should return a named list of
  results.

- scenarios:

  List of scenario objects from
  [`medsim_scenario()`](https://data-wise.github.io/medsim/reference/medsim_scenario.md)
  or
  [`medsim_scenarios_mediation()`](https://data-wise.github.io/medsim/reference/medsim_scenarios_mediation.md)

- config:

  Configuration object from
  [`medsim_config()`](https://data-wise.github.io/medsim/reference/medsim_config.md)

- compute_truth:

  Function: Optional function to compute ground truth. Takes same
  arguments as `method`. If NULL, no ground truth computed.

- parallel:

  Logical: Use parallel processing (default TRUE)

- verbose:

  Logical: Print progress messages (default TRUE)

## Value

A medsim_results object (list) containing:

- `results`: data.frame with all simulation results

- `summary`: data.frame with summary statistics

- `truth`: data.frame with ground truth values (if compute_truth
  provided)

- `config`: configuration used

- `scenarios`: scenarios used

- `method_name`: name of method function

- `timestamp`: when simulation was run

## Details

### Method Function Requirements

The `method` function must:

- Accept `data` (data.frame) as first argument

- Accept `params` (list) as second argument

- Return a named list with at least one numeric element

Example:

    my_method <- function(data, params) {
      fit_m <- lm(M ~ X, data = data)
      fit_y <- lm(Y ~ X + M, data = data)

      a <- coef(fit_m)["X"]
      b <- coef(fit_y)["M"]

      list(
        indirect = a * b,
        a_path = a,
        b_path = b,
        se_indirect = ...,  # Optional
        ci_lower = ...,     # Optional
        ci_upper = ...      # Optional
      )
    }

### Parallel Processing

When `parallel = TRUE`:

- Uses
  [`medsim_run_parallel()`](https://data-wise.github.io/medsim/reference/medsim_run_parallel.md)
  internally

- Number of cores from `config$n_cores`

- Progress bars shown (local) or suppressed (cluster)

- All necessary objects exported to workers

### Ground Truth

If `compute_truth` is provided:

- Computed once per scenario (cached automatically)

- Used to calculate errors/bias in results

- Can use Monte Carlo or analytical methods

## See also

[`medsim_config()`](https://data-wise.github.io/medsim/reference/medsim_config.md),
[`medsim_scenario()`](https://data-wise.github.io/medsim/reference/medsim_scenario.md),
[`medsim_run_parallel()`](https://data-wise.github.io/medsim/reference/medsim_run_parallel.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Define method
my_method <- function(data, params) {
  fit_m <- lm(M ~ X, data = data)
  fit_y <- lm(Y ~ X + M, data = data)
  list(indirect = coef(fit_m)["X"] * coef(fit_y)["M"])
}

# Configure and run
config <- medsim_config("local")
scenarios <- medsim_scenarios_mediation()

results <- medsim_run(
  method = my_method,
  scenarios = scenarios,
  config = config
)

# View results
print(results)
summary(results)
} # }
```
