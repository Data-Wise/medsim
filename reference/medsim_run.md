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
  verbose = TRUE,
  truth_on_failure = c("stop", "warn")
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

- truth_on_failure:

  `"stop"` (default) or `"warn"`: what to do when `compute_truth`
  errors, or succeeds but returns no usable parameter, for a scenario.
  `"stop"` aborts before any replication runs; `"warn"` continues with
  the scenario omitted from `$truth`. Nothing is cached either way. See
  *Ground Truth*.

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

- NOT use the reserved field name `error` – it marks failed replications
  in the results schema (a non-NA `error` forces `converged = 0`); a
  method returning it stops with an informative error

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

`compute_truth` must return a **named list of length-1 numerics**, e.g.
`list(indirect = 0.09)`. A bare named vector (`c(indirect = 0.09)`) or a
list element of length \> 1 contributes no parameter and is treated as a
failure.

A failed or unusable truth is never cached, so the next run retries it,
and the scenario is left out of `$truth`. What happens to the run is set
by `truth_on_failure`: `"stop"` (the default) raises a
`medsim_truth_failure_error` (subclass of `medsim_error`) carrying
`$scenario` and `$parent`; `"warn"` warns and continues. Truth is
computed in Step 1, before any replication runs, so stopping wastes no
replication compute.

### Task-level failures

An error that escapes a replication task – a contract violation (e.g.
the reserved-`error`-field stop) or an infrastructure error, as opposed
to a `method()` or `data_generator()` failure (both of which become
failure rows and let the run continue) – stops `medsim_run()` with a
`medsim_run_failure` condition (subclass of `medsim_error`) rather than
silently dropping the failed replications. The condition carries
whatever completed before the failure: `$results` is a partial
`medsim_results` object (prior scenarios complete, the failing
scenario's successful reps included, later scenarios never started), and
`$errors` is a list of `list(scenario, global_rep_id, message)`. Recover
the partial run instead of losing it:

    partial <- tryCatch(
      medsim_run(method, scenarios, config),
      medsim_run_failure = function(e) e$results
    )

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
