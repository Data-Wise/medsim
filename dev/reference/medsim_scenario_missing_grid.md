# Build the full factorial of missing-data scenarios

Convenience: expand `SPEC-simulation-design` cells (mechanism × prop × n
× effect × nonnormality) into a list of
[`medsim_scenario_missing()`](https://data-wise.github.io/medsim/dev/reference/medsim_scenario_missing.md)
objects in one call.

## Usage

``` r
medsim_scenario_missing_grid(
  true_params_list,
  mechanisms,
  props,
  nonnormal_list = list(NULL)
)
```

## Arguments

- true_params_list:

  List of `true_params` lists (one per effect-size cell).

- mechanisms, props:

  Character/numeric vectors crossed factorially.

- nonnormal_list:

  List of `nonnormal` specs (incl. `NULL` for normal).

## Value

A list of `medsim_scenario` objects.
