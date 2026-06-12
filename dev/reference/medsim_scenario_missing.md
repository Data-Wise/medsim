# Construct a missing-data mediation scenario

Wraps
[`medsim_scenario()`](https://data-wise.github.io/medsim/dev/reference/medsim_scenario.md)
with a `data_generator` that (1) draws complete `X -> M -> Y` with
optional nonnormal residuals
([`medsim_rnonnormal()`](https://data-wise.github.io/medsim/dev/reference/medsim_rnonnormal.md)),
then (2) amputes `target` under `mechanism`
([`medsim_amputate()`](https://data-wise.github.io/medsim/dev/reference/medsim_amputate.md)).

## Usage

``` r
medsim_scenario_missing(
  name,
  true_params,
  mechanism,
  prop = 0.2,
  target = "M",
  nonnormal = NULL
)
```

## Arguments

- name:

  Scenario name.

- true_params:

  List of true generating parameters: `a`, `b`, `cp`, residual SDs.

- mechanism:

  One of `"MCAR"`, `"MAR"`, `"MNAR"`.

- prop:

  Target missingness proportion.

- target:

  Column(s) to make missing (default `"M"`).

- nonnormal:

  `NULL` for normal residuals, or `list(skew=, kurtosis=)`.

## Value

A `medsim_scenario` object.
