# Diagnostic scatter of model SE against the point estimate

Plots each replication's model standard error against its point
estimate, faceted by scenario, to expose numerator-denominator coupling
and outliers (White et al. 2023, *How to check a simulation study*,
IJE).

## Usage

``` r
medsim_plot_se_vs_estimate(results, param = "indirect", by = "scenario")
```

## Arguments

- results:

  A results data.frame from
  [`medsim_run()`](https://data-wise.github.io/medsim/reference/medsim_run.md).

- param:

  Parameter name; expects `<param>` and `<param>_se` columns. If
  `<param>_se` is absent, derived from `<param>_ci_lower/_upper`.

- by:

  Grouping/faceting column (default "scenario").

## Value

A ggplot object.
