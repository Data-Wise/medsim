# ADEMP performance summary (bias, SEs, RMSE) with Monte Carlo SEs

Per scenario, computes bias, empirical SE, mean model SE, and RMSE for a
parameter, each with its Monte Carlo SE (Morris, White & Crowther 2019).

## Usage

``` r
medsim_analyze_performance(results, param = "indirect")
```

## Arguments

- results:

  Results data.frame from
  [`medsim_run()`](https://data-wise.github.io/medsim/reference/medsim_run.md);
  expects `<param>`, `<param>_truth`, and (optionally) `<param>_se`
  columns.

- param:

  Parameter name (default "indirect").

## Value

A data.frame, one row per scenario.

## Details

Truth is taken as the first converged row's `<param>_truth` value and
assumed constant within scenario. Rows with NA estimates are excluded
from computations; if all estimates are NA, numeric columns return NA.
