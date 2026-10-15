# ADEMP performance summary (bias, SEs, RMSE) with Monte Carlo SEs

Per scenario, computes bias, empirical SE, mean model SE, and RMSE for a
parameter, each with its Monte Carlo SE (Morris, White & Crowther 2019).

## Usage

``` r
medsim_analyze_performance(results, param = "indirect")
```

## Arguments

- results:

  A `medsim_results` object from
  [`medsim_run()`](https://data-wise.github.io/medsim/reference/medsim_run.md)
  or
  [`medsim_combine_chunks()`](https://data-wise.github.io/medsim/reference/medsim_combine_chunks.md),
  whose `$truth` table is merged in by scenario; or a results data.frame
  that already has `<param>` and `<param>_truth` columns. Either way, a
  `<param>_se` column (optional) supplies the model SE. For a
  `medsim_results` object, a scenario with no truth for `param` gets
  `NA` bias and RMSE, and a warning names it.

- param:

  Parameter name (default "indirect").

## Value

A data.frame, one row per scenario.

## Details

Truth is taken as the first converged row's `<param>_truth` value and
assumed constant within scenario. Rows with NA estimates are excluded
from computations; if all estimates are NA, numeric columns return NA.
