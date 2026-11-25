# Plot Timing Comparison

Creates bar plots comparing computational time across methods and
scenarios.

## Usage

``` r
medsim_plot_timing(
  results,
  metric = "mean",
  log_scale = FALSE,
  title = NULL,
  ...
)
```

## Arguments

- results:

  A named list of medsim_results objects (required for comparison)

- metric:

  Character: which timing metric to plot. Options: "mean", "median",
  "total" (default: "mean")

- log_scale:

  Logical: use log10 scale for y-axis (default: FALSE)

- title:

  Character: plot title (default: auto-generated)

- ...:

  Additional arguments passed to ggplot2::theme()

## Value

A ggplot2 object

## Examples

``` r
if (FALSE) { # \dontrun{
p <- medsim_plot_timing(list(
  Proposed = results1,
  Delta = results2,
  Bootstrap = results3
))
} # }
```
