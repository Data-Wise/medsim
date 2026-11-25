# Plot Coverage Rates

Creates a plot showing confidence interval coverage rates across
scenarios and methods.

## Usage

``` r
medsim_plot_coverage(
  coverage,
  expected_coverage = 0.95,
  tolerance = 0.02,
  title = NULL,
  ...
)
```

## Arguments

- coverage:

  A medsim_coverage object from
  [`medsim_analyze_coverage()`](https://data-wise.github.io/medsim/reference/medsim_analyze_coverage.md),
  or a named list of coverage objects for method comparison

- expected_coverage:

  Numeric: expected coverage rate to show as reference line (default:
  0.95)

- tolerance:

  Numeric: tolerance band around expected coverage (default: 0.02)

- title:

  Character: plot title (default: auto-generated)

- ...:

  Additional arguments passed to ggplot2::theme()

## Value

A ggplot2 object

## Examples

``` r
if (FALSE) { # \dontrun{
coverage <- medsim_analyze_coverage(results)
p <- medsim_plot_coverage(coverage, expected_coverage = 0.95)
} # }
```
