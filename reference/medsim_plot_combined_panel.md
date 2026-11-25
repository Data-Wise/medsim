# Create Combined Multi-Panel Figure

Creates a combined figure with multiple panels for manuscript
submission. Useful for showing multiple aspects of simulation results in
one figure.

## Usage

``` r
medsim_plot_combined_panel(
  results,
  panels = c("error", "timing"),
  layout = NULL,
  ...
)
```

## Arguments

- results:

  A medsim_results object or named list of results objects

- panels:

  Character vector: which panels to include. Options: "error", "timing",
  "coverage" (default: c("error", "timing"))

- layout:

  Character: layout specification like "2x1", "1x2", "2x2" (default:
  auto-determined from number of panels)

- ...:

  Additional arguments passed to individual plotting functions

## Value

A combined plot object (patchwork or gridExtra)

## Details

Requires either `patchwork` or `gridExtra` package for combining plots.

## Examples

``` r
if (FALSE) { # \dontrun{
# Two-panel figure
p <- medsim_plot_combined_panel(
  results,
  panels = c("error", "timing"),
  layout = "1x2"
)

# Four-panel figure for manuscript
p <- medsim_plot_combined_panel(
  list(Proposed = res1, Delta = res2),
  panels = c("error", "timing", "coverage"),
  layout = "2x2"
)
} # }
```
