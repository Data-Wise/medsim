# Plot Error Distribution Boxplots

Creates boxplots showing the distribution of estimation errors across
scenarios. Useful for comparing accuracy of different methods.

## Usage

``` r
medsim_plot_error_boxplot(
  results,
  parameter = "indirect",
  log_scale = FALSE,
  color_palette = NULL,
  title = NULL,
  ...
)
```

## Arguments

- results:

  A medsim_results object from
  [`medsim_run()`](https://data-wise.github.io/medsim/reference/medsim_run.md),
  or a list of named results objects for method comparison

- parameter:

  Character: which parameter to plot (default: "indirect"). If NULL,
  plots all parameters with available ground truth.

- log_scale:

  Logical: use log10 scale for y-axis (default: FALSE)

- color_palette:

  Character: color palette name from RColorBrewer or "viridis" (default:
  NULL uses ggplot2 defaults)

- title:

  Character: plot title (default: auto-generated)

- ...:

  Additional arguments passed to ggplot2::theme()

## Value

A ggplot2 object

## Details

### Interpreting Boxplots

- Box: IQR (25th to 75th percentile)

- Line in box: Median error

- Whiskers: 1.5 × IQR

- Points: Outliers beyond whiskers

Tighter boxes = more consistent estimates

### Usage Patterns

    # Single method
    p <- medsim_plot_error_boxplot(results)

    # Compare methods
    p <- medsim_plot_error_boxplot(list(
      Proposed = results1,
      Delta = results2,
      Bootstrap = results3
    ))

    # Customize
    p <- medsim_plot_error_boxplot(
      results,
      log_scale = TRUE,
      color_palette = "Set2"
    )
    p + ggplot2::theme_minimal()

## Examples

``` r
if (FALSE) { # \dontrun{
results <- medsim_run(method, scenarios, config)
p <- medsim_plot_error_boxplot(results)
print(p)

# Save to file
ggplot2::ggsave("error_boxplot.pdf", p, width = 10, height = 6)
} # }
```
