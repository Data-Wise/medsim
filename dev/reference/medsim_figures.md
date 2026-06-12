# Generate all standard figures from simulation results

Convenience wrapper that produces the standard set of medsim plots
(coverage, error boxplot, timing) and writes them to `output_dir`.

## Usage

``` r
medsim_figures(
  results,
  output_dir = "figures",
  format = "pdf",
  width = 8,
  height = 6,
  ...
)
```

## Arguments

- results:

  A `medsim_results` object from
  [`medsim_run()`](https://data-wise.github.io/medsim/dev/reference/medsim_run.md).

- output_dir:

  Directory to write figures to. Created if it doesn't exist.

- format:

  File extension passed to
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)
  (e.g. `"pdf"`, `"png"`).

- width, height:

  Figure dimensions in inches.

- ...:

  Additional arguments passed to
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html).

## Value

Invisibly, a named character vector of file paths written.
