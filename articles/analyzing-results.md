# Analyzing and Reporting Simulation Results

## Overview

[`medsim_run()`](https://data-wise.github.io/medsim/reference/medsim_run.md)
returns a `medsim_results` object. This article covers what that object
holds, which analysis function answers which question, how to compare
methods fairly, and how to turn the results into tables and figures.

The examples continue the Sobel study from [Tutorial: Your First
Coverage
Study](https://data-wise.github.io/medsim/articles/tutorial-first-study.md):
two scenarios (“Medium”, $`ab = .1521`$; “Null (a = 0)”, $`ab = 0`$),
500 replications each.

| Question | Function | Needs from the method |
|----|----|----|
| Is the estimator biased or noisy? | [`medsim_analyze()`](https://data-wise.github.io/medsim/reference/medsim_analyze.md) | `<param>` + truth |
| Bias, empirical SE and model SE, with MCSEs | [`medsim_analyze_performance()`](https://data-wise.github.io/medsim/reference/medsim_analyze_performance.md) | `<param>`, `<param>_se` + truth |
| Do the intervals cover? | [`medsim_analyze_coverage()`](https://data-wise.github.io/medsim/reference/medsim_analyze_coverage.md) | `<param>_ci_lower`, `<param>_ci_upper` + truth |
| How often does the test reject? | [`medsim_analyze_power()`](https://data-wise.github.io/medsim/reference/medsim_analyze_power.md) | `<param>_p` |
| Which method is better? | [`medsim_compare_methods()`](https://data-wise.github.io/medsim/reference/medsim_compare_methods.md) | two or more runs on the same scenarios |
| How many replications? | [`medsim_nsim_for_mcse()`](https://data-wise.github.io/medsim/reference/medsim_nsim_for_mcse.md) | nothing (planning) |

## The results object

``` r

names(results)
#> [1] "results"     "summary"     "truth"       "config"      "scenarios"
#> [6] "method_name" "timestamp"
```

- `results$results` holds one row per scenario × replication: the
  method’s outputs, plus `scenario`, `replication`, `elapsed` (seconds)
  and `error` (`NA` when the replication succeeded).
- `results$truth` holds one row per scenario: the values returned by
  `compute_truth`.
- `results$config` and `results$scenarios` record how the run was made,
  so a saved results file documents itself.

Failed replications are kept, not dropped. Count them before you
analyze:

``` r

tapply(!is.na(results$results$error), results$results$scenario, sum)
```

The coverage table reports these as `n_failed` and `failure_rate`. A
method that fails selectively (for example, on hard data sets) can look
better than it is if the failures are ignored.

## Accuracy: `medsim_analyze()`

``` r

an <- medsim_analyze(results)
an$by_scenario[, c("scenario", "parameter", "bias", "rmse", "relative_bias")]
#>       scenario parameter          bias       rmse relative_bias
#> 1       Medium  indirect  0.0013953010 0.03928207     0.9173576
#> 2 Null (a = 0)  indirect -0.0008496464 0.02809685            NA
```

`relative_bias` is in percent of the true value. It is `NA` where the
truth is zero, so report absolute bias for null scenarios.

[`medsim_analyze()`](https://data-wise.github.io/medsim/reference/medsim_analyze.md)
scores every output column that has a matching truth value. It skips SE,
interval and p-value columns (`indirect_se`, `indirect_ci_lower`,
`indirect_ci_upper`, `indirect_p`) silently, because they belong to a
scored estimate. Any other output column without a truth is named in a
single warning, so a value you forgot to return from `compute_truth` is
still flagged.

## Performance with Monte Carlo SEs

Every number in a simulation table is itself an estimate. Morris, White
and Crowther (2019) recommend reporting each with its Monte Carlo SE
(MCSE).
[`medsim_analyze_performance()`](https://data-wise.github.io/medsim/reference/medsim_analyze_performance.md)
does that for bias, and compares the method’s own SE with the empirical
SD of its estimates:

``` r

perf <- medsim_analyze_performance(results)
perf
#>       scenario parameter          bias   bias_mcse empirical_se   model_se
#> 1       Medium  indirect  0.0013953010 0.001757397   0.03929660 0.03969967
#> 2 Null (a = 0)  indirect -0.0008496464 0.001257212   0.02811212 0.02846886
#>         rmse n_converged n_failed
#> 1 0.03928207         500        0
#> 2 0.02809685         500        0
```

Read bias against `bias_mcse`. Here both biases are under one MCSE, so
there is no detectable bias. A `model_se` well below `empirical_se`
means the method’s standard errors are too small, which usually shows up
as under-coverage.

## Coverage and rejection rates

``` r

cov <- medsim_analyze_coverage(results, ci_levels = 0.95)
pw  <- medsim_analyze_power(results, alpha = 0.05)
```

Coverage reports its own MCSE (`coverage_mcse`). For a rejection rate
$`p`$ estimated from $`R`$ replications, the MCSE is
$`\sqrt{p(1-p)/R}`$. At the nominal .05 and $`R = 500`$ that is about
.0097, so a type I error between roughly .03 and .07 is consistent with
a correct test.

### How many replications?

Invert the MCSE formula when you plan a study. To estimate 95% coverage
with an MCSE of .005:

``` r

medsim_nsim_for_mcse(target_mcse = 0.005, p = 0.95)
#> [1] 1900
```

Plan for the worst case you need to resolve. A rejection rate near .5
has the largest MCSE, so power studies need more replications than
coverage studies at the same precision.

## Comparing methods fairly

Run each method on the **same scenario list** and pass the results to
[`medsim_compare_methods()`](https://data-wise.github.io/medsim/reference/medsim_compare_methods.md):

``` r

mc <- function(data, params) {
  fm <- summary(lm(M ~ X, data = data))$coefficients
  fy <- summary(lm(Y ~ X + M, data = data))$coefficients
  ab <- rnorm(5000, fm["X", 1], fm["X", 2]) * rnorm(5000, fy["M", 1], fy["M", 2])
  q  <- quantile(ab, c(0.025, 0.975), names = FALSE)
  list(indirect = fm["X", 1] * fy["M", 1],
       indirect_ci_lower = q[1], indirect_ci_upper = q[2])
}
results_mc <- medsim_run(mc, scenarios, config, compute_truth = truth)

medsim_compare_methods(sobel = results, mc = results_mc)
#> Accuracy Comparison:
#>  method parameter        mae      rmse         bias
#>   sobel  indirect 0.02630142 0.0341505 0.0002728273
#>     mc  indirect 0.02630142 0.0341505 0.0002728273
#> ...
#> Coverage Comparison:
#>  method parameter coverage n_valid
#>   sobel  indirect    0.952    1000
#>     mc  indirect    0.945    1000
```

The two accuracy rows are identical, and that is by design. Both methods
report the same point estimate $`\hat a \hat b`$, and `medsim` seeds
each replication from the scenario name and replication number, so both
methods saw exactly the same 1,000 data sets. This pairing (common
random numbers) means differences between methods are not diluted by
data-set noise. The comparison above isolates the interval: the Monte
Carlo CI covers .945 against Sobel’s .952 on identical data.

> **Keep scenario names fixed**
>
> Pairing holds only when the scenario names and replication numbers
> match. Renaming a scenario between runs breaks it silently.

## Tables

Each `medsim_table_*()` function takes an analysis object and returns
LaTeX.
[`medsim_write_table()`](https://data-wise.github.io/medsim/reference/medsim_write_table.md)
writes it to disk, with the format inferred from the file extension:

``` r

tab <- medsim_table_coverage(cov, expected = 0.95,
                             caption = "Sobel CI coverage")
medsim_write_table(tab, "tables/coverage.tex")

medsim_table_power(pw)
medsim_table_accuracy(an, metrics = c("bias", "rmse"))
```

`medsim_tables(results, output_dir = "tables")` writes the standard set
(accuracy, coverage, power) in one call.

## Figures

The plotting functions return `ggplot2` objects, so you can restyle
them:

``` r

medsim_plot_coverage(cov, expected_coverage = 0.95, tolerance = 0.02)
medsim_plot_error_boxplot(results, parameter = "indirect")
```

`medsim_figures(results, output_dir = "figures")` writes the standard
set as PDFs.

## Everything at once

[`medsim_workflow()`](https://data-wise.github.io/medsim/reference/medsim_workflow.md)
runs the analysis and writes all standard tables and figures:

``` r

medsim_workflow(results, output_dir = "results")
list.files("results", recursive = TRUE)
#> [1] "figures/coverage.pdf"      "figures/error_boxplot.pdf"
#> [3] "figures/timing.pdf"        "tables/table_accuracy.tex"
#> [5] "tables/table_coverage.tex" "tables/table_power.tex"
```

Use it for a first look. For a manuscript, call the individual functions
so you control captions, labels and which parameters appear.

## Reference

Morris, T. P., White, I. R., & Crowther, M. J. (2019). Using simulation
studies to evaluate statistical methods. *Statistics in Medicine,
38*(11), 2074–2102. https://doi.org/10.1002/sim.8086
