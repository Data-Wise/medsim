# Summarize the MBCO majority-branch rate per scenario

Reads the `branch_switch` column emitted by
[`medsim_method_mbco_mi()`](https://data-wise.github.io/medsim/reference/medsim_method_mbco_mi.md):
for each replicate, the union-null (`ab = 0`) branch chosen by the
majority of imputations (1 = the `a = 0` branch, 0 = the `b = 0`
branch). The returned `branch_switch_rate` is therefore the share of
replicates whose majority branch is `a = 0`, not the share in which
imputations disagree; that is the `branch_mix` column. The result is
suitable for joining as a column onto a
[`medsim_table_coverage()`](https://data-wise.github.io/medsim/reference/medsim_table_coverage.md)
table. Non-converged rows (`converged == 0`) are dropped before
summarizing.

## Usage

``` r
medsim_summarize_branch_switch(results, by = "scenario")
```

## Arguments

- results:

  A `medsim_results` object from
  [`medsim_run()`](https://data-wise.github.io/medsim/reference/medsim_run.md).

- by:

  Grouping columns (default `"scenario"`).

## Value

A data.frame: one row per group with `branch_switch_rate` and `n_valid`.

## Examples

``` r
res <- data.frame(
  scenario = rep(c("s1", "s2"), each = 4),
  branch_switch = c(1, 1, 0, 1, 0, 0, 1, 0),
  converged = 1
)
medsim_summarize_branch_switch(res)
#>   scenario branch_switch_rate n_valid
#> 1       s1               0.75       4
#> 2       s2               0.25       4
```
