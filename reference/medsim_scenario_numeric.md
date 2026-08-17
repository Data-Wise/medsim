# Create a numeric accuracy simulation scenario

Thin wrapper over
[`medsim_scenario()`](https://data-wise.github.io/medsim/reference/medsim_scenario.md)
for studies that evaluate numerical **accuracy** or **timing** of a
computation – not coverage or power. The estimand kind is `"numeric"`,
which signals downstream commands to skip coverage/power analysis and
produce accuracy/timing tables instead.

Typical use-cases:

- Product-of-three CI: compare DOP, MBCO, Monte-Carlo CI point accuracy
  and computation time across parameter cells.

- Approximation quality: absolute error and relative error vs. analytic
  truth.

The `data_generator` for numeric scenarios is optional (default: returns
[`data.frame()`](https://rdrr.io/r/base/data.frame.html) with `n` rows
of `NA`s as a placeholder). Methods that only need `params` can ignore
the `data` argument.

## Usage

``` r
medsim_scenario_numeric(
  name,
  true_params = list(),
  data_generator = NULL,
  extra = character()
)
```

## Arguments

- name:

  Character: scenario name.

- true_params:

  Named list of ground-truth parameter values. These are passed to
  `method(data, params)` as `params`; what counts as "truth" is
  method-defined. For accuracy studies this typically includes the
  analytic reference value (e.g. `list(ci_true = 0.95)`).

- data_generator:

  Function `function(n)` returning a data frame. Defaults to a no-op
  that returns an empty data frame (suitable for methods that derive
  everything from `params` rather than a random sample).

- extra:

  Character vector of additional result columns beyond the mandatory
  `error`/`abs_error`/`elapsed_sec` columns. These are passed to
  [`medsim_estimand()`](https://data-wise.github.io/medsim/reference/medsim_estimand.md)
  and stored on the scenario.

## Value

A `medsim_scenario` object with `estimand$kind = "numeric"`.

## See also

[`medsim_estimand()`](https://data-wise.github.io/medsim/reference/medsim_estimand.md),
[`medsim_scenario()`](https://data-wise.github.io/medsim/reference/medsim_scenario.md),
[`medsim_analyze()`](https://data-wise.github.io/medsim/reference/medsim_analyze.md)

## Examples

``` r
# Product-of-three scenario cell
sc <- medsim_scenario_numeric(
  name        = "dop_small_n",
  true_params = list(a  = 0.5, b = 0.3, se_a = 0.1, se_b = 0.1,
                     ci_true = 0.01),
  extra       = c("elapsed_sec")
)
str(sc$estimand)
#> List of 5
#>  $ kind  : chr "numeric"
#>  $ params: chr(0) 
#>  $ truth : NULL
#>  $ ci    : chr "none"
#>  $ extra : chr [1:3] "error" "abs_error" "elapsed_sec"
#>  - attr(*, "class")= chr [1:2] "medsim_estimand" "list"
```
