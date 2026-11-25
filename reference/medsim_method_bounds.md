# Bounds estimator adapter for differential-misclassification (DM) studies

Returns partial-identification bounds (NDE/NIE) following the
`interval`-kind result contract expected by
[`medsim_run()`](https://data-wise.github.io/medsim/reference/medsim_run.md),
so that
[`medsim_analyze_coverage()`](https://data-wise.github.io/medsim/reference/medsim_analyze_coverage.md)
dispatches the Imbens-Manski / partial-ID interval coverage branch.

**Current implementation:** OLS decomposition + inflated interval
(synthetic bounds for unit-testing the harness). A future version will
call `medrobust::bound_ne()` once its accessor API stabilises on CRAN.

Result columns returned (flat named list):

- `NDE_lower`, `NDE_upper` – bounds on natural direct effect

- `NDE_im_lower`, `NDE_im_upper` – Imbens-Manski CI for NDE

- `NIE_lower`, `NIE_upper` – bounds on natural indirect effect

- `NIE_im_lower`, `NIE_im_upper` – Imbens-Manski CI for NIE

- `feasible` – logical: bounds are non-empty / consistent

- `falsified` – logical: data rejects identifying assumptions

## Usage

``` r
medsim_method_bounds(
  data,
  params,
  misclass_type = c("mediator", "exposure"),
  alpha = 0.05
)
```

## Arguments

- data:

  A data.frame produced by the scenario's `data_generator`. Expected
  columns: `A` (binary treatment), `M` or `M_star` (mediator, possibly
  misclassified), `Y` (outcome).

- params:

  Named list of scenario parameters. Must include `NDE` and `NIE`
  (ground-truth natural effects) so truth can be tracked.

- misclass_type:

  `"mediator"` (default) or `"exposure"`.

- alpha:

  Nominal level; `1 - alpha` CI (default `0.05`).

## Value

Named list with elements `NDE_lower`, `NDE_upper`, `NDE_im_lower`,
`NDE_im_upper`, `NIE_lower`, `NIE_upper`, `NIE_im_lower`,
`NIE_im_upper`, `feasible`, `falsified`.

## See also

[`medsim_scenario_dm()`](https://data-wise.github.io/medsim/reference/medsim_scenario_dm.md)
for the companion scenario factory;
[`medsim_analyze_coverage()`](https://data-wise.github.io/medsim/reference/medsim_analyze_coverage.md)
for interval-kind coverage analysis.

## Examples

``` r
set.seed(42)
d <- data.frame(A = rbinom(200, 1, 0.5), M = rnorm(200), Y = rnorm(200))
result <- medsim_method_bounds(d, list(NDE = 0.2, NIE = 0.3))
str(result)
#> List of 10
#>  $ NDE_lower   : num -0.163
#>  $ NDE_upper   : num -0.0944
#>  $ NDE_im_lower: num -0.429
#>  $ NDE_im_upper: num 0.171
#>  $ NIE_lower   : num -0.0351
#>  $ NIE_upper   : num 0.0338
#>  $ NIE_im_lower: num -0.0439
#>  $ NIE_im_upper: num 0.0427
#>  $ feasible    : logi TRUE
#>  $ falsified   : logi FALSE
```
