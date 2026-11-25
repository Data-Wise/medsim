# Construct a differential-misclassification (DM) mediation scenario

Builds a
[`medsim_scenario()`](https://data-wise.github.io/medsim/reference/medsim_scenario.md)
for the bounds-pair simulation studies (me-mediator-bounds /
me-exposure-recall). The `data_generator` wraps
`medrobust::simulate_dm_data(...)@observed`; if medrobust is not
installed a lightweight synthetic fallback is used so that the harness
and `interval`-kind analysis machinery can be unit-tested without the
package.

The scenario is tagged with
`estimand = medsim_estimand("interval", ...)` so that
[`medsim_analyze_coverage()`](https://data-wise.github.io/medsim/reference/medsim_analyze_coverage.md)
automatically dispatches the Imbens-Manski / partial-ID interval branch.

## Usage

``` r
medsim_scenario_dm(
  name,
  true_params,
  dm_params = list(),
  misclass_type = c("mediator", "exposure")
)
```

## Arguments

- name:

  Scenario name (character).

- true_params:

  List of true generating parameters passed through to
  `medrobust::simulate_dm_data()` and stored in `scenario$params`. Must
  include at minimum `NDE` and `NIE` (the true natural direct/indirect
  effects) so that coverage against ground truth can be computed.

- dm_params:

  List of differential-misclassification parameters passed through to
  `medrobust::simulate_dm_data()`.

- misclass_type:

  `"mediator"` (default, me-mediator-bounds study) or `"exposure"`
  (me-exposure-recall study).

## Value

A
[`medsim_scenario()`](https://data-wise.github.io/medsim/reference/medsim_scenario.md)
object with
`estimand = medsim_estimand("interval", params = c("NDE", "NIE"), ...)`.

## See also

[`medsim_method_bounds()`](https://data-wise.github.io/medsim/reference/medsim_method_bounds.md)
for the corresponding estimator adapter;
[`medsim_analyze_coverage()`](https://data-wise.github.io/medsim/reference/medsim_analyze_coverage.md)
for interval-kind coverage analysis.

## Examples

``` r
# Synthetic scenario (no medrobust required):
sc <- medsim_scenario_dm(
  name        = "small_misclass",
  true_params = list(NDE = 0.2, NIE = 0.3, n = 200),
  dm_params   = list(delta = 0.1),
  misclass_type = "mediator"
)
# Data generator uses synthetic fallback when medrobust is absent:
d <- sc$data_generator(n = 50)
head(d)
#>   A           M          Y A_star
#> 1 1 -0.19756945 -0.5886208      1
#> 2 0  0.09172404  1.3710939      0
#> 3 0 -0.23869407  0.3978258      0
#> 4 0 -0.46196236 -0.3916938      0
#> 5 1  2.12634061  1.0070059      1
#> 6 1  0.32205481 -1.2500907      1
```
