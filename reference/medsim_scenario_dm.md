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
#>   A          M          Y A_star
#> 1 1  0.9423952 -0.6906756      1
#> 2 1 -0.6742927 -1.0221131      1
#> 3 1 -1.8828138  0.7980748      1
#> 4 0  0.0937679  0.9895013      1
#> 5 1  0.1981802 -0.8242704      1
#> 6 1  0.1868993 -0.8440223      1
```
