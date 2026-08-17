# Create a P_med simulation scenario

Thin wrapper over
[`medsim_scenario()`](https://data-wise.github.io/medsim/reference/medsim_scenario.md)
that bakes in:

- A linear structural equation model (SEM) data generator for the
  `A -> M -> Y` causal chain.

- An
  `estimand = medsim_estimand("probabilistic", params = "pmed", ci = "mbco", extra = "branch_switch")`
  descriptor so that
  [`medsim_analyze_coverage()`](https://data-wise.github.io/medsim/reference/medsim_analyze_coverage.md)
  dispatches the MBCO-CI coverage branch.

- An exact closed-form ground-truth P_med: under the all-Gaussian linear
  SEM the cross-world difference `Y1 - Y0` is Normal, so
  `P_med = Phi((beta_ay + alpha_ax * beta_my) / sqrt(2 * (beta_my^2 * sigma_m^2 + sigma_y^2)))`.

The estimand
`P_med = P(Y_a=1(M_a=1) > Y_a=0(M_a=1)) + 0.5 * P(Y_a=1(M_a=1) == Y_a=0(M_a=1))`
uses the cross-world assumption – it is a *probabilistic* mediation
effect, not a difference in expectations.

## Usage

``` r
medsim_scenario_pmed(name, true_params = list())
```

## Arguments

- name:

  Character: scenario name passed to
  [`medsim_scenario()`](https://data-wise.github.io/medsim/reference/medsim_scenario.md).

- true_params:

  Named list with entries:

  - `alpha_ax`: path coefficient A -\> M (default 0.5)

  - `beta_my`: path coefficient M -\> Y (default 0.5)

  - `beta_ay`: direct path A -\> Y (default 0.0; set 0 for perfect
    mediation)

  - `sigma_m`: residual SD for M (default 1.0)

  - `sigma_y`: residual SD for Y (default 1.0)

## Value

A `medsim_scenario` object with `estimand$kind = "probabilistic"`.

## See also

[`medsim_method_pmed_mbco()`](https://data-wise.github.io/medsim/reference/medsim_method_pmed_mbco.md),
[`medsim_analyze_coverage()`](https://data-wise.github.io/medsim/reference/medsim_analyze_coverage.md)

## Examples

``` r
sc <- medsim_scenario_pmed(
  name        = "full_mediation",
  true_params = list(alpha_ax = 0.6, beta_my = 0.5, beta_ay = 0.0)
)
str(sc$estimand)
#> List of 5
#>  $ kind  : chr "probabilistic"
#>  $ params: chr "pmed"
#>  $ truth : NULL
#>  $ ci    : chr "mbco"
#>  $ extra : chr "branch_switch"
#>  - attr(*, "class")= chr [1:2] "medsim_estimand" "list"
```
