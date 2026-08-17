# Create a gauge-residual (P_med + W) simulation scenario

Thin wrapper over
[`medsim_scenario()`](https://data-wise.github.io/medsim/reference/medsim_scenario.md)
mirroring
[`medsim_scenario_sobol()`](https://data-wise.github.io/medsim/reference/medsim_scenario_sobol.md):
an `A -> M -> Y` linear-Gaussian DGP with covariate `C` and `A*M`
interaction (`kappa`), a `variance_share` estimand, and **closed-form**
ground truth for the interventional proportion mediated `P_med = IIE/OE`
and the gauge residual `W = R/OE`, where `R = OE - IDE - IIE`.

Corner means `theta_{a,a'} = tau_a*a + (tau_m + kappa*a)*beta_a*a'` (C
centered) give `OE = theta11-theta00`, `IDE = theta10-theta00`,
`IIE = theta01-theta00`. At `kappa = 0`, `R = 0` and `W = 0`.

## Usage

``` r
medsim_scenario_gauge(name, true_params = list())
```

## Arguments

- name:

  Character scenario name.

- true_params:

  Named list; defaults
  `beta_a=0.6, tau_a=0.5, tau_m=0.7, kappa=0, gamma_mc=0.5, gamma_yc=0.4, p_a=0.5`.

## Value

A `medsim_scenario` with `estimand$kind = "variance_share"` and
`params$pmed`, `params$w` closed-form truths.

## See also

[`medsim_method_gauge()`](https://data-wise.github.io/medsim/reference/medsim_method_gauge.md),
[`medsim_scenario_sobol()`](https://data-wise.github.io/medsim/reference/medsim_scenario_sobol.md)

## Examples

``` r
sc <- medsim_scenario_gauge("interaction", list(kappa = 0.4))
c(sc$params$pmed, sc$params$w)
#> [1] 0.3620690 0.2068966
```
