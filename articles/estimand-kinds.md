# Estimand Kinds: Sobol Variance Share and Gauge Residual

## Introduction

Every `medsim` scenario carries an **estimand descriptor**
([`medsim_estimand()`](https://data-wise.github.io/medsim/reference/medsim_estimand.md))
that tells downstream analysis commands — most importantly
[`medsim_analyze_coverage()`](https://data-wise.github.io/medsim/reference/medsim_analyze_coverage.md)
— which coverage/power branch to dispatch:

``` r

medsim_estimand(
  kind   = c("point", "interval", "probabilistic", "variance_share", "numeric"),
  params = character(),
  truth  = NULL,
  ci     = c("standard", "imbens_manski", "mbco", "none"),
  extra  = character()
)
```

Most scenario factories
([`medsim_scenarios_mediation()`](https://data-wise.github.io/medsim/reference/medsim_scenarios_mediation.md),
[`medsim_scenario_missing()`](https://data-wise.github.io/medsim/reference/medsim_scenario_missing.md))
default to `kind = "interval"` — a single estimate with a standard
percentile or Wald CI. This vignette covers the two factories that use
`kind = "variance_share"`: **Sobol / functional-ANOVA variance share**
and the **gauge-residual estimand**. Both decompose total outcome
variance rather than reporting a single point estimate, so they share an
estimand kind — but they answer different scientific questions and wrap
different estimators.

## Sobol Variance-Share Proportion Mediated

[`medsim_scenario_sobol()`](https://data-wise.github.io/medsim/reference/medsim_scenario_sobol.md)
targets the Sobol/functional-ANOVA proportion mediated,
`P_med^sigma2 = V_med / V_T` — the share of total outcome variance
attributable to the mediated (indirect) path, with a closed-form ground
truth computed at scenario construction time:

``` r

library(medsim)

scn_sobol <- medsim_scenario_sobol(
  name = "sobol-baseline",
  true_params = list(beta_a = 0.6, tau_a = 0.5, tau_m = 0.7, kappa = 0),
  pd = 0.5,
  pm = 0.5
)

results_sobol <- medsim_run(
  method = medsim_method_sobol,
  scenarios = list(scn_sobol),
  config = medsim_config("test")
)

summary(results_sobol)
```

[`medsim_method_sobol()`](https://data-wise.github.io/medsim/reference/medsim_method_sobol.md)
wraps an injected estimator (`sobol_pmed` by default, if available)
rather than vendoring the implementation — the estimator-agnostic
boundary medsim maintains throughout its method adapters.

## Gauge-Residual Estimand

[`medsim_scenario_gauge()`](https://data-wise.github.io/medsim/reference/medsim_scenario_gauge.md)
targets a pair of related quantities from the gauge-residual framework:
the **interventional proportion mediated** `P_med = IIE/OE` and the
**gauge residual** `W = R/OE`, where `R = OE - IDE - IIE` quantifies the
share of the overall effect not captured by the standard
direct-plus-indirect decomposition. Ground truth is closed-form via
corner-mean algebra.

``` r

library(probmed)  # provides ward_residual(), the default estimator

scn_gauge <- medsim_scenario_gauge("interaction", true_params = list(kappa = 0.4))

results_gauge <- medsim_run(
  method = medsim_method_gauge,
  scenarios = list(scn_gauge),
  config = medsim_config("test")
)

summary(results_gauge)
```

[`medsim_method_gauge()`](https://data-wise.github.io/medsim/reference/medsim_method_gauge.md)
wraps `probmed::ward_residual()` by default and supports both analytic
(influence-function) and percentile-bootstrap CIs via `se_method`:

``` r

results_gauge_boot <- medsim_run(
  method = function(data, params) {
    medsim_method_gauge(data, params, se_method = "bootstrap", B = 200)
  },
  scenarios = list(scn_gauge),
  config = medsim_config("test")
)
```

## Comparing the Two

|  | Sobol variance share | Gauge residual |
|----|----|----|
| Quantity | `P_med^sigma2 = V_med/V_T` | `P_med = IIE/OE`, `W = R/OE` |
| Estimand kind | `variance_share` | `variance_share` |
| Default estimator | `sobol_pmed` (injected) | `probmed::ward_residual()` (injected) |
| CI method | Estimator-supplied | Analytic (default) or percentile bootstrap |
| Ground truth | Closed-form at construction | Closed-form corner-mean at construction |

Both are functional-ANOVA-style variance decompositions, which is why
they share `kind = "variance_share"` rather than the simpler `interval`
kind used by point-estimate scenarios like
[`medsim_scenarios_mediation()`](https://data-wise.github.io/medsim/reference/medsim_scenarios_mediation.md).

## Next Steps

- See
  [`vignette("getting-started")`](https://data-wise.github.io/medsim/articles/getting-started.md)
  for the general
  [`medsim_run()`](https://data-wise.github.io/medsim/reference/medsim_run.md) +
  [`medsim_analyze()`](https://data-wise.github.io/medsim/reference/medsim_analyze.md)
  workflow this builds on.
- See
  [`vignette("missing-data-mediation")`](https://data-wise.github.io/medsim/articles/missing-data-mediation.md)
  for the `interval`-kind missing-data scenarios and their MBCO-MI /
  MC-CI comparison.
- [`medsim_analyze_coverage()`](https://data-wise.github.io/medsim/reference/medsim_analyze_coverage.md)
  dispatches its coverage branch from `estimand$kind` automatically — no
  extra configuration needed once a scenario is built via
  [`medsim_scenario_sobol()`](https://data-wise.github.io/medsim/reference/medsim_scenario_sobol.md)
  or
  [`medsim_scenario_gauge()`](https://data-wise.github.io/medsim/reference/medsim_scenario_gauge.md).
