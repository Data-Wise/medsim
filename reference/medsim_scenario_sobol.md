# Create a Sobol / functional-ANOVA variance-share simulation scenario

Thin wrapper over
[`medsim_scenario()`](https://data-wise.github.io/medsim/reference/medsim_scenario.md)
that bakes in:

- A linear-Gaussian-with-interaction data generator for the
  `A -> M -> Y` causal chain with a single covariate `C` and an `A*M`
  interaction term (`kappa`).

- An
  `estimand = medsim_estimand("variance_share", params = "pmed_sobol", ci = "standard")`
  descriptor, so
  [`medsim_analyze_coverage()`](https://data-wise.github.io/medsim/reference/medsim_analyze_coverage.md)
  uses the generic Wald `truth %in% CI` path and
  [`medsim_validate_scenario()`](https://data-wise.github.io/medsim/reference/medsim_validate_scenario.md)
  checks for the `A/M/Y/C` columns the Sobol estimator requires.

- A **closed-form** `truth` for the Sobol share, exact for the
  linear-Gaussian-with-interaction DGP (no Monte-Carlo PO draw needed).

The estimand is the variance-scale proportion mediated
\\P\_{med}^{\sigma^2} = V\_{med}/V_T\\, where the functional-ANOVA
variance components are built from the four corner means
\\\theta\_{a,a'} = E\[Y(a, M(a'))\]\\. For the linear-Gaussian DGP with
centered `C`, \$\$\theta\_{a,a'} = \tau_a a + (\tau_m + \kappa
a)\\\beta_a a'.\$\$ At `kappa = 0` the share reduces to \\NIE^2/(NIE^2 +
NDE^2)\\ with \\NIE = \tau_m \beta_a\\ and \\NDE = \tau_a\\.

## Usage

``` r
medsim_scenario_sobol(name, true_params = list(), pd = 0.5, pm = 0.5)
```

## Arguments

- name:

  Character: scenario name passed to
  [`medsim_scenario()`](https://data-wise.github.io/medsim/reference/medsim_scenario.md).

- true_params:

  Named list with entries (defaults in parentheses):

  - `beta_a`: path coefficient A -\> M (0.6)

  - `tau_a`: direct path A -\> Y (0.5)

  - `tau_m`: path coefficient M -\> Y (0.7)

  - `kappa`: A\*M interaction coefficient in the Y model (0.0)

  - `gamma_mc`: covariate effect C -\> M (0.5)

  - `gamma_yc`: covariate effect C -\> Y (0.4)

  - `p_a`: treatment-assignment probability used for the variance
    weights `pd = pm = p_a` (0.5)

- pd, pm:

  Bernoulli weights for the direct / mediator design variances in the
  functional-ANOVA decomposition (default `0.5`; override only for
  non-balanced designs).

## Value

A `medsim_scenario` object with `estimand$kind = "variance_share"` and
`params$pmed_sobol` holding the closed-form ground truth.

## See also

[`medsim_method_sobol()`](https://data-wise.github.io/medsim/reference/medsim_method_sobol.md),
[`medsim_analyze_coverage()`](https://data-wise.github.io/medsim/reference/medsim_analyze_coverage.md),
[`medsim_estimand()`](https://data-wise.github.io/medsim/reference/medsim_estimand.md)

## Examples

``` r
sc <- medsim_scenario_sobol(
  name        = "interaction",
  true_params = list(beta_a = 0.6, tau_a = 0.5, tau_m = 0.7, kappa = 0.4)
)
sc$params$pmed_sobol   # closed-form Sobol share
#> [1] 0.4223638
```
