# Declare the estimand kind for a simulation scenario

Attach a first-class estimand descriptor to a
[`medsim_scenario()`](https://data-wise.github.io/medsim/reference/medsim_scenario.md).
Every downstream command (`medsim_run`, `medsim_analyze_coverage`,
`medsim_tables`, `medsim_figures`, `medsim_workflow`) dispatches on
`estimand$kind` rather than assuming a scalar point-estimand.

Five kinds are recognised:

|  |  |  |
|----|----|----|
| kind | what `method()` returns | coverage notion |
| `"point"` | `{p}`, `{p}_ci_lower/_upper`, `{p}_p` | truth in CI |
| `"interval"` | `{p}_lower/_upper`, `{p}_im_lower/_im_upper`, `feasible`, `falsified` | truth in \[lower, upper\]; IM-CI coverage |
| `"probabilistic"` | `pmed`, `pmed_ci_lower/_upper`, `pmed_p`, `branch_switch` | truth in CI (truth from potential outcomes) |
| `"variance_share"` | `{p}`, `{p}_ci_lower/_upper` | truth in Wald CI (bounded scalar share in \[0,1\]) |
| `"numeric"` | `error`, `abs_error`, `elapsed_sec` | none |

`"variance_share"` is a bounded scalar point estimand with a standard
Wald CI — the Sobol / functional-ANOVA proportion mediated
\\P\_{med}^{\sigma^2} = V\_{med}/V_T \in \[0,1\]\\ (see
[`medsim_scenario_sobol()`](https://data-wise.github.io/medsim/reference/medsim_scenario_sobol.md),
[`medsim_method_sobol()`](https://data-wise.github.io/medsim/reference/medsim_method_sobol.md)).
Coverage uses the generic `truth in CI` path; the dedicated kind exists
for clear labeling and so
[`medsim_validate_scenario()`](https://data-wise.github.io/medsim/reference/medsim_validate_scenario.md)
checks the causal-notation `A/M/Y/C` columns the Sobol estimator
requires (rather than the legacy `X/M/Y` of `"point"`).

Scenarios with `estimand = NULL` (the default in
[`medsim_scenario()`](https://data-wise.github.io/medsim/reference/medsim_scenario.md))
are treated as `kind = "point"` throughout the package – full
back-compatibility with all existing code.

## Usage

``` r
medsim_estimand(
  kind = c("point", "interval", "probabilistic", "variance_share", "numeric"),
  params = character(),
  truth = NULL,
  ci = c("standard", "imbens_manski", "mbco", "none"),
  extra = character()
)
```

## Arguments

- kind:

  Estimand kind: `"point"` (default), `"interval"`, `"probabilistic"`,
  `"variance_share"`, or `"numeric"`.

- params:

  Character vector of estimand parameter names – e.g. `c("indirect")`
  for point, `c("NDE", "NIE")` for interval, `c("pmed")` for
  probabilistic. Defaults to
  [`character()`](https://rdrr.io/r/base/character.html) (auto-inferred
  downstream).

- truth:

  Optional truth-extractor `function(scenario)` returning a **named**
  numeric vector keyed by the names in `params`. Required for
  `kind = "interval"` (bounds truth differs from the estimand itself).

- ci:

  CI method: `"standard"` (Wald/bootstrap), `"imbens_manski"`
  (partial-ID dual CI), `"mbco"` (constrained optimisation), or
  `"none"`.

- extra:

  Character vector of additional result columns beyond `params` that
  `method()` returns – e.g. `c("feasible", "branch_switch", "timing")`.

## Value

An object of class `c("medsim_estimand", "list")`.

## See also

[`medsim_scenario()`](https://data-wise.github.io/medsim/reference/medsim_scenario.md)
for attaching an estimand to a scenario;
[`medsim_analyze_coverage()`](https://data-wise.github.io/medsim/reference/medsim_analyze_coverage.md)
for the keystone dispatch on kind.

## Examples

``` r
# Point estimand (the historic default -- back-compat)
medsim_estimand("point")
#> <medsim_estimand> kind = point  ci = standard

# Interval (partial-ID bounds)
medsim_estimand("interval",
  params = c("NDE", "NIE"),
  ci     = "imbens_manski",
  extra  = c("feasible", "falsified"))
#> <medsim_estimand> kind = interval  ci = imbens_manski
#>   params: NDE, NIE 
#>   extra:  feasible, falsified 

# Probabilistic (P_med)
medsim_estimand("probabilistic",
  params = "pmed",
  ci     = "mbco",
  extra  = "branch_switch")
#> <medsim_estimand> kind = probabilistic  ci = mbco
#>   params: pmed 
#>   extra:  branch_switch 

# Numerical accuracy (no coverage/power)
medsim_estimand("numeric", params = "error", ci = "none",
  extra = "elapsed_sec")
#> <medsim_estimand> kind = numeric  ci = none
#>   params: error 
#>   extra:  elapsed_sec 
```
