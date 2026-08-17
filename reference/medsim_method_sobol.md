# Sobol / functional-ANOVA variance-share estimator wrapper

Adapter that calls an external `sobol_pmed()` estimator and returns the
flat field contract keyed by the `pmed_sobol` token:

|                       |                                                    |
|-----------------------|----------------------------------------------------|
| Field                 | Description                                        |
| `pmed_sobol`          | Point estimate of the Sobol share \\V\_{med}/V_T\\ |
| `pmed_sobol_ci_lower` | Lower Wald CI bound                                |
| `pmed_sobol_ci_upper` | Upper Wald CI bound                                |

Following medsim convention, the actual estimator is **not vendored**
into the package (medsim keeps estimators external — it `Suggests`
companion packages, never hard-depends on them). The prototype lives at
`pmed-modern-sobol/03-sobol-pmed/sims/sobol_pmed.R`;
[`source()`](https://rdrr.io/r/base/source.html) it (or load the future
`probmed` export) before running, or pass it via `estimator`.

The external `sobol_pmed(d, ...)` is expected to return a list with
`P_med_sobol` (scalar) and `ci` (length-2 numeric `c(lower, upper)`).

## Usage

``` r
medsim_method_sobol(
  data,
  params = list(),
  estimator = get0("sobol_pmed"),
  covars = "C",
  K = 5L,
  level = 0.95,
  seed = 1L
)
```

## Arguments

- data:

  A `data.frame` with columns `A`, `M`, `Y`, `C`.

- params:

  Named list from
  [`medsim_scenario_sobol()`](https://data-wise.github.io/medsim/reference/medsim_scenario_sobol.md)
  (unused by the estimator itself, but accepted for the standard
  `method(data, params)` signature; may carry `covars`, `K`, `level`).

- estimator:

  Function implementing the Sobol estimator. Defaults to
  `get0("sobol_pmed")`, i.e. an object named `sobol_pmed` visible on the
  search path (e.g. after
  [`source()`](https://rdrr.io/r/base/source.html)-ing the prototype).

- covars:

  Character vector of covariate column names passed to the estimator
  (default `"C"`).

- K:

  Integer cross-fitting folds passed to the estimator (default 5).

- level:

  Numeric CI level passed to the estimator (default 0.95).

- seed:

  Integer seed passed to the estimator (default 1).

## Value

A named list with fields `pmed_sobol`, `pmed_sobol_ci_lower`,
`pmed_sobol_ci_upper`.

## See also

[`medsim_scenario_sobol()`](https://data-wise.github.io/medsim/reference/medsim_scenario_sobol.md),
[`medsim_analyze_coverage()`](https://data-wise.github.io/medsim/reference/medsim_analyze_coverage.md)
