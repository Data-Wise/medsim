# Gauge-residual (P_med + W) estimator wrapper

Adapter that calls an external `ward_residual()` estimator and returns
the flat field contract for two parameters:

|  |  |
|----|----|
| Field | Description |
| `pmed`, `pmed_ci_lower/_upper` | interventional proportion mediated `IIE/OE` |
| `w`, `w_ci_lower/_upper` | gauge residual `R/OE` |

The estimator is **not vendored** (medsim keeps estimators external).
Pass it via `estimator`, or default to an object named `ward_residual`
on the search path / `probmed::ward_residual`. The estimator must return
an S7 `GaugePmedResult` (read via `@p_med/@p_med_ci/@W/@W_ci`) or a
plain list with fields `p_med`, `p_med_ci` (length 2), `W`, `W_ci`
(length 2).

## Usage

``` r
medsim_method_gauge(
  data,
  params = list(),
  estimator = get0("ward_residual"),
  covars = "C",
  K = 5L,
  ci_level = 0.95,
  seed = 1L,
  reps = 1L,
  se_method = "analytic",
  B = 200L,
  fieller = FALSE
)
```

## Arguments

- data:

  data.frame with columns `A`, `M`, `Y`, `C`.

- params:

  Named list from
  [`medsim_scenario_gauge()`](https://data-wise.github.io/medsim/reference/medsim_scenario_gauge.md)
  (accepted for the `method(data, params)` signature; unused by the
  estimator).

- estimator:

  Function; default `get0("ward_residual")`.

- covars:

  Character covariate names (default `"C"`).

- K:

  Integer cross-fitting folds (default 5).

- ci_level:

  Numeric CI level (default 0.95).

- seed:

  Integer seed (default 1).

- reps:

  Integer repeated cross-fitting fold draws (default 1).

- se_method:

  `"analytic"` (default) or `"bootstrap"` (percentile).

- B:

  Integer bootstrap resamples when `se_method = "bootstrap"` (default
  200).

- fieller:

  Logical: compute the estimator's Fieller bounds (default `FALSE`).
  medsim reports only the Wald/percentile `pmed`/`w` CIs, so the Fieller
  set is discarded — disabling it avoids wasted compute per replication
  across a large grid.

## Value

Named list
`pmed, pmed_ci_lower, pmed_ci_upper, w, w_ci_lower, w_ci_upper`.

## See also

[`medsim_scenario_gauge()`](https://data-wise.github.io/medsim/reference/medsim_scenario_gauge.md),
[`medsim_analyze_coverage()`](https://data-wise.github.io/medsim/reference/medsim_analyze_coverage.md)
