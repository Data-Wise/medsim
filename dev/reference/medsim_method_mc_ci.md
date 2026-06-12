# Monte-Carlo CI estimator adapter

Multiple imputation (via `mice`) + Monte-Carlo confidence interval for
the indirect effect (via
[`RMediation::medci()`](https://data-wise.github.io/rmediation/reference/medci.html)
when available, else a base-R product-of-normals draw on the
Rubin-pooled paths).

## Usage

``` r
medsim_method_mc_ci(model, m = 20L, ...)
```

## Arguments

- model:

  Mediation model spec (accepted for API symmetry; the estimator reads
  the `X`, `M`, `Y` (+ optional `C*`) columns of `data`).

- m:

  Integer number of imputations.

- ...:

  Reserved for future MI / MBCO options.

## Value

A `function(data, params)` returning the shared method contract
(`branch_switch = NA`).
