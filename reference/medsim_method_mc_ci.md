# Monte-Carlo CI estimator adapter

Multiple imputation (via `mice`) + Monte-Carlo confidence interval for
the indirect effect (via `RMediation::medci()` when available, else a
base-R product-of-normals draw on the Rubin-pooled paths).

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

## Examples

``` r
# \donttest{
if (requireNamespace("mice", quietly = TRUE)) {
  set.seed(1)
  d <- data.frame(X = rnorm(200))
  d$M <- 0.39 * d$X + rnorm(200)
  d$Y <- 0.39 * d$M + 0.14 * d$X + rnorm(200)
  d <- medsim_amputate(d, target = "M", mechanism = "MAR", prop = 0.25)
  out <- medsim_method_mc_ci(model = NULL, m = 10)(d, list())
  out[c("indirect", "indirect_ci_lower", "indirect_ci_upper")]
}
# }
```
