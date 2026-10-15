# Thin IPW estimator adapter (robustness appendix)

Inverse-probability-weighting comparator. Positioned as a robustness
appendix, not a co-equal main-results arm (decided 2026-06-11).

## Usage

``` r
medsim_method_ipw(model, ...)
```

## Arguments

- model:

  Mediation model spec.

- ...:

  Passed to the underlying weighting / fitting routines.

## Value

A `function(data, params)` returning the shared method contract
(`branch_switch = NA`).

## Examples

``` r
set.seed(1)
d <- data.frame(X = rnorm(200))
d$M <- 0.39 * d$X + rnorm(200)
d$Y <- 0.39 * d$M + 0.14 * d$X + rnorm(200)
d <- medsim_amputate(d, target = "M", mechanism = "MAR", prop = 0.25)
out <- medsim_method_ipw(model = NULL)(d, list())
out[c("indirect", "indirect_ci_lower", "indirect_ci_upper")]
#> $indirect
#> [1] 0.1067168
#> 
#> $indirect_ci_lower
#> [1] 0.02387662
#> 
#> $indirect_ci_upper
#> [1] 0.1895569
#> 
```
