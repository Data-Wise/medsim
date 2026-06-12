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
