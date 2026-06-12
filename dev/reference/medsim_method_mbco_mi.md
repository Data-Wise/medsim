# MBCO-MI estimator adapter

Multiple imputation (via `mice`) + **D4-stacked MBCO** likelihood-ratio
inference for the union null `H0: ab = 0`. Ports the validated
`Missing Effect/code/prototype-d4-mbco.R` (phase-2, exact match vs
`mitml::testModels(method = "D4")`): per-imputation MBCO LRTs are pooled
with the Reiter/Chan-Meng D4 rule (Grund et al. 2021) into an F
reference.

## Usage

``` r
medsim_method_mbco_mi(model, m = 20L, ...)
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

A `function(data, params)` returning the shared method contract (see
file header).

## Details

The point estimate is the Rubin-pooled `a*b`; the CI is a Monte-Carlo
product interval on the pooled paths; the p-value is the D4-pooled MBCO
test. With no missingness (or without `mice`) it degrades to the
complete-case MBCO LRT with a chi-square reference — never to a Sobel
normal approximation for the test.
