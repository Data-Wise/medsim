# MBCO-MI estimator adapter

Multiple imputation (via `mice`) + **D4-stacked MBCO** likelihood-ratio
inference for the union null `H0: ab = 0`. Ports the validated
`Missing Effect/code/prototype-d4-mbco.R` (phase-2; the D4 statistic
matches `mitml::testModels(method = "D4")`, and so does the p-value with
`df = "chan_meng"`): per-imputation MBCO LRTs are pooled with the
Reiter/Chan-Meng D4 rule (Grund et al. 2021) into an F reference.

## Usage

``` r
medsim_method_mbco_mi(
  model,
  m = 20L,
  ariv = c("own", "fixed"),
  df = c("li", "chan_meng"),
  ...
)
```

## Arguments

- model:

  Mediation model spec (accepted for API symmetry; the estimator reads
  the `X`, `M`, `Y` (+ optional `C*`) columns of `data`).

- m:

  Integer number of imputations.

- ariv:

  Which relative-increase-in-variance (ARIV) estimate feeds the
  `indirect_p` column: `"own"` (default, legacy) pools per-imputation
  MBCO statistics each computed on its own winning branch (standard
  Chan-Meng `r4`); `"fixed"` recomputes every per-imputation statistic
  on the branch the stacked constrained fit selected, so branch
  disagreement across imputations cannot pull the ARIV down. Both
  p-values are always emitted (`indirect_p` and `indirect_p_fixed`),
  together with the branch-mixing diagnostics `branch_mix`,
  `p_branch_a`, `stacked_branch`, `r4`, `r4_fixed` (with a single
  imputation there is no stacked fit: `stacked_branch` is that fit's
  branch and `r4 = r4_fixed = 0`; on the error path all six are `NA`).

- df:

  Denominator degrees of freedom for the D4 `F` reference, used for both
  `indirect_p` and `indirect_p_fixed`: `"li"` (default, legacy) is the
  Li et al. (1991) / Meng-Rubin approximation, \\\nu = 4 + (k(K-1) -
  4)\\1 + (1 - 2/(k(K-1)))/r_4\\^2\\ when \\k(K-1) \> 4\\; `"chan_meng"`
  is Chan and Meng (2022, eq. 2.15), \\\nu = k(K-1)(1 + 1/r_4)^2\\, the
  reference used by `mitml::testModels(method = "D4")`. Both give \\\nu
  = \infty\\ (a chi-square reference) when \\r_4 = 0\\. Chan and Meng
  derive eq. 2.15 for the own-branch \\r_4\\ at a regular null; for
  `ariv = "fixed"`, and at the nonregular null \\a = b = 0\\, either
  choice is a working reference whose calibration rests on simulation.
  The choice changes only the p-values: the statistic, \\r_4\\ and the
  random-number stream are identical.

- ...:

  Reserved for future MI / MBCO options.

## Value

A `function(data, params)` returning the shared method contract (see
file header).

## Details

The point estimate is the Rubin-pooled `a*b`; the CI is a Monte-Carlo
product interval on the pooled paths; the p-value is the D4-pooled MBCO
test. With no missingness (or without `mice`) it degrades to the
complete-case MBCO LRT with a chi-square reference – never to a Sobel
normal approximation for the test.

## Examples

``` r
# \donttest{
if (requireNamespace("mice", quietly = TRUE)) {
  set.seed(1)
  d <- data.frame(X = rnorm(200))
  d$M <- 0.39 * d$X + rnorm(200)
  d$Y <- 0.39 * d$M + 0.14 * d$X + rnorm(200)
  d <- medsim_amputate(d, target = "M", mechanism = "MAR", prop = 0.25)
  fit <- medsim_method_mbco_mi(model = NULL, m = 10, ariv = "fixed", df = "chan_meng")
  out <- fit(d, list())
  out[c("indirect", "indirect_p", "indirect_p_fixed", "r4_fixed")]
}
# }
```
