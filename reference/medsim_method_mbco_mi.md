# MBCO-MI estimator adapter

Multiple imputation (via `mice`) + **D4-stacked MBCO** likelihood-ratio
inference for the union null `H0: ab = 0`. Per-imputation MBCO LRTs are
pooled with the Reiter/Chan-Meng D4 rule (Grund et al. 2021). With
identical likelihood-ratio inputs, `ariv = "own", df = "chan_meng"`
reproduces the D4 statistic and p-value from
`mitml::testModels(method = "D4", ariv = "positive")`. The default
fixed-branch ARIV can change the pooled D4 statistic and p-value when
imputations select different branches. The adapter ports the phase-2
prototype `Missing Effect/code/prototype-d4-mbco.R`.

## Usage

``` r
medsim_method_mbco_mi(
  model,
  m = 20L,
  ariv = c("fixed", "own"),
  df = c("chan_meng", "li"),
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
  `indirect_p` column: `"fixed"` (default, as in missingmed) recomputes
  every per-imputation statistic on the branch the stacked constrained
  fit selected, so branch disagreement across imputations cannot pull
  the ARIV down; `"own"` (the previous default) pools per-imputation
  MBCO statistics each computed on its own winning branch (standard
  Chan-Meng `r4`) and can be liberal at high missingness. `indirect_p`
  uses the selected mode and `indirect_p_fixed` always uses fixed-branch
  ARIV. The two columns are equal under the default. Branch-mixing
  diagnostics are `branch_mix`, `p_branch_a`, `stacked_branch`, `r4`,
  `r4_fixed` (with a single imputation there is no stacked fit:
  `stacked_branch` is that fit's branch and `r4 = r4_fixed = 0`; on the
  error path all six are `NA`).

- df:

  Denominator degrees of freedom for the D4 `F` reference, used for both
  `indirect_p` and `indirect_p_fixed`: `"chan_meng"` (default, as in
  missingmed) is Chan and Meng (2022, eq. 2.15), \\\nu = k(K-1)(1 +
  1/r_4)^2\\, the reference used by `mitml::testModels(method = "D4")`;
  `"li"` (the previous default) is the Li et al. (1991) / Meng-Rubin
  approximation, \\\nu = 4 + (k(K-1) - 4)\\1 + (1 -
  2/(k(K-1)))/r_4\\^2\\ when \\k(K-1) \> 4\\. Both give \\\nu = \infty\\
  (a chi-square reference) when \\r_4 = 0\\. Chan and Meng derive eq.
  2.15 for the own-branch \\r_4\\ at a regular null; for
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
