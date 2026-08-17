# Missing-Data Mediation: MBCO-MI vs. Monte-Carlo CI

## Introduction

Real mediation studies rarely have complete data, and the underlying
variables are rarely normal. **medsim** ships a set of reusable
data-generating utilities and estimator adapters — added for the
*Missing Effect* study (MBCO-MI vs. Monte-Carlo CI under missingness x
nonnormality) — for simulating mediation analysis under
`MCAR`/`MAR`/`MNAR` missingness and Fleishman-power nonnormality.

This vignette walks through generating nonnormal data, amputating it
under a missingness mechanism, and comparing two estimators of the
indirect effect’s confidence interval: multiple-imputation-based
(`MBCO-MI`) and the Monte-Carlo delta-method CI (`MC-CI`).

## Installation

``` r

pak::pak("Data-Wise/medsim")
```

[`medsim_method_mbco_mi()`](https://data-wise.github.io/medsim/reference/medsim_method_mbco_mi.md)
needs `mice` (multiple imputation); without it, it degrades to a
complete-case MBCO chi-square test. Install the full stack for the
validated D4-stacked pooling:

``` r

install.packages(c("mice", "mitml", "RMediation"))
```

## Step 1: Generate Nonnormal Data

[`medsim_rnonnormal()`](https://data-wise.github.io/medsim/reference/medsim_rnonnormal.md)
draws values with a target skew/kurtosis using the Fleishman power
method:

``` r

library(medsim)

x <- medsim_rnonnormal(n = 500, mean = 0, sd = 1, skew = 1.5, kurtosis = 5)
c(mean = mean(x), sd = sd(x))
```

## Step 2: Insert Missingness

[`medsim_amputate()`](https://data-wise.github.io/medsim/reference/medsim_amputate.md)
inserts `NA`s under a chosen missingness mechanism (MCAR, MAR, or MNAR)
using a logistic amputer calibrated to a target missing rate:

``` r

set.seed(1)
d <- data.frame(X = rnorm(500), M = rnorm(500), Y = rnorm(500))

d_mar <- medsim_amputate(d, vars = "M", mechanism = "MAR", rate = 0.3)
mean(is.na(d_mar$M))
#> [1] 0.3
```

## Step 3: Define a Missing-Data Scenario

[`medsim_scenario_missing()`](https://data-wise.github.io/medsim/reference/medsim_scenario_missing.md)
builds a single scenario combining a DGM, an amputation mechanism, and a
target missing rate.
[`medsim_scenario_missing_grid()`](https://data-wise.github.io/medsim/reference/medsim_scenario_missing_grid.md)
expands a factorial grid over mechanism x rate x nonnormality — the
canonical building block for a full missing-data simulation study:

``` r

scn <- medsim_scenario_missing(
  name = "MAR-30pct",
  mechanism = "MAR",
  rate = 0.3,
  skew = 0,
  kurtosis = 0
)

grid <- medsim_scenario_missing_grid(
  mechanisms = c("MCAR", "MAR", "MNAR"),
  rates = c(0.1, 0.3),
  skew = c(0, 1.5)
)
length(grid)
```

## Step 4: Run Both Estimators

Both
[`medsim_method_mbco_mi()`](https://data-wise.github.io/medsim/reference/medsim_method_mbco_mi.md)
and
[`medsim_method_mc_ci()`](https://data-wise.github.io/medsim/reference/medsim_method_mc_ci.md)
follow the `method(data, params)` contract and return the same 6-field
core result — this is what lets
[`medsim_run()`](https://data-wise.github.io/medsim/reference/medsim_run.md)
compare them scenario-for-scenario. (MBCO-MI additionally emits
branch-mixing diagnostics — `indirect_p_fixed`, `branch_mix`,
`p_branch_a`, `stacked_branch`, `r4`, `r4_fixed` — which
[`medsim_run()`](https://data-wise.github.io/medsim/reference/medsim_run.md)
NA-pads for the MC-CI rows.)

``` r

results <- medsim_run(
  method = list(
    "MBCO-MI" = medsim_method_mbco_mi,
    "MC-CI"   = medsim_method_mc_ci
  ),
  scenarios = list(scn),
  config = medsim_config("test")
)

summary(results)
```

## Step 5: Branch-Switch Diagnostics

Under the union-null LRT used by MBCO, an imputed dataset can
“branch-switch” between the null and alternative decisions across
imputations.
[`medsim_summarize_branch_switch()`](https://data-wise.github.io/medsim/reference/medsim_summarize_branch_switch.md)
reports that rate per scenario — a diagnostic for how much the
missing-data mechanism destabilizes the test decision:

``` r

medsim_summarize_branch_switch(results)
```

## Optional: Inverse-Probability Weighting

A thin IPW adapter,
[`medsim_method_ipw()`](https://data-wise.github.io/medsim/reference/medsim_method_ipw.md),
is also available for comparison against the imputation-based
approaches:

``` r

results_ipw <- medsim_run(
  method = medsim_method_ipw,
  scenarios = list(scn),
  config = medsim_config("test")
)
```

## Next Steps

- See
  [`vignette("getting-started")`](https://data-wise.github.io/medsim/articles/getting-started.md)
  for the general
  [`medsim_run()`](https://data-wise.github.io/medsim/reference/medsim_run.md)
  workflow.
- See
  [`vignette("estimand-kinds")`](https://data-wise.github.io/medsim/articles/estimand-kinds.md)
  for how missing-data scenarios relate to the estimand-kind system used
  elsewhere in medsim.
- [`medsim_method_mbco_mi()`](https://data-wise.github.io/medsim/reference/medsim_method_mbco_mi.md)
  reproduces `mitml::testModels(method = "D4")` exactly when
  `mice`/`mitml` are installed (Reiter/Chan-Meng D4 pooling).
