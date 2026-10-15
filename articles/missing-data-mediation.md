# Missing-Data Mediation: MBCO-MI vs. Monte-Carlo CI

## Introduction

Real mediation studies rarely have complete data, and the underlying
variables are rarely normal. **medsim** ships a set of reusable
data-generating utilities and estimator adapters — added for the
*Missing Effect* study (MBCO-MI vs. Monte-Carlo CI under missingness x
nonnormality) — for simulating mediation analysis under
`MCAR`/`MAR`/`MNAR` missingness and Fleishman-power nonnormality.

This vignette walks through generating nonnormal data, amputating it
under a missingness mechanism, and comparing two ways to test the
indirect effect under multiple imputation: the D4-stacked MBCO
likelihood-ratio test (`MBCO-MI`) and the Monte-Carlo product interval
on the Rubin-pooled paths (`MC-CI`).

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
inserts `NA`s into the `target` column(s) under a chosen missingness
mechanism (MCAR, MAR, or MNAR), with a logistic amputer calibrated to
the target proportion `prop`:

``` r

set.seed(1)
d <- data.frame(X = rnorm(500), M = rnorm(500), Y = rnorm(500))

d_mar <- medsim_amputate(d, target = "M", mechanism = "MAR", prop = 0.3)
mean(is.na(d_mar$M))
```

## Step 3: Define a Missing-Data Scenario

[`medsim_scenario_missing()`](https://data-wise.github.io/medsim/reference/medsim_scenario_missing.md)
builds a single scenario combining the generating paths (`true_params`:
`a`, `b`, `cp`), an amputation mechanism, a target missing proportion,
and optional nonnormal residuals.
[`medsim_scenario_missing_grid()`](https://data-wise.github.io/medsim/reference/medsim_scenario_missing_grid.md)
crosses a list of effect-size cells with mechanisms, proportions, and
residual shapes, the building block for a full missing-data simulation
study:

``` r

scn <- medsim_scenario_missing(
  name = "MAR-30pct",
  true_params = list(a = 0.39, b = 0.39, cp = 0.14),
  mechanism = "MAR",
  prop = 0.3
)

grid <- medsim_scenario_missing_grid(
  true_params_list = list(
    small  = list(a = 0.14, b = 0.14, cp = 0.14),
    medium = list(a = 0.39, b = 0.39, cp = 0.14)
  ),
  mechanisms = c("MCAR", "MAR", "MNAR"),
  props = c(0.1, 0.3),
  nonnormal_list = list(normal = NULL, skew = list(skew = 1.5, kurtosis = 4))
)
length(grid)
```

## Step 4: Run Both Estimators

[`medsim_method_mbco_mi()`](https://data-wise.github.io/medsim/reference/medsim_method_mbco_mi.md)
and
[`medsim_method_mc_ci()`](https://data-wise.github.io/medsim/reference/medsim_method_mc_ci.md)
are factories: each returns a function with the `method(data, params)`
contract and the same 6-field core result.
[`medsim_run()`](https://data-wise.github.io/medsim/reference/medsim_run.md)
takes one method, so run each and compare the results scenario by
scenario. (MBCO-MI additionally emits branch diagnostics:
`indirect_p_fixed`, `branch_mix`, `p_branch_a`, `stacked_branch`, `r4`,
`r4_fixed`.)

``` r

results <- medsim_run(
  method = medsim_method_mbco_mi(model = NULL, m = 20),
  scenarios = list(scn),
  config = medsim_config("test")
)
results_mc <- medsim_run(
  method = medsim_method_mc_ci(model = NULL, m = 20),
  scenarios = list(scn),
  config = medsim_config("test")
)

summary(results)
```

### Fixed-branch correction and the reference degrees of freedom

Two options of
[`medsim_method_mbco_mi()`](https://data-wise.github.io/medsim/reference/medsim_method_mbco_mi.md)
change how the D4 test is referred to its `F` distribution. Neither
changes the data, the imputations, or the D4 statistic:

- `ariv = "fixed"` computes the relative increase in variance (`r4`)
  from per-imputation statistics on the branch that the stacked
  constrained fit selects, so imputations that disagree on the branch
  cannot deflate it. The default `ariv = "own"` is the standard
  Chan-Meng pooling. Both p-values are always returned (`indirect_p`,
  `indirect_p_fixed`); `ariv` chooses which one fills `indirect_p`.
- `df = "chan_meng"` uses the Chan and Meng (2022, eq. 2.15) denominator
  degrees of freedom, as `mitml::testModels(method = "D4")` does; the
  default `df = "li"` keeps the Li et al. (1991) approximation. Only the
  p-values change.

``` r

results_fixed <- medsim_run(
  method = medsim_method_mbco_mi(model = NULL, m = 20, ariv = "fixed", df = "chan_meng"),
  scenarios = list(scn),
  config = medsim_config("test")
)
```

## Step 5: Branch-Switch Diagnostics

MBCO fits the union null `ab = 0` as the better of two single-path fits,
`a = 0` or `b = 0`, and the imputations of one dataset can disagree on
which branch wins. Each MBCO-MI row records the majority branch
(`branch_switch`, 1 = `a = 0`) and whether the imputations disagreed
(`branch_mix`).
[`medsim_summarize_branch_switch()`](https://data-wise.github.io/medsim/reference/medsim_summarize_branch_switch.md)
reports, per scenario, the share of replicates in which the `a = 0`
branch wins:

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
  method = medsim_method_ipw(model = NULL),
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
  reproduces the `mitml::testModels(method = "D4")` statistic when
  `mice`/`mitml` are installed (Reiter/Chan-Meng D4 pooling). Its
  p-value matches `mitml` (with `ariv = "positive"`) under
  `df = "chan_meng"`; the default `df = "li"` keeps the legacy Li et
  al. (1991) denominator df.
