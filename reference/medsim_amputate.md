# Insert missing values under MCAR / MAR / MNAR

Thin, documented amputer aligning `mice::ampute` semantics to the medsim
DGM contract. Returns the input frame with `NA`s inserted in `target`
column(s); column names/order are preserved (DGM data contract).

## Usage

``` r
medsim_amputate(
  data,
  target,
  mechanism = c("MCAR", "MAR", "MNAR"),
  prop = 0.2,
  predictors = NULL,
  weights = NULL,
  type = c("RIGHT", "LEFT", "MID", "TAIL")
)
```

## Arguments

- data:

  data.frame with columns `X, M, Y` (+ optional covariates).

- target:

  Character vector of column(s) to set `NA` (e.g. `"M"`, or
  `c("M","Y")`).

- mechanism:

  One of `"MCAR"`, `"MAR"`, `"MNAR"`. MCAR: constant prob; MAR: logistic
  on observed `predictors`; MNAR: logistic including the `target` itself
  (self-mechanism).

- prop:

  Target overall missingness proportion in `target`.

- predictors:

  Columns driving missingness for MAR/MNAR (defaults to all
  non-`target`).

- weights:

  Optional named weights for the missingness logistic.

- type:

  Amputation tail type, `mice::ampute`-style: one of
  `"RIGHT","LEFT","MID","TAIL"`.

## Value

`data` with `NA`s inserted in `target`; names/order preserved.

## Examples

``` r
set.seed(1)
d <- data.frame(X = rnorm(300), M = rnorm(300), Y = rnorm(300))
d_mar <- medsim_amputate(d, target = "M", mechanism = "MAR", prop = 0.3)
mean(is.na(d_mar$M))
#> [1] 0.3133333
```
