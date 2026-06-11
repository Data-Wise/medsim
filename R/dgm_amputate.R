# INTERFACE FREEZE (Step 0) — signature stub for WS-B.
# Spec: SPEC-medsim-missingdata-generators-2026-06-11.md  (§"2. Missingness amputation")

#' Insert missing values under MCAR / MAR / MNAR (WS-B)
#'
#' Thin, documented amputer aligning `mice::ampute` semantics to the medsim DGM contract.
#' Returns the input frame with `NA`s inserted in `target` column(s); column names/order
#' are preserved (DGM data contract).
#'
#' @param data data.frame with columns `X, M, Y` (+ optional covariates).
#' @param target Character vector of column(s) to set `NA` (e.g. `"M"`, or `c("M","Y")`).
#' @param mechanism One of `"MCAR"`, `"MAR"`, `"MNAR"`. MCAR: constant prob; MAR: logistic on
#'   observed `predictors`; MNAR: logistic including the `target` itself (self-mechanism).
#' @param prop Target overall missingness proportion in `target`.
#' @param predictors Columns driving missingness for MAR/MNAR (defaults to all non-`target`).
#' @param weights Optional named weights for the missingness logistic.
#' @param type Amputation tail type, `mice::ampute`-style: one of `"RIGHT","LEFT","MID","TAIL"`.
#' @return `data` with `NA`s inserted in `target`; names/order preserved.
#' @export
medsim_amputate <- function(data, target, mechanism = c("MCAR", "MAR", "MNAR"),
                            prop = 0.2, predictors = NULL, weights = NULL,
                            type = c("RIGHT", "LEFT", "MID", "TAIL")) {
  mechanism <- match.arg(mechanism)
  type <- match.arg(type)

  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  if (missing(target) || is.null(target) || length(target) == 0L) {
    stop("`target` must name at least one column of `data`.", call. = FALSE)
  }
  target <- as.character(target)
  missing_cols <- setdiff(target, names(data))
  if (length(missing_cols) > 0L) {
    stop(sprintf("`target` column(s) not found in `data`: %s",
                 paste(missing_cols, collapse = ", ")), call. = FALSE)
  }
  if (!is.numeric(prop) || length(prop) != 1L || prop < 0 || prop > 1) {
    stop("`prop` must be a single number in [0, 1].", call. = FALSE)
  }

  orig_names <- names(data)
  n <- nrow(data)

  # Nothing to do (degenerate but valid) — return unchanged with structure preserved.
  if (n == 0L || prop == 0) {
    return(data[orig_names])
  }

  # Resolve predictors for MAR/MNAR: default to all non-target columns.
  if (is.null(predictors)) {
    predictors <- setdiff(orig_names, target)
  } else {
    predictors <- as.character(predictors)
    bad_pred <- setdiff(predictors, orig_names)
    if (length(bad_pred) > 0L) {
      stop(sprintf("`predictors` not found in `data`: %s",
                   paste(bad_pred, collapse = ", ")), call. = FALSE)
    }
  }

  # Each target column is amputed independently so that the realized rate in
  # every target column is ~= `prop` (multi-column support).
  for (col in target) {
    miss <- .medsim_missing_indicator(
      data = data, col = col, mechanism = mechanism, prop = prop,
      predictors = setdiff(predictors, col), weights = weights, type = type
    )
    data[[col]][miss] <- NA
  }

  data[orig_names]
}

# ---- internal: build the per-column missingness indicator -------------------
#
# Returns a logical vector (length nrow(data)) marking rows whose `col` value
# should be set to NA. Mechanism semantics:
#   MCAR  constant Bernoulli prob == prop, independent of all variables.
#   MAR   logistic on observed `predictors` only.
#   MNAR  logistic including the target column's own (pre-amputation) value.
# For MAR/MNAR the logistic intercept is calibrated so that the *expected*
# missingness equals `prop` (mean of the Bernoulli probabilities == prop).
.medsim_missing_indicator <- function(data, col, mechanism, prop, predictors,
                                      weights, type) {
  n <- nrow(data)

  if (mechanism == "MCAR") {
    # MCAR == i.i.d. Bernoulli(prop), independent of all variables. We draw
    # exactly round(prop * n) missing rows so the realized per-column rate
    # equals `prop` up to integer rounding (deterministic count, random
    # placement). This is the lowest-variance unbiased MCAR draw; mice::ampute
    # with a single pattern + bycases=TRUE adds run-to-run variance without
    # changing the MCAR semantics, so we do not route MCAR through it.
    k <- as.integer(round(prop * n))
    ind <- rep(FALSE, n)
    if (k > 0L) {
      ind[sample.int(n, k)] <- TRUE
    }
    return(ind)
  }

  # ----- MAR / MNAR: logistic missingness model --------------------------------
  # Assemble the linear-predictor inputs. MNAR adds the target's own value.
  score_vars <- predictors
  if (mechanism == "MNAR") {
    score_vars <- union(predictors, col)
  }
  score_vars <- intersect(score_vars, names(data))

  # No usable predictor (e.g. MAR with empty predictor set): fall back to MCAR.
  if (length(score_vars) == 0L) {
    return(stats::runif(n) < prop)
  }

  # Build a numeric, standardized design from the score variables so the
  # logistic slopes have a comparable scale and calibration is well-behaved.
  cols <- lapply(score_vars, function(v) .medsim_as_numeric(data[[v]]))
  names(cols) <- score_vars
  Z <- vapply(cols, function(z) {
    s <- stats::sd(z, na.rm = TRUE)
    if (!is.finite(s) || s == 0) {
      rep(0, n)
    } else {
      zc <- (z - mean(z, na.rm = TRUE)) / s
      zc[!is.finite(zc)] <- 0
      zc
    }
  }, numeric(n))
  if (is.null(dim(Z))) {
    Z <- matrix(Z, ncol = length(score_vars))
  }

  # Slope weights: default 1 per score variable; honor user `weights` by name.
  beta <- rep(1, length(score_vars))
  names(beta) <- score_vars
  if (!is.null(weights)) {
    common <- intersect(names(weights), score_vars)
    if (length(common) > 0L) {
      beta[common] <- as.numeric(weights[common])
    }
  }

  # Tail orientation (mice::ampute-style `type`).
  lin <- as.vector(Z %*% beta)
  lin <- switch(type,
    RIGHT = lin,
    LEFT  = -lin,
    MID   = -abs(lin),
    TAIL  = abs(lin),
    lin
  )

  # Calibrate the intercept so mean(plogis(intercept + lin)) == prop.
  intercept <- .medsim_calibrate_intercept(lin, prop)
  probs <- stats::plogis(intercept + lin)
  stats::runif(n) < probs
}

# Coerce a column to numeric for the missingness model (factors/chars -> codes).
.medsim_as_numeric <- function(x) {
  if (is.numeric(x)) {
    return(as.numeric(x))
  }
  if (is.logical(x)) {
    return(as.numeric(x))
  }
  as.numeric(as.integer(as.factor(x)))
}

# Solve for intercept a such that mean(plogis(a + lin)) == prop, via bisection
# on the monotone increasing function a -> mean(plogis(a + lin)).
.medsim_calibrate_intercept <- function(lin, prop) {
  if (prop <= 0) return(-Inf)
  if (prop >= 1) return(Inf)
  f <- function(a) mean(stats::plogis(a + lin)) - prop
  lo <- -50
  hi <- 50
  flo <- f(lo)
  fhi <- f(hi)
  if (flo > 0) return(lo)  # already over-missing at the floor
  if (fhi < 0) return(hi)  # cannot reach prop even at the ceiling
  for (i in seq_len(100)) {
    mid <- (lo + hi) / 2
    fm <- f(mid)
    if (abs(fm) < 1e-9) return(mid)
    if (fm < 0) lo <- mid else hi <- mid
  }
  (lo + hi) / 2
}
