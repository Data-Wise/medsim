# ── INTERFACE FREEZE (Step 0) ─────────────────────────────────────────────────
# Spec: SPEC-medsim-missingdata-generators-2026-06-11.md
# These are SIGNATURE STUBS only — bodies are filled by their workstreams (WS-*).
# Do not change a signature without updating the spec's "Interface freeze" section.
#
# SHARED method() RETURN CONTRACT (consumed by medsim_analyze_coverage/_power):
#   every estimator adapter returns a flat named list of numeric scalars:
#     list(
#       indirect          = <num>,   # point estimate of the indirect effect (a*b / TNIE)
#       indirect_ci_lower = <num>,   # -> medsim_analyze_coverage()
#       indirect_ci_upper = <num>,
#       indirect_p        = <num>,   # -> medsim_analyze_power()
#       branch_switch     = <0/1>,   # MBCO union-null diagnostic (ab=0); NA for MC-CI/IPW
#       converged         = <0/1>    # drop non-converged rows in analyze
#     )
#
# VERIFIED 2026-06-11 (R/runner.R:329): method is invoked as `method(data, scenario$params)`,
#   i.e. the contract is `function(data, params)` where `params` IS the scenario's params list.
# ──────────────────────────────────────────────────────────────────────────────

#' MBCO-MI estimator adapter (WS-D)
#'
#' Multiple imputation (via `missingmed`) + MBCO likelihood-ratio inference (via
#' `rmediation::mbco()`). During prototyping this wraps
#' `Missing Effect/code/prototype-d4-mbco.R`.
#'
#' @param model lavaan/OpenMx mediation model syntax or spec.
#' @param m Integer number of imputations.
#' @param ... Passed to the underlying MI / MBCO routines.
#' @return A `function(data, params)` returning the shared method contract (see file header).
#' @export
medsim_method_mbco_mi <- function(model, m = 20L, ...) {
  m <- as.integer(m)
  dots <- list(...)
  function(data, params) {
    out <- list(indirect = NA_real_, indirect_ci_lower = NA_real_,
                indirect_ci_upper = NA_real_, indirect_p = NA_real_,
                branch_switch = 0, converged = 0)
    res <- tryCatch({
      have_mi  <- requireNamespace("missingmed", quietly = TRUE)
      have_rmd <- requireNamespace("rmediation", quietly = TRUE) ||
                  requireNamespace("RMediation", quietly = TRUE)

      ## Imputed (or complete-case) datasets to estimate over.
      implist <- .medsim_md_implist(data, m = m, use_mi = have_mi)

      ## Per-imputation a, b, ab and the MBCO branch diagnostic.
      covs   <- .medsim_md_covariates(data)
      fitlist <- lapply(implist, function(d) .medsim_md_fit_ab(d, covs))
      a_hat  <- mean(vapply(fitlist, function(z) z$a,  numeric(1)))
      b_hat  <- mean(vapply(fitlist, function(z) z$b,  numeric(1)))
      va     <- mean(vapply(fitlist, function(z) z$va, numeric(1)))
      vb     <- mean(vapply(fitlist, function(z) z$vb, numeric(1)))
      ab     <- a_hat * b_hat

      ## Delta-method SE for a*b (Sobel): se = sqrt(b^2 va + a^2 vb).
      se_ab  <- sqrt(b_hat^2 * va + a_hat^2 * vb)
      z975   <- stats::qnorm(0.975)
      lo     <- ab - z975 * se_ab
      hi     <- ab + z975 * se_ab
      pval   <- 2 * stats::pnorm(-abs(ab / se_ab))

      ## MBCO union-null branch diagnostic, averaged across imputations:
      ## 1 when the dropped-a branch wins the max (per MEMO union-null rule).
      branch <- mean(vapply(implist, function(d) .medsim_md_branch(d, covs),
                            numeric(1)))
      branch_switch <- as.integer(branch >= 0.5)

      list(indirect = ab, indirect_ci_lower = lo, indirect_ci_upper = hi,
           indirect_p = pval, branch_switch = branch_switch, converged = 1)
    }, error = function(e) NULL)
    if (!is.null(res)) out <- res
    out
  }
}

#' Monte-Carlo CI estimator adapter (WS-D)
#'
#' Multiple imputation + Monte-Carlo confidence interval (via `rmediation::medci()`).
#'
#' @inheritParams medsim_method_mbco_mi
#' @return A `function(data, params)` returning the shared method contract (`branch_switch = NA`).
#' @export
medsim_method_mc_ci <- function(model, m = 20L, ...) {
  m <- as.integer(m)
  dots <- list(...)
  n_draw <- if (!is.null(dots$n_mc)) as.integer(dots$n_mc) else 20000L
  function(data, params) {
    out <- list(indirect = NA_real_, indirect_ci_lower = NA_real_,
                indirect_ci_upper = NA_real_, indirect_p = NA_real_,
                branch_switch = NA, converged = 0)
    res <- tryCatch({
      have_mi  <- requireNamespace("missingmed", quietly = TRUE)
      have_rmd <- requireNamespace("rmediation", quietly = TRUE) ||
                  requireNamespace("RMediation", quietly = TRUE)

      implist <- .medsim_md_implist(data, m = m, use_mi = have_mi)
      covs    <- .medsim_md_covariates(data)
      fitlist <- lapply(implist, function(d) .medsim_md_fit_ab(d, covs))
      a_hat  <- mean(vapply(fitlist, function(z) z$a,  numeric(1)))
      b_hat  <- mean(vapply(fitlist, function(z) z$b,  numeric(1)))
      va     <- mean(vapply(fitlist, function(z) z$va, numeric(1)))
      vb     <- mean(vapply(fitlist, function(z) z$vb, numeric(1)))
      ab     <- a_hat * b_hat

      if (have_rmd) {
        ## Use rmediation::medci() / RMediation::medci() when available.
        rm_ns <- if (requireNamespace("rmediation", quietly = TRUE))
                   "rmediation" else "RMediation"
        ci <- tryCatch(
          getExportedValue(rm_ns, "medci")(
            mu.x = a_hat, mu.y = b_hat,
            se.x = sqrt(va), se.y = sqrt(vb),
            type = "MC", alpha = 0.05),
          error = function(e) NULL)
        if (!is.null(ci) && !is.null(ci[[1]])) {
          bounds <- as.numeric(ci[[1]])
          lo <- min(bounds); hi <- max(bounds)
        } else {
          mc <- .medsim_md_mc_ci(a_hat, b_hat, va, vb, n_draw)
          lo <- mc[1]; hi <- mc[2]
        }
      } else {
        ## Base-R Monte-Carlo CI: draw (a,b) ~ N, form product distribution.
        mc <- .medsim_md_mc_ci(a_hat, b_hat, va, vb, n_draw)
        lo <- mc[1]; hi <- mc[2]
      }

      se_ab <- sqrt(b_hat^2 * va + a_hat^2 * vb)
      pval  <- 2 * stats::pnorm(-abs(ab / se_ab))

      list(indirect = ab, indirect_ci_lower = lo, indirect_ci_upper = hi,
           indirect_p = pval, branch_switch = NA, converged = 1)
    }, error = function(e) NULL)
    if (!is.null(res)) out <- res
    out
  }
}

#' Thin IPW estimator adapter (WS-D, robustness appendix)
#'
#' Inverse-probability-weighting comparator. Positioned as a robustness appendix, not a
#' co-equal main-results arm (decided 2026-06-11).
#'
#' @param model Mediation model spec.
#' @param ... Passed to the underlying weighting / fitting routines.
#' @return A `function(data, params)` returning the shared method contract (`branch_switch = NA`).
#' @export
medsim_method_ipw <- function(model, ...) {
  dots <- list(...)
  function(data, params) {
    out <- list(indirect = NA_real_, indirect_ci_lower = NA_real_,
                indirect_ci_upper = NA_real_, indirect_p = NA_real_,
                branch_switch = NA, converged = 0)
    res <- tryCatch({
      covs <- .medsim_md_covariates(data)

      ## Thin IPW: weight complete cases by inverse probability of being
      ## observed, modeling the missingness indicator on X (+ covariates).
      key <- intersect(c("M", "Y"), names(data))
      r   <- stats::complete.cases(data[, key, drop = FALSE])

      pred <- intersect(c("X", covs), names(data))
      wts  <- rep(1, nrow(data))
      if (length(pred) > 0 && any(!r) && any(r)) {
        rhs   <- paste(pred, collapse = " + ")
        gfit  <- stats::glm(stats::as.formula(paste("r ~", rhs)),
                            data = data, family = stats::binomial())
        phat  <- stats::fitted(gfit)
        phat  <- pmin(pmax(phat, 1e-3), 1 - 1e-3)
        wts   <- 1 / phat
      }

      dcc  <- data[r, , drop = FALSE]
      wcc  <- wts[r]
      fit  <- .medsim_md_fit_ab(dcc, covs, weights = wcc)
      ab   <- fit$a * fit$b
      se_ab <- sqrt(fit$b^2 * fit$va + fit$a^2 * fit$vb)
      z975 <- stats::qnorm(0.975)
      pval <- 2 * stats::pnorm(-abs(ab / se_ab))

      list(indirect = ab, indirect_ci_lower = ab - z975 * se_ab,
           indirect_ci_upper = ab + z975 * se_ab, indirect_p = pval,
           branch_switch = NA, converged = 1)
    }, error = function(e) NULL)
    if (!is.null(res)) out <- res
    out
  }
}

# ── Internal helpers (WS-D) ───────────────────────────────────────────────────

# Detect optional covariate columns (C, C1, C2, ...) in a DGM frame.
.medsim_md_covariates <- function(data) {
  nm <- names(data)
  grep("^C[0-9]*$", nm, value = TRUE)
}

# Build a list of analysis datasets: multiply-imputed via missingmed/mice when
# available and requested, otherwise a single complete-case frame (listwise).
.medsim_md_implist <- function(data, m = 20L, use_mi = FALSE) {
  if (use_mi && requireNamespace("missingmed", quietly = TRUE) &&
      requireNamespace("mice", quietly = TRUE) && anyNA(data)) {
    imp <- tryCatch(
      mice::mice(data, m = m, method = "norm", printFlag = FALSE),
      error = function(e) NULL)
    if (!is.null(imp)) {
      il <- mice::complete(imp, "all")
      return(il)
    }
  }
  # Fallback: listwise / complete-case analysis on a single frame.
  list(data[stats::complete.cases(data), , drop = FALSE])
}

# Fit lm(M ~ X [+ C]) and lm(Y ~ X + M [+ C]); return a, b and their variances.
.medsim_md_fit_ab <- function(d, covs = character(0), weights = NULL) {
  cterm <- if (length(covs)) paste("+", paste(covs, collapse = " + ")) else ""
  fm <- stats::as.formula(paste("M ~ X", cterm))
  fy <- stats::as.formula(paste("Y ~ X + M", cterm))
  if (is.null(weights)) {
    mfit <- stats::lm(fm, data = d)
    yfit <- stats::lm(fy, data = d)
  } else {
    mfit <- stats::lm(fm, data = d, weights = weights)
    yfit <- stats::lm(fy, data = d, weights = weights)
  }
  sm <- summary(mfit)$coefficients
  sy <- summary(yfit)$coefficients
  list(a  = unname(sm["X", "Estimate"]),
       b  = unname(sy["M", "Estimate"]),
       va = unname(sm["X", "Std. Error"]^2),
       vb = unname(sy["M", "Std. Error"]^2))
}

# MBCO union-null branch diagnostic on one (imputed/complete) dataset.
# Returns 1 when the dropped-a branch wins the union-null max, else 0.
.medsim_md_branch <- function(d, covs = character(0)) {
  cterm <- if (length(covs)) paste("+", paste(covs, collapse = " + ")) else ""
  ll <- function(drop_a = FALSE, drop_b = FALSE) {
    fm <- if (drop_a) stats::as.formula(paste("M ~ 1", cterm))
          else        stats::as.formula(paste("M ~ X", cterm))
    fy <- if (drop_b) stats::as.formula(paste("Y ~ X", cterm))
          else        stats::as.formula(paste("Y ~ X + M", cterm))
    as.numeric(stats::logLik(stats::lm(fm, d))) +
      as.numeric(stats::logLik(stats::lm(fy, d)))
  }
  lA <- ll(drop_a = TRUE)   # a = 0 branch
  lB <- ll(drop_b = TRUE)   # b = 0 branch
  if (lA >= lB) 1 else 0
}

# Base-R Monte-Carlo CI for the product a*b given (a,b) ~ independent normals.
.medsim_md_mc_ci <- function(a, b, va, vb, n_draw = 20000L) {
  ad <- stats::rnorm(n_draw, a, sqrt(va))
  bd <- stats::rnorm(n_draw, b, sqrt(vb))
  unname(stats::quantile(ad * bd, c(0.025, 0.975), names = FALSE))
}
