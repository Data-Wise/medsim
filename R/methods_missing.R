# ── Missing-data mediation estimator adapters (WS-D) ──────────────────────────
# Spec: SPEC-medsim-missingdata-generators-2026-06-11.md
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

#' MBCO-MI estimator adapter
#'
#' Multiple imputation (via `mice`) + **D4-stacked MBCO** likelihood-ratio
#' inference for the union null `H0: ab = 0`. Ports the validated
#' `Missing Effect/code/prototype-d4-mbco.R` (phase-2, exact match vs
#' `mitml::testModels(method = "D4")`): per-imputation MBCO LRTs are pooled with
#' the Reiter/Chan-Meng D4 rule (Grund et al. 2021) into an F reference.
#'
#' The point estimate is the Rubin-pooled `a*b`; the CI is a Monte-Carlo product
#' interval on the pooled paths; the p-value is the D4-pooled MBCO test. With no
#' missingness (or without `mice`) it degrades to the complete-case MBCO LRT with
#' a chi-square reference — never to a Sobel normal approximation for the test.
#'
#' @param model Mediation model spec (accepted for API symmetry; the estimator
#'   reads the `X`, `M`, `Y` (+ optional `C*`) columns of `data`).
#' @param m Integer number of imputations.
#' @param ... Reserved for future MI / MBCO options.
#' @return A `function(data, params)` returning the shared method contract (see file header).
#' @export
medsim_method_mbco_mi <- function(model, m = 20L, ...) {
  m <- as.integer(m)
  function(data, params) {
    out <- list(
      indirect = NA_real_, indirect_ci_lower = NA_real_,
      indirect_ci_upper = NA_real_, indirect_p = NA_real_,
      branch_switch = 0, converged = 0
    )
    res <- tryCatch(
      {
        covs <- .medsim_md_covariates(data)
        use_mi <- requireNamespace("mice", quietly = TRUE) && anyNA(data)
        implist <- .medsim_md_implist(data, m = m, use_mi = use_mi)
        kk <- length(implist)

        ## Point estimate a*b, Rubin-pooled across imputations.
        fitlist <- lapply(implist, function(d) .medsim_md_fit_ab(d, covs))
        a_est <- vapply(fitlist, function(z) z$a, numeric(1))
        b_est <- vapply(fitlist, function(z) z$b, numeric(1))
        a_hat <- mean(a_est)
        b_hat <- mean(b_est)
        va <- .medsim_rubin_var(a_est, vapply(fitlist, function(z) z$va, numeric(1)))
        vb <- .medsim_rubin_var(b_est, vapply(fitlist, function(z) z$vb, numeric(1)))
        ab <- a_hat * b_hat

        ## CI: Monte-Carlo product interval on the pooled (a, b) with Rubin totals.
        mc <- .medsim_md_mc_ci(a_hat, b_hat, va, vb)

        ## P-value: the validated MBCO test (D4 pooling when K >= 2, else chi-sq).
        pval <- if (kk >= 2L) {
          unname(.medsim_d4_mbco(implist, covs)[["p"]])
        } else {
          tstat <- .medsim_mbco_T(implist[[1L]], covs)
          stats::pchisq(max(tstat, 0), df = 1, lower.tail = FALSE)
        }

        ## branch_switch: majority union-null branch (1 = the a = 0 branch wins).
        branch <- mean(vapply(implist, function(d) .medsim_mbco_branch(d, covs), 0))
        branch_switch <- as.integer(branch >= 0.5)

        list(
          indirect = ab, indirect_ci_lower = mc[1L], indirect_ci_upper = mc[2L],
          indirect_p = pval, branch_switch = branch_switch, converged = 1
        )
      },
      error = function(e) NULL
    )
    if (!is.null(res)) out <- res
    out
  }
}

#' Monte-Carlo CI estimator adapter
#'
#' Multiple imputation (via `mice`) + Monte-Carlo confidence interval for the
#' indirect effect (via `RMediation::medci()` when available, else a base-R
#' product-of-normals draw on the Rubin-pooled paths).
#'
#' @inheritParams medsim_method_mbco_mi
#' @return A `function(data, params)` returning the shared method contract (`branch_switch = NA`).
#' @export
medsim_method_mc_ci <- function(model, m = 20L, ...) {
  m <- as.integer(m)
  dots <- list(...)
  n_draw <- if (!is.null(dots$n_mc)) as.integer(dots$n_mc) else 20000L
  function(data, params) {
    out <- list(
      indirect = NA_real_, indirect_ci_lower = NA_real_,
      indirect_ci_upper = NA_real_, indirect_p = NA_real_,
      branch_switch = NA, converged = 0
    )
    res <- tryCatch(
      {
        covs <- .medsim_md_covariates(data)
        use_mi <- requireNamespace("mice", quietly = TRUE) && anyNA(data)
        have_rmd <- requireNamespace("RMediation", quietly = TRUE)

        implist <- .medsim_md_implist(data, m = m, use_mi = use_mi)
        fitlist <- lapply(implist, function(d) .medsim_md_fit_ab(d, covs))
        a_est <- vapply(fitlist, function(z) z$a, numeric(1))
        b_est <- vapply(fitlist, function(z) z$b, numeric(1))
        a_hat <- mean(a_est)
        b_hat <- mean(b_est)
        va <- .medsim_rubin_var(a_est, vapply(fitlist, function(z) z$va, numeric(1)))
        vb <- .medsim_rubin_var(b_est, vapply(fitlist, function(z) z$vb, numeric(1)))
        ab <- a_hat * b_hat

        if (have_rmd) {
          ci <- tryCatch(
            getExportedValue("RMediation", "medci")(
              mu.x = a_hat, mu.y = b_hat,
              se.x = sqrt(va), se.y = sqrt(vb),
              type = "MC", alpha = 0.05
            ),
            error = function(e) NULL
          )
          if (!is.null(ci) && !is.null(ci[[1]])) {
            bounds <- as.numeric(ci[[1]])
            mc <- c(min(bounds), max(bounds))
          } else {
            mc <- .medsim_md_mc_ci(a_hat, b_hat, va, vb, n_draw)
          }
        } else {
          mc <- .medsim_md_mc_ci(a_hat, b_hat, va, vb, n_draw)
        }

        se_ab <- sqrt(b_hat^2 * va + a_hat^2 * vb)
        pval <- 2 * stats::pnorm(-abs(ab / se_ab))

        list(
          indirect = ab, indirect_ci_lower = mc[1L], indirect_ci_upper = mc[2L],
          indirect_p = pval, branch_switch = NA, converged = 1
        )
      },
      error = function(e) NULL
    )
    if (!is.null(res)) out <- res
    out
  }
}

#' Thin IPW estimator adapter (robustness appendix)
#'
#' Inverse-probability-weighting comparator. Positioned as a robustness appendix, not a
#' co-equal main-results arm (decided 2026-06-11).
#'
#' @param model Mediation model spec.
#' @param ... Passed to the underlying weighting / fitting routines.
#' @return A `function(data, params)` returning the shared method contract (`branch_switch = NA`).
#' @export
medsim_method_ipw <- function(model, ...) {
  function(data, params) {
    out <- list(
      indirect = NA_real_, indirect_ci_lower = NA_real_,
      indirect_ci_upper = NA_real_, indirect_p = NA_real_,
      branch_switch = NA, converged = 0
    )
    res <- tryCatch(
      {
        covs <- .medsim_md_covariates(data)

        ## Thin IPW: weight complete cases by inverse probability of being
        ## observed, modeling the missingness indicator on X (+ covariates).
        key <- intersect(c("M", "Y"), names(data))
        r <- stats::complete.cases(data[, key, drop = FALSE])

        pred <- intersect(c("X", covs), names(data))
        wts <- rep(1, nrow(data))
        if (length(pred) > 0 && any(!r) && any(r)) {
          rhs <- paste(pred, collapse = " + ")
          gfit <- stats::glm(stats::as.formula(paste("r ~", rhs)),
            data = data, family = stats::binomial()
          )
          phat <- stats::fitted(gfit)
          phat <- pmin(pmax(phat, 1e-3), 1 - 1e-3)
          wts <- 1 / phat
        }

        dcc <- data[r, , drop = FALSE]
        wcc <- wts[r]
        fit <- .medsim_md_fit_ab(dcc, covs, weights = wcc)
        ab <- fit$a * fit$b
        se_ab <- sqrt(fit$b^2 * fit$va + fit$a^2 * fit$vb)
        z975 <- stats::qnorm(0.975)
        pval <- 2 * stats::pnorm(-abs(ab / se_ab))

        list(
          indirect = ab, indirect_ci_lower = ab - z975 * se_ab,
          indirect_ci_upper = ab + z975 * se_ab, indirect_p = pval,
          branch_switch = NA, converged = 1
        )
      },
      error = function(e) NULL
    )
    if (!is.null(res)) out <- res
    out
  }
}

# ── Internal helpers ──────────────────────────────────────────────────────────

# Detect optional covariate columns (C, C1, C2, ...) in a DGM frame.
.medsim_md_covariates <- function(data) {
  nm <- names(data)
  grep("^C[0-9]*$", nm, value = TRUE)
}

# Build a list of analysis datasets: multiply-imputed via `mice` when requested
# and missingness is present, otherwise a single complete-case (listwise) frame.
.medsim_md_implist <- function(data, m = 20L, use_mi = FALSE) {
  if (use_mi && requireNamespace("mice", quietly = TRUE) && anyNA(data)) {
    imp <- tryCatch(
      suppressWarnings(mice::mice(data, m = m, method = "norm", printFlag = FALSE)),
      error = function(e) NULL
    )
    if (!is.null(imp)) {
      return(mice::complete(imp, "all"))
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
  list(
    a = unname(sm["X", "Estimate"]),
    b = unname(sy["M", "Estimate"]),
    va = unname(sm["X", "Std. Error"]^2),
    vb = unname(sy["M", "Std. Error"]^2)
  )
}

# Rubin's total variance: within-imputation mean + (1 + 1/K) between-imputation var.
.medsim_rubin_var <- function(est, within) {
  kk <- length(est)
  bvar <- if (kk > 1L) stats::var(est) else 0
  mean(within) + (1 + 1 / kk) * bvar
}

# Joint log-likelihood of the mediation model lm(M ~ X[+C]) + lm(Y ~ X + M[+C]),
# with optional dropped paths: drop_a => M ~ 1[+C]; drop_b => Y ~ X[+C].
.medsim_mbco_ll <- function(d, covs = character(0), drop_a = FALSE, drop_b = FALSE) {
  cterm <- if (length(covs)) paste("+", paste(covs, collapse = " + ")) else ""
  fm <- if (drop_a) {
    stats::as.formula(paste("M ~ 1", cterm))
  } else {
    stats::as.formula(paste("M ~ X", cterm))
  }
  fy <- if (drop_b) {
    stats::as.formula(paste("Y ~ X", cterm))
  } else {
    stats::as.formula(paste("Y ~ X + M", cterm))
  }
  as.numeric(stats::logLik(stats::lm(fm, d))) +
    as.numeric(stats::logLik(stats::lm(fy, d)))
}

# Complete-data MBCO union-null LRT: T = 2 (llF - max(ll_{a=0}, ll_{b=0})).
.medsim_mbco_T <- function(d, covs = character(0)) {
  ll_full <- .medsim_mbco_ll(d, covs)
  ll_null <- max(
    .medsim_mbco_ll(d, covs, drop_a = TRUE),
    .medsim_mbco_ll(d, covs, drop_b = TRUE)
  )
  2 * (ll_full - ll_null)
}

# Which union-null branch wins: 1 = a = 0 branch, 0 = b = 0 branch.
.medsim_mbco_branch <- function(d, covs = character(0)) {
  la <- .medsim_mbco_ll(d, covs, drop_a = TRUE)
  lb <- .medsim_mbco_ll(d, covs, drop_b = TRUE)
  if (la >= lb) 1 else 0
}

# D4 pooled test (Reiter 2007 / Chan & Meng 2022 / Grund et al. 2021) from the
# per-imputation LRTs `d_k` and the stacked-data LRT `d_S`; `k` = # constraints.
.medsim_d4_from_stats <- function(d_k, d_S, k = 1) {
  kk <- length(d_k)
  dbar <- mean(d_k)
  r4 <- max(0, (kk + 1) / (k * (kk - 1)) * (dbar - d_S))
  d4 <- d_S / (k * (1 + r4))
  km1 <- k * (kk - 1)
  nu <- if (km1 > 4) {
    4 + (km1 - 4) * (1 + (1 - 2 / km1) / r4)^2
  } else {
    0.5 * km1 * (1 + 1 / k) * (1 + 1 / r4)^2
  }
  c(D4 = d4, p = stats::pf(d4, k, nu, lower.tail = FALSE), r4 = r4, nu = nu)
}

# D4-pooled MBCO p-value across an imputation list (requires K >= 2).
.medsim_d4_mbco <- function(implist, covs = character(0)) {
  kk <- length(implist)
  d_k <- vapply(implist, function(d) .medsim_mbco_T(d, covs), 0)
  stacked <- do.call(rbind, implist)
  d_S <- .medsim_mbco_T(stacked, covs) / kk
  .medsim_d4_from_stats(d_k, d_S, k = 1)
}

# Base-R Monte-Carlo CI for the product a*b given (a,b) ~ independent normals.
.medsim_md_mc_ci <- function(a, b, va, vb, n_draw = 20000L) {
  ad <- stats::rnorm(n_draw, a, sqrt(va))
  bd <- stats::rnorm(n_draw, b, sqrt(vb))
  unname(stats::quantile(ad * bd, c(0.025, 0.975), names = FALSE))
}
