# INTERFACE FREEZE (Step 0) — signature stub for WS-A.
# Spec: SPEC-medsim-missingdata-generators-2026-06-11.md  (§"1. Nonnormality generator")

#' Draw nonnormal values with a target marginal skew/kurtosis (WS-A)
#'
#' Fleishman / Vale–Maurelli power-method draws. Pure base R; no hard dependency.
#' Warns/errors on (skew, kurtosis) outside the Fleishman-feasible region.
#'
#' @param n Integer sample size.
#' @param mean,sd Target marginal mean and SD (applied after standardizing).
#' @param skew Target marginal skewness (3rd standardized moment).
#' @param kurtosis Target marginal excess kurtosis (4th standardized moment − 3).
#' @return Numeric vector of length `n` with the requested marginal moments.
#' @export
medsim_rnonnormal <- function(n, mean = 0, sd = 1, skew = 0, kurtosis = 0) {
  if (length(n) != 1L || !is.finite(n) || n < 0) {
    stop("`n` must be a single non-negative number.", call. = FALSE)
  }
  n <- as.integer(n)

  coefs <- .medsim_fleishman_coef(skew, kurtosis)
  b <- coefs[["b"]]; cc <- coefs[["c"]]; d <- coefs[["d"]]; a <- -cc

  z <- stats::rnorm(n)
  y <- a + b * z + cc * z^2 + d * z^3

  # y already has (theoretical) mean 0 and variance 1 by construction; rescale to
  # the requested marginal mean / sd.
  mean + sd * y
}

# Solve the Fleishman cubic coefficients (b, c, d) for a target skewness and
# excess kurtosis. The standardized polynomial Y = -c + b*Z + c*Z^2 + d*Z^3 (Z
# standard normal) has, in expectation, mean 0 and variance 1, with skew and
# excess kurtosis given by Fleishman's (1978) moment equations. Solved by
# Newton-Raphson on the 3 nonlinear equations.
#
# Feasibility: not every (skew, kurtosis) pair is reachable. A necessary lower
# bound is kurtosis > skew^2 - 2 (the general moment bound); Fleishman's
# polynomial region is tighter still. We error if the solver fails to converge
# to a real solution.
.medsim_fleishman_coef <- function(skew, kurtosis) {
  if (length(skew) != 1L || length(kurtosis) != 1L ||
      !is.finite(skew) || !is.finite(kurtosis)) {
    stop("`skew` and `kurtosis` must each be a single finite number.",
         call. = FALSE)
  }

  # General moment lower bound on excess kurtosis given skew.
  if (kurtosis <= skew^2 - 2) {
    stop(sprintf(
      paste0("Infeasible (skew = %g, kurtosis = %g): excess kurtosis must ",
             "exceed skew^2 - 2 = %g."),
      skew, kurtosis, skew^2 - 2), call. = FALSE)
  }

  # Trivial normal case.
  if (skew == 0 && kurtosis == 0) {
    return(c(b = 1, c = 0, d = 0))
  }

  # Fleishman moment equations f(b, c, d) = 0:
  #   var:  b^2 + 6*b*d + 2*c^2 + 15*d^2 - 1 = 0
  #   skew: 2*c*(b^2 + 24*b*d + 105*d^2 + 2) - skew = 0
  #   kurt: 24*(b*d + c^2*(1 + b^2 + 28*b*d)
  #             + d^2*(12 + 48*b*d + 141*c^2 + 225*d^2)) - kurtosis = 0
  fn <- function(p) {
    b <- p[1]; cc <- p[2]; d <- p[3]
    c(
      b^2 + 6 * b * d + 2 * cc^2 + 15 * d^2 - 1,
      2 * cc * (b^2 + 24 * b * d + 105 * d^2 + 2) - skew,
      24 * (b * d + cc^2 * (1 + b^2 + 28 * b * d) +
              d^2 * (12 + 48 * b * d + 141 * cc^2 + 225 * d^2)) - kurtosis
    )
  }

  # Analytic Jacobian (rows = equations, cols = b, c, d).
  jac <- function(p) {
    b <- p[1]; cc <- p[2]; d <- p[3]
    matrix(c(
      # d/db,                d/dc,                            d/dd
      2 * b + 6 * d,         4 * cc,                          6 * b + 30 * d,
      2 * cc * (2 * b + 24 * d),
        2 * (b^2 + 24 * b * d + 105 * d^2 + 2),
        2 * cc * (24 * b + 210 * d),
      24 * (d + cc^2 * (2 * b + 28 * d) + d^2 * (48 * d)),
        24 * (2 * cc * (1 + b^2 + 28 * b * d) + d^2 * (282 * cc)),
        24 * (b + cc^2 * 28 * b +
                2 * d * (12 + 48 * b * d + 141 * cc^2 + 225 * d^2) +
                d^2 * (48 * b + 450 * d))
    ), nrow = 3L, byrow = TRUE)
  }

  # Newton-Raphson from Fleishman's standard starting values.
  p <- c(1.0, 0.0, 0.0)
  converged <- FALSE
  for (iter in seq_len(200L)) {
    fval <- fn(p)
    if (max(abs(fval)) < 1e-10) {
      converged <- TRUE
      break
    }
    J <- jac(p)
    step <- tryCatch(solve(J, fval), error = function(e) NULL)
    if (is.null(step) || any(!is.finite(step))) break
    # Damped step for stability.
    p_new <- p - step
    # Backtrack if the step overshoots (objective grows).
    lambda <- 1
    while (lambda > 1e-4 &&
           max(abs(fn(p - lambda * step))) > max(abs(fval))) {
      lambda <- lambda / 2
    }
    p <- p - lambda * step
    if (any(!is.finite(p))) break
  }

  if (!converged || any(!is.finite(p))) {
    stop(sprintf(
      paste0("Could not solve Fleishman coefficients for (skew = %g, ",
             "kurtosis = %g); the pair is likely outside the Fleishman ",
             "power-method region."),
      skew, kurtosis), call. = FALSE)
  }

  c(b = p[1], c = p[2], d = p[3])
}
