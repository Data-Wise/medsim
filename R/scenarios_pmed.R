# P_med scenario factory — probabilistic estimand kind
# P_med = P(Y1 > Y0) + 0.5 * P(Y1 == Y0), computed via cross-world PO draw.

#' Create a P_med simulation scenario
#'
#' @description
#' Thin wrapper over [medsim_scenario()] that bakes in:
#' - A linear structural equation model (SEM) data generator for the
#'   `A → M → Y` causal chain.
#' - An `estimand = medsim_estimand("probabilistic", params = "pmed",
#'   ci = "mbco", extra = "branch_switch")` descriptor so that
#'   [medsim_analyze_coverage()] dispatches the MBCO-CI coverage branch.
#' - A `truth` function that draws potential outcomes under the SEM to
#'   compute the ground-truth P_med (not analytically tractable in general).
#'
#' The estimand `P_med = P(Y_a=1(M_a=1) > Y_a=0(M_a=1)) +
#' 0.5 * P(Y_a=1(M_a=1) == Y_a=0(M_a=1))` uses the cross-world assumption —
#' it is a *probabilistic* mediation effect, not a difference in expectations.
#'
#' @param name Character: scenario name passed to [medsim_scenario()].
#' @param true_params Named list with entries:
#'   - `alpha_ax`: path coefficient A → M (default 0.5)
#'   - `beta_my`: path coefficient M → Y (default 0.5)
#'   - `beta_ay`: direct path A → Y (default 0.0; set 0 for perfect mediation)
#'   - `sigma_m`: residual SD for M (default 1.0)
#'   - `sigma_y`: residual SD for Y (default 1.0)
#' @param n_po Integer: number of potential-outcome draws for truth estimation
#'   (default 50000).  Larger = more accurate ground truth.
#'
#' @return A `medsim_scenario` object with `estimand$kind = "probabilistic"`.
#'
#' @examples
#' sc <- medsim_scenario_pmed(
#'   name        = "full_mediation",
#'   true_params = list(alpha_ax = 0.6, beta_my = 0.5, beta_ay = 0.0)
#' )
#' str(sc$estimand)
#'
#' @seealso [medsim_method_pmed_mbco()], [medsim_analyze_coverage()]
#'
#' @export
medsim_scenario_pmed <- function(name,
                                  true_params = list(),
                                  n_po        = 50000L) {

  # Defaults for the SEM coefficients
  tp <- list(alpha_ax = 0.5, beta_my = 0.5, beta_ay = 0.0,
             sigma_m  = 1.0, sigma_y  = 1.0)
  tp[names(true_params)] <- true_params

  # Compute ground-truth P_med via a large PO draw at construction time.
  # The truth is a constant for a given SEM, not a per-rep quantity.
  truth_val <- .medsim_pmed_truth(tp, n_po = n_po)

  # Observed-data generator: linear SEM
  gen_fn <- local({
    p <- tp
    function(n) {
      a <- rbinom(n, 1L, 0.5)
      m <- p$alpha_ax * a + rnorm(n, 0, p$sigma_m)
      y <- p$beta_ay  * a + p$beta_my * m + rnorm(n, 0, p$sigma_y)
      data.frame(A = a, M = m, Y = y)
    }
  })

  estimand <- medsim_estimand("probabilistic",
                               params = "pmed",
                               ci     = "mbco",
                               extra  = "branch_switch")

  sc <- medsim_scenario(
    name            = name,
    description     = sprintf("P_med scenario (alpha=%.2f, beta=%.2f, direct=%.2f)",
                               tp$alpha_ax, tp$beta_my, tp$beta_ay),
    data_generator  = gen_fn,
    params          = c(tp, list(pmed = truth_val)),
    estimand        = estimand
  )
  sc
}

# ── Internal helpers ───────────────────────────────────────────────────────

# Compute ground-truth P_med via cross-world potential outcomes under the SEM.
# P_med = P(Y1 > Y0) where Y1 = Y(A=1, M(1)) and Y0 = Y(A=0, M(0)).
# Each person's potential outcomes are drawn with INDEPENDENT residuals —
# the standard "random-effects" cross-world assumption in the P_med literature.
# Using SHARED residuals would make Y1 - Y0 = constant, giving P_med ∈ {0,1}.
.medsim_pmed_truth <- function(tp, n_po = 50000L) {
  n <- n_po
  # Independent residuals for each potential outcome world
  eps_m0 <- rnorm(n, 0, tp$sigma_m)
  eps_m1 <- rnorm(n, 0, tp$sigma_m)
  eps_y0 <- rnorm(n, 0, tp$sigma_y)
  eps_y1 <- rnorm(n, 0, tp$sigma_y)

  m0 <- tp$alpha_ax * 0 + eps_m0
  m1 <- tp$alpha_ax * 1 + eps_m1
  y1 <- tp$beta_ay * 1 + tp$beta_my * m1 + eps_y1
  y0 <- tp$beta_ay * 0 + tp$beta_my * m0 + eps_y0

  # For continuous Y, P(Y1 == Y0) ≈ 0; P_med ≈ P(Y1 > Y0)
  # Analytic: Phi(alpha*beta / sqrt(2*(beta^2*sigma_m^2 + sigma_y^2)))
  mean(y1 > y0)
}
