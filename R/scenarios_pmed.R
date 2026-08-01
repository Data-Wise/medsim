# P_med scenario factory -- probabilistic estimand kind
# P_med = P(Y1 > Y0) + 0.5 * P(Y1 == Y0), exact under the all-Gaussian SEM.

#' Create a P_med simulation scenario
#'
#' @description
#' Thin wrapper over [medsim_scenario()] that bakes in:
#' - A linear structural equation model (SEM) data generator for the
#'   `A -> M -> Y` causal chain.
#' - An `estimand = medsim_estimand("probabilistic", params = "pmed",
#'   ci = "mbco", extra = "branch_switch")` descriptor so that
#'   [medsim_analyze_coverage()] dispatches the MBCO-CI coverage branch.
#' - An exact closed-form ground-truth P_med: under the all-Gaussian linear
#'   SEM the cross-world difference `Y1 - Y0` is Normal, so
#'   `P_med = Phi((beta_ay + alpha_ax * beta_my) /
#'   sqrt(2 * (beta_my^2 * sigma_m^2 + sigma_y^2)))`.
#'
#' The estimand `P_med = P(Y_a=1(M_a=1) > Y_a=0(M_a=1)) +
#' 0.5 * P(Y_a=1(M_a=1) == Y_a=0(M_a=1))` uses the cross-world assumption --
#' it is a *probabilistic* mediation effect, not a difference in expectations.
#'
#' @param name Character: scenario name passed to [medsim_scenario()].
#' @param true_params Named list with entries:
#'   - `alpha_ax`: path coefficient A -> M (default 0.5)
#'   - `beta_my`: path coefficient M -> Y (default 0.5)
#'   - `beta_ay`: direct path A -> Y (default 0.0; set 0 for perfect mediation)
#'   - `sigma_m`: residual SD for M (default 1.0)
#'   - `sigma_y`: residual SD for Y (default 1.0)
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
                                  true_params = list()) {

  # Defaults for the SEM coefficients
  tp <- list(alpha_ax = 0.5, beta_my = 0.5, beta_ay = 0.0,
             sigma_m  = 1.0, sigma_y  = 1.0)
  tp[names(true_params)] <- true_params

  # Exact ground-truth P_med at construction time (closed form -- no Monte
  # Carlo draw, no RNG use).  The truth is a constant for a given SEM, not a
  # per-rep quantity.
  truth_val <- .medsim_pmed_truth(tp)

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

# -- Internal helpers -------------------------------------------------------

# Exact ground-truth P_med via cross-world potential outcomes under the SEM.
# P_med = P(Y1 > Y0) where Y1 = Y(A=1, M(1)) and Y0 = Y(A=0, M(0)).
# Each person's potential outcomes carry INDEPENDENT residuals --
# the standard "random-effects" cross-world assumption in the P_med literature.
# Using SHARED residuals would make Y1 - Y0 = constant, giving P_med in {0,1}.
#
# Under the all-Gaussian linear SEM this is analytically tractable:
#   M(0) = eps_m0,                    M(1) = alpha_ax + eps_m1
#   Y1   = beta_ay + beta_my * M(1) + eps_y1
#   Y0   =           beta_my * M(0) + eps_y0
#   Y1 - Y0 ~ N(beta_ay + alpha_ax * beta_my,
#               2 * (beta_my^2 * sigma_m^2 + sigma_y^2))
# so (Y continuous => P(Y1 == Y0) = 0):
#   P_med = Phi((beta_ay + alpha_ax * beta_my) /
#               sqrt(2 * (beta_my^2 * sigma_m^2 + sigma_y^2)))
# Note the direct effect beta_ay does NOT cancel: it enters Y1 but not Y0
# under this total-effect PO contrast (it vanishes at the beta_ay = 0 default).
.medsim_pmed_truth <- function(tp) {
  stats::pnorm(
    (tp$beta_ay + tp$alpha_ax * tp$beta_my) /
      sqrt(2 * (tp$beta_my^2 * tp$sigma_m^2 + tp$sigma_y^2))
  )
}
