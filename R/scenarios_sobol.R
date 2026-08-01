# Sobol / functional-ANOVA share scenario factory — variance_share estimand kind
# P_med^{sigma2} = V_med / V_T in [0,1], a bounded scalar with a Wald CI.

#' Create a Sobol / functional-ANOVA variance-share simulation scenario
#'
#' @description
#' Thin wrapper over [medsim_scenario()] that bakes in:
#' - A linear-Gaussian-with-interaction data generator for the
#'   `A -> M -> Y` causal chain with a single covariate `C` and an `A*M`
#'   interaction term (`kappa`).
#' - An `estimand = medsim_estimand("variance_share", params = "pmed_sobol",
#'   ci = "standard")` descriptor, so [medsim_analyze_coverage()] uses the
#'   generic Wald `truth %in% CI` path and [medsim_validate_scenario()] checks
#'   for the `A/M/Y/C` columns the Sobol estimator requires.
#' - A **closed-form** `truth` for the Sobol share, exact for the
#'   linear-Gaussian-with-interaction DGP (no Monte-Carlo PO draw needed).
#'
#' The estimand is the variance-scale proportion mediated
#' \eqn{P_{med}^{\sigma^2} = V_{med}/V_T}, where the functional-ANOVA variance
#' components are built from the four corner means
#' \eqn{\theta_{a,a'} = E[Y(a, M(a'))]}.  For the linear-Gaussian DGP with
#' centered `C`,
#' \deqn{\theta_{a,a'} = \tau_a a + (\tau_m + \kappa a)\,\beta_a a'.}
#' At `kappa = 0` the share reduces to \eqn{NIE^2/(NIE^2 + NDE^2)} with
#' \eqn{NIE = \tau_m \beta_a} and \eqn{NDE = \tau_a}.
#'
#' @param name Character: scenario name passed to [medsim_scenario()].
#' @param true_params Named list with entries (defaults in parentheses):
#'   - `beta_a`: path coefficient A -> M (0.6)
#'   - `tau_a`: direct path A -> Y (0.5)
#'   - `tau_m`: path coefficient M -> Y (0.7)
#'   - `kappa`: A*M interaction coefficient in the Y model (0.0)
#'   - `gamma_mc`: covariate effect C -> M (0.5)
#'   - `gamma_yc`: covariate effect C -> Y (0.4)
#'   - `p_a`: treatment-assignment probability used for the variance weights
#'     `pd = pm = p_a` (0.5)
#' @param pd,pm Bernoulli weights for the direct / mediator design variances in
#'   the functional-ANOVA decomposition (default `0.5`; override only for
#'   non-balanced designs).
#'
#' @return A `medsim_scenario` object with `estimand$kind = "variance_share"`
#'   and `params$pmed_sobol` holding the closed-form ground truth.
#'
#' @examples
#' sc <- medsim_scenario_sobol(
#'   name        = "interaction",
#'   true_params = list(beta_a = 0.6, tau_a = 0.5, tau_m = 0.7, kappa = 0.4)
#' )
#' sc$params$pmed_sobol   # closed-form Sobol share
#'
#' @seealso [medsim_method_sobol()], [medsim_analyze_coverage()],
#'   [medsim_estimand()]
#'
#' @export
medsim_scenario_sobol <- function(name,
                                   true_params = list(),
                                   pd = 0.5,
                                   pm = 0.5) {

  tp <- list(beta_a = 0.6, tau_a = 0.5, tau_m = 0.7, kappa = 0.0,
             gamma_mc = 0.5, gamma_yc = 0.4, p_a = 0.5)
  tp[names(true_params)] <- true_params

  # Closed-form ground-truth Sobol share for this DGP.
  truth_val <- .medsim_sobol_truth(tp, pd = pd, pm = pm)

  gen_fn <- .medsim_lingauss_dgp(tp)

  estimand <- medsim_estimand("variance_share",
                              params = "pmed_sobol",
                              ci     = "standard")

  medsim_scenario(
    name           = name,
    description    = sprintf(
      "Sobol share scenario (beta_a=%.2f, tau_a=%.2f, tau_m=%.2f, kappa=%.2f)",
      tp$beta_a, tp$tau_a, tp$tau_m, tp$kappa),
    data_generator = gen_fn,
    params         = c(tp, list(pmed_sobol = truth_val)),
    estimand       = estimand
  )
}

# Internal helpers -----------------------------------------------------------
# Shared by medsim_scenario_sobol() and medsim_scenario_gauge() (R/scenarios_gauge.R).

# Linear-Gaussian-with-interaction data generator for the A -> M -> Y chain
# with a single covariate C and an A*M interaction (kappa). Returns the
# `function(n)` closure used as a scenario `data_generator`.
#' @noRd
.medsim_lingauss_dgp <- function(tp) {
  p <- tp
  function(n) {
    C <- rnorm(n)
    A <- rbinom(n, 1L, p$p_a)
    M <- p$beta_a * A + p$gamma_mc * C + rnorm(n)
    Y <- p$tau_a * A + p$tau_m * M + p$kappa * A * M +
      p$gamma_yc * C + rnorm(n)
    data.frame(C = C, A = A, M = M, Y = Y)
  }
}

# Closed-form corner means for that DGP (C centered):
# theta_{a,a'} = tau_a*a + (tau_m + kappa*a) * beta_a * a'.
# Returns c(t11, t10, t01, t00).
#' @noRd
.medsim_corner_means <- function(tp) {
  th <- function(a, ap) tp$tau_a * a + (tp$tau_m + tp$kappa * a) * tp$beta_a * ap
  c(t11 = th(1, 1), t10 = th(1, 0), t01 = th(0, 1), t00 = th(0, 0))
}

# Closed-form corner means -> functional-ANOVA components -> Sobol share.
# Mirrors the Dd/Dm/R -> Vd/Vm/Vdm -> Vm/VT algebra of sobol_from_theta()
# in the prototype sobol_pmed.R, so the truth matches the estimator's target.
#' @noRd
.medsim_sobol_truth <- function(tp, pd = 0.5, pm = 0.5) {
  tm <- .medsim_corner_means(tp)
  t11 <- tm[["t11"]]; t10 <- tm[["t10"]]; t01 <- tm[["t01"]]; t00 <- tm[["t00"]]

  Dd <- (1 - pm) * (t10 - t00) + pm * (t11 - t01)
  Dm <- (1 - pd) * (t01 - t00) + pd * (t11 - t10)
  R  <- t11 - t10 - t01 + t00
  cd <- pd * (1 - pd); cm <- pm * (1 - pm)

  Vd  <- cd * Dd^2
  Vm  <- cm * Dm^2
  Vdm <- cd * cm * R^2
  VT  <- Vd + Vm + Vdm

  if (VT <= 0) return(NA_real_)
  unname(Vm / VT)
}
