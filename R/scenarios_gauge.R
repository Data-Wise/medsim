# R/scenarios_gauge.R
# Gauge-residual scenario factory -- reports P_med = IIE/OE and W = R/OE.
# Reuses the linear-Gaussian-with-interaction DGP and corner-mean algebra of
# scenarios_sobol.R; truth is exact for this DGP (no Monte-Carlo PO draw).

#' Create a gauge-residual (P_med + W) simulation scenario
#'
#' @description
#' Thin wrapper over [medsim_scenario()] mirroring [medsim_scenario_sobol()]:
#' an `A -> M -> Y` linear-Gaussian DGP with covariate `C` and `A*M`
#' interaction (`kappa`), a `variance_share` estimand, and **closed-form**
#' ground truth for the interventional proportion mediated `P_med = IIE/OE`
#' and the gauge residual `W = R/OE`, where `R = OE - IDE - IIE`.
#'
#' Corner means `theta_{a,a'} = tau_a*a + (tau_m + kappa*a)*beta_a*a'` (C
#' centered) give `OE = theta11-theta00`, `IDE = theta10-theta00`,
#' `IIE = theta01-theta00`. At `kappa = 0`, `R = 0` and `W = 0`.
#'
#' @param name Character scenario name.
#' @param true_params Named list; defaults `beta_a=0.6, tau_a=0.5, tau_m=0.7,
#'   kappa=0, gamma_mc=0.5, gamma_yc=0.4, p_a=0.5`.
#' @return A `medsim_scenario` with `estimand$kind = "variance_share"` and
#'   `params$pmed`, `params$w` closed-form truths.
#' @seealso [medsim_method_gauge()], [medsim_scenario_sobol()]
#' @examples
#' sc <- medsim_scenario_gauge("interaction", list(kappa = 0.4))
#' c(sc$params$pmed, sc$params$w)
#' @export
medsim_scenario_gauge <- function(name, true_params = list()) {
  tp <- list(beta_a = 0.6, tau_a = 0.5, tau_m = 0.7, kappa = 0.0,
             gamma_mc = 0.5, gamma_yc = 0.4, p_a = 0.5)
  tp[names(true_params)] <- true_params

  truth <- .medsim_gauge_truth(tp)

  gen_fn <- .medsim_lingauss_dgp(tp)

  estimand <- medsim_estimand("variance_share",
                              params = c("pmed", "w"),
                              ci     = "standard")

  medsim_scenario(
    name           = name,
    description    = sprintf(
      "Gauge scenario (beta_a=%.2f, tau_a=%.2f, tau_m=%.2f, kappa=%.2f)",
      tp$beta_a, tp$tau_a, tp$tau_m, tp$kappa),
    data_generator = gen_fn,
    params         = c(tp, list(pmed = unname(truth["pmed"]),
                                w    = unname(truth["w"]))),
    estimand       = estimand
  )
}

# Closed-form corner means -> OE/IDE/IIE/R -> (P_med, W).
# Corner means via .medsim_corner_means() (R/scenarios_sobol.R, shared DGP).
# Matches ward_residual()'s decomposition (probmed gauge-pmed.R lines 176-178).
#' @noRd
.medsim_gauge_truth <- function(tp) {
  tm  <- .medsim_corner_means(tp)
  t11 <- tm[["t11"]]; t10 <- tm[["t10"]]; t01 <- tm[["t01"]]; t00 <- tm[["t00"]]
  OE  <- t11 - t00; IDE <- t10 - t00; IIE <- t01 - t00; R <- OE - IDE - IIE
  if (abs(OE) < 1e-12) return(c(pmed = NA_real_, w = NA_real_))
  c(pmed = IIE / OE, w = R / OE)
}
