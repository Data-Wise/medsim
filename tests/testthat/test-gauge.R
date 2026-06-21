# tests/testthat/test-gauge.R
test_that("gauge truth matches the verified decomposition", {
  # kappa = 0: R = 0 => W = 0, P_med = NIE/(NDE+NIE)
  sc0 <- medsim_scenario_gauge("k0",
           list(beta_a = 0.6, tau_a = 0.5, tau_m = 0.7, kappa = 0))
  nie <- 0.7 * 0.6; nde <- 0.5
  expect_equal(sc0$params$pmed, nie / (nde + nie))
  expect_equal(sc0$params$w, 0)

  # kappa != 0: hand-compute from theta corners
  tp  <- list(beta_a = 0.6, tau_a = 0.5, tau_m = 0.7, kappa = 0.4)
  th  <- function(a, ap) tp$tau_a*a + (tp$tau_m + tp$kappa*a)*tp$beta_a*ap
  t11<-th(1,1); t10<-th(1,0); t01<-th(0,1); t00<-th(0,0)
  OE<-t11-t00; IDE<-t10-t00; IIE<-t01-t00; R<-OE-IDE-IIE
  sc1 <- medsim_scenario_gauge("k1", tp)
  expect_equal(sc1$params$pmed, IIE/OE)
  expect_equal(sc1$params$w,    R/OE)
  expect_equal(sc1$estimand$kind, "variance_share")
})
