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

test_that("gauge method maps estimator output to flat fields (stub)", {
  stub <- function(data, ...) list(
    p_med = 0.4, p_med_ci = c(0.30, 0.50),
    W = 0.1,     W_ci = c(0.02, 0.18)
  )
  d <- data.frame(A = c(0,1,0,1), M = rnorm(4), Y = rnorm(4), C = rnorm(4))
  out <- medsim_method_gauge(d, params = list(), estimator = stub)
  expect_equal(out$pmed, 0.4)
  expect_equal(out$pmed_ci_lower, 0.30)
  expect_equal(out$pmed_ci_upper, 0.50)
  expect_equal(out$w, 0.1)
  expect_equal(out$w_ci_lower, 0.02)
  expect_equal(out$w_ci_upper, 0.18)
})

test_that("gauge method errors on missing columns and bad estimator", {
  d <- data.frame(A = 0:1, M = 0:1, Y = 0:1)  # no C
  expect_error(medsim_method_gauge(d, estimator = function(...) NULL),
               "A, M, Y, C")
  d2 <- data.frame(A = 0:1, M = 0:1, Y = 0:1, C = 0:1)
  expect_error(medsim_method_gauge(d2, estimator = "notafn"), "function")
})

.gauge_bootstrap_available <- function() {
  if (!requireNamespace("probmed", quietly = TRUE)) return(FALSE)
  fn <- tryCatch(get("ward_residual", envir = asNamespace("probmed")),
                 error = function(e) NULL)
  is.function(fn) || inherits(fn, "S7_generic")
}

test_that("bootstrap arm actually fires (not a silent se_method no-op)", {
  skip_if_not(.gauge_bootstrap_available(), "probmed ward_residual unavailable")
  set.seed(1)
  sc <- medsim_scenario_gauge("smoke", list(kappa = 0.3))
  d  <- sc$data_generator(400)
  est <- get("ward_residual", envir = asNamespace("probmed"))
  a <- medsim_method_gauge(d, estimator = est, se_method = "analytic",  K = 2L)
  b <- medsim_method_gauge(d, estimator = est, se_method = "bootstrap", K = 2L, B = 50L)
  wa <- a$pmed_ci_upper - a$pmed_ci_lower
  wb <- b$pmed_ci_upper - b$pmed_ci_lower
  # bootstrap percentile width must differ from analytic Wald width — proves
  # se_method was honored, not silently ignored (issue #24's headline risk).
  expect_false(isTRUE(all.equal(wa, wb)))
})
