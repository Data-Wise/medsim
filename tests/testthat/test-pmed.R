# Tests for Phase 3 — P_med probabilistic estimand
# Covers: medsim_scenario_pmed(), medsim_method_pmed_mbco(),
#         .medsim_pmed_truth(), .medsim_pmed_boot()

# ── medsim_scenario_pmed ──────────────────────────────────────────────────

test_that("medsim_scenario_pmed returns a medsim_scenario object", {
  sc <- medsim_scenario_pmed("test_pmed")
  expect_s3_class(sc, "medsim_scenario")
})

test_that("medsim_scenario_pmed has estimand kind 'probabilistic'", {
  sc <- medsim_scenario_pmed("test_pmed")
  expect_equal(sc$estimand$kind, "probabilistic")
})

test_that("medsim_scenario_pmed estimand has pmed param and mbco ci", {
  sc <- medsim_scenario_pmed("test_pmed")
  expect_equal(sc$estimand$params, "pmed")
  expect_equal(sc$estimand$ci, "mbco")
  expect_true("branch_switch" %in% sc$estimand$extra)
})

test_that("medsim_scenario_pmed stores pmed truth in params", {
  sc <- medsim_scenario_pmed("test_pmed")
  expect_true("pmed" %in% names(sc$params))
  pmed_truth <- sc$params$pmed
  expect_true(is.numeric(pmed_truth))
  expect_true(pmed_truth >= 0 && pmed_truth <= 1)
})

test_that("medsim_scenario_pmed default SEM truth is exact (alpha=beta=0.5)", {
  # Under alpha=0.5, beta=0.5, direct=0, sigma_m=sigma_y=1:
  # P_med = Φ(0.25 / sqrt(2*(0.25+1))) — exact closed form, no MC error
  sc <- medsim_scenario_pmed("test_pmed")
  expect_equal(sc$params$pmed, pnorm(0.25 / sqrt(2 * 1.25)), tolerance = 1e-12)
})

test_that("medsim_scenario_pmed with no mediation (alpha=0, direct=0) has truth exactly 0.5", {
  sc <- medsim_scenario_pmed("no_med",
                              true_params = list(alpha_ax = 0.0, beta_my = 0.5))
  # When A has no effect on M and no direct effect, Y1 - Y0 is mean-zero
  # Normal → P_med = 0.5 exactly
  expect_equal(sc$params$pmed, 0.5, tolerance = 1e-12)
})

test_that("medsim_scenario_pmed truth is deterministic and leaves RNG untouched", {
  set.seed(123L)
  sc1 <- medsim_scenario_pmed("det_test")
  rng_after <- .Random.seed
  sc2 <- medsim_scenario_pmed("det_test")
  expect_identical(sc1$params$pmed, sc2$params$pmed)
  # Closed form must not consume RNG state
  expect_identical(rng_after, .Random.seed)
})

test_that(".medsim_pmed_truth includes the direct effect (beta_ay does not cancel)", {
  tp <- list(alpha_ax = 0.6, beta_my = 0.5, beta_ay = 0.3,
             sigma_m = 1.2, sigma_y = 0.8)
  expect_equal(
    medsim:::.medsim_pmed_truth(tp),
    pnorm((0.3 + 0.6 * 0.5) / sqrt(2 * (0.5^2 * 1.2^2 + 0.8^2))),
    tolerance = 1e-12
  )
})

test_that("medsim_scenario_pmed data_generator returns correct columns", {
  sc <- medsim_scenario_pmed("test_pmed")
  dat <- sc$data_generator(50L)
  expect_s3_class(dat, "data.frame")
  expect_true(all(c("A", "M", "Y") %in% names(dat)))
  expect_equal(nrow(dat), 50L)
})

test_that("medsim_scenario_pmed respects true_params overrides", {
  sc <- medsim_scenario_pmed("custom",
                              true_params = list(alpha_ax = 0.9, beta_my = 0.1))
  expect_equal(sc$params$alpha_ax, 0.9)
  expect_equal(sc$params$beta_my, 0.1)
})

# ── medsim_method_pmed_mbco ───────────────────────────────────────────────

make_pmed_data <- function(n = 200L, alpha = 0.5, beta = 0.5,
                            gamma = 0.0, seed = 42L) {
  set.seed(seed)
  a <- rbinom(n, 1L, 0.5)
  m <- alpha * a + rnorm(n)
  y <- gamma * a + beta  * m + rnorm(n)
  data.frame(A = a, M = m, Y = y)
}

test_that("medsim_method_pmed_mbco returns the 6-field contract", {
  dat <- make_pmed_data()
  res <- medsim_method_pmed_mbco(dat, params = list(), n_boot = 500L)
  expect_named(res, c("pmed", "pmed_ci_lower", "pmed_ci_upper",
                       "pmed_p", "branch_switch", "converged"),
               ignore.order = TRUE)
})

test_that("medsim_method_pmed_mbco pmed estimate is in [0,1]", {
  dat <- make_pmed_data()
  res <- medsim_method_pmed_mbco(dat, params = list(), n_boot = 500L)
  expect_true(res$pmed >= 0 && res$pmed <= 1)
})

test_that("medsim_method_pmed_mbco CI is valid (lower <= pmed <= upper)", {
  dat <- make_pmed_data()
  res <- medsim_method_pmed_mbco(dat, params = list(), n_boot = 500L)
  expect_true(res$pmed_ci_lower <= res$pmed)
  expect_true(res$pmed <= res$pmed_ci_upper)
  expect_true(res$pmed_ci_lower >= 0)
  expect_true(res$pmed_ci_upper <= 1)
})

test_that("medsim_method_pmed_mbco p-value is in [0,1]", {
  dat <- make_pmed_data()
  res <- medsim_method_pmed_mbco(dat, params = list(), n_boot = 500L)
  expect_true(res$pmed_p >= 0 && res$pmed_p <= 1)
})

test_that("medsim_method_pmed_mbco converged flag is 1", {
  dat <- make_pmed_data()
  res <- medsim_method_pmed_mbco(dat, params = list(), n_boot = 500L)
  expect_equal(res$converged, 1L)
})

test_that("medsim_method_pmed_mbco branch_switch is 0 for positive product", {
  # alpha=0.5, beta=0.5 → alpha*beta > 0 → unconstrained branch
  dat <- make_pmed_data(alpha = 0.5, beta = 0.5)
  res <- medsim_method_pmed_mbco(dat, params = list(), n_boot = 500L)
  expect_equal(res$branch_switch, 0L)
})

test_that("medsim_method_pmed_mbco branch_switch is 1 when alpha*beta <= 0", {
  # Negative alpha → alpha*beta < 0 → constrained branch
  dat <- make_pmed_data(alpha = -0.5, beta = 0.5)
  res <- medsim_method_pmed_mbco(dat, params = list(), n_boot = 500L)
  expect_equal(res$branch_switch, 1L)
})

test_that("medsim_method_pmed_mbco errors with missing columns", {
  bad <- data.frame(X = 1:10, Y = rnorm(10))
  expect_error(
    medsim_method_pmed_mbco(bad, params = list()),
    "columns A, M, Y"
  )
})

test_that("medsim_method_pmed_mbco pmed estimate near truth for large n", {
  # With n=1000 and alpha=beta=0.5 the OLS-based estimate should recover
  # a value within ±0.1 of the ground truth (~0.56)
  set.seed(7L)
  dat <- make_pmed_data(n = 1000L, alpha = 0.5, beta = 0.5, seed = 7L)
  res <- medsim_method_pmed_mbco(dat, params = list(), n_boot = 5000L)
  truth <- medsim:::.medsim_pmed_truth(
    list(alpha_ax = 0.5, beta_my = 0.5, beta_ay = 0.0,
         sigma_m  = 1.0, sigma_y  = 1.0)
  )
  expect_equal(res$pmed, truth, tolerance = 0.1)
})
