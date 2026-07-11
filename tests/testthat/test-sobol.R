# Tests for the Sobol / functional-ANOVA variance-share estimand
# Covers: medsim_estimand("variance_share"), medsim_scenario_sobol(),
#         medsim_validate_scenario() (A/M/Y/C branch), medsim_method_sobol(),
#         and a tiny end-to-end coverage run with a stubbed estimator.

# -- medsim_estimand("variance_share") --------------------------------------

test_that("medsim_estimand accepts the variance_share kind", {
  est <- medsim_estimand("variance_share", params = "pmed_sobol", ci = "standard")
  expect_s3_class(est, "medsim_estimand")
  expect_equal(est$kind, "variance_share")
  expect_equal(est$params, "pmed_sobol")
  expect_equal(est$ci, "standard")
})

# -- medsim_scenario_sobol --------------------------------------------------

test_that("medsim_scenario_sobol returns a scenario with variance_share kind", {
  sc <- medsim_scenario_sobol("test_sobol")
  expect_s3_class(sc, "medsim_scenario")
  expect_equal(sc$estimand$kind, "variance_share")
  expect_equal(sc$estimand$params, "pmed_sobol")
  expect_equal(sc$estimand$ci, "standard")
})

test_that("medsim_scenario_sobol stores a closed-form pmed_sobol truth in [0,1]", {
  sc <- medsim_scenario_sobol("test_sobol")
  expect_true("pmed_sobol" %in% names(sc$params))
  truth <- sc$params$pmed_sobol
  expect_true(is.numeric(truth) && length(truth) == 1L)
  expect_true(truth >= 0 && truth <= 1)
})

test_that("medsim_scenario_sobol truth reduces to NIE^2/(NIE^2+NDE^2) at kappa=0", {
  sc <- medsim_scenario_sobol(
    "no_interaction",
    true_params = list(beta_a = 0.6, tau_a = 0.5, tau_m = 0.7, kappa = 0.0))
  NIE <- 0.7 * 0.6; NDE <- 0.5
  ref <- NIE^2 / (NIE^2 + NDE^2)
  expect_equal(sc$params$pmed_sobol, ref, tolerance = 1e-10)
})

test_that("medsim_scenario_sobol respects true_params overrides", {
  sc <- medsim_scenario_sobol("custom",
                              true_params = list(beta_a = 0.9, kappa = 0.3))
  expect_equal(sc$params$beta_a, 0.9)
  expect_equal(sc$params$kappa, 0.3)
})

test_that("medsim_scenario_sobol data_generator returns A, M, Y, C", {
  sc <- medsim_scenario_sobol("test_sobol")
  dat <- sc$data_generator(40L)
  expect_s3_class(dat, "data.frame")
  expect_true(all(c("A", "M", "Y", "C") %in% names(dat)))
  expect_equal(nrow(dat), 40L)
})

# -- medsim_validate_scenario (variance_share branch) -----------------------

test_that("medsim_validate_scenario accepts a valid Sobol scenario", {
  sc <- medsim_scenario_sobol("test_sobol")
  expect_true(suppressMessages(medsim_validate_scenario(sc, n = 20)))
})

test_that("medsim_validate_scenario rejects a Sobol scenario missing C", {
  bad_gen <- function(n) {
    data.frame(A = rbinom(n, 1, 0.5), M = rnorm(n), Y = rnorm(n))
  }
  sc <- medsim_scenario(
    "bad_sobol", data_generator = bad_gen, params = list(),
    estimand = medsim_estimand("variance_share", params = "pmed_sobol",
                               ci = "standard"))
  expect_error(medsim_validate_scenario(sc, n = 20),
               "A, M, Y, C")
})

# -- medsim_method_sobol ----------------------------------------------------

# Trivial stub estimator returning known fields (does NOT require the external
# sobol_pmed.R prototype).
.stub_sobol <- function(d, covars = "C", K = 5L, level = 0.95, seed = 1L) {
  list(P_med_sobol = 0.40, ci = c(0.30, 0.50))
}

test_that("medsim_method_sobol maps estimator output to the flat contract", {
  dat <- medsim_scenario_sobol("s")$data_generator(50L)
  res <- medsim_method_sobol(dat, estimator = .stub_sobol)
  expect_named(res, c("pmed_sobol", "pmed_sobol_ci_lower", "pmed_sobol_ci_upper"),
               ignore.order = TRUE)
  expect_equal(res$pmed_sobol, 0.40)
  expect_equal(res$pmed_sobol_ci_lower, 0.30)
  expect_equal(res$pmed_sobol_ci_upper, 0.50)
})

test_that("medsim_method_sobol errors on missing columns", {
  bad <- data.frame(X = 1:10, M = rnorm(10), Y = rnorm(10))
  expect_error(medsim_method_sobol(bad, estimator = .stub_sobol),
               "columns A, M, Y, C")
})

test_that("medsim_method_sobol errors when no estimator is available", {
  dat <- medsim_scenario_sobol("s")$data_generator(20L)
  expect_error(medsim_method_sobol(dat, estimator = NULL),
               "estimator must be a function")
})

test_that("medsim_method_sobol errors on malformed estimator output", {
  dat <- medsim_scenario_sobol("s")$data_generator(20L)
  bad_est <- function(d, ...) list(P_med_sobol = 0.4)  # no ci
  expect_error(medsim_method_sobol(dat, estimator = bad_est),
               "P_med_sobol")
})

# -- End-to-end coverage run on synthetic numbers ---------------------------

test_that("medsim_analyze_coverage computes Sobol share coverage end-to-end", {
  # Hand-build a tiny medsim_results object so we don't depend on the external
  # estimator or a full medsim_run().  The generic Wald path keys off
  # `pmed_sobol_ci_lower`/`_upper` and matches truth via the merge collision
  # on `pmed_sobol`.
  truth_val <- 0.40
  set.seed(123)
  nrep <- 200L
  # Centers jitter around truth; CIs ~95% wide, so coverage ~0.95.
  centers <- truth_val + rnorm(nrep, 0, 0.03)
  half    <- 0.06  # +/- half-width chosen so ~95% contain truth
  results_df <- data.frame(
    scenario            = "sobol_cell",
    replication         = seq_len(nrep),
    pmed_sobol          = centers,
    pmed_sobol_ci_lower = centers - half,
    pmed_sobol_ci_upper = centers + half,
    stringsAsFactors    = FALSE
  )
  truth_df <- data.frame(scenario = "sobol_cell", pmed_sobol = truth_val,
                         stringsAsFactors = FALSE)
  results <- structure(list(results = results_df, truth = truth_df),
                       class = c("medsim_results", "list"))

  estimand <- medsim_estimand("variance_share", params = "pmed_sobol",
                              ci = "standard")
  cov <- medsim_analyze_coverage(results, estimand = estimand,
                                 by_scenario = TRUE)
  expect_s3_class(cov, "medsim_coverage")
  expect_true("pmed_sobol" %in% cov$coverage$parameter)
  cov_rate <- cov$coverage$coverage[cov$coverage$parameter == "pmed_sobol"]
  # With half-width 0.06 and SD 0.03 (~2 SD), coverage should be high.
  expect_true(cov_rate > 0.90 && cov_rate <= 1.0,
              info = sprintf("coverage = %.3f", cov_rate))
})
