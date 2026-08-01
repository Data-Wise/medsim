# Tests for Phase 4 — numeric accuracy scenario factory

test_that("medsim_scenario_numeric returns medsim_scenario", {
  sc <- medsim_scenario_numeric("acc_test")
  expect_s3_class(sc, "medsim_scenario")
})

test_that("medsim_scenario_numeric estimand kind is 'numeric'", {
  sc <- medsim_scenario_numeric("acc_test")
  expect_equal(sc$estimand$kind, "numeric")
})

test_that("medsim_scenario_numeric estimand ci is 'none'", {
  sc <- medsim_scenario_numeric("acc_test")
  expect_equal(sc$estimand$ci, "none")
})

test_that("medsim_scenario_numeric extra cols always include est_error/abs_error/elapsed_sec", {
  sc <- medsim_scenario_numeric("acc_test")
  expect_true(all(c("est_error", "abs_error", "elapsed_sec") %in%
                    sc$estimand$extra))
  # `error` is reserved for the failure-row schema -- must NOT be mandated
  expect_false("error" %in% sc$estimand$extra)
})

test_that("medsim_scenario_numeric user extra cols are unioned in", {
  sc <- medsim_scenario_numeric("acc_test", extra = c("rel_error", "n_iter"))
  expect_true("rel_error" %in% sc$estimand$extra)
  expect_true("n_iter" %in% sc$estimand$extra)
  # Standard cols still present
  expect_true("abs_error" %in% sc$estimand$extra)
})

test_that("medsim_scenario_numeric true_params stored correctly", {
  tp <- list(a = 0.5, b = 0.3, ci_true = 0.95)
  sc <- medsim_scenario_numeric("p3", true_params = tp)
  expect_equal(sc$params$a, 0.5)
  expect_equal(sc$params$ci_true, 0.95)
})

test_that("medsim_scenario_numeric default data_generator returns data.frame", {
  sc <- medsim_scenario_numeric("acc_test")
  dat <- sc$data_generator(10L)
  expect_s3_class(dat, "data.frame")
  expect_equal(nrow(dat), 10L)
})

test_that("medsim_scenario_numeric accepts custom data_generator", {
  gen <- function(n) data.frame(x = rnorm(n), y = rnorm(n))
  sc  <- medsim_scenario_numeric("custom_gen", data_generator = gen)
  dat <- sc$data_generator(5L)
  expect_s3_class(dat, "data.frame")
  expect_equal(nrow(dat), 5L)
  expect_true("x" %in% names(dat))
})

test_that("medsim_scenario_numeric errors on non-list true_params", {
  expect_error(
    medsim_scenario_numeric("bad", true_params = "not_a_list"),
    "true_params"
  )
})

test_that("medsim_scenario_numeric errors on non-function data_generator", {
  expect_error(
    medsim_scenario_numeric("bad", data_generator = 42L),
    "data_generator"
  )
})

test_that("medsim_scenario_numeric estimand has no params (character(0))", {
  sc <- medsim_scenario_numeric("acc_test")
  expect_equal(sc$estimand$params, character(0L))
})

# ---- Regression: documented numeric contract must be runnable ---------------
# v0.5.0 declared `error` BOTH as the numeric kind's mandatory result column
# (here) AND as a reserved failure-schema field that medsim_run() rejects on a
# successful method() (runner.R) -- so the documented numeric contract was
# unusable as written. The mandatory column is now `est_error`.

test_that("numeric-kind method returning the documented columns runs end-to-end", {
  sc <- medsim_scenario_numeric("acc_run",
                                true_params = list(ci_true = 0.95))
  meth <- function(data, params) {
    est <- params$ci_true + 0.01
    list(est_error   = est - params$ci_true,
         abs_error   = abs(est - params$ci_true),
         elapsed_sec = 0.001)
  }
  cfg <- medsim_config("test", output_dir = withr::local_tempdir(),
                       n_cores = 1L)
  cfg$n_replications <- 3L

  res <- suppressWarnings(suppressMessages(
    medsim_run(meth, list(sc), cfg, parallel = FALSE, verbose = FALSE)
  ))

  expect_s3_class(res, "medsim_results")
  expect_equal(nrow(res$results), 3L)
  expect_true(all(c("est_error", "abs_error", "elapsed_sec") %in%
                    names(res$results)))
  # Reserved failure column is NA on every successful row
  expect_true(all(is.na(res$results$error)))
})
