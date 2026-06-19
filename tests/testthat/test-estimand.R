test_that("medsim_estimand() constructs with defaults", {
  e <- medsim_estimand()
  expect_s3_class(e, "medsim_estimand")
  expect_equal(e$kind, "point")
  expect_equal(e$ci, "standard")
  expect_length(e$params, 0L)
  expect_length(e$extra, 0L)
  expect_null(e$truth)
})

test_that("medsim_estimand() validates kind arg", {
  expect_error(medsim_estimand("bad_kind"), "should be one of")
})

test_that("medsim_estimand() validates params must be character", {
  expect_error(medsim_estimand("point", params = 1L), "character vector")
})

test_that("medsim_estimand() validates truth must be function or NULL", {
  expect_error(medsim_estimand("point", truth = "not_a_fn"), "function")
  # NULL is fine
  expect_no_error(medsim_estimand("point", truth = NULL))
  # Function is fine
  expect_no_error(medsim_estimand("interval", truth = function(s) c(NDE = 0.1, NIE = 0.2)))
})

test_that("medsim_estimand() all four kinds round-trip", {
  for (k in c("point", "interval", "probabilistic", "numeric")) {
    e <- medsim_estimand(k)
    expect_equal(e$kind, k)
  }
})

test_that("print.medsim_estimand() runs without error", {
  e <- medsim_estimand("interval",
    params = c("NDE", "NIE"),
    ci     = "imbens_manski",
    extra  = c("feasible", "falsified"))
  expect_output(print(e), "interval")
  expect_output(print(e), "NDE")
})

test_that("medsim_scenario() accepts estimand = NULL (back-compat)", {
  sc <- medsim_scenario(
    name = "test",
    data_generator = function(n) data.frame(X = rnorm(n), M = rnorm(n), Y = rnorm(n)),
    estimand = NULL
  )
  expect_null(sc$estimand)
})

test_that("medsim_scenario() stores a medsim_estimand object", {
  e <- medsim_estimand("probabilistic", params = "pmed", ci = "mbco")
  sc <- medsim_scenario(
    name = "pmed_test",
    data_generator = function(n) data.frame(X = rnorm(n), M = rnorm(n), Y = rnorm(n)),
    estimand = e
  )
  expect_s3_class(sc$estimand, "medsim_estimand")
  expect_equal(sc$estimand$kind, "probabilistic")
})

test_that("medsim_scenario() rejects non-estimand objects", {
  expect_error(
    medsim_scenario(
      name = "bad",
      data_generator = function(n) data.frame(X = rnorm(n), M = rnorm(n), Y = rnorm(n)),
      estimand = list(kind = "point")   # raw list, not medsim_estimand
    ),
    "medsim_estimand"
  )
})

test_that("medsim_validate_scenario() still enforces X/M/Y for NULL estimand", {
  sc <- medsim_scenario(
    name = "no_xmy",
    data_generator = function(n) data.frame(A = rnorm(n)),
    estimand = NULL
  )
  expect_error(medsim_validate_scenario(sc), "missing")
})

test_that("medsim_validate_scenario() still enforces X/M/Y for point kind", {
  sc <- medsim_scenario(
    name = "point_no_xmy",
    data_generator = function(n) data.frame(A = rnorm(n)),
    estimand = medsim_estimand("point")
  )
  expect_error(medsim_validate_scenario(sc), "missing")
})

test_that("medsim_validate_scenario() skips X/M/Y check for numeric kind", {
  sc <- medsim_scenario(
    name = "numeric_no_xmy",
    data_generator = function(n) data.frame(error = runif(n), elapsed_sec = runif(n)),
    estimand = medsim_estimand("numeric", params = "error", ci = "none")
  )
  # Should validate without error — numeric kind has no X/M/Y requirement
  expect_true(medsim_validate_scenario(sc))
})

test_that("medsim_validate_scenario() skips X/M/Y check for interval kind", {
  sc <- medsim_scenario(
    name = "bounds_no_xmy",
    data_generator = function(n) {
      data.frame(
        NDE_lower = runif(n, -1, 0), NDE_upper = runif(n, 0, 1),
        NIE_lower = runif(n, -1, 0), NIE_upper = runif(n, 0, 1)
      )
    },
    estimand = medsim_estimand("interval", params = c("NDE", "NIE"),
                               ci = "imbens_manski", extra = c("feasible", "falsified"))
  )
  expect_true(medsim_validate_scenario(sc))
})

test_that(".medsim_estimand_kind() returns 'point' for NULL estimand", {
  sc <- medsim_scenario(
    name = "null_est",
    data_generator = function(n) data.frame(X = rnorm(n), M = rnorm(n), Y = rnorm(n))
  )
  expect_equal(medsim:::.medsim_estimand_kind(sc), "point")
})
