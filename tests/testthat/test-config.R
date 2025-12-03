# Tests for configuration functions

test_that("medsim_config creates valid configuration", {
  config <- medsim_config("test")

  expect_s3_class(config, "medsim_config")
  expect_s3_class(config, "list")

  # Check required fields
  expect_true("mode" %in% names(config))
  expect_true("name" %in% names(config))
  expect_true("n_replications" %in% names(config))
  expect_true("n_cores" %in% names(config))
  expect_true("seed" %in% names(config))

  # Check types
  expect_type(config$mode, "character")
  expect_type(config$n_replications, "double")
  expect_type(config$n_cores, "double")
  expect_type(config$seed, "double")
})

test_that("medsim_config test mode has correct defaults", {
  config <- medsim_config("test")

  expect_equal(config$mode, "test")
  expect_equal(config$n_replications, 20)
  expect_equal(config$n_cores, 4)
  expect_equal(config$scenarios, "test")
})

test_that("medsim_config local mode has correct defaults", {
  config <- medsim_config("local")

  expect_equal(config$mode, "local")
  expect_equal(config$n_replications, 100)
  expect_equal(config$scenarios, "all")
})

test_that("medsim_config cluster mode has correct defaults", {
  config <- medsim_config("cluster")

  expect_equal(config$mode, "cluster")
  expect_equal(config$n_replications, 1000)
  expect_equal(config$scenarios, "all")
})

test_that("medsim_config allows custom parameters", {
  config <- medsim_config(
    "test",
    n_replications = 50,
    custom_param = "value"
  )

  expect_equal(config$n_replications, 50)
  expect_equal(config$custom_param, "value")
})

test_that("medsim_config rejects invalid mode", {
  expect_error(
    medsim_config("invalid_mode"),
    "Unknown mode"
  )
})

test_that("medsim_detect_environment returns character", {
  env <- medsim_detect_environment()

  expect_type(env, "character")
  expect_true(env %in% c("local", "cluster"))
})

test_that("medsim_detect_cores returns positive integer", {
  n_cores <- medsim_detect_cores()

  expect_type(n_cores, "double")
  expect_true(n_cores >= 1)
  expect_true(n_cores <= parallel::detectCores())
})

test_that("print.medsim_config doesn't error", {
  config <- medsim_config("test")

  expect_output(print(config), "TEST MODE")
  expect_output(print(config), "Replications")
})

test_that("medsim_compare_configs returns data.frame", {
  comparison <- medsim_compare_configs()

  expect_s3_class(comparison, "data.frame")
  expect_true(nrow(comparison) > 0)
  expect_true("Parameter" %in% names(comparison))
  expect_true("Test" %in% names(comparison))
  expect_true("Local" %in% names(comparison))
  expect_true("Cluster" %in% names(comparison))
})
