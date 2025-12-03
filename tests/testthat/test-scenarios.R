# Tests for scenario functions

test_that("medsim_scenarios_mediation returns list", {
  scenarios <- medsim_scenarios_mediation()

  expect_type(scenarios, "list")
  expect_true(length(scenarios) > 0)
})

test_that("all standard scenarios are valid", {
  scenarios <- medsim_scenarios_mediation()

  for (scenario in scenarios) {
    expect_s3_class(scenario, "medsim_scenario")
    expect_true("name" %in% names(scenario))
    expect_true("data_generator" %in% names(scenario))
    expect_true("params" %in% names(scenario))

    expect_type(scenario$name, "character")
    expect_type(scenario$data_generator, "closure")
    expect_type(scenario$params, "list")
  }
})

test_that("standard scenarios have expected names", {
  scenarios <- medsim_scenarios_mediation()
  names <- sapply(scenarios, function(s) s$name)

  expect_true("Independent" %in% names)
  expect_true("Moderate Correlation" %in% names)
  expect_true("High Correlation" %in% names)
  expect_true("Suppression" %in% names)
  expect_true("Non-zero Effects" %in% names)
  expect_true("Unequal Variances" %in% names)

  expect_equal(length(scenarios), 6)
})

test_that("scenario data generators work", {
  scenarios <- medsim_scenarios_mediation()

  for (scenario in scenarios) {
    data <- scenario$data_generator(n = 50)

    expect_s3_class(data, "data.frame")
    expect_equal(nrow(data), 50)
    expect_true("X" %in% names(data))
    expect_true("M" %in% names(data))
    expect_true("Y" %in% names(data))
  }
})

test_that("medsim_scenario creates valid scenario", {
  scenario <- medsim_scenario(
    name = "Test Scenario",
    description = "A test",
    data_generator = function(n) {
      data.frame(X = rnorm(n), M = rnorm(n), Y = rnorm(n))
    },
    params = list(a = 0.3, b = 0.5)
  )

  expect_s3_class(scenario, "medsim_scenario")
  expect_equal(scenario$name, "Test Scenario")
  expect_equal(scenario$description, "A test")
  expect_type(scenario$data_generator, "closure")
  expect_equal(scenario$params$a, 0.3)
  expect_equal(scenario$params$b, 0.5)
})

test_that("medsim_scenario validates inputs", {
  expect_error(
    medsim_scenario(
      name = 123,  # Invalid - should be character
      data_generator = function(n) data.frame(X = 1:n),
      params = list()
    ),
    "name must be a single character string"
  )

  expect_error(
    medsim_scenario(
      name = "Test",
      data_generator = "not a function",  # Invalid
      params = list()
    ),
    "data_generator must be a function"
  )

  expect_error(
    medsim_scenario(
      name = "Test",
      data_generator = function(n) data.frame(X = 1:n),
      params = "not a list"  # Invalid
    ),
    "params must be a list"
  )
})

test_that("print.medsim_scenario doesn't error", {
  scenario <- medsim_scenarios_mediation()[[1]]

  expect_output(print(scenario), "Scenario")
  expect_output(print(scenario), scenario$name)
})

test_that("medsim_validate_scenario works for valid scenario", {
  scenario <- medsim_scenarios_mediation()[[1]]

  expect_message(
    medsim_validate_scenario(scenario),
    "is valid"
  )

  expect_true(medsim_validate_scenario(scenario))
})

test_that("medsim_validate_scenario catches missing columns", {
  bad_scenario <- medsim_scenario(
    name = "Bad",
    data_generator = function(n) {
      data.frame(X = rnorm(n))  # Missing M and Y
    },
    params = list()
  )

  expect_error(
    medsim_validate_scenario(bad_scenario),
    "missing"
  )
})

test_that("medsim_validate_scenario catches wrong row count", {
  bad_scenario <- medsim_scenario(
    name = "Bad",
    data_generator = function(n) {
      data.frame(X = rnorm(5), M = rnorm(5), Y = rnorm(5))  # Always 5 rows
    },
    params = list()
  )

  expect_error(
    medsim_validate_scenario(bad_scenario, n = 10),
    "returned 5 rows, expected 10"
  )
})

test_that("medsim_validate_scenario catches non-data.frame", {
  bad_scenario <- medsim_scenario(
    name = "Bad",
    data_generator = function(n) {
      list(X = rnorm(n), M = rnorm(n), Y = rnorm(n))  # Not a data.frame
    },
    params = list()
  )

  expect_error(
    medsim_validate_scenario(bad_scenario),
    "must return a data.frame"
  )
})
