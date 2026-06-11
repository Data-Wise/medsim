# WS-A tests — medsim_rnonnormal()
# Spec: SPEC-medsim-missingdata-generators-2026-06-11.md (test matrix, row A)
# RED until WS-A fills the stub body.

test_that("recovers target skew and (excess) kurtosis within Monte Carlo tolerance", {
  cells <- list(
    list(skew = 0,   kurt = 0),
    list(skew = 1.5, kurt = 3),
    list(skew = 0,   kurt = 6)
  )
  for (cell in cells) {
    x <- medsim_rnonnormal(n = 1e5, skew = cell$skew, kurtosis = cell$kurt)
    g1 <- mean((x - mean(x))^3) / sd(x)^3
    g2 <- mean((x - mean(x))^4) / sd(x)^4 - 3
    expect_equal(g1, cell$skew, tolerance = 0.1)
    expect_equal(g2, cell$kurt, tolerance = 0.4)
  }
})

test_that("preserves requested mean and sd", {
  x <- medsim_rnonnormal(n = 1e5, mean = 5, sd = 2, skew = 1, kurtosis = 2)
  expect_equal(mean(x), 5, tolerance = 0.05)
  expect_equal(sd(x), 2, tolerance = 0.05)
})

test_that("errors on an infeasible (skew, kurtosis) combination", {
  # Fleishman feasibility requires kurtosis > skew^2 - 2 (roughly); pick a clear violation.
  expect_error(medsim_rnonnormal(n = 100, skew = 3, kurtosis = -1))
})

test_that("returns a numeric vector of the requested length", {
  x <- medsim_rnonnormal(n = 50)
  expect_type(x, "double")
  expect_length(x, 50)
})

test_that("validates n and (skew, kurtosis) arguments", {
  expect_error(medsim_rnonnormal(n = c(1, 2)), "single non-negative")
  expect_error(medsim_rnonnormal(n = -5), "single non-negative")
  expect_error(medsim_rnonnormal(n = 10, skew = c(1, 2)), "single finite")
  expect_error(medsim_rnonnormal(n = 10, kurtosis = NA), "single finite")
})

test_that("n = 0 yields an empty numeric vector", {
  expect_length(medsim_rnonnormal(n = 0, skew = 1, kurtosis = 3), 0)
})
