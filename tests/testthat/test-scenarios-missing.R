# WS-C tests — medsim_scenario_missing() / medsim_scenario_missing_grid()
# Spec: SPEC-medsim-missingdata-generators-2026-06-11.md (test matrix, row C)
# RED until WS-C fills the stubs.

tp <- list(a = 0.4, b = 0.4, cp = 0.2, sd_M = 1, sd_Y = 1)

test_that("returns a valid medsim_scenario", {
  s <- medsim_scenario_missing("MCAR_M", true_params = tp, mechanism = "MCAR", prop = 0.2)
  expect_s3_class(s, "medsim_scenario")
  expect_true(isTRUE(medsim_validate_scenario(s, n = 50)))
})

test_that("generator returns X/M/Y with NAs only in the target column", {
  s <- medsim_scenario_missing("MCAR_M", true_params = tp, mechanism = "MCAR",
                               prop = 0.25, target = "M")
  d <- s$data_generator(2000)
  expect_setequal(c("X", "M", "Y"), names(d))
  expect_gt(mean(is.na(d$M)), 0)
  expect_false(any(is.na(d$X)))
  expect_false(any(is.na(d$Y)))
})

test_that("nonnormal flag is wired through to the residual draws", {
  s <- medsim_scenario_missing("MCAR_nn", true_params = tp, mechanism = "MCAR", prop = 0.2,
                               nonnormal = list(skew = 1.5, kurtosis = 4))
  d <- s$data_generator(1e5)
  # Y residual should be visibly skewed when nonnormal is requested.
  ry <- residuals(lm(Y ~ X + M, data = d[stats::complete.cases(d), ]))
  g1 <- mean((ry - mean(ry))^3) / sd(ry)^3
  expect_gt(abs(g1), 0.5)
})

test_that("grid expands the factorial into a list of scenarios", {
  grid <- medsim_scenario_missing_grid(
    true_params_list = list(small = tp),
    mechanisms = c("MCAR", "MAR", "MNAR"),
    props = c(0.1, 0.3)
  )
  expect_type(grid, "list")
  expect_length(grid, 1 * 3 * 2)
  expect_true(all(vapply(grid, inherits, logical(1), "medsim_scenario")))
})

test_that("medsim_scenario_missing validates its arguments", {
  expect_error(medsim_scenario_missing(c("a", "b"), tp, "MCAR"), "single character")
  expect_error(medsim_scenario_missing("s", true_params = 1, mechanism = "MCAR"), "must be a list")
  expect_error(medsim_scenario_missing("s", tp, mechanism = c("MCAR", "MAR")), "single character")
  expect_error(medsim_scenario_missing("s", tp, "MCAR", prop = 2), "\\[0, 1\\]")
  expect_error(medsim_scenario_missing("s", tp, "MCAR", target = character(0)), "non-empty")
  expect_error(medsim_scenario_missing("s", tp, "MCAR", nonnormal = list(skew = 1)), "kurtosis")
})

test_that("grid validates inputs and labels cells from list names", {
  expect_error(medsim_scenario_missing_grid(list(), "MCAR", 0.2), "non-empty list")
  expect_error(medsim_scenario_missing_grid(list(tp), character(0), 0.2), "character")
  expect_error(medsim_scenario_missing_grid(list(tp), "MCAR", numeric(0)), "numeric")
  expect_error(
    medsim_scenario_missing_grid(list(tp), "MCAR", 0.2, nonnormal_list = list()),
    "non-empty"
  )

  # Unnamed true_params_list -> integer-coded names; named nonnormal -> labeled cell.
  g <- medsim_scenario_missing_grid(
    true_params_list = list(tp),
    mechanisms = "MCAR", props = 0.2,
    nonnormal_list = list(normal = NULL, heavy = list(skew = 0, kurtosis = 4))
  )
  expect_length(g, 1 * 1 * 1 * 2)
  nms <- vapply(g, function(s) s$name, character(1))
  expect_true(any(grepl("_heavy$", nms)))
})
