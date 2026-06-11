# WS-E tests — medsim_summarize_branch_switch()
# Spec: SPEC-medsim-missingdata-generators-2026-06-11.md (test matrix, row E)
# RED until WS-E fills the stub.

# Minimal fake medsim_results: a results data.frame with the columns WS-E reads.
fake_results <- function() {
  df <- data.frame(
    scenario      = rep(c("MCAR_M", "MNAR_M"), each = 4),
    replication   = rep(1:4, 2),
    branch_switch = c(0, 1, 0, 0,   1, 1, 0, NA),
    converged     = c(1, 1, 1, 0,   1, 1, 1, 1)
  )
  structure(list(results = df), class = c("medsim_results", "list"))
}

test_that("returns one row per scenario with a branch_switch_rate", {
  out <- medsim_summarize_branch_switch(fake_results())
  expect_s3_class(out, "data.frame")
  expect_setequal(out$scenario, c("MCAR_M", "MNAR_M"))
  expect_true(all(c("branch_switch_rate", "n_valid") %in% names(out)))
})

test_that("drops non-converged rows before summarizing", {
  out <- medsim_summarize_branch_switch(fake_results())
  # MCAR_M: 4 rows, 1 non-converged dropped -> n_valid = 3, rate = 1/3
  mcar <- out[out$scenario == "MCAR_M", ]
  expect_equal(mcar$n_valid, 3)
  expect_equal(mcar$branch_switch_rate, 1 / 3, tolerance = 1e-8)
})

test_that("errors on bad input or missing required columns", {
  expect_error(medsim_summarize_branch_switch(42), "data.frame or a medsim_results")
  bad <- data.frame(scenario = "a", converged = 1) # no branch_switch column
  expect_error(medsim_summarize_branch_switch(bad), "branch_switch")
})

test_that("accepts a bare results data.frame and a custom `by`", {
  df <- data.frame(
    grp = c("x", "x", "y"),
    branch_switch = c(1, 0, 1),
    converged = c(1, 1, 1)
  )
  out <- medsim_summarize_branch_switch(df, by = "grp")
  expect_setequal(out$grp, c("x", "y"))
  expect_equal(out$branch_switch_rate[out$grp == "y"], 1)
})
