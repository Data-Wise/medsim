# WS-D tests — medsim_method_mbco_mi() / medsim_method_mc_ci() / medsim_method_ipw()
# Spec: SPEC-medsim-missingdata-generators-2026-06-11.md (test matrix, row D)
#
# Graceful-degrade design: the adapters MUST run without missingmed/rmediation
# (documented base-R fallback). So the contract tests exercise that fallback path
# UNCONDITIONALLY (no skip) — it is the code that ships and runs today. The
# heavy-dependency path is additionally smoke-checked only when those siblings
# happen to be installed (validated in full by the #3 estimator-wiring follow-up).

contract_fields <- c(
  "indirect", "indirect_ci_lower", "indirect_ci_upper",
  "indirect_p", "branch_switch", "converged"
)

model <- "
  M ~ a*X
  Y ~ b*M + cp*X
  ab := a*b
"

toy <- function(n = 300) {
  X <- rnorm(n)
  M <- 0.4 * X + rnorm(n)
  Y <- 0.3 * X + 0.4 * M + rnorm(n)
  d <- data.frame(X = X, M = M, Y = Y)
  d$M[sample(n, n * 0.2)] <- NA
  d
}

expect_contract <- function(out) {
  expect_true(all(contract_fields %in% names(out)))
  expect_true(is.numeric(out$indirect))
  expect_true(out$converged %in% c(0, 1))
  # CI bounds are populated (and ordered) only when the estimator converged;
  # a degenerate failure returns the NA-filled contract with converged = 0.
  if (isTRUE(out$converged == 1)) {
    expect_false(is.na(out$indirect_ci_lower))
    expect_true(out$indirect_ci_lower <= out$indirect_ci_upper)
  }
}

test_that("adapters are function factories returning a function(data, params)", {
  expect_type(medsim_method_mbco_mi(model), "closure")
  expect_type(medsim_method_mc_ci(model), "closure")
  expect_type(medsim_method_ipw(model), "closure")
})

test_that("MBCO-MI adapter runs the base-R fallback and returns the contract", {
  set.seed(1)
  out <- medsim_method_mbco_mi(model, m = 5)(toy(), list())
  expect_contract(out)
  expect_true(out$branch_switch %in% c(0, 1))
  expect_equal(out$converged, 1)
})

test_that("MC-CI adapter runs the fallback and sets branch_switch to NA", {
  set.seed(2)
  out <- medsim_method_mc_ci(model, m = 5)(toy(), list())
  expect_contract(out)
  expect_true(is.na(out$branch_switch))
})

test_that("IPW adapter runs the fallback and sets branch_switch to NA", {
  set.seed(3)
  out <- medsim_method_ipw(model)(toy(), list())
  expect_contract(out)
  expect_true(is.na(out$branch_switch))
})

test_that("adapters auto-detect covariate (C*) columns and keep the contract", {
  set.seed(4)
  d <- toy()
  d$C1 <- rnorm(nrow(d))
  out <- medsim_method_mbco_mi(model, m = 3)(d, list())
  expect_contract(out)
})

test_that("an estimator failure degrades to converged = 0 rather than erroring", {
  # Degenerate input (no complete cases) -> lm cannot fit -> tryCatch path
  # returns the NA-filled contract with converged = 0.
  out <- medsim_method_ipw(model)(
    data.frame(X = c(1, 2), M = c(NA_real_, NA_real_), Y = c(1, 2)),
    list()
  )
  expect_contract(out)
  expect_equal(out$converged, 0)
})

test_that("MBCO-MI uses the heavy-dependency path when it is available", {
  skip_if_not_installed("missingmed")
  skip_if_not_installed("rmediation")
  set.seed(5)
  out <- medsim_method_mbco_mi(model, m = 5)(toy(), list())
  expect_contract(out)
  expect_true(out$branch_switch %in% c(0, 1))
})
