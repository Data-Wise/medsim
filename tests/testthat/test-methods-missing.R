# WS-D tests — medsim_method_mbco_mi() / medsim_method_mc_ci() / medsim_method_ipw()
# Spec: SPEC-medsim-missingdata-generators-2026-06-11.md (test matrix, row D)
# RED until WS-D fills the stubs.

contract_fields <- c("indirect", "indirect_ci_lower", "indirect_ci_upper",
                     "indirect_p", "branch_switch", "converged")

model <- "
  M ~ a*X
  Y ~ b*M + cp*X
  ab := a*b
"

toy <- function(n = 300) {
  X <- rnorm(n); M <- 0.4 * X + rnorm(n); Y <- 0.3 * X + 0.4 * M + rnorm(n)
  d <- data.frame(X = X, M = M, Y = Y)
  d$M[sample(n, n * 0.2)] <- NA
  d
}

test_that("adapters are function factories returning a function(data, params)", {
  expect_type(medsim_method_mbco_mi(model), "closure")
  expect_type(medsim_method_mc_ci(model), "closure")
  expect_type(medsim_method_ipw(model), "closure")
})

test_that("MBCO-MI adapter returns the full method() contract", {
  skip_if_not_installed("missingmed")
  skip_if_not_installed("rmediation")
  m <- medsim_method_mbco_mi(model, m = 5)
  out <- m(toy(), list())
  expect_true(all(contract_fields %in% names(out)))
  expect_true(out$branch_switch %in% c(0, 1))
  expect_true(out$indirect_ci_lower <= out$indirect_ci_upper)
})

test_that("MC-CI and IPW adapters set branch_switch to NA", {
  skip_if_not_installed("missingmed")
  skip_if_not_installed("rmediation")
  mc <- medsim_method_mc_ci(model, m = 5)(toy(), list())
  expect_true(is.na(mc$branch_switch))
  expect_true(all(contract_fields %in% names(mc)))
})
