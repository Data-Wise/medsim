# WS-D tests — medsim_method_mbco_mi() / medsim_method_mc_ci() / medsim_method_ipw()
# Spec: SPEC-medsim-missingdata-generators-2026-06-11.md (test matrix, row D)
#
# medsim_method_mbco_mi() ports the validated D4-stacked MBCO (mice MI + D4
# pooling + union-null LRT; prototype-d4-mbco.R, exact match vs mitml). With
# mice + missingness it runs D4; with complete data / no mice it degrades to the
# complete-case MBCO LRT with a chi-square reference (never a Sobel test). The
# contract tests run unconditionally; D4 pooling is validated against mitml.

contract_fields <- c(
  "indirect", "indirect_ci_lower", "indirect_ci_upper",
  "indirect_p", "branch_switch", "converged"
)

model <- "
  M ~ a*X
  Y ~ b*M + cp*X
  ab := a*b
"

toy <- function(n = 300, a = 0.4, b = 0.4) {
  X <- rnorm(n)
  M <- a * X + rnorm(n)
  Y <- 0.3 * X + b * M + rnorm(n)
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
    expect_true(out$indirect_p >= 0 && out$indirect_p <= 1)
  }
}

test_that("adapters are function factories returning a function(data, params)", {
  expect_type(medsim_method_mbco_mi(model), "closure")
  expect_type(medsim_method_mc_ci(model), "closure")
  expect_type(medsim_method_ipw(model), "closure")
})

test_that("MBCO-MI returns the contract and a {0,1} branch on MAR data", {
  set.seed(1)
  out <- medsim_method_mbco_mi(model, m = 5)(toy(), list())
  expect_contract(out)
  expect_true(out$branch_switch %in% c(0, 1))
  expect_equal(out$converged, 1)
})

test_that("MC-CI returns the contract and sets branch_switch to NA", {
  set.seed(2)
  out <- medsim_method_mc_ci(model, m = 5)(toy(), list())
  expect_contract(out)
  expect_true(is.na(out$branch_switch))
})

test_that("IPW returns the contract and sets branch_switch to NA", {
  set.seed(3)
  out <- medsim_method_ipw(model)(toy(), list())
  expect_contract(out)
  expect_true(is.na(out$branch_switch))
})

test_that("MBCO-MI degrades to the complete-case MBCO chi-square test (no missingness)", {
  set.seed(7)
  d <- toy()
  d <- d[stats::complete.cases(d), ] # complete data -> K = 1 -> chi-square path
  out <- medsim_method_mbco_mi(model, m = 5)(d, list())
  expect_contract(out)
  expect_equal(out$converged, 1)
})

test_that("adapters auto-detect covariate (C*) columns and keep the contract", {
  set.seed(4)
  d <- toy()
  d$C1 <- rnorm(nrow(d))
  out <- medsim_method_mbco_mi(model, m = 3)(d, list())
  expect_contract(out)
})

test_that("an estimator failure degrades to converged = 0 rather than erroring", {
  out <- medsim_method_ipw(model)(
    data.frame(X = c(1, 2), M = c(NA_real_, NA_real_), Y = c(1, 2)),
    list()
  )
  expect_contract(out)
  expect_equal(out$converged, 0)
})

test_that("MBCO-MI detects a clear indirect effect (high power)", {
  skip_if_not_installed("mice")
  set.seed(11)
  out <- medsim_method_mbco_mi(model, m = 10)(toy(400, a = 0.5, b = 0.5), list())
  expect_lt(out$indirect_p, 0.05)
  expect_gt(out$indirect, 0)
})

test_that("D4 pooling reproduces mitml::testModels(method = 'D4') [acceptance]", {
  skip_if_not_installed("mice")
  skip_if_not_installed("mitml")
  set.seed(20260611)
  n <- 200
  cc <- rnorm(n)
  x <- rbinom(n, 1, plogis(0.3 * cc))
  mm <- 0.39 * x + 0.3 * cc + rnorm(n)
  yy <- 0.2 * x + 0.39 * mm + 0.3 * cc + rnorm(n)
  d <- data.frame(X = x, M = mm, Y = yy, C = cc)
  d$M[runif(n) < plogis(qlogis(0.25) + 0.5 * x + 0.5 * cc)] <- NA
  d$Y[runif(n) < plogis(qlogis(0.25) + 0.5 * x + 0.5 * cc)] <- NA

  il <- mice::complete(
    suppressWarnings(mice::mice(d, m = 20, method = "norm", printFlag = FALSE)), "all"
  )
  lrt_lin <- function(z) {
    2 * (as.numeric(stats::logLik(stats::lm(Y ~ X + M + C, z))) -
      as.numeric(stats::logLik(stats::lm(Y ~ X + C, z))))
  }
  d_k <- vapply(il, lrt_lin, 0)
  d_s <- lrt_lin(do.call(rbind, il)) / length(il)
  mine <- medsim:::.medsim_d4_from_stats(d_k, d_s, k = 1)
  ref <- mitml::testModels(
    lapply(il, function(z) stats::lm(Y ~ X + M + C, z)),
    lapply(il, function(z) stats::lm(Y ~ X + C, z)),
    method = "D4"
  )
  expect_equal(unname(mine[["D4"]]), unname(ref$test[1, "F.value"]), tolerance = 1e-3)
  expect_equal(unname(mine[["p"]]), unname(ref$test[1, "P(>F)"]), tolerance = 1e-3)
})
