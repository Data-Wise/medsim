# WS-B tests — medsim_amputate()
# Spec: SPEC-medsim-missingdata-generators-2026-06-11.md (test matrix, row B)
# RED until WS-B fills the stub body.

make_complete <- function(n = 4000) {
  X <- rnorm(n)
  M <- 0.4 * X + rnorm(n)
  Y <- 0.3 * X + 0.4 * M + rnorm(n)
  data.frame(X = X, M = M, Y = Y)
}

test_that("realized missingness rate is close to `prop` and only in `target`", {
  d <- make_complete()
  out <- medsim_amputate(d, target = "M", mechanism = "MCAR", prop = 0.25)
  expect_equal(mean(is.na(out$M)), 0.25, tolerance = 0.03)
  expect_false(any(is.na(out$X)))
  expect_false(any(is.na(out$Y)))
})

test_that("column names and order are preserved (DGM data contract)", {
  d <- make_complete(200)
  out <- medsim_amputate(d, target = "M", mechanism = "MCAR", prop = 0.2)
  expect_identical(names(out), names(d))
  expect_equal(nrow(out), nrow(d))
})

test_that("MCAR: missingness is independent of observed variables", {
  d <- make_complete(6000)
  out <- medsim_amputate(d, target = "M", mechanism = "MCAR", prop = 0.3)
  r <- as.integer(is.na(out$M))
  # X is fully observed; under MCAR, P(missing) should not depend on X.
  fit <- glm(r ~ d$X, family = binomial())
  expect_gt(summary(fit)$coefficients["d$X", "Pr(>|z|)"], 0.05)
})

test_that("MAR: missingness depends on an observed predictor", {
  d <- make_complete(6000)
  out <- medsim_amputate(d, target = "M", mechanism = "MAR", prop = 0.3, predictors = "X")
  r <- as.integer(is.na(out$M))
  fit <- glm(r ~ d$X, family = binomial())
  expect_lt(summary(fit)$coefficients["d$X", "Pr(>|z|)"], 0.05)
})

test_that("MNAR: missingness depends on the (latent) target value itself", {
  d <- make_complete(6000)
  out <- medsim_amputate(d, target = "M", mechanism = "MNAR", prop = 0.3)
  r <- as.integer(is.na(out$M))
  # Use the TRUE M (pre-amputation) — under MNAR-self, missingness tracks it.
  fit <- glm(r ~ d$M, family = binomial())
  expect_lt(summary(fit)$coefficients["d$M", "Pr(>|z|)"], 0.05)
})

test_that("supports multi-column targets (M and Y)", {
  d <- make_complete(2000)
  out <- medsim_amputate(d, target = c("M", "Y"), mechanism = "MCAR", prop = 0.2)
  expect_true(any(is.na(out$M)))
  expect_true(any(is.na(out$Y)))
  expect_false(any(is.na(out$X)))
})

test_that("input validation errors are raised", {
  d <- make_complete(50)
  expect_error(medsim_amputate(list(a = 1), target = "M"), "data.frame")
  expect_error(medsim_amputate(d), "at least one column")
  expect_error(medsim_amputate(d, target = "Z"), "not found")
  expect_error(medsim_amputate(d, target = "M", prop = 1.5), "\\[0, 1\\]")
  expect_error(medsim_amputate(d, target = "M", predictors = "Z"), "not found")
})

test_that("prop = 0 and zero-row input are structure-preserving no-ops", {
  d <- make_complete(100)
  out0 <- medsim_amputate(d, target = "M", prop = 0)
  expect_false(any(is.na(out0$M)))
  empty <- medsim_amputate(d[0, ], target = "M", prop = 0.2)
  expect_equal(nrow(empty), 0)
  expect_identical(names(empty), names(d))
})

test_that("MAR honors tail `type` and named `weights`", {
  d <- make_complete(4000)
  for (ty in c("LEFT", "MID", "TAIL")) {
    out <- medsim_amputate(d,
      target = "M", mechanism = "MAR", prop = 0.25,
      predictors = "X", type = ty
    )
    expect_equal(mean(is.na(out$M)), 0.25, tolerance = 0.04)
  }
  w <- medsim_amputate(d,
    target = "M", mechanism = "MAR", prop = 0.2,
    predictors = "X", weights = c(X = 2)
  )
  expect_equal(mean(is.na(w$M)), 0.2, tolerance = 0.04)
})

test_that("non-numeric (factor/logical) predictors are coerced for the model", {
  d <- make_complete(3000)
  d$G <- factor(sample(c("a", "b"), nrow(d), replace = TRUE))
  d$L <- d$X > 0
  out <- medsim_amputate(d,
    target = "M", mechanism = "MAR", prop = 0.2,
    predictors = c("G", "L")
  )
  expect_equal(mean(is.na(out$M)), 0.2, tolerance = 0.05)
  expect_identical(names(out), names(d))
})

test_that("MAR with no usable predictor falls back to a constant-rate draw", {
  d <- make_complete(3000)
  out <- medsim_amputate(d,
    target = "M", mechanism = "MAR", prop = 0.2,
    predictors = character(0)
  )
  expect_equal(mean(is.na(out$M)), 0.2, tolerance = 0.05)
})
