# API ergonomics for 0.6.0 (docs/specs/GRILL-medsim-api-ergonomics-2026-10-14.md):
# D1 medsim_config(n =, n_truth =); D3 medsim_analyze_performance() on a
# medsim_results object; D4 one summary warning from medsim_analyze().

api_scenarios <- function() {
  gen <- function(a) function(n) {
    X <- rnorm(n); M <- a * X + rnorm(n); Y <- 0.39 * M + rnorm(n)
    data.frame(X = X, M = M, Y = Y)
  }
  list(
    medsim_scenario("api_medium", data_generator = gen(0.39),
                    params = list(indirect = 0.39 * 0.39)),
    medsim_scenario("api_null", data_generator = gen(0),
                    params = list(indirect = 0))
  )
}
api_sobel <- function(data, params) {
  fm <- summary(lm(M ~ X, data = data))$coefficients
  fy <- summary(lm(Y ~ X + M, data = data))$coefficients
  a <- fm["X", 1]; sa <- fm["X", 2]; b <- fy["M", 1]; sb <- fy["M", 2]
  se <- sqrt(b^2 * sa^2 + a^2 * sb^2)
  list(indirect = a * b, indirect_se = se,
       indirect_ci_lower = a * b - 1.96 * se, indirect_ci_upper = a * b + 1.96 * se,
       indirect_p = 2 * stats::pnorm(-abs(a * b / se)))
}
api_truth <- function(data, params) list(indirect = params$indirect)
api_run <- function(method = api_sobel, ...) {
  cfg <- medsim_config("test", output_dir = withr::local_tempdir(.local_envir = parent.frame()),
                       ...)
  suppressMessages(medsim_run(method, api_scenarios(), cfg, compute_truth = api_truth,
                              parallel = FALSE, verbose = FALSE))
}

# ---- D1: n and n_truth ------------------------------------------------------

test_that("medsim_config(n =) sets the sample size every generator receives", {
  nobs <- function(data, params) list(indirect = nrow(data))
  res <- api_run(nobs, n = 50)
  expect_true(all(res$results$indirect == 50))
  expect_identical(res$config$n, 50L)
})

test_that("without n the config is unchanged and the runner uses 200", {
  cfg <- medsim_config("test")
  expect_false("n" %in% names(cfg))
  expect_false("n_truth" %in% names(cfg))
  nobs <- function(data, params) list(indirect = nrow(data))
  expect_true(all(api_run(nobs)$results$indirect == 200))
})

test_that("n and n_truth are exact-match only and validated", {
  # positional and partial-matching calls still reach the earlier formals
  expect_identical(medsim_config("local", 100)$n_replications, 100)
  expect_identical(medsim_config("local", n_rep = 30)$n_replications, 30)
  expect_identical(medsim_config("test", n = 75, n_truth = 5000)$n_truth, 5000L)
  for (bad in list(0, -5, 2.5, NA, c(10, 20), "100")) {
    expect_error(medsim_config("test", n = bad), "single positive whole number")
  }
  expect_error(medsim_config("test", n_truth = 0), "`n_truth`")
})

test_that("n_truth sets the size of the truth draw", {
  sizes <- integer()
  truth_n <- function(data, params) {
    sizes <<- c(sizes, nrow(data))
    list(indirect = params$indirect)
  }
  cfg <- medsim_config("test", output_dir = withr::local_tempdir(), n_truth = 1234)
  suppressMessages(medsim_run(api_sobel, api_scenarios()[1], cfg,
                              compute_truth = truth_n, parallel = FALSE, verbose = FALSE))
  expect_identical(unique(sizes), 1234L)
})

test_that("print shows the sample size, and n is not listed as a custom parameter", {
  out <- capture.output(print(medsim_config("test", n = 60)))
  expect_true(any(grepl("Sample size (n):     60", out, fixed = TRUE)))
  expect_true(any(grepl("200 (default)", capture.output(print(medsim_config("test"))),
                        fixed = TRUE)))
})

# ---- D3: performance on a medsim_results object -----------------------------

test_that("medsim_analyze_performance accepts a medsim_results object", {
  res <- api_run()
  tr <- res$truth
  names(tr)[-1] <- paste0(names(tr)[-1], "_truth")
  by_hand <- medsim_analyze_performance(merge(res$results, tr, by = "scenario"))
  direct  <- medsim_analyze_performance(res)
  expect_equal(direct, by_hand)
  expect_equal(nrow(direct), 2L)
  expect_false(anyNA(direct$model_se))
})

test_that("medsim_analyze_performance explains a missing truth", {
  res <- api_run()
  expect_error(medsim_analyze_performance(res, param = "indirect_se"),
               "no truth for 'indirect_se'")
  res$truth <- NULL
  expect_error(medsim_analyze_performance(res), "Pass compute_truth")
})

# ---- D4: one summary warning ------------------------------------------------

test_that("medsim_analyze is silent about SE, CI and p-value companions", {
  res <- api_run()
  expect_no_warning(an <- medsim_analyze(res))
  expect_identical(an$accuracy$parameter, "indirect")
})

test_that("medsim_analyze reports other truthless columns in one warning", {
  extra <- function(data, params) {
    c(api_sobel(data, params), list(slope_y = 1, branch_flag = 0))
  }
  res <- api_run(extra)
  w <- character()
  withCallingHandlers(medsim_analyze(res), warning = function(cnd) {
    w <<- c(w, conditionMessage(cnd))
    invokeRestart("muffleWarning")
  })
  expect_length(w, 1L)
  expect_match(w, "No ground truth for 2 output column(s)", fixed = TRUE)
  expect_match(w, "'slope_y', 'branch_flag'", fixed = TRUE)
  expect_false(grepl("indirect_se", w, fixed = TRUE))
})

test_that("a companion suffix is reported when its base has no truth", {
  orphan <- function(data, params) {
    c(api_sobel(data, params), list(orphan_se = 0.1))
  }
  res <- api_run(orphan)
  expect_warning(medsim_analyze(res), "'orphan_se'")
})
