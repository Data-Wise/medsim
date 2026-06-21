test_that("performance metrics match hand computation", {
  res <- data.frame(
    scenario = "s1",
    indirect = c(0.2, 0.4, 0.3),         # truth 0.3 -> bias 0
    indirect_se = c(0.1, 0.1, 0.1),
    indirect_truth = rep(0.3, 3)
  )
  perf <- medsim_analyze_performance(res, param = "indirect")
  expect_equal(perf$bias, 0)
  expect_equal(perf$empirical_se, sd(c(0.2, 0.4, 0.3)))
  expect_equal(perf$model_se, 0.1)
  expect_equal(perf$rmse, sqrt(mean((c(0.2,0.4,0.3) - 0.3)^2)))
  expect_equal(perf$n_converged, 3L)
})

test_that("all-NA estimates returns n_converged=0, n_failed=nrow, bias=NA (no NaN/Inf)", {
  res <- data.frame(
    scenario = "s_empty",
    indirect = c(NA_real_, NA_real_, NA_real_),
    indirect_truth = rep(0.3, 3)
  )
  perf <- medsim_analyze_performance(res, param = "indirect")
  expect_equal(perf$n_converged, 0L)
  expect_equal(perf$n_failed, 3L)
  expect_true(is.na(perf$bias))
  expect_true(is.na(perf$empirical_se))
  expect_true(is.na(perf$rmse))
  expect_false(is.nan(perf$bias))
  expect_false(is.infinite(perf$rmse))
})

test_that("NA truth sets bias/rmse/bias_mcse to NA but empirical_se still computes", {
  res <- data.frame(
    scenario = "s_natruth",
    indirect = c(0.2, 0.3, 0.4),
    indirect_truth = rep(NA_real_, 3)
  )
  perf <- medsim_analyze_performance(res, param = "indirect")
  expect_true(is.na(perf$bias))
  expect_true(is.na(perf$rmse))
  expect_true(is.na(perf$bias_mcse))
  expect_false(is.na(perf$empirical_se))
})

test_that("medsim_table_performance formats to 4 decimals and includes MCSE column", {
  res <- data.frame(
    scenario = "s1",
    indirect = c(0.2, 0.4, 0.3),
    indirect_se = c(0.1, 0.1, 0.1),
    indirect_truth = rep(0.3, 3)
  )
  perf <- medsim_analyze_performance(res, param = "indirect")
  tbl <- medsim_table_performance(perf)
  expect_true(is.list(tbl))
  expect_true(!is.null(tbl$latex))
  # Must contain a 4-decimal formatted number
  expect_true(grepl("[0-9]\\.[0-9]{4}", tbl$latex))
  # Must NOT contain full-precision strings (more than 6 sig figs after decimal)
  expect_false(grepl("[0-9]\\.[0-9]{7,}", tbl$latex))
  # Must contain MCSE header
  expect_true(grepl("MCSE", tbl$latex))
  # Column count in tabular spec: lrrrrrr (7 cols)
  expect_true(grepl("\\{lrrrrrr\\}", tbl$latex))
})
