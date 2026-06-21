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
