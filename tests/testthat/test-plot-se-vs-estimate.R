test_that("se_vs_estimate returns a faceted ggplot", {
  skip_if_not_installed("ggplot2")
  res <- data.frame(
    scenario = rep(c("a", "b"), each = 3),
    indirect = c(0.1, 0.2, 0.15, 0.3, 0.25, 0.28),
    indirect_se = c(0.02, 0.03, 0.02, 0.05, 0.04, 0.06)
  )
  p <- medsim_plot_se_vs_estimate(res, param = "indirect", by = "scenario")
  expect_s3_class(p, "ggplot")
})
