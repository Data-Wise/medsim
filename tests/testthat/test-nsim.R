test_that("nsim_for_mcse inverts the MCSE formula", {
  expect_equal(medsim_nsim_for_mcse(0.005, 0.95), 1900)
  expect_equal(medsim_nsim_for_mcse(0.01, 0.5), 2500)
  expect_error(medsim_nsim_for_mcse(0))
})
