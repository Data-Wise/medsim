test_that("p-value precision distinguishes small values and still flags repeats", {
  ids <- seq_len(40L)
  df <- data.frame(scenario = "precision", replication = ids, p = ids * 1e-17)
  attr(df, "medsim_schema") <- 2L
  original <- df
  expect_error(medsim_audit_results(df), "collapse")
  expect_silent(medsim_audit_results(df, collapse_p_columns = "p"))
  expect_identical(df, original)
  df$p <- rep_len(c(0, 1e-17), length(ids))
  expect_error(medsim_audit_results(df, collapse_p_columns = "p"), "collapse")
})
