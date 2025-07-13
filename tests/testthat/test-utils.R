context("utility functions")

test_that("stack_ts combines time series correctly", {
  ts1 <- ts(1:3, start = c(2000, 1), frequency = 1)
  ts2 <- ts(4:6, start = c(2000, 1), frequency = 1)
  df <- spadjustr:::stack_ts(list(a = ts1, b = ts2))
  expect_equal(nrow(df), 6)
  expect_true(all(c("time", "value", "series") %in% names(df)))
  expect_equal(df$series, c(rep("a", 3), rep("b", 3)))
  expect_equal(df$value, c(1:3, 4:6))
})
