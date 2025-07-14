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

test_that("fit_measures computes correct metrics for perfect and shifted series", {
  # Create two series with identical year-over-year growth via scaling
  base <- ts(101:124, frequency = 12)
  shifted <- base * 2
  res <- spadjustr::fit_measures(base, shifted)
  expect_equal(res$mae, 0)
  expect_equal(res$mde, 0)
  expect_equal(res$cor, 1)
})

test_that("fit_measures computes expected metrics for a simple mismatched example", {
  actual <- ts(c(100, 110, 120, 130, 140, 150), frequency = 1)
  predicted <- ts(c(100,  90, 130, 120, 160, 140), frequency = 1)
  # compute relative year-over-year changes
  a <- (actual - stats::lag(actual, -1)) / stats::lag(actual, -1)
  p <- (predicted - stats::lag(predicted, -1)) / stats::lag(predicted, -1)
  valid <- stats::complete.cases(a, p)
  a <- as.numeric(a[valid]); p <- as.numeric(p[valid])
  exp_mae <- mean(abs(p - a))
  exp_mde <- mean(sign(p) != sign(a))
  exp_cor <- stats::cor(a, p)
  res2 <- spadjustr::fit_measures(actual, predicted, lag = 1)
  expect_equal(res2$mae, exp_mae)
  expect_equal(res2$mde, exp_mde)
  expect_equal(res2$cor, exp_cor)
})
