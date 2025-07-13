context("TsDecomp class and methods")

test_that("ts_decomp initializes correctly and exposes components", {
  ts1 <- ts(rnorm(12), frequency = 12)
  ts2 <- ts(rnorm(12), frequency = 12)
  td <- ts_decomp(ts1, ts2, method = "ma12")
  expect_s3_class(td, "TsDecomp")
  expect_equal(td$method, "ma12")
  expect_named(td$decomposed_user, c("trend", "seasonal", "idiosyncratic"))
  expect_named(td$decomposed_benchmark, c("trend", "seasonal", "idiosyncratic"))
})

test_that("replace_trend swaps in benchmark trend correctly", {
  ts1 <- ts(1:10, frequency = 1)
  ts2 <- ts(11:20, frequency = 1)
  decom1 <- spadjustr:::ma_decomp(ts1, n = 2)
  decom2 <- spadjustr:::ma_decomp(ts2, n = 2)
  mod <- spadjustr:::replace_trend(decom1, decom2)
  expect_equal(start(mod), start(decom2$trend))
  expect_equal(frequency(mod), frequency(decom2$trend))
  expect_equal(as.numeric(mod - decom1$seasonal - decom1$idiosyncratic),
               as.numeric(decom2$trend))
})

test_that("summary prints expected output", {
  ts1 <- ts(1:12, frequency = 12)
  ts2 <- ts(2:13, frequency = 12)
  td <- ts_decomp(ts1, ts2, method = "ma12")
  expect_output(td$summary(), "Decomposition method:\\s*ma12")
  expect_output(td$summary(), "Length.*user")
  expect_output(td$summary(), "Trend correlation")
})

test_that("plot executes without error", {
  ts1 <- ts(rnorm(12), frequency = 12)
  ts2 <- ts(rnorm(12), frequency = 12)
  td <- ts_decomp(ts1, ts2, method = "ma12")
  expect_silent(td$plot())
})
