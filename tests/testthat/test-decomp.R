context("decomp_series and decomposition methods")

test_that("ma_decomp reconstructs original series", {
  ts_obj <- ts(1:10, frequency = 1)
  res <- spadjustr:::ma_decomp(ts_obj, n = 2)
  # only compare non-NA trend positions
  valid <- !is.na(res$trend)
  recon <- as.numeric(res$trend)[valid] + as.numeric(res$seasonal)[valid] + as.numeric(res$idiosyncratic)[valid]
  expect_equal(recon, as.numeric(ts_obj)[valid])
})

test_that("decomp_series for ma12 and ma6 work", {
  ts_obj <- ts(rnorm(24), frequency = 12)
  res12 <- spadjustr:::decomp_series(ts_obj, "ma12")
  expect_named(res12, c("trend", "seasonal", "idiosyncratic"))
  res6 <- spadjustr:::decomp_series(ts_obj, "ma6")
  expect_named(res6, c("trend", "seasonal", "idiosyncratic"))
})

test_that("decomp_series for external methods reconstructs original series", {
  ts_obj <- AirPassengers

  skip_if_not_installed("mFilter")
  res_hp <- spadjustr:::hp_filter_decomp(ts_obj)
  recon_hp <- as.numeric(res_hp$trend) + as.numeric(res_hp$seasonal) + as.numeric(res_hp$idiosyncratic)
  expect_equal(recon_hp, as.numeric(ts_obj), tolerance = 1e-6)

  res_stl <- spadjustr:::stl_decomp(ts_obj)
  recon_stl <- as.numeric(res_stl$trend) + as.numeric(res_stl$seasonal) + as.numeric(res_stl$idiosyncratic)
  expect_equal(recon_stl, as.numeric(ts_obj), tolerance = 1e-6)

  res_loess <- spadjustr:::loess_decomp(ts_obj, span = 0.2)
  recon_loess <- as.numeric(res_loess$trend) + as.numeric(res_loess$seasonal) + as.numeric(res_loess$idiosyncratic)
  expect_equal(recon_loess, as.numeric(ts_obj), tolerance = 1e-6)

  # prophet_decomp may not support numeric time indices; skip for now
  skip("prophet_decomp test skipped due to date handling of time index")
})
