# central dispatcher
decomp_series <- function(ts_obj, method) {
  switch(method,
    ma12    = ma_decomp(ts_obj, n = 12),
    ma6     = ma_decomp(ts_obj, n = 6),
    x13     = x13_decomp(ts_obj),
    hp      = hp_filter_decomp(ts_obj),
    stl     = stl_decomp(ts_obj),
    loess   = loess_decomp(ts_obj),
    prophet = prophet_decomp(ts_obj),
    stop("Unknown decomposition method: ", method)
  )
}

ma_decomp <- function(ts_obj, n) {
  trend <- stats::filter(ts_obj, rep(1 / n, n), sides = 1)
  seasonal <- ts_obj - trend
  idio <- ts_obj - trend - seasonal
  list(trend = trend, seasonal = seasonal, idiosyncratic = idio)
}

x13_decomp <- function(ts_obj) {
  if (!requireNamespace("seasonal", quietly = TRUE)) {
    stop("Please install the 'seasonal' package")
  }
  m <- seasonal::seas(ts_obj)
  list(
    trend = seasonal::final(m),
    seasonal = seasonal::seasonal(m),
    idiosyncratic = residuals(m)
  )
}

hp_filter_decomp <- function(ts_obj, lambda = 1600) {
  if (!requireNamespace("mFilter", quietly = TRUE)) {
    stop("Please install the 'mFilter' package")
  }
  # one-sided HP filter: reverse series, apply symmetric filter, then reverse back
  rev_vec <- rev(as.numeric(ts_obj))
  rev_start <- end(ts_obj)
  ts_rev <- ts(rev_vec, start = rev_start, frequency = frequency(ts_obj))
  hp_rev <- mFilter::hpfilter(ts_rev, freq = lambda)
  trend_rev <- as.numeric(hp_rev$trend)
  trend_one <- ts(rev(trend_rev), start = start(ts_obj), frequency = frequency(ts_obj))
  idio <- ts_obj - trend_one
  list(
    trend = trend_one,
    seasonal = ts(0, start = start(ts_obj), frequency = frequency(ts_obj)),
    idiosyncratic = idio
  )
}

stl_decomp <- function(ts_obj, s.window = "periodic") {
  fit <- stats::stl(ts_obj, s.window = s.window, robust = TRUE)
  list(
    trend = fit$time.series[, "trend"],
    seasonal = fit$time.series[, "seasonal"],
    idiosyncratic = fit$time.series[, "remainder"]
  )
}

loess_decomp <- function(ts_obj, span = 0.2) {
  t <- seq_along(ts_obj)
  fit_lo <- stats::loess(as.numeric(ts_obj) ~ t, span = span)
  trend <- ts(fitted(fit_lo), start = start(ts_obj), freq = frequency(ts_obj))
  seasonal <- ts_obj - trend
  idio <- ts_obj - trend - seasonal
  list(trend = trend, seasonal = seasonal, idiosyncratic = idio)
}

prophet_decomp <- function(ts_obj) {
  if (!requireNamespace("prophet", quietly = TRUE)) {
    stop("Please install the 'prophet' package")
  }
  # Convert ts time index (fractional years) to Date via zoo::as.yearmon
  df <- data.frame(
    # convert fractional-year index to Date (first of month)
    ds = zoo::as.Date(zoo::as.yearmon(time(ts_obj)), frac = 0),
    y = as.numeric(ts_obj)
  )
  m <- prophet::prophet(df, verbose = FALSE)
  # use S3 dispatch for predict, since predict() is not exported from prophet namespace
  fc <- suppressWarnings(predict(m, df))
  list(
    trend = ts(fc$trend, start = start(ts_obj), freq = frequency(ts_obj)),
    seasonal = ts(fc$seasonal, start = start(ts_obj), freq = frequency(ts_obj)),
    idiosyncratic = ts(fc$yhat - fc$trend - fc$seasonal,
      start = start(ts_obj), freq = frequency(ts_obj)
    )
  )
}
