#' TsDecomp: Dual-Series Decomposition R6 Class
#'
#' @import R6 ggplot2 gridExtra
#' @export
TsDecomp <- R6::R6Class("TsDecomp",
  public = list(
    user_ts = NULL,
    benchmark_ts = NULL,
    method = NULL,
    decomposed_user = NULL,
    decomposed_benchmark = NULL,
    modified_user = NULL,

    #' @description
    #' Create a new TsDecomp object.
    #' @param user_ts A `ts` object: the primary series.
    #' @param benchmark_ts A `ts` object: the benchmark series.
    #' @param method Character: one of `"ma12"`, `"ma6"`, `"x13"`, `"hp"`, `"stl"`, `"loess"`, `"prophet"`.
    #' @return A `TsDecomp` R6 object.
    initialize = function(user_ts, benchmark_ts, method = "ma12") {
      stopifnot(inherits(user_ts, "ts"), inherits(benchmark_ts, "ts"))
      self$user_ts <- user_ts
      self$benchmark_ts <- benchmark_ts
      self$method <- match.arg(
        method,
        c("ma12", "ma6", "x13", "hp", "stl", "loess", "prophet")
      )
      self$decomposed_user <- decomp_series(self$user_ts, self$method)
      self$decomposed_benchmark <- decomp_series(self$benchmark_ts, self$method)
      # compute ratio of user vs. benchmark trends
      tr_user <- self$decomposed_user$trend
      tr_bench <- self$decomposed_benchmark$trend
      self$modified_user <- ts(as.numeric(tr_user) / as.numeric(tr_bench),
                              start = start(tr_user),
                              frequency = frequency(tr_user))
    },

    #' @description
    #' Plot original, benchmark, trends and modified series via ggplot2.
    plot = function() {
      df <- stack_ts(list(
        Original_User      = self$user_ts,
        Benchmark          = self$benchmark_ts,
        User_Trend         = self$decomposed_user$trend,
        Trend_Ratio        = self$modified_user
      ))
      p <- ggplot2::ggplot(df, ggplot2::aes(x = time, y = value)) +
        ggplot2::geom_line() +
        ggplot2::facet_wrap(~series, scales = "free_y", ncol = 2) +
        ggplot2::labs(x = "Time", y = NULL) +
        ggplot2::theme_minimal()
      print(p)
    },

    #' @description
    #' Print a brief summary of the decomposition.
    summary = function() {
      cor_trends <- stats::cor(
        self$decomposed_user$trend,
        self$decomposed_benchmark$trend,
        use = "complete.obs"
      )
      cat("Decomposition method: ", self$method, "\n")
      cat("Length (user):       ", length(self$user_ts), "\n")
      cat("Length (benchmark): ", length(self$benchmark_ts), "\n")
      cat("Trend correlation:  ", round(cor_trends, 3), "\n")
    },

    #' @description
    #' Forecast the one-step-ahead ratio of user vs. benchmark trends using multiple models.
    #' @param h Integer: number of steps ahead to forecast (default 1).
    #' @return A named list of numeric forecasts (length h) for methods: ets, arima, hw, naive.
    forecast = function(h = 1) {
      ratio_ts <- self$modified_user
      # exponential smoothing
      fc_ets <- forecast::ets(ratio_ts)
      # ARIMA(1,0,0)(1,0,0)
      fc_arima <- forecast::Arima(ratio_ts,
        order = c(1, 0, 0),
        seasonal = list(order = c(1, 0, 0), period = frequency(ratio_ts))
      )
      fc_arima_fc <- forecast::forecast(fc_arima, h = h)
      # Holt-Winters
      fc_hw <- forecast::hw(ratio_ts, h = h)
      # Naive
      fc_naive <- forecast::naive(ratio_ts, h = h)
      list(
        ets   = as.numeric(forecast::forecast(fc_ets, h = h)$mean),
        arima = as.numeric(fc_arima_fc$mean),
        hw    = as.numeric(fc_hw$mean),
        naive = as.numeric(fc_naive$mean)
      )
    },

    #' @description
    #' Evaluate 1-step-ahead user series predictions by combining forecasted trend ratio
    #' with a naive benchmark trend forecast.
    #' @param h Integer: number of steps ahead to forecast (default 1).
    #' @return A named list of numeric user-series forecasts (length h) per method.
    evaluate = function(h = 1) {
      fc_ratio <- self$forecast(h)
      # forecast benchmark trend by naive method
      bench_trend <- self$decomposed_benchmark$trend
      fc_bench <- forecast::naive(bench_trend, h = h)$mean
      # combine to get user-series forecast
      lapply(fc_ratio, function(r) as.numeric(fc_bench) * r)
    }
  )
)

#' @export
ts_decomp <- function(user_ts, benchmark_ts, method = "ma12") {
  TsDecomp$new(user_ts, benchmark_ts, method)
}
