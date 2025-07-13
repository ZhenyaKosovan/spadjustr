#' Replace user trend with benchmark trend
#'
#' @param decom_user List from decomp_series(user_ts)
#' @param decom_bench List from decomp_series(benchmark_ts)
#' @return A `ts` object of the reconstructued user series
#' @noRd
replace_trend <- function(decom_user, decom_bench) {
  decom_user$trend <- decom_bench$trend
  ts_recon <- decom_user$trend +
    decom_user$seasonal +
    decom_user$idiosyncratic
  ts(ts_recon, start = start(decom_user$trend), frequency = frequency(decom_user$trend))
}
