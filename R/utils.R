#' Stack multiple ts into a data.frame for ggplot
#' @noRd
stack_ts <- function(lst) {
  df_list <- lapply(names(lst), function(nm) {
    ts_obj <- lst[[nm]]
    data.frame(
      time = as.numeric(time(ts_obj)),
      value = as.numeric(ts_obj),
      series = nm,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, df_list)
}

#' Compute fit measures between two time series based on year-over-year changes
#'
#' @param actual A `ts` object: the reference (benchmark) series.
#' @param predicted A `ts` object: the reconstructed or forecasted series to compare.
#' @param lag Integer: the lag (in periods) for year-over-year comparison (default is frequency of the series).
#' @return A named list with elements:
#'   \describe{
#'     \item{mae}{Mean absolute error between year-over-year changes.}
#'     \item{mde}{Mean directional error: proportion of mismatched directions between series changes.}
#'     \item{cor}{Correlation between year-over-year changes.}
#'   }
#' @export
fit_measures <- function(actual, predicted, lag = frequency(actual)) {
  stopifnot(inherits(actual, "ts"), inherits(predicted, "ts"),
            frequency(actual) == frequency(predicted))
  lag <- as.integer(lag)
  if (lag <= 0) stop("'lag' must be a positive integer")
  # year-over-year (or lag) changes (relative)
  actual_yoy <- (actual - stats::lag(actual, -lag)) / stats::lag(actual, -lag)
  predicted_yoy <- (predicted - stats::lag(predicted, -lag)) / stats::lag(predicted, -lag)
  # align and drop incomplete cases
  valid <- stats::complete.cases(actual_yoy, predicted_yoy)
  if (!any(valid)) stop("No overlapping periods for year-over-year comparison")
  a <- as.numeric(actual_yoy[valid])
  p <- as.numeric(predicted_yoy[valid])
  mae <- mean(abs(p - a))
  mde <- mean(sign(p) != sign(a))
  cor_val <- stats::cor(a, p)
  list(mae = mae, mde = mde, cor = cor_val)
}
