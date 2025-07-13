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
