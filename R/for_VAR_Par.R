#' for_VAR_Par
#'
#' @description Backtest VAR. Function designed to be used for parallel
#' computing
#'
#' @param d iterator
#' @param returnsVAR xts of returns
#' @param wL window length
#'
#' @return Return positin of ne period look ahead
#' @export
#'
for_VAR_Par <- function(d, returnsVAR = Period, wL = windowLength) {
  ReturnsOffset = returnsVAR[(1+d):(wL+d),]

  ts.matrix <- stats::as.ts(ReturnsOffset)

  var = vars::VAR(ts.matrix, lag.max = 8, ic = c("AIC"), type = "both")
  fore = forecast::forecast(var, h = 1)

  ind = do.call(c, lapply(fore$forecast, function(x) {
    as.numeric(x$mean)
  }))

  return(forecastsVAR = ifelse(ind < 0, -1, 1))
}
