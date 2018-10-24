#' arimaBacktest
#'
#' @description ARIMA-GARCH one period forecast. Designed to work for parallel computing
#'
#' @param d iterrator
#' @param returns xts objects of returns
#' @param wL window length
#' @param ARIMAfit If TRUE ARIMA will be fit for each time
#'
#' @return position
#' @export
#'
arimaBacktest <- function(d, returns = Period, wL = windowLength, ARIMAfit = TRUE){
  ReturnsOffset = returns[(1+d):(wL+d)]

  if(ARIMAfit == TRUE) {
    Order = forecast::auto.arima(ReturnsOffset)
    Order <- as.character(Order)
    Order <- stringr::str_extract_all(Order, "(?<=\\().*(?=\\))")
    Order <- stringr::str_split_fixed(Order, ",", 3)
    Order <- as.numeric(Order)

    if(Order[2] !=0) {
      for(i in 1:Order[2]) {
        ReturnsOffset <- diff(ReturnsOffset)
        ReturnsOffset[,1] <- 0
      }
    }
  } else {
    Order = c(2,0,1)
  }
  spec = rugarch::ugarchspec(
    variance.model=list(garchOrder=c(1,1)),
    mean.model=list(armaOrder=c(Order[1], Order[3]), include.mean=T),
    distribution.model="sged"
  )
  fit = tryCatch(
    rugarch::ugarchfit(
      spec, ReturnsOffset, solver = 'hybrid'
    ), error=function(e) e, warning=function(w) w
  )
  if(is(fit, "warning")) {
    forecasts = paste(zoo::index(ReturnsOffset[wL]), 1, sep=",")
  } else {
    fore = rugarch::ugarchforecast(fit, n.ahead=1)
    ind = fore@forecast$seriesFor
    forecasts = paste(colnames(ind), ifelse(ind[1] < 0, -1, 1), sep=",")
  }
  return(forecasts)
}
