#' Cop_test_Par
#'
#' @description Backtest using copula simulation. Function designed to be used for parallel
#' computing
#'
#' @param d iterator
#' @param returnsCop xts of returns
#' @param wL window length
#' @param ARIMAfit If true, then an ARIMA will be fit for each iterations. Else a ARMA(2,1) is used
#'
#' @return Object of type "cGARCHfit". OBS a lot of non-fits objects are return as warnings or errors
#' @export
#'
Cop_test_Par <- function(d, returnsCop = Period, wL = windowLength, ARIMAfit = FALSE) {
  ReturnsOffset = returnsCop[(1+d):(wL+d),]

  if(ARIMAfit == TRUE) {

    margins = list()
    for(i in 1:ncol(ReturnsOffset)) {
      Order = forecast::auto.arima(ReturnsOffset[,i])
      Order <- as.character(Order)
      Order <- stringr::str_extract_all(Order, "(?<=\\().*(?=\\))")
      Order <- stringr::str_split_fixed(Order, ",", 3)
      Order <- as.numeric(Order)

      if(Order[2] !=0) {
        for(i in 1:Order[2]) {
          ReturnsOffset[,i] <- diff(ReturnsOffset[,i])
          ReturnsOffset[1,i] <- 0
        }
      }
      unifit = tryCatch(rugarch::ugarchspec(
        variance.model=list(model = "sGARCH", garchOrder=c(1,1)),
        mean.model=list(armaOrder=c(Order[1], Order[3]), include.mean=T),
        distribution.model="std"),
        error=function(e) e, warning=function(w) w)
      if(is(unifit, "warning") | is(unifit, "error")) {
        margins[[i]] = rugarch::ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                                           mean.model = list(armaOrder = c(2,1), include.mean = TRUE),
                                           distribution.model = "std")
      }  else {
        margins[[i]] = unifit
      }
    }
    mulspec = tryCatch(rugarch::multispec(margins),
                       error=function(e) e, warning=function(w) w)
    if(is(unifit, "warning") | is(unifit, "error")) {
      uspec <- rugarch::ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                                   mean.model = list(armaOrder = c(2,1), include.mean = TRUE),
                                   distribution.model = "std")
      marginspec <- rugarch::multispec(replicate(ncol(ReturnsOffset), uspec))
    } else {
      marginspec <- mulspec
    }
  } else {
    uspec <- rugarch::ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                                 mean.model = list(armaOrder = c(2,1), include.mean = TRUE),
                                 distribution.model = "std")
    marginspec <- rugarch::multispec(replicate(ncol(ReturnsOffset), uspec))
  }

  if("error" %in% class(marginspec)) {
    uspec <- rugarch::ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                                 mean.model = list(armaOrder = c(2,1), include.mean = TRUE),
                                 distribution.model = "std")
    marginspec <- rugarch::multispec(replicate(ncol(ReturnsOffset), uspec))
  }

  copspec <- rmgarch::cgarchspec(uspec = marginspec,
                                 distribution.model = list(copula = "mvt", method = "ML",
                                                           time.varying = TRUE, transformation = "parametric"))

  fit = tryCatch(rmgarch::cgarchfit(copspec, data = ReturnsOffset,
                                    spd.control = list(lower = 0.1, upper = 0.9, type = "pwm",
                                                       kernel = "epanech"),
                                    fit.control = list(eval.se = FALSE, trace = TRUE),
                                    solver = "solnp"),
                 error=function(e) e, warning=function(w) w)
  return(fit)
}
