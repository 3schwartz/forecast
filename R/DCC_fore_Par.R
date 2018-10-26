#' DCC_fore_Par
#'
#' @description Simulate one day ahead forecast using DCC.
#'
#' @param Period xts object of log returns or other risk measures
#' @param foreLength length of forecast
#' @param windowLength length of window used for fitting
#' @param ARIMAfit If true, then an ARIMA will be fit for each iterations. Else a ARMA(2,1) is used
#' @param outDir Output dir
#' @param version Version  of dataset (if run multipe times and want to save each data)
#' @param Name Name of asset (colname)
#' @param tickers Name of columns
#'
#' @return Write to files position each day and its cumulative return
#' @export
#'
DCC_fore_Par  <- function(Period = Period, foreLength = foreLength, windowLength = windowLength,
                          ARIMAfit = FALSE, outDir = "./output/", version = "v1",
                          Name = NULL, tickers = tickers) {

  Index = zoo::index(Period[(nrow(Period)-foreLength):nrow(Period),])

  no_cores <- parallel::detectCores()
  cl <- parallel::makeCluster(no_cores)
  parallel::clusterEvalQ(cl, library(dplyr))

  forecastsDCC <- parallel::parLapply(cl, 0:foreLength, dcEvent::DCC_test_Par,
                            returnsDCC = Period, wL = windowLength, ARIMAfit = FALSE)
  parallel::stopCluster(cl)

  foreDCC <- do.call(rbind, lapply(forecastsDCC, function(x) {
    if(any(class(x) != "DCCfit")) {
      return(rep(1, ncol(Period)))
    } else {
      fore <- rmgarch::dccforecast(x, n.ahead = 1, n.roll = 0)
      return(as.numeric(ifelse(fitted(fore) < 0, -1, 1)))}
    })) %>%
    dplyr::as_tibble()

  readr::write_csv(foreDCC, path=paste0(outDir, "forecasts_DCC_", version,".csv"))

  fore_DCC <- readr::read_csv(file=paste0(outDir, "forecasts_DCC_", version,".csv"))
  colnames(fore_DCC) <- paste0(tickers,".pos")

  #### Univariate returns ####

  if(!is.null(Name)) {
    for_DCC <- fore_DCC %>%
      dplyr::transmute(Date = lubridate::as_date(Index),
                       pos = as.numeric(pull(fore_DCC, paste0(Name, ".pos"))))

    for_DCCxts <- xts::xts(for_DCC$pos[1:(nrow(for_DCC)-1)],
                           order.by = for_DCC$Date[1:(nrow(for_DCC)-1)])

    # Create the ARIMA+GARCH returns
    Intersect_DCC = merge(for_DCCxts, Period[,Name], all = FALSE)
    ArimaGarchReturns_DCC = Intersect_DCC[,1] * Intersect_DCC[,2]

    # Create the backtests for ARIMA+GARCH and Buy & Hold
    ArimaGarchCurve_DCC = cumsum( ArimaGarchReturns_DCC )
    BuyHoldCurve_DCC = cumsum( Intersect_DCC[,2] )
    CombinedCurve_DCC = merge( ArimaGarchCurve_DCC, BuyHoldCurve_DCC, all=F ) %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(Date = zoo::index(BuyHoldCurve_DCC))

    readr::write_csv(CombinedCurve_DCC, path=paste0(outDir, "forecasts_DCC_Uni_", version,".csv"))
  }

  #### Multivariate returns ####
  fore_DCCmul <- fore_DCC
  colnames(fore_DCCmul) <- paste0(tickers,".pos")

  for_DCCmul <- fore_DCCmul %>%
    dplyr::mutate(Date = lubridate::as_date(Index))

  for_DCCmul[1:(nrow(for_DCCmul)-1), 1:(ncol(for_DCCmul)-1)]

  for_DCCmulxts <- xts::xts(for_DCCmul[1:(nrow(for_DCCmul)-1), 1:(ncol(for_DCCmul)-1)],
                            order.by = for_DCCmul$Date[1:(nrow(for_DCCmul)-1)])

  ArimaGarchReturns_DCCmul = xts::xts(rowSums(for_DCCmulxts * Period[zoo::index(for_DCCmulxts)] * (1/ncol(for_DCCmulxts))),
                                      order.by = zoo::index(for_DCCmulxts))
  BuyHoldReturns_DCCmul <- xts::xts(rowSums(Period[zoo::index(for_DCCmulxts)] * (1/ncol(for_DCCmulxts))),
                                    order.by = zoo::index(for_DCCmulxts))
  # Create the backtests for ARIMA+GARCH and Buy & Hold
  ArimaGarchCurve_DCCmul = cumsum( ArimaGarchReturns_DCCmul )
  BuyHoldCurve_DCCmul = cumsum( BuyHoldReturns_DCCmul )
  CombinedCurve_DCCmul = merge( ArimaGarchCurve_DCCmul, BuyHoldCurve_DCCmul, all=F ) %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(Date = zoo::index(BuyHoldCurve_DCCmul))

  readr::write_csv(CombinedCurve_DCCmul, path=paste0(outDir, "forecasts_DCC_Mul_", version,".csv"))
}
