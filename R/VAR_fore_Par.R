#' VAR_fore_Par
#'
#' @description Forecast oneday ahead VAR and backtest
#'
#' @param Period xts object of log returns or other risk measures
#' @param foreLength length of forecast
#' @param windowLength length of window used for fitting
#' @param outDir Output dir
#' @param version Version  of dataset (if run multipe times and want to save each data)
#' @param Name Name of asset (colname)
#' @param tickers Names of columns
#'
#' @return Write to files position each day and its cumulative return
#' @export
#'
VAR_fore_Par <- function(Period = Period, foreLength = foreLength, windowLength = windowLength,
                         outDir = "./output/", version = "v1",
                         Name = NULL, tickers = tickers) {


  Index = zoo::index(Period[(nrow(Period)-foreLength):nrow(Period),])

  no_cores <- parallel::detectCores()
  cl <- parallel::makeCluster(no_cores)
  parallel::clusterEvalQ(cl, library(dplyr))

  forecastsVAR <- do.call(rbind, parLapply(cl, 0:foreLength, for_VAR_Par,
                                           returnsVAR = Period, wL = windowLength)) %>%
    dplyr::as_tibble()

  stopCluster(cl)

  readr::write_csv(forecastsVAR, path=paste0(outDir, "forecasts_VAR_", version,".csv"))

  fore_VAR <- readr::read_csv(file=paste0(outDir, "forecasts_VAR_", version,".csv"))
  colnames(fore_VAR) <- paste0(tickers,".pos")

  #### Univariate returns ####
  if(!is.null(Name)) {
    for_VAR <- fore_VAR %>%
      dplyr::transmute(Date = lubridate::as_date(Index),
                       pos = as.numeric(pull(fore_VAR, paste0(Name, ".pos"))))

    for_VARxts <- xts::xts(for_VAR$pos[1:(nrow(for_VAR)-1)],
                           order.by = for_VAR$Date[1:(nrow(for_VAR)-1)])

    # Create the ARIMA+GARCH returns
    Intersect_VAR = merge(for_VARxts, Period[,Name], all = FALSE)
    ArimaGarchReturns_VAR = Intersect_VAR[,1] * Intersect_VAR[,2]

    # Create the backtests for ARIMA+GARCH and Buy & Hold
    ArimaGarchCurve_VAR = exp( cumsum( ArimaGarchReturns_VAR )) - 1
    BuyHoldCurve_VAR = exp( cumsum( Intersect_VAR[,2] )) - 1
    CombinedCurve_VAR = merge( ArimaGarchCurve_VAR, BuyHoldCurve_VAR, all=F ) %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(Date = zoo::index(BuyHoldCurve_VAR))

    readr::write_csv(CombinedCurve_VAR, path=paste0(outDir, "forecasts_VAR_Uni_", version,".csv"))
  }

  #### Multivariate returns ####
  for_VAR <- fore_VAR %>%
    dplyr::mutate(Date = lubridate::as_date(Index))

  for_VAR[1:(nrow(for_VAR)-1), 1:(ncol(for_VAR)-1)]

  for_VARxts <- xts::xts(for_VAR[1:(nrow(for_VAR)-1), 1:(ncol(for_VAR)-1)],
                         order.by = for_VAR$Date[1:(nrow(for_VAR)-1)])

  ArimaGarchReturns_VAR = xts::xts(rowSums(for_VARxts * Period[zoo::index(for_VARxts)] * (1/ncol(for_VARxts))),
                                   order.by = zoo::index(for_VARxts))
  BuyHoldReturns_VAR <- xts::xts(rowSums(Period[zoo::index(for_VARxts)] * (1/ncol(for_VARxts))),
                                 order.by = zoo::index(for_VARxts))
  # Create the backtests for ARIMA+GARCH and Buy & Hold
  ArimaGarchCurve_VAR = exp( cumsum( ArimaGarchReturns_VAR )) - 1
  BuyHoldCurve_VAR = exp( cumsum( BuyHoldReturns_VAR )) - 1
  CombinedCurve_VAR = merge( ArimaGarchCurve_VAR, BuyHoldCurve_VAR, all=F ) %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(Date = zoo::index(BuyHoldCurve_VAR))

  readr::write_csv(CombinedCurve_VAR, path=paste0(outDir, "forecasts_VAR_Mul_", version,".csv"))
}
