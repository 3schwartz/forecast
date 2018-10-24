#' ARIMAGACH_fore_Par
#'
#' @description Forecast one-day ahead forecast using ARIMA-GARCH.
#'
#' @param Period xts object of log returns (one-dim)
#' @param foreLength length of forecast
#' @param windowLength length of window used for fitting
#' @param ARIMAfit If true, then an ARIMA will be fit for each iterations. Else a ARMA(2,1) is used
#' @param outDir Output dir
#' @param version Version  of dataset (if run multipe times and want to save each data)
#'
#' @return Write to files position each day and its cumulative return
#' @export
#'
ARIMAGACH_fore_Par <- function(Period = Period, foreLength = foreLength, windowLength = windowLength,
                               ARIMAfit = TRUE,
                               outDir = "./output/", version = "v1") {
  Index = zoo::index(Period[(nrow(Period)-foreLength):nrow(Period),])

  no_cores <- parallel::detectCores()
  cl <- parallel::makeCluster(no_cores)
  parallel::clusterEvalQ(cl, library(dplyr))

  fore_Par <- parallel::parSapply(cl, 0:foreLength, arimaBacktest,
                        returns = Period, wL = windowLength)

  parallel::stopCluster(cl)

  write.csv(fore_Par, file=paste0(outDir, "forecasts_AG_", version,".csv"), row.names=FALSE)

  fore_Par <- readr::read_csv(file=paste0(outDir, "forecasts_AG_", version,".csv"))

  fore_Par <- fore_Par %>%
    tidyr::separate("x", c("Date", "pos"), ",") %>%
    dplyr::transmute(Date = lubridate::as_date(zoo::index(Period[(nrow(Period)-foreLength):nrow(Period),])),
                     pos = as.numeric(pos))

  for_Parxts <- xts::xts(fore_Par$pos[1:(nrow(fore_Par)-1)],
                         order.by = fore_Par$Date[1:(nrow(fore_Par)-1)])

  # Create the ARIMA+GARCH returns
  Intersect_Par = merge(for_Parxts, Period, all = FALSE)
  ArimaGarchReturns_Par = Intersect_Par[,1] * Intersect_Par[,2]

  # Create the backtests for ARIMA+GARCH and Buy & Hold
  ArimaGarchCurve_Par = exp( cumsum( ArimaGarchReturns_Par )) - 1
  BuyHoldCurve_Par = exp( cumsum( Intersect_Par[,2] )) - 1
  # ArimaGarchCurve_Par = cumsum( ArimaGarchReturns_Par )
  # BuyHoldCurve_Par = cumsum( Intersect_Par[,2] )
  CombinedCurve_Par = merge( ArimaGarchCurve_Par, BuyHoldCurve_Par, all=F ) %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(Date = zoo::index(BuyHoldCurve_Par))

  readr::write_csv(CombinedCurve_Par, path=paste0(outDir, "forecasts_AG_Uni_", version,".csv"))
}
