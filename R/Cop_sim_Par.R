#' Cop_sim_Par
#'
#' @description Simulate one day ahead forecast using dynamic copula.
#'
#' @param Period xts object of log returns or other risk measures
#' @param foreLength length of forecast
#' @param windowLength length of window used for fitting
#' @param ARIMAfit If true, then an ARIMA will be fit for each iterations. Else a ARMA(2,1) is used
#' @param m.sim Number of simualtions
#' @param outDir Output dir
#' @param version Version  of dataset (if run multipe times and want to save each data)
#' @param Name Name of asset (colname)
#' @param tickers Names of columns
#'
#' @return Write to files position each day and its cumulative return
#' @export
#'
Cop_sim_Par <- function(Period = Period, foreLength = foreLength, windowLength = windowLength,
                        ARIMAfit = FALSE, m.sim = 1000,
                        outDir = "./output/", version = "v1",
                        Name = NULL,  tickers = tickers) {
  Index = zoo::index(Period[(nrow(Period)-foreLength):nrow(Period),])

  no_cores <- parallel::detectCores()
  cl <- parallel::makeCluster(no_cores)
  parallel::clusterEvalQ(cl, library(dplyr))

  forecastsCop <- parallel::parLapply(cl, 0:foreLength, dcEvent::Cop_test_Par,
                                      returnsCop = Period, wL = windowLength,
                                      ARIMAfit = ARIMAfit)

  foreCopTest <- do.call(rbind, lapply(0:foreLength, function(returnsCop, wL, Copfit, d) {
    ReturnsOffset = returnsCop[(1+d):(wL+d),]
    x = Copfit[[d+1]]

    if(any(class(x) != "cGARCHfit")) {
      rep(1, ncol(ReturnsOffset))
    } else {
      preR = rmgarch::last(rmgarch::rcor(x),1)[,,1]
      diag(preR) = 1
      maxGO = x@model$maxgarchOrder
      sim1 = rmgarch::cgarchsim(x, n.sim = 1, n.start = 0, m.sim = m.sim, presigma = tail(rugarch::sigma(x), maxGO),
                                startMethod = "sample", preR = preR, prereturns = tail( as.matrix(Period), maxGO),
                                preresiduals = tail(rugarch::residuals(x), maxGO),rseed = 1:m.sim,
                                cluster = cl)
      sim = list()
      for(z in 1:ncol(sim1@msim$simX[[1]])) {
        sim[[z]] <- mean(sapply(sim1@msim$simX, FUN = function(x) x[,z]))
      }
      simmean = do.call(c, sim)
      as.numeric(ifelse(simmean < 0, -1, 1))
    }}, returnsCop = Period, wL = windowLength,
    Copfit = forecastsCop))
  parallel::stopCluster(cl)

  forCopTest <- foreCopTest %>%
    dplyr::as_tibble()

  readr::write_csv(forCopTest, path=paste0(outDir, "forecasts_Cop_", version,".csv"))

  fore_Cop <- readr::read_csv(file=paste0(outDir, "forecasts_Cop_", version,".csv"))
  colnames(fore_Cop) <- paste0(tickers,".pos")
  #### Univariate returns ####
  if(!is.null(Name)) {
    for_Cop <- fore_Cop %>%
      dplyr::transmute(Date = lubridate::as_date(Index),
                       pos = as.numeric(pull(fore_Cop, paste0(Name, ".pos"))))

    for_Copxts <- xts::xts(for_Cop$pos[1:(nrow(for_Cop)-1)],
                           order.by = for_Cop$Date[1:(nrow(for_Cop)-1)])

    # Create the ARIMA+GARCH returns
    Intersect_Cop = merge(for_Copxts, Period[,Name], all = FALSE)
    ArimaGarchReturns_Cop = Intersect_Cop[,1] * Intersect_Cop[,2]

    # Create the backtests for ARIMA+GARCH and Buy & Hold
    ArimaGarchCurve_Cop = exp( cumsum( ArimaGarchReturns_Cop )) - 1
    BuyHoldCurve_Cop = exp( cumsum( Intersect_Cop[,2] )) - 1
    CombinedCurve_Cop = merge( ArimaGarchCurve_Cop, BuyHoldCurve_Cop, all=F ) %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(Date = zoo::index(BuyHoldCurve_Cop))

    readr::write_csv(CombinedCurve_Cop, path=paste0(outDir, "forecasts_Cop_Uni_", version,".csv"))
  }
  #### Multivariate returns ####
  fore_Copmul <- fore_Cop

  for_Copmul <- fore_Copmul %>%
    dplyr::mutate(Date = lubridate::as_date(Index))

  for_Copmulxts <- xts::xts(for_Copmul[1:(nrow(for_Copmul)-1), 1:(ncol(for_Copmul)-1)],
                            order.by = for_Copmul$Date[1:(nrow(for_Copmul)-1)])

  ArimaGarchReturns_Copmul = xts::xts(rowSums(for_Copmulxts * Period[zoo::index(for_Copmulxts)] * (1/ncol(for_Copmulxts))),
                                      order.by = zoo::index(for_Copmulxts))
  BuyHoldReturns_Copmul <- xts::xts(rowSums(Period[zoo::index(for_Copmulxts)] * (1/ncol(for_Copmulxts))),
                                    order.by = zoo::index(for_Copmulxts))
  # Create the backtests for ARIMA+GARCH and Buy & Hold
  ArimaGarchCurve_Copmul = exp( cumsum( ArimaGarchReturns_Copmul )) - 1
  BuyHoldCurve_Copmul = exp( cumsum( BuyHoldReturns_Copmul )) - 1
  CombinedCurve_Copmul = merge( ArimaGarchCurve_Copmul, BuyHoldCurve_Copmul, all=F ) %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(Date = zoo::index(BuyHoldCurve_Copmul))

  readr::write_csv(CombinedCurve_Copmul, path=paste0(outDir, "forecasts_Cop_Mul_", version,".csv"))
}
