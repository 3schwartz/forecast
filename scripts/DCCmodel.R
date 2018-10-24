library(xts)
library(qrmdata)
library(rmgarch)

## Load some real data
tickers <- c("AAPL", "AMZN", "MSFT")
stocks <- lapply(tickers, quantmod::getSymbols , auto.assign = FALSE)
stocksAd <- do.call(cbind, lapply(stocks, function(x) {
  quantmod::Ad(x)
}))
stocksdiff = diff(log(stocksAd))
colnames(stocksdiff) <- tickers

ReturnsDCC = stocksdiff[-1,]

DCC_test_Par <- function(d, returnsDCC = ReturnsDCC, wL = windowLength, ARIMAfit = FALSE) {
  ReturnsOffset = returnsDCC[(1+d):(wL+d),]

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
      ## Specify univariate AR(1)-GARCH(1,1) for both marginal processes
      uspec <- rugarch::ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                                   mean.model = list(armaOrder = c(2,1), include.mean = TRUE),
                                   distribution.model = "std")
      ## Combine univariate specs to obtain spec for marginal models
      marginspec <- rugarch::multispec(replicate(ncol(ReturnsOffset), uspec))
    } else {
      marginspec <- mulspec
    }
    } else {
      ## Specify univariate AR(1)-GARCH(1,1) for both marginal processes
      uspec <- rugarch::ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                                   mean.model = list(armaOrder = c(2,1), include.mean = TRUE),
                                   distribution.model = "std")
      ## Combine univariate specs to obtain spec for marginal models
      marginspec <- rugarch::multispec(replicate(ncol(ReturnsOffset), uspec))
    }

  if("error" %in% class(marginspec)) {
    ## Specify univariate AR(1)-GARCH(1,1) for both marginal processes
    uspec <- rugarch::ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                                 mean.model = list(armaOrder = c(2,1), include.mean = TRUE),
                                 distribution.model = "std")
    ## Combine univariate specs to obtain spec for marginal models
    marginspec <- rugarch::multispec(replicate(ncol(ReturnsOffset), uspec))
  }

  ## Create spec for DCC
  mspec <- rmgarch::dccspec(marginspec, dccOrder = c(1,1), model = "DCC", distribution = "mvt")

  fit = tryCatch(rmgarch::dccfit(mspec, ReturnsOffset),
                 error=function(e) e, warning=function(w) w)
  return(fit)
}

windowLength = 100
foreLength = nrow(ReturnsDCC["2018/"]) - windowLength
Period = ReturnsDCC["2018/"]
Index = zoo::index(Period[(nrow(Period)-foreLength):nrow(Period),])
Name = NULL

# In parallel
require(parallel)
no_cores <- detectCores()
cl <- makeCluster(no_cores)
clusterEvalQ(cl, library(dplyr))
system.time(
  forecastsDCC <- parLapply(cl, 0:foreLength, DCC_test_Par,
            returnsDCC = Period, wL = windowLength, ARIMAfit = FALSE)
)
stopCluster(cl)

foreDCC <- do.call(rbind, lapply(forecastsDCC, function(x) {
  if(any(class(x) != "DCCfit")) {
    rep(1, ncol(Period))
  } else {
    fore <- rmgarch::dccforecast(x, n.ahead = 1, n.roll = 0)
    return(as.numeric(ifelse(fitted(fore) < 0, -1, 1)))}})) %>%
  dplyr::as_tibble()

readr::write_csv(foreDCC, path="./output/forecasts_DCC_mult.csv")

#### Univariate returns ####
Name = "AAPL"
fore_DCC <- readr::read_csv(file="./output/forecasts_DCC_mult.csv")
colnames(fore_DCC) <- paste0(tickers,".pos")

for_DCC <- fore_DCC %>%
  dplyr::transmute(Date = lubridate::as_date(Index),
                   pos = as.numeric(pull(fore_DCC, paste0(Name, ".pos"))))

for_DCCxts <- xts::xts(for_DCC$pos[1:(nrow(for_DCC)-1)],
                       order.by = for_DCC$Date[1:(nrow(for_DCC)-1)])

# Create the ARIMA+GARCH returns
Intersect_DCC = merge(for_DCCxts, ReturnsDCC[,Name], all = FALSE)
ArimaGarchReturns_DCC = Intersect_DCC[,1] * Intersect_DCC[,2]

# Create the backtests for ARIMA+GARCH and Buy & Hold
ArimaGarchCurve_DCC = exp( cumsum( ArimaGarchReturns_DCC )) - 1
BuyHoldCurve_DCC = exp( cumsum( Intersect_DCC[,2] )) - 1
CombinedCurve_DCC = merge( ArimaGarchCurve_DCC, BuyHoldCurve_DCC, all=F )

# Plot the equity curves
lattice::xyplot(
  CombinedCurve_DCC,
  superpose=T,
  col=c("darkred", "darkblue"),
  lwd=2,
  key=list(
    text=list(
      c("DCC", "Buy & Hold")
    ),
    lines=list(
      lwd=2, col=c("darkred", "darkblue")
    )
  )
)

#### Multivariate returns ####
fore_DCCmul <- readr::read_csv(file="./output/forecasts_DCC_mult.csv")
colnames(fore_DCCmul) <- paste0(tickers,".pos")

for_DCCmul <- fore_DCCmul %>%
  dplyr::mutate(Date = lubridate::as_date(Index))

for_DCCmul[1:(nrow(for_DCCmul)-1), 1:(ncol(for_DCCmul)-1)]

for_DCCmulxts <- xts::xts(for_DCCmul[1:(nrow(for_DCCmul)-1), 1:(ncol(for_DCCmul)-1)],
                       order.by = for_DCCmul$Date[1:(nrow(for_DCCmul)-1)])

# Create the ARIMA+GARCH returns
ArimaGarchReturns_DCCmul = xts::xts(rowSums(for_DCCmulxts * ReturnsDCC[zoo::index(for_DCCmulxts)] * (1/ncol(for_DCCmulxts))),
                                 order.by = zoo::index(for_DCCmulxts))
BuyHoldReturns_DCCmul <- xts::xts(rowSums(ReturnsDCC[zoo::index(for_DCCmulxts)] * (1/ncol(for_DCCmulxts))),
                               order.by = zoo::index(for_DCCmulxts))
# Create the backtests for ARIMA+GARCH and Buy & Hold
ArimaGarchCurve_DCCmul = exp( cumsum( ArimaGarchReturns_DCCmul )) - 1
BuyHoldCurve_DCCmul = exp( cumsum( BuyHoldReturns_DCCmul )) - 1
CombinedCurve_DCCmul = merge( ArimaGarchCurve_DCCmul, BuyHoldCurve_DCCmul, all=F )

# Plot the equity curves
lattice::xyplot(
  CombinedCurve_DCCmul,
  superpose=T,
  col=c("darkred", "darkblue"),
  lwd=2,
  key=list(
    text=list(
      c("DCCmul", "Buy & Hold")
    ),
    lines=list(
      lwd=2, col=c("darkred", "darkblue")
    )
  )
)
