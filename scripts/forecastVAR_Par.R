library(vars)
library(fpp)

tickers <- c("AAPL", "AMZN", "MSFT")
stocks <- lapply(tickers, quantmod::getSymbols , auto.assign = FALSE)
stocksAd <- do.call(cbind, lapply(stocks, function(x) {
  quantmod::Ad(x)
}))
stocksdiff = diff(log(stocksAd))
colnames(stocksdiff) <- tickers

ReturnsVAR = stocksdiff[-1,]

for_VAR_Par <- function(d, returnsVAR = ReturnsVAR, wL = windowLength, Name = NULL) {
    ReturnsOffset = returnsVAR[(1+d):(wL+d),]

    ts.matrix <- stats::as.ts(ReturnsOffset)

    var = vars::VAR(ts.matrix, lag.max = 8, ic = c("AIC"), type = "both")
    fore = forecast::forecast(var, h = 1)

    if(!is.null(Name)) {
      ind = as.numeric(fore$forecast[[Name]]$mean)
    } else {
      ind = do.call(c, lapply(fore$forecast, function(x) {
        as.numeric(x$mean)
      }))
    }
    return(forecastsVAR = ifelse(ind < 0, -1, 1))
    # print(forecastsVAR[d+1,])
}
for_VAR_Loop <- function(returnsVAR = ReturnsVAR, wL = windowLength, Name = NULL) {

  foreLength = nrow(returnsVAR) - wL
  forecastsVAR <- matrix(nrow = (foreLength + 1), ncol = ncol(returnsVAR))

  for (d in 0:foreLength) {
    ReturnsOffset = returnsVAR[(1+d):(wL+d)]

    ts.matrix <- as.ts(ReturnsOffset)

    var = vars::VAR(ts.matrix, lag.max = 8, ic = c("AIC"), type = "both")
    fore = forecast(var, h = 1)

    if(!is.null(Name)) {
      ind = as.numeric(fore$forecast[[Name]]$mean)
    } else {
      ind = do.call(c, lapply(fore$forecast, function(x) {
        as.numeric(x$mean)
      }))
    }
    forecastsVAR[d+1,] = ifelse(ind < 0, -1, 1)
    # print(forecastsVAR[d+1,])
  }

  if(!is.null(Name)) {
    write.csv(forecastsVAR, file="./output/forecasts_VAR.csv", row.names=FALSE)
  } else {
    write.csv(forecastsVAR, file="./output/forecasts_VAR_mult.csv", row.names=FALSE)
  }
}

windowLength = 100
foreLength = nrow(ReturnsVAR["2018/"]) - windowLength
Period = ReturnsVAR["2018/"]
Index = zoo::index(Period[(nrow(Period)-foreLength):nrow(Period),])

# In parallel
require(parallel)
no_cores <- detectCores()
cl <- makeCluster(no_cores)
clusterEvalQ(cl, library(dplyr))
system.time(
    forecastsVAR <- do.call(rbind, parLapply(cl, 0:foreLength, for_VAR_Par,
                                             returnsVAR = Period, wL = windowLength, Name = NULL)) %>%
      dplyr::as_tibble()
)
stopCluster(cl)
if(!is.null(Name)) {
  readr::write_csv(forecastsVAR, path="./output/forecasts_VAR.csv")
} else {
  readr::write_csv(forecastsVAR, path="./output/forecasts_VAR_mult.csv")
}
# Multivariate call #
system.time(
for_VAR_Loop(returnsVAR = Period, wL = windowLength, Name = NULL)
)
# Univariate call #
for_VAR_Loop(returnsVAR = Period, wL = windowLength, Name = "AAPL")


#### Univariate returns ####
Name = "AAPL"
fore_VAR <- readr::read_csv(file="./output/forecasts_VAR.csv")
colnames(fore_VAR) <- paste0(tickers,".pos")

for_VAR <- fore_VAR %>%
  # tibble() %>% #only if not read from file and the use "." instead of "x"
  dplyr::transmute(Date = lubridate::as_date(Index),
                   pos = as.numeric(pull(fore_VAR, paste0(Name, ".pos"))))

for_VARxts <- xts::xts(for_VAR$pos[1:(nrow(for_VAR)-1)],
                       order.by = for_VAR$Date[1:(nrow(for_VAR)-1)])

# Create the ARIMA+GARCH returns
Intersect_VAR = merge(for_VARxts, ReturnsVAR[,Name], all = FALSE)
ArimaGarchReturns_VAR = Intersect_VAR[,1] * Intersect_VAR[,2]

# Create the backtests for ARIMA+GARCH and Buy & Hold
ArimaGarchCurve_VAR = exp( cumsum( ArimaGarchReturns_VAR )) - 1
BuyHoldCurve_VAR = exp( cumsum( Intersect_VAR[,2] )) - 1
CombinedCurve_VAR = merge( ArimaGarchCurve_VAR, BuyHoldCurve_VAR, all=F )

# Plot the equity curves
lattice::xyplot(
  CombinedCurve_VAR,
  superpose=T,
  col=c("darkred", "darkblue"),
  lwd=2,
  key=list(
    text=list(
      c("VAR", "Buy & Hold")
    ),
    lines=list(
      lwd=2, col=c("darkred", "darkblue")
    )
  )
)

#### Multivariate returns ####
# for_VAR_Loop(returnsVAR = ReturnsVAR["2018/"], wL = 100, Name = NULL)

fore_VAR <- readr::read_csv(file="./output/forecasts_VAR_mult.csv")
colnames(fore_VAR) <- paste0(tickers,".pos")

for_VAR <- fore_VAR %>%
  # tibble() %>% #only if not read from file and the use "." instead of "x"
  dplyr::mutate(Date = lubridate::as_date(Index))

for_VAR[1:(nrow(for_VAR)-1), 1:(ncol(for_VAR)-1)]

for_VARxts <- xts::xts(for_VAR[1:(nrow(for_VAR)-1), 1:(ncol(for_VAR)-1)],
                       order.by = for_VAR$Date[1:(nrow(for_VAR)-1)])

# Create the ARIMA+GARCH returns
ArimaGarchReturns_VAR = xts::xts(rowSums(for_VARxts * ReturnsVAR[zoo::index(for_VARxts)] * (1/ncol(for_VARxts))),
                                 order.by = zoo::index(for_VARxts))
BuyHoldReturns_VAR <- xts::xts(rowSums(ReturnsVAR[zoo::index(for_VARxts)] * (1/ncol(for_VARxts))),
         order.by = zoo::index(for_VARxts))
# Create the backtests for ARIMA+GARCH and Buy & Hold
ArimaGarchCurve_VAR = exp( cumsum( ArimaGarchReturns_VAR )) - 1
BuyHoldCurve_VAR = exp( cumsum( BuyHoldReturns_VAR )) - 1
CombinedCurve_VAR = merge( ArimaGarchCurve_VAR, BuyHoldCurve_VAR, all=F )

# Plot the equity curves
lattice::xyplot(
  CombinedCurve_VAR,
  superpose=T,
  col=c("darkred", "darkblue"),
  lwd=2,
  key=list(
    text=list(
      c("VAR", "Buy & Hold")
    ),
    lines=list(
      lwd=2, col=c("darkred", "darkblue")
    )
  )
)
