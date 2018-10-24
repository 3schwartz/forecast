library(vars)
library(fpp)
# head(usconsumption)
# VARselect(usconsumption, lag.max = 8, type = "both")$selection
var <- VAR(usconsumption, lag.max = 8, ic = c("AIC"), type = "both")
# serial.test(var, lags.pt = 10, type = "PT.asymptotic")
# summary(var)
fcst <- forecast(var, h = 1)
# plot(fcst, xlab = "Year")
fcst$forecast$consumption$mean

tickers <- c("AAPL", "AMZN", "MSFT")
stocks <- lapply(tickers, quantmod::getSymbols , auto.assign = FALSE)
stocksAd <- do.call(cbind, lapply(stocks, function(x) {
  quantmod::Ad(x)
}))
stocksdiff = diff(log(stocksAd))
colnames(stocksdiff) <- tickers

ReturnsVAR = stocksdiff[-1,]

forecastsVAR <- numeric(foreLength)

windowLength = 500
foreLength = nrow(stocksdiff) - windowLength

for (d in 0:foreLength) {
  ReturnsOffset = stocksdiff[(1+d):(windowLength+d)]

  ts.matrix <- as.ts(ReturnsOffset)

  var = vars::VAR(ts.matrix, lag.max = 8, ic = c("AIC"), type = "both")
  fore = forecast(var, h = 1)
  ind = fore$forecast$AAPL$mean

  forecastsVAR[d+1] = ifelse(ind[1] < 0, -1, 1)
  # print(ifelse(ind[1] < 0, -1, 1))
}

write.csv(forecastsVAR, file="./output/forecasts_VAR.csv", row.names=FALSE)

fore_VAR <- readr::read_csv(file="./output/forecasts_VAR.csv")
for_VAR <- fore_VAR %>%
  # tibble() %>% #only if not read from file and the use "." instead of "x"
  dplyr::transmute(Date = lubridate::as_date(zoo::index(stocksdiff[(nrow(stocksdiff)-foreLength):nrow(stocksdiff),])),
                   pos = as.numeric(x))

for_VARxts <- xts::xts(for_VAR$pos[1:(nrow(for_VAR)-1)],
                       order.by = for_VAR$Date[1:(nrow(for_VAR)-1)])

# Create the ARIMA+GARCH returns
Intersect_VAR = merge(for_VARxts, ReturnsVAR[,"AAPL"], all = FALSE)
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
