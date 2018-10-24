library(dplyr)
library(quantmod)
quantmod::getSymbols("^GSPC", from="1950-01-01")

spReturns = diff(log(Cl(GSPC)))
spReturns[as.character(head(index(Cl(GSPC)),1))] = 0

ReturnsPar <- spReturns["2018-04-20/"]

windowLength = 100
foreLength = length(ReturnsPar) - windowLength

arimaBacktest <- function(d, returns = Returns, wL = windowLength){
  # Obtain rolling window for this period
  ReturnsOffset = returns[(1+d):(wL+d)]

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
# return(forecasts)
}

require(parallel)
no_cores <- detectCores()
cl <- makeCluster(no_cores)
clusterEvalQ(cl, library(dplyr))
# system.time(
# fooC <- do.call(c,
#             clusterApply(cl, 0:foreLength,
#                          arimaBacktest, returns = Returns, wL = windowLength))
# )
system.time(
(fore_Par <- parSapply(cl, 0:foreLength, arimaBacktest,
          returns = ReturnsPar, wL = windowLength))
)
stopCluster(cl)
write.csv(fore_Par, file="./output/fore_Par.csv", row.names=FALSE)

fore_Par <- readr::read_csv(file = "./output/fore_Par.csv")
fore_Par <- fore_Par %>%
  # tibble() %>% #only if not read from file
  tidyr::separate("x", c("Date", "pos"), ",") %>%
  dplyr::transmute(Date = lubridate::as_date(zoo::index(ReturnsPar[(nrow(ReturnsPar)-foreLength):nrow(ReturnsPar),])),
                   pos = as.numeric(pos))

for_Parxts <- xts::xts(fore_Par$pos[1:(nrow(fore_Par)-1)],
                    order.by = fore_Par$Date[1:(nrow(fore_Par)-1)])

# Create the ARIMA+GARCH returns
Intersect_Par = merge(for_Parxts, ReturnsPar, all = FALSE)
ArimaGarchReturns_Par = Intersect_Par[,1] * Intersect_Par[,2]

# Create the backtests for ARIMA+GARCH and Buy & Hold
ArimaGarchCurve_Par = exp( cumsum( ArimaGarchReturns_Par )) - 1
BuyHoldCurve_Par = exp( cumsum( Intersect_Par[,2] )) - 1
# ArimaGarchCurve_Par = cumsum( ArimaGarchReturns_Par )
# BuyHoldCurve_Par = cumsum( Intersect_Par[,2] )
CombinedCurve_Par = merge( ArimaGarchCurve_Par, BuyHoldCurve_Par, all=F )

# Plot the equity curves
lattice::xyplot(
  CombinedCurve_Par,
  superpose=T,
  col=c("darkred", "darkblue"),
  lwd=2,
  key=list(
    text=list(
      c("ARIMA+GARCH", "Buy & Hold")
    ),
    lines=list(
      lwd=2, col=c("darkred", "darkblue")
    )
  )
)

