library(dplyr)
library(tidyr)

getSymbols("^GSPC", from="1950-01-01")

spReturns = diff(log(Cl(GSPC)))
spReturns[as.character(head(index(Cl(GSPC)),1))] = 0
Returns <- spReturns["2016/"]

windowLength = 500
foreLength = length(Returns) - windowLength
forecasts <- vector(mode="character", length=foreLength)
forenum <- numeric(foreLength)
foreDate <- character(foreLength)

quantLoop <- function(){
  for (d in 0:foreLength) {
  # Obtain rolling window for this period
  ReturnsOffset = Returns[(1+d):(windowLength+d)]

  Order = forecast::auto.arima(ReturnsOffset) %>%
    as.character() %>%
    stringr::str_extract_all("(?<=\\().*(?=\\))") %>%
    stringr::str_split_fixed(",", 3) %>%
    as.numeric()

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
    forecasts[d+1] = paste(index(ReturnsOffset[windowLength]), 1, sep=",")
    forenum[d+1] <- 1
    foreDate[d+1] <- as.character(index(ReturnsOffset[windowLength]))

    # print(paste(zoo::index(ReturnsOffset[windowLength]), 1, sep=","))
  } else {
    fore = rugarch::ugarchforecast(fit, n.ahead=1)
    ind = fore@forecast$seriesFor
    forecasts[d+1] = paste(colnames(ind), ifelse(ind[1] < 0, -1, 1), sep=",")
    forenum[d+1] <- ifelse(ind[1] < 0, -1, 1)
    foreDate[d+1] <- colnames(ind)

    # print(paste(colnames(ind), ifelse(ind[1] < 0, -1, 1), sep=","))
  }
  }
  write.csv(forecasts, file="./output/fore_Loop.csv", row.names=FALSE)
}
system.time(
  quantLoop()
)

foreCsv <- readr::read_csv("./output/fore_Loop.csv")

fore_Loop <- foreCsv %>%
  tidyr::separate(x, c("Date", "pos"), ",") %>%
  dplyr::transmute(Date = lubridate::as_date(Date),
            pos = as.numeric(pos))

for_Loopxts <- xts::xts(fore_Loop$pos[1:(nrow(fore_Loop)-1)],
                    order.by = fore_Loop$Date[1:(nrow(fore_Loop)-1)])

# Create the ARIMA+GARCH returns
Intersect_Loop = merge(for_Loopxts, Returns, all = FALSE)
ArimaGarchReturns_Loop = Intersect_Loop[,1] * Intersect_Loop[,2]

# Create the backtests for ARIMA+GARCH and Buy & Hold
ArimaGarchCurve_Loop = log( cumprod( 1 + ArimaGarchReturns_Loop ) )
BuyHoldCurve_Loop = log( cumprod( 1 + Intersect_Loop[,2] ) )
CombinedCurve_Loop = merge( ArimaGarchCurve_Loop, BuyHoldCurve_Loop, all=F )

# Plot the equity curves
lattice::xyplot(
  CombinedCurve_Loop,
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

