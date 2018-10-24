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

ReturnsCop = stocksdiff[-1,]

Cop_test_Par <- function(d, returnsCop = ReturnsCop, wL = windowLength, ARIMAfit = FALSE) {
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

  # A model with a changing copula
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
# CopSim_test_Par <- function(d, returnsCop = ReturnsCop,
#                             wL = windowLength, Copfit = forecastsCop) {
#   ReturnsOffset = returnsCop[(1+d):(wL+d),]
#   x = Copfit[[d+1]]
#
#   if(any(class(x) != "cGARCHfit")) {
#     return(rep(1, ncol(ReturnsOffset)))
#   } else {
#     preR = last(rmgarch::rcor(x),1)[,,1]
#     diag(preR) = 1
#     # maxGO = x@model$maxgarchOrder
#     sim1 = rmgarch::cgarchsim(x, n.sim = 1, n.start = 0, m.sim = 1000,
#                               startMethod = "sample", preR = preR, rseed = 1:1000)
#     sim = list()
#     for(z in 1:ncol(sim1@msim$simX[[1]])) {
#       sim[[z]] <- mean(sapply(sim1@msim$simX, FUN = function(x) x[,z]))
#     }
#     simmean = do.call(c, sim)
#     return(as.numeric(ifelse(simmean < 0, -1, 1)))
#   }
# }
windowLength = 100
foreLength = nrow(ReturnsCop["2018/"]) - windowLength
Period = ReturnsCop["2018/"]
Index = zoo::index(Period[(nrow(Period)-foreLength):nrow(Period),])
Name = NULL

# In parallel
require(parallel)
no_cores <- detectCores()
cl <- makeCluster(no_cores)
clusterEvalQ(cl, library(dplyr))
system.time(
  forecastsCop <- parLapply(cl, 0:foreLength, Cop_test_Par,
                            returnsCop = Period, wL = windowLength,
                            ARIMAfit = FALSE)
)

# stopCluster(cl)
# cl <- makeCluster(no_cores)
# clusterEvalQ(cl, library(dplyr))
# system.time(
#   foreSimCop <- parLapply(cl, 0:foreLength, CopSim_test_Par,
#                             returnsCop = Period, wL = windowLength,
#                           Copfit = forecastsCop)
# )
# stopCluster(cl)


# lapply(forecastsCop, function(x){any(class(x) == "cGARCHfit")})
# lapply(seq_along(x), function(y, n, i) { paste(n[[i]], y[[i]]) }, y=x, n=names(x))

system.time(
foreCopTest <- do.call(rbind, lapply(0:foreLength, function(returnsCop, wL, Copfit, d) {
  ReturnsOffset = returnsCop[(1+d):(wL+d),]
  x = Copfit[[d+1]]

  if(any(class(x) != "cGARCHfit")) {
    rep(1, ncol(Period))
  } else {
    preR = last(rmgarch::rcor(x),1)[,,1]
  diag(preR) = 1
  maxGO = x@model$maxgarchOrder
  sim1 = rmgarch::cgarchsim(x, n.sim = 1, n.start = 0, m.sim = 1000, presigma = tail(rugarch::sigma(x), maxGO),
                            startMethod = "sample", preR = preR, prereturns = tail( as.matrix(Period), maxGO),
                            preresiduals = tail(rugarch::residuals(x), maxGO),rseed = 1:1000,
                            cluster = cl)
  sim = list()
  for(z in 1:ncol(sim1@msim$simX[[1]])) {
    sim[[z]] <- mean(sapply(sim1@msim$simX, FUN = function(x) x[,z]))
  }
  simmean = do.call(c, sim)
  as.numeric(ifelse(simmean < 0, -1, 1))
  }}, returnsCop = Period, wL = windowLength,
  Copfit = forecastsCop))
)
stopCluster(cl)

forCopTest <- foreCopTest %>%
  dplyr::as_tibble()

readr::write_csv(forCopTest, path="./output/forecasts_Cop_mult.csv")

#### Univariate returns ####
Name = "AAPL"
fore_Cop <- readr::read_csv(file="./output/forecasts_Cop_mult.csv")
colnames(fore_Cop) <- paste0(tickers,".pos")

for_Cop <- fore_Cop %>%
  dplyr::transmute(Date = lubridate::as_date(Index),
                   pos = as.numeric(pull(fore_Cop, paste0(Name, ".pos"))))

for_Copxts <- xts::xts(for_Cop$pos[1:(nrow(for_Cop)-1)],
                       order.by = for_Cop$Date[1:(nrow(for_Cop)-1)])

# Create the ARIMA+GARCH returns
Intersect_Cop = merge(for_Copxts, ReturnsCop[,Name], all = FALSE)
ArimaGarchReturns_Cop = Intersect_Cop[,1] * Intersect_Cop[,2]

# Create the backtests for ARIMA+GARCH and Buy & Hold
ArimaGarchCurve_Cop = exp( cumsum( ArimaGarchReturns_Cop )) - 1
BuyHoldCurve_Cop = exp( cumsum( Intersect_Cop[,2] )) - 1
CombinedCurve_Cop = merge( ArimaGarchCurve_Cop, BuyHoldCurve_Cop, all=F )

# Plot the equity curves
lattice::xyplot(
  CombinedCurve_Cop,
  superpose=T,
  col=c("darkred", "darkblue"),
  lwd=2,
  key=list(
    text=list(
      c("Cop", "Buy & Hold")
    ),
    lines=list(
      lwd=2, col=c("darkred", "darkblue")
    )
  )
)

#### Multivariate returns ####
fore_Copmul <- readr::read_csv(file="./output/forecasts_Cop_mult.csv")
colnames(fore_Copmul) <- paste0(tickers,".pos")

for_Copmul <- fore_Copmul %>%
  dplyr::mutate(Date = lubridate::as_date(Index))

for_Copmulxts <- xts::xts(for_Copmul[1:(nrow(for_Copmul)-1), 1:(ncol(for_Copmul)-1)],
                          order.by = for_Copmul$Date[1:(nrow(for_Copmul)-1)])

# Create the ARIMA+GARCH returns
ArimaGarchReturns_Copmul = xts::xts(rowSums(for_Copmulxts * ReturnsCop[zoo::index(for_Copmulxts)] * (1/ncol(for_Copmulxts))),
                                    order.by = zoo::index(for_Copmulxts))
BuyHoldReturns_Copmul <- xts::xts(rowSums(ReturnsCop[zoo::index(for_Copmulxts)] * (1/ncol(for_Copmulxts))),
                                  order.by = zoo::index(for_Copmulxts))
# Create the backtests for ARIMA+GARCH and Buy & Hold
ArimaGarchCurve_Copmul = exp( cumsum( ArimaGarchReturns_Copmul )) - 1
BuyHoldCurve_Copmul = exp( cumsum( BuyHoldReturns_Copmul )) - 1
CombinedCurve_Copmul = merge( ArimaGarchCurve_Copmul, BuyHoldCurve_Copmul, all=F )

# Plot the equity curves
lattice::xyplot(
  CombinedCurve_Copmul,
  superpose=T,
  col=c("darkred", "darkblue"),
  lwd=2,
  key=list(
    text=list(
      c("Copmul", "Buy & Hold")
    ),
    lines=list(
      lwd=2, col=c("darkred", "darkblue")
    )
  )
)
