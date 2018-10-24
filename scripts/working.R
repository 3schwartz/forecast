# library(xts)
# library(qrmdata)
# library(rmgarch)
library(dplyr)
library(ggplot2)
library(gganimate)
library(dcEvent)

## Load some real data
tickers <- c("AAPL", "AMZN", "MSFT")
stocks <- lapply(tickers, quantmod::getSymbols , auto.assign = FALSE)
stocksAd <- do.call(cbind, lapply(stocks, function(x) {
  quantmod::Ad(x)
}))
stocksdiff = diff(log(stocksAd))
colnames(stocksdiff) <- tickers

stocksdiff[1,] = 0
Returns = stocksdiff

windowLength = 100
foreLength = nrow(Returns["2018/"]) - windowLength
Period = Returns["2018/"]
# Index = zoo::index(Period[(nrow(Period)-foreLength):nrow(Period),])
Name = "AAPL"

# nrow(Period) = 204, foreLength = 104, windowLength = 100
system.time(
Cop_sim_Par(Period = Period, foreLength = foreLength, windowLength = windowLength,
            ARIMAfit = FALSE, m.sim = 1000,
            outDir = "./output/", version = "v1",
            Name = Name, tickers = tickers)
) # 652.21 s
system.time(
DCC_fore_Par(Period = Period, foreLength = foreLength, windowLength = windowLength,
             ARIMAfit = FALSE, outDir = "./output/", version = "v1",
             Name = Name, tickers = tickers)
) # 515.98
system.time(
VAR_fore_Par(Period = Period, foreLength = foreLength, windowLength = windowLength,
                         outDir = "./output/", version = "v1",
                         Name = Name, tickers = tickers)
) # 25.14
system.time(
ARIMAGACH_fore_Par(Period = Period[, Name], foreLength = foreLength, windowLength = windowLength,
                               ARIMAfit = TRUE,
                               outDir = "./output/", version = "v1")
) # 112.92

#### Uni ####
Cop <- readr::read_csv("./output/forecasts_Cop_Uni_v1.csv")
DCC <- readr::read_csv("./output/forecasts_DCC_Uni_v1.csv")
VAR <- readr::read_csv("./output/forecasts_VAR_Uni_v1.csv")
AG <- readr::read_csv("./output/forecasts_AG_Uni_v1.csv")

All <- Cop %>%
  transmute(Date = Date,
            `Buy and Hold` = pull(Cop, Name),
            Cop = for_Copxts) %>%
  mutate(DCC = pull(DCC, for_DCCxts),
         VAR = pull(VAR, for_VARxts),
         AG = pull(AG, for_Parxts))

All.tidy <- tidyr::gather(All, -Date, key = "Strategy", value = "Return")

ggplot(All.tidy, aes(x = Date, y = Return, color = Strategy)) +
  geom_line(size = 1) +
  ggtitle("Backtest of financial strategies") +
  theme_classic()

#### Multi ####

CopMul <- readr::read_csv("./output/forecasts_Cop_Mul_v1.csv")
DCCMul <- readr::read_csv("./output/forecasts_DCC_Mul_v1.csv")
VARMul <- readr::read_csv("./output/forecasts_VAR_Mul_v1.csv")

All.Mul <- CopMul %>%
  transmute(Date = Date,
            `Buy and Hold` = pull(CopMul, Name),
            Cop = for_Copxts) %>%
  mutate(DCC = pull(DCCMul, for_DCCxts),
         VAR = pull(VARMul, ArimaGarchCurve_VAR))

All.tidy.Mul <- tidyr::gather(All.Mul, -Date, key = "Strategy", value = "Return")

ggplot(All.tidy.Mul, aes(x = Date, y = Return, color = Strategy)) +
  geom_line() +
  ggtitle("Backtest of financial strategies") +
  theme_classic()


# CopRaw <- readr::read_csv("./output/forecasts_Cop_v1.csv")
# DCCRaw <- readr::read_csv("./output/forecasts_DCC_v1.csv")
# VARRaw <- readr::read_csv("./output/forecasts_VAR_v1.csv")
# AGRaw <- readr::read_csv("./output/forecasts_AG_v1.csv")
# cbind(CopRaw[,1], DCCRaw[,1], VARRaw[,1], AGRaw[,1]) %>%  View()

listAll <- list()
for(i in 1:nrow(All)){
  listAll[[i]] <- All[1:i,] %>%
    mutate(Frame = i)
}

All.tidy.frame <- tidyr::gather(do.call(bind_rows, listAll),
                                -Date, -Frame, key = "Strategy", value = "Return")

p <- ggplot(All.tidy.frame, aes(x = Date, y = Return, color = Strategy)) +
  geom_line(size = 0.85) +
  ggtitle("Backtest of financial strategies") +
    transition_manual(Frame) +
  # ease_aes('linear') +
  theme_classic()

animate(p, fps = 5, renderer = gifski_renderer(loop = F))
