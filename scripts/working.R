# library(xts)
# library(qrmdata)
library(dplyr)
library(ggplot2)
library(gganimate)
# library(rmgarch)
library(dcEvent)

# ## Load some real data
# tickers <- c("AAPL", "AMZN", "MSFT")
# stocks <- lapply(tickers, quantmod::getSymbols , auto.assign = FALSE)
# stocksAd <- do.call(cbind, lapply(stocks, function(x) {
#   quantmod::Ad(x)
# }))
# stocksdiff = diff(log(stocksAd))
# colnames(stocksdiff) <- tickers
#
# stocksdiff[1,] = 0
# Returns = stocksdiff

# nrow(Period) = 204, foreLength = 104, windowLength = 100
system.time(
  Cop_sim_Par(Period = Period, foreLength = foreLength, windowLength = windowLength,
              ARIMAfit = FALSE, m.sim = 1000,
              outDir = "./output/", version = "v1",
              Name = Name, tickers = tickers)
) # 652.21 s / 785.50 s / 653 s / 583.70 s / 802.46 s
system.time(
  DCC_fore_Par(Period = Period, foreLength = foreLength, windowLength = windowLength,
               ARIMAfit = FALSE, outDir = "./output/", version = "v1",
               Name = Name, tickers = tickers)
) # 515.98 s / 583.47 s / 493.63 s / 529.64 s
system.time(
  VAR_fore_Par(Period = Period, foreLength = foreLength, windowLength = windowLength,
               outDir = "./output/", version = "v1",
               Name = Name, tickers = tickers)
) # 25.14 s / 11.85 s / 10.44 s / 15.39 s


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

sharpe <- All %>%
  select(-Date) %>%
  apply(2, tseries::sharpe, scale = sqrt(250))
SD <- All %>%
  select(-Date) %>%
  apply(2, function(x) {sd(x) * sqrt(250)})

stats <- cbind(sharpe, SD) %>%
  t() %>%
  as_tibble()

readr::write_csv(stats, path = "./output/stats.csv")

All.tidy <- tidyr::gather(All, -Date, key = "Strategy", value = "Return")

listAll <- list()
for(i in 1:nrow(All)){
  listAll[[i]] <- All[1:i,] %>%
    mutate(Frame = i)
}

All.tidy.frame <- tidyr::gather(do.call(bind_rows, listAll),
                                -Date, -Frame, key = "Strategy", value = "Return")
readr::write_csv(All.tidy.frame, path = "./output/Alltidyframe.csv")

ggplot(All.tidy.frame, aes(x = Date, y = Return, color = Strategy)) +
  geom_line(size = 0.85) +
  ggtitle("Backtest of financial strategies") +
  scale_color_manual(values = c(RColorBrewer::brewer.pal(length(unique(All.tidy.frame$Strategy)),
                                                         "Set1"))) +
  theme_classic() +
  theme(plot.title = element_text(size = 20, face = "bold"))

ggsave(filename = "animate.pdf", path = "./output/",
       width = 8, height = 4, device = "pdf")

p <- ggplot(All.tidy.frame, aes(x = Date, y = Return, color = Strategy)) +
  geom_line(size = 0.85) +
  ggtitle("Backtest of financial strategies") +
  scale_color_manual(values = c(RColorBrewer::brewer.pal(length(unique(All.tidy.frame$Strategy)),
                                                         "Set1"))) +
  transition_manual(Frame) +
  # ease_aes('linear') +
  theme_classic() +
  theme(plot.title = element_text(size = 20, face = "bold"))

animate(p, fps = 10, renderer = gifski_renderer(loop = FALSE),
        width = 800, height = 400)

anim_save("./output/animation.gif")

#### Multi ####

CopMul <- readr::read_csv("./output/forecasts_Cop_Mul_v1.csv")
DCCMul <- readr::read_csv("./output/forecasts_DCC_Mul_v1.csv")
VARMul <- readr::read_csv("./output/forecasts_VAR_Mul_v1.csv")

All.Mul <- CopMul %>%
  transmute(Date = Date,
            `Buy and Hold` = BuyHoldCurve_Copmul,
            Cop = ArimaGarchCurve_Copmul) %>%
  mutate(DCC = pull(DCCMul, ArimaGarchCurve_DCCmul),
         VAR = pull(VARMul, ArimaGarchCurve_VAR))

sharpe <- All.Mul %>%
  select(-Date) %>%
  apply(2, tseries::sharpe, scale = sqrt(250))
SD <- All.Mul %>%
  select(-Date) %>%
  apply(2, function(x) {sd(x) * sqrt(250)})

stats.Mul <- cbind(sharpe, SD) %>%
  t() %>%
  as_tibble()

readr::write_csv(stats.Mul, path = "./output/stats_Mul.csv")

All.Mul.tidy <- tidyr::gather(All.Mul, -Date, key = "Strategy", value = "Return")

listAll.Mul <- list()
for(i in 1:nrow(All.Mul)){
  listAll.Mul[[i]] <- All.Mul[1:i,] %>%
    mutate(Frame = i)
}

All.Mul.tidy.frame <- tidyr::gather(do.call(bind_rows, listAll.Mul),
                                    -Date, -Frame, key = "Strategy", value = "Return")
readr::write_csv(All.Mul.tidy.frame, path = "./output/All_Mul_tidy_frame.csv")

ggplot(All.Mul.tidy.frame, aes(x = Date, y = Return, color = Strategy)) +
  geom_line(size = 0.85) +
  ggtitle("Backtest of financial strategies") +
  scale_color_manual(values = c(RColorBrewer::brewer.pal(length(unique(All.Mul.tidy.frame$Strategy)),
                                                         "Set1"))) +
  theme_classic() +
  theme(plot.title = element_text(size = 20, face = "bold"))

ggsave(filename = "animate_Mul.pdf", path = "./output/",
       width = 8, height = 4, device = "pdf")

p_mul <- ggplot(All.Mul.tidy.frame, aes(x = Date, y = Return, color = Strategy)) +
  geom_line(size = 0.85) +
  ggtitle("Backtest of financial strategies") +
  scale_color_manual(values = c(RColorBrewer::brewer.pal(length(unique(All.Mul.tidy.frame$Strategy)),
                                                         "Set1"))) +
  transition_manual(Frame) +
  # ease_aes('linear') +
  theme_classic() +
  theme(plot.title = element_text(size = 20, face = "bold"))

animate(p_mul, fps = 10, renderer = gifski_renderer(loop = FALSE),
        width = 800, height = 400)

anim_save("./output/animation_Mul.gif")


# CopRaw <- readr::read_csv("./output/forecasts_Cop_v1.csv")
# DCCRaw <- readr::read_csv("./output/forecasts_DCC_v1.csv")
# VARRaw <- readr::read_csv("./output/forecasts_VAR_v1.csv")
# AGRaw <- readr::read_csv("./output/forecasts_AG_v1.csv")
# cbind(CopRaw[,1], DCCRaw[,1], VARRaw[,1], AGRaw[,1]) %>%  View()


#### Case ####
modeldataNew <- readr::read_delim("C:/Users/Soren Schwartz/Desktop/devent/forecasting_data.csv", delim = ";")
modeldatathirten <- readr::read_delim("C:/Users/Soren Schwartz/Desktop/devent/thirten.csv", delim = ";")

lubridate::dmy(modeldata$DeliveryDate)

hour1 <- modeldata %>%
  filter(DeliveryHour == 1)
hour2 <- modeldata %>%
  filter(DeliveryHour == 2)
hour3 <- modeldata %>%
  filter(DeliveryHour == 3)
hour4 <- modeldata %>%
  filter(DeliveryHour == 4)
hour5 <- modeldata %>%
  filter(DeliveryHour == 5)
hour6 <- modeldata %>%
  filter(DeliveryHour == 6)
hour7 <- modeldata %>%
  filter(DeliveryHour == 7)
hour8 <- modeldata %>%
  filter(DeliveryHour == 8)
hour9 <- modeldata %>%
  filter(DeliveryHour == 9)
hour10 <- modeldata %>%
  filter(DeliveryHour == 10)
hour11 <- modeldata %>%
  filter(DeliveryHour == 11)
hour12 <- modeldata %>%
  filter(DeliveryHour == 12)
hour13 <- modeldata %>%
  filter(DeliveryHour == 13)
hour14 <- modeldata %>%
  filter(DeliveryHour == 14)
hour15 <- modeldata %>%
  filter(DeliveryHour == 15)
hour16 <- modeldata %>%
  filter(DeliveryHour == 16)
hour17 <- modeldata %>%
  filter(DeliveryHour == 17)
hour18 <- modeldata %>%
  filter(DeliveryHour == 18)
hour19 <- modeldata %>%
  filter(DeliveryHour == 19)
hour20 <- modeldata %>%
  filter(DeliveryHour == 20)
hour21 <- modeldata %>%
  filter(DeliveryHour == 21)
hour22 <- modeldata %>%
  filter(DeliveryHour == 22)
hour23 <- modeldata %>%
  filter(DeliveryHour == 23)
hour24 <- modeldata %>%
  filter(DeliveryHour == 24)

hourUse <- xts::xts(hour1 %>%
                      select(DEPrem, DEWindFCDelta, DEConsDelta),
                    order.by = lubridate::dmy(hour1$DeliveryDate))

modeldataNew %>%
  select(DeliveryHour, DEPrem) %>%
  tail()

Use <- xts::xts(modeldataNew %>%
                  select(DEPrem, DEWindFCDelta, DEConsDelta),
                order.by = lubridate::dmy(modeldataNew$DeliveryDate))
Use[-unique(which(is.na(Use), arr.ind = TRUE)[,1]),]  %>%
  tail()



windowLength = 100
foreLength = nrow(hourUse["2018-05-25/"]) - windowLength
Period = hourUse["2018-05-25/"]
# Index = zoo::index(Period[(nrow(Period)-foreLength):nrow(Period),])
Name = "DEPrem"
tickers = c("DEPrem", "DEWindFCDelta", "DEConsDelta")

system.time(
  ARIMAGACH_fore_Par(Period = Period[, Name], foreLength = foreLength, windowLength = windowLength,
                     ARIMAfit = TRUE,
                     outDir = "./output/", version = "v1")
) # 112.92 s / 91.42 s / 80.68 s / 81.45 s

tradesCom <- c(readr::read_csv("C:/R/dcEvent/output/forecasts_AG_v1.csv")[12,],
               readr::read_csv("C:/R/dcEvent/output/forecasts_AG_v2.csv")[12,],
               readr::read_csv("C:/R/dcEvent/output/forecasts_AG_v3.csv")[12,],
               readr::read_csv("C:/R/dcEvent/output/forecasts_AG_v4.csv")[12,],
               readr::read_csv("C:/R/dcEvent/output/forecasts_AG_v5.csv")[12,],
               readr::read_csv("C:/R/dcEvent/output/forecasts_AG_v6.csv")[12,],
               readr::read_csv("C:/R/dcEvent/output/forecasts_AG_v7.csv")[12,],
               readr::read_csv("C:/R/dcEvent/output/forecasts_AG_v8.csv")[12,],
               readr::read_csv("C:/R/dcEvent/output/forecasts_AG_v9.csv")[12,],
               readr::read_csv("C:/R/dcEvent/output/forecasts_AG_v10.csv")[12,],
               readr::read_csv("C:/R/dcEvent/output/forecasts_AG_v11.csv")[12,],
               readr::read_csv("C:/R/dcEvent/output/forecasts_AG_v12.csv")[12,],
               readr::read_csv("C:/R/dcEvent/output/forecasts_AG_v13.csv")[12,],
               readr::read_csv("C:/R/dcEvent/output/forecasts_AG_v14.csv")[12,],
               readr::read_csv("C:/R/dcEvent/output/forecasts_AG_v15.csv")[12,],
               readr::read_csv("C:/R/dcEvent/output/forecasts_AG_v16.csv")[12,],
               readr::read_csv("C:/R/dcEvent/output/forecasts_AG_v17.csv")[12,],
               readr::read_csv("C:/R/dcEvent/output/forecasts_AG_v18.csv")[12,],
               readr::read_csv("C:/R/dcEvent/output/forecasts_AG_v19.csv")[12,],
               readr::read_csv("C:/R/dcEvent/output/forecasts_AG_v20.csv")[12,],
               readr::read_csv("C:/R/dcEvent/output/forecasts_AG_v21.csv")[12,],
               readr::read_csv("C:/R/dcEvent/output/forecasts_AG_v22.csv")[12,],
               readr::read_csv("C:/R/dcEvent/output/forecasts_AG_v23.csv")[12,],
               readr::read_csv("C:/R/dcEvent/output/forecasts_AG_v24.csv")[12,])

hej2 <- do.call(c, lapply(tradesCom, function(x){foo <- stringr::str_split(x, ",", 2)}))
foo2 <- as.numeric(do.call(c, lapply(hej2, function(x) {x[2]}))) %>%
  data.frame()

readr::write_csv(foo2, "C:/R/dcEvent/output/results13_Team_foo_v2.csv")

readr::read_csv("C:/R/dcEvent/output/results13_Team_foo_v2.csv") %>%
  View()
