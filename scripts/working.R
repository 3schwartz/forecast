# library(xts)
# library(qrmdata)
# library(rmgarch)
library(dplyr)
library(ggplot2)
library(gganimate)
library(rmgarch)
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
) # 652.21 s / 785.50 s / 653 s
system.time(
DCC_fore_Par(Period = Period, foreLength = foreLength, windowLength = windowLength,
             ARIMAfit = FALSE, outDir = "./output/", version = "v1",
             Name = Name, tickers = tickers)
) # 515.98 s / 583.47 s
system.time(
VAR_fore_Par(Period = Period, foreLength = foreLength, windowLength = windowLength,
                         outDir = "./output/", version = "v1",
                         Name = Name, tickers = tickers)
) # 25.14 s / 11.85
system.time(
ARIMAGACH_fore_Par(Period = Period[, Name], foreLength = foreLength, windowLength = windowLength,
                               ARIMAfit = TRUE,
                               outDir = "./output/", version = "v1")
) # 112.92 s / 91.42 s

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
            `Buy and Hold` = pull(CopMul, Name),
            Cop = for_Copxts) %>%
  mutate(DCC = pull(DCCMul, for_DCCxts),
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

p <- ggplot(All.Mul.tidy.frame, aes(x = Date, y = Return, color = Strategy)) +
  geom_line(size = 0.85) +
  ggtitle("Backtest of financial strategies") +
  scale_color_manual(values = c(RColorBrewer::brewer.pal(length(unique(All.Mul.tidy.frame$Strategy)),
                                                         "Set1"))) +
  transition_manual(Frame) +
  # ease_aes('linear') +
  theme_classic() +
  theme(plot.title = element_text(size = 20, face = "bold"))

animate(p, fps = 10, renderer = gifski_renderer(loop = FALSE),
        width = 800, height = 400)

anim_save("./output/animation_Mul.gif")


# CopRaw <- readr::read_csv("./output/forecasts_Cop_v1.csv")
# DCCRaw <- readr::read_csv("./output/forecasts_DCC_v1.csv")
# VARRaw <- readr::read_csv("./output/forecasts_VAR_v1.csv")
# AGRaw <- readr::read_csv("./output/forecasts_AG_v1.csv")
# cbind(CopRaw[,1], DCCRaw[,1], VARRaw[,1], AGRaw[,1]) %>%  View()
