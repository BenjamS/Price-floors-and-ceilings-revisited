library(tidyverse)
library(tidyquant)
#library(pracma)
#=============================================================================
# Define functions
effProdPrice <- function(P, pIntervene, tau, s, type, output){
  
  d1 <- log(P / pIntervene) / (s * sqrt(tau)) + s / 2 * sqrt(tau)
  d2 <- d1 - s * sqrt(tau)
  N1 <- pnorm(d1)
  N2 <- pnorm(d2)
  
  if(type == "floor"){
    PeffProd <- P * N1 + pIntervene * (1 - N2)
    
  }
  
  if(type == "ceiling"){
    PeffProd <- P * (1 - N1) + pIntervene * N2
  }
  
  if(output == "Peff"){
    out <- PeffProd
  }
  
  if(output == "N2"){
    out <- N2
  }
  
  if(output == "N1"){
    out <- N1
  }
  
  
  return(out)
}

#Implied volatility root fun
ivRootFn <- function(s, optP, P, strike, tau){
  Pfloor <- strike
  #slack <- exp(0.05 * tau) * optP - effProdPrice(P, Pfloor, tau, s) - Pfloor
  slack <- exp(0.05 * tau) * optP - effProdPrice(P, Pfloor, tau, s, type = "floor") + Pfloor
  return(slack)
}
#=============================================================================
#=============================================================================
#=============================================================================
# Wheat prices are lognormally distributed
# #av_api_key("HQBAWJK4Y3YW81VG")
# stock_symbol <- "KE=F"
# tbl_ohlcv <- stock_symbol %>%
#   tq_get(get = "stock.prices", from = "2000-01-01", to = "2011-07-01")
#   # tq_get(get = "stock.prices", av_fun = "TIME_SERIES_DAILY_ADJUSTED", outputsize = "full")
# df_ohlcv1 <- as.data.frame(tbl_ohlcv)
# tbl_ohlcv <- stock_symbol %>%
#   tq_get(get = "stock.prices", from = "2011-07-01")
# df_ohlcv2 <- as.data.frame(tbl_ohlcv)
# # This is from yahoo finance, only the december contract given (kez).
# # Possible alternative data source: http://www.eoddata.com/stockquote/KCBT/KWN15.htm?AspxAutoDetectCookieSupport=1
# # Contract specs:
# # Contract is for delivery of 5000 bushels
# # https://www.cmegroup.com/markets/agriculture/grains/kc-wheat.contractSpecs.html
# df_ohlcv <- as.data.frame(rbind(df_ohlcv1, df_ohlcv2))
# write.csv(df_ohlcv, "Wheat futures KEZ price 2000-2021.csv", row.names = F)
#---------------------------------------------------------------------------
# this_folder <- "C:/Users/bensc/OneDrive/Documents/A new price floor model/"
# this_file <- "Wheat futures KEZ price 2000-2021.csv"
# this_filePath <- paste0(this_folder, this_file)
# df <- read.csv(this_filePath, stringsAsFactors = F)
# colnames(df)[ncol(df)] <- "Price"
# colnames(df)[1] <- "Date"
# df$Date <- as.Date(df$Date)
# df <- df[-which(is.na(df$Price)), ]
# gg <- ggplot(df, aes(x = Date, y = Price, group = 1))
# gg <- gg + geom_line()
# gg
# 
# df_hist <- subset(df[, c("Date", "Price")], Date > "2014-09-01" &
#                Date < "2022-08-31")
# #---
# hist(df$Price, breaks = 10)
# #---
# df2 <- subset(df[, c("Date", "Price")], Date < "2014-09-01")
# #---
# hist(df2$Price, breaks = 10)
# #---
# 
# 
# # point_size <- 1.5
# # smallPoint_size <- 1
# # label_size <- 2.5
# # smallLabel_size <- 2
# title_size <- 8
# subtitle_size <- 7
# # legendText_size <- 7
# axisText_size <- 6
# axisTitle_size <- 7
# # facetTitle_size <- 7
# 
# 
# 
# 
# df_plot <- df
# gg <- ggplot(df_plot, aes(x = Date, y = Price))
# gg <- gg + geom_line()
# gg
# 
# 
# df_plot <- subset(df, Date > "2016-09-01" &
#                     Date < "2017-08-01")
# out <- polyfit(1:nrow(df_plot), df_plot$Price)
# slope <- round(out[1], 2)
# s <- round(sd(diff(log(df_plot$Price)), na.rm = T), 2) # p. 326 Hull 9th edition
# 
# this_title <- "2016/17"
# this_subtitle <- paste0("Drift = ", slope, ", Volatility = ", s)
# gg <- ggplot(df_plot, aes(x = Date, y = Price))
# gg <- gg + geom_line()
# gg <- gg + geom_smooth(aes(group = NULL, fill = NULL, shape = NULL), method = lm, se = F)
# gg <- gg + labs(title = this_title, subtitle = this_subtitle)
# gg <- gg + theme(plot.title = element_text(size = title_size),
#                  plot.subtitle = element_text(size = subtitle_size),
#                  axis.title = element_text(size = axisTitle_size),
#                  axis.text = element_text(size = axisText_size))
# gg
# #hist(df_plot$Price)
# 
# 
# 
# df_plot <- subset(df, Date > "2017-09-01" &
#                Date < "2018-08-01")
# out <- polyfit(1:nrow(df_plot), df_plot$Price)
# slope <- round(out[1], 2)
# s <- round(sd(diff(log(df_plot$Price)), na.rm = T), 2) # p. 326 Hull 9th edition
# 
# this_title <- "2017/18"
# this_subtitle <- paste0("Drift = ", slope, ", Volatility = ", s)
# gg <- ggplot(df_plot, aes(x = Date, y = Price))
# gg <- gg + geom_line()
# gg <- gg + geom_smooth(aes(group = NULL, fill = NULL, shape = NULL), method = lm, se = F)
# gg <- gg + labs(title = this_title, subtitle = this_subtitle)
# gg <- gg + theme(plot.title = element_text(size = title_size),
#                  plot.subtitle = element_text(size = subtitle_size),
#                  axis.title = element_text(size = axisTitle_size),
#                  axis.text = element_text(size = axisText_size))
# gg
# #hist(df_plot$Price)
# 
# 
# df_plot <- subset(df, Date > "2018-09-01" &
#                     Date < "2019-08-01")
# out <- polyfit(1:nrow(df_plot), df_plot$Price)
# slope <- round(out[1], 2)
# s <- round(sd(diff(log(df_plot$Price)), na.rm = T), 2) # p. 326 Hull 9th edition
# 
# this_title <- "2018/19"
# this_subtitle <- paste0("Drift = ", slope, ", Volatility = ", s)
# gg <- ggplot(df_plot, aes(x = Date, y = Price))
# gg <- gg + geom_line()
# gg <- gg + geom_smooth(aes(group = NULL, fill = NULL, shape = NULL), method = lm, se = F)
# gg <- gg + labs(title = this_title, subtitle = this_subtitle)
# gg <- gg + theme(plot.title = element_text(size = title_size),
#                  plot.subtitle = element_text(size = subtitle_size),
#                  axis.title = element_text(size = axisTitle_size),
#                  axis.text = element_text(size = axisText_size))
# gg
# 
# # hist(df_plot$Price)
# 
# 
# 
# 
# # legend.position = "bottom",
# # legend.spacing.x = unit(0.25, 'cm'),
# # legend.title = element_blank(),
# # legend.text = element_text(size = legendText_size),
# # Resembles geometric brownian motion, but be careful...
# # Not safe to assume last year's drift rate will be same as next year's.
# # Wheat price drift rate completely reversed in 2018-19
# df_plot <- subset(df[, c("date", "p")], date > "2018-07-31" &
#                     date < "2019-09-01")
# df_plot$dt <- pracma::detrend(df_plot$p)
# s2 <- sd(diff(log(df_plot$p))) # p. 326 Hull 9th edition
# df_plot <- df_plot %>% gather(Type, Value, p:dt) 
# gg <- ggplot(df_plot, aes(x = date, y = Value))
# gg <- gg + geom_line()
# gg <- gg + facet_wrap(~Type, ncol = 1, scales = "free_y")
# gg
# 
# 
# # tsStart <- c(year(df_plot$date)[1], yday(df_plot$date)[1])
# # df_plot$adjusted <- na.approx(df_plot$adjusted)
# # this_ts <- ts(df_plot$adjusted, start = tsStart, frequency = 52)
# # #plot.ts(this_ts)
# # ts_decomp <- this_ts %>%
# #   decompose(type = "additive") #"additive" or "multiplicative"
# # #plot(ts_decomp)
# #=============================================================================
# 
# #Get implied volatility
# 
# # File "Wheat Data-Recent.xls"
# # Worksheet "WheatYearbookTable01":
# # 2018/2019 HRW production: 662.25 million bushels at 39.1 bushels/acre, Avg. price $5.01/bushel
# # Worksheet "WheatYearbookTable19":
# # July 2019 Kansas City price: $5.56/bushel
# 
# # File "futmodwheat-2018-19.xlsx", "Data" worksheet, Table 12:
# # September 2018 WASDE SAP forecast for 2019: $5.10/bushel, avg over all classes
# # Table 13, same worksheet, gives the price floors. For MY 2019/2020, it's $3.38
# # (Mkting year (MY) befins in June and ends in May)
# 
# P_vec <- seq(1.5, 10, length.out = 40)
# Pfloor <- 3.38 #2019/20 For 2018/19 and several yrs prior it was 2.94
# tau <- 10 * 21 / 252 # Assume planting in October and harvest & sale in July
# #(num. months * (num. trading days in month) / (Number of trading days in year) See Hull)
# # Solve for implied volatility based on KCHRW July option price
# # optP <- 7.06
# # P <- 6.20
# # strike <- P
# # this_interval <- c(0, 10000)
# # out <- rootSolve::uniroot.all(ivRootFn, interval = this_interval,
# #                               lower = min(this_interval),
# #                               upper = max(this_interval),
# #                               optP = optP,
# #                               P = P,
# #                               strike = strike,
# #                               tau = tau)
# # s <- out
# 
# Pforecast <- 5.60
# Pfloor <- 2.94
# cv <- 0.36
# s <- sqrt(log(1 + cv^2) / tau)
# Peff <- effProdPrice(Pforecast, Pfloor, tau, s, type = "floor")
# Qe <- 662.25
# Pe_actual <- 6.20
# 
# eta_D <- -0.4
# eta_S <- 0.55
# theta_S <- Qe / Peff^eta_S
# theta_D <- Qe / Pe_actual^eta_D
# # theta_S <- 300.3
# # theta_D <- 800.5
# 
# Peff_vec <- effProdPrice(P_vec, Pfloor, tau, s, type = "floor")
# 
# QS0 <- theta_S * P_vec^eta_S
# QS <- theta_S * Peff_vec^eta_S
# QD <- theta_D * P_vec^eta_D
# 
# df_plot <- data.frame(Price = P_vec, QS0, QS, QD)
# df_plot <- df_plot %>% gather(Type, Quantity, QS0:QD)
# 
# gg <- ggplot(df_plot, aes(x = Quantity, y = Price, group = Type, color = Type))
# gg <- gg + geom_line(lwd = 1)
# gg <- gg + geom_hline(yintercept = Pfloor, linetype = "dashed")
# gg
# 
# 
# # Equilib price-qty curve
# rootfn <- function(Pe, Pfloor, tau, s, theta_D, theta_S, eta_S, eta_D){
#   Peff <- effProdPrice(Pe, Pfloor, tau, s)
#   lQS <- eta_S * log(Peff) + log(theta_S)
#   lQD <- eta_D * log(Pe) + log(theta_D)
#   slack <- lQS - lQD
#   return(slack)
# }
# 
# 
# P <- 4.51
# Pfloor_vec <- seq(0.1, 0.9999, length.out = 20) * P
# this_interval <- c(0, 50)
# 
# Pe_vec <- c()
# Qe_vec <- c()
# for(i in 1:length(Pfloor_vec)){
#   out <- rootSolve::uniroot.all(rootfn, interval = this_interval,
#                                 lower = min(this_interval),
#                                 upper = max(this_interval),
#                                 Pfloor = Pfloor_vec[i],
#                                 tau = tau,
#                                 s = s,
#                                 theta_D = theta_D,
#                                 theta_S = theta_S,
#                                 eta_S = eta_S,
#                                 eta_D = eta_D)
#   Pe <- out
#   Qe <- theta_D * Pe^eta_D
#   Pe_vec[i] <- Pe
#   Qe_vec[i] <- Qe
# }
# 
# df_plot <- data.frame(Pfloor = Pfloor_vec, Qe = Qe_vec, Pe = Pe_vec)
# gg <- ggplot(df_plot, aes(x = Qe, y = Pe, color = Pfloor / P))
# gg <- gg + geom_line(lwd = 1)
# gg
# 
# 
# # Miranda and Fackler problem
# #a <- 0.5 + 0.5 * Ep
# #Ey <- 1
# #q <- a * Ey
# #Ep <- 3 - 2 * q
# 
# 
# #==========================================================================
# 
# this_folder <- "C:/Users/bensc/OneDrive/Documents/Agricultural price floors and ceilings revisited/Price floor research/"
# this_file <- "Wheat Data-All Years.xls"
# this_filePath <- paste0(this_folder, this_file)
# df_p <- readxl::read_xls(this_filePath, sheet = "WheatYearbookTable19-Full")
# colnames(df_p) <- df_p[1, ]
# colnames(df_p)[2] <- "Year"
# df_p <- as.data.frame(df_p[2:53, c("Year", "Jul", "Aug")])
# df_p[, 2:3] <- apply(df_p[, 2:3], 2, as.numeric)
# df_p <- as.data.frame(df_p)
# df_p$`Jul-Aug` <- rowMeans(df_p[, 2:3])
# df_a <- readxl::read_xls(this_filePath, sheet = "WheatYearbookTable01-Full")
# colnames(df_a) <- df_a[1, ]
# colnames(df_a)[2] <- "Year"
# colnames(df_a)[ncol(df_a)] <- "Price"
# df_a <- df_a[160:262, c("Year", "Harvested acreage", "Production", "Yield", "Price")]
# df_a[, -1] <- as.data.frame(apply(df_a[, -1], 2, as.numeric))
# 
# df <- merge(df_a, df_p, by = "Year")
# df <- df[-nrow(df), ]
# df <- df[-which(is.na(df$`Harvested acreage`)), ]
# 
# ccf(df$`Harvested acreage`, df$`Jul-Aug`)
# df$P1 <- lag(df$`Jul-Aug`, 1)
# df$P5 <- lag(df$`Jul-Aug`, 5)
# df$P6 <- lag(df$`Jul-Aug`, 6)
# mod <- lm(`Harvested acreage` ~ P6, df)
# summary(mod)
# 
# df$lJulAug <- log(df$`Jul-Aug`)
# df$lA <- log(df$`Harvested acreage`)
# ccf(df$lJulAug, df$lA)
# df$lA6 <- lag(df$lA, 6)
# mod <- lm(lJulAug ~ lA6, df)
# summary(mod)
# 
# gg <- ggplot(df, aes(x = lA6, y = lJulAug, group = 1))
# gg <- gg + geom_point()
# gg
# 
# 
# ccf(df$Production, df$`Jul-Aug`)
# ccf(df$Yield, df$`Jul-Aug`)
# 
# 
# 
# n_d <- 1
# df$dA <- c(NA, diff(log(df$`Harvested acreage`, n_d)))
# df$dP <- c(NA, diff(log(df$`Jul-Aug`, n_d)))
# df$dY <- c(NA, diff(log(df$Yield, n_d)))
# df$dProd <- c(NA, diff(log(df$Production, n_d)))
# 
# df <- df[-c(1:n_d), ]
# ccf(df$dA, df$dP)
# df$dP1 <- lag(df$dP, 1)
# mod <- lm(dA ~ dP1, df)
# summary(mod)
# 
# 
# 
# df_plot <- df[, c("Year", "dP", "dA", "dY", "dProd")] %>%
#   gather(Type, Value, `Jul-Aug`:`Harvested acreage`)
# gg <- ggplot(df_plot, aes(x = Year, y = Value, group = 1))
# gg <- gg + geom_line()
# gg <- gg + facet_wrap(~Type, scales = "free_y")
# gg

#==========================================================================
#==========================================================================
#==========================================================================
#==========================================================================
library(lubridate)
library(tidyverse)
# Get the USDA WASDE September report MYA price forecast for each year
this_folder <- "C:/Users/bensc/OneDrive/Documents/Agricultural price floors and ceilings revisited/Price floor research/Historical USDA wheat MYA price forecasts/"
fileName_vec <- list.files(this_folder)
myaPforecast_vec <- c()
myaPfinal_vec <- c()
pubDate_vec <- c()
for(i in 1:length(fileName_vec)){
  this_file <- fileName_vec[i]
  print(this_file)
  this_filePath <- paste0(this_folder, this_file)
  df <- readxl::read_excel(this_filePath, sheet = "Data")
  df <- as.data.frame(df)
  df[, 1] <- as.character(df[, 1])
  ind <- grep("Table 12", df[, 1])[2]
  df <- df[ind:nrow(df), c(3, 5)]
  colnames(df) <- c("MYA forecast", "Publication date")
  df$`MYA forecast` <- as.numeric(df$`MYA forecast`)
  df$`Publication date` <- as.Date(as.numeric(df$`Publication date`), origin = "1899-12-30")
  ind_sept <- which(month(df$`Publication date`) == 9)[1]
  myaPforecast <- df$`MYA forecast`[ind_sept]
  ind_may <- which(month(df$`Publication date`) == 5)[2]
  myaPfinal <- df$`MYA forecast`[ind_may]
  myaPforecast_vec[i] <- myaPforecast
  myaPfinal_vec[i] <- myaPfinal
  pubDate_vec[i] <- df$`Publication date`[ind_sept]
}

pubDate_vec <- as.Date(as.numeric(pubDate_vec), origin = "1970-01-01")
pubDate_vec[which(year(pubDate_vec) == 2018)[2]] <- as.Date("2019-09-11")
df_myaPforecast <- data.frame(myaPforecast_vec, pubDate_vec, myaPfinal_vec)
colnames(df_myaPforecast) <- c("MYA price forecast", "Publication date",
                               "MYA price final")
df_myaPforecast$`HRW planting year` <- year(df_myaPforecast$`Publication date`)
#df_myaPforecast$MY <- 
# Note: Since the harvest and most of the marketing has taken place in July-Aug,
# the MYA price forecast published in the September WASDE report is not so much
# a forecast of next year's harvest price as it is a reflection of the
# previous harvest price, a fait acompli.
#----------------------------------------------------------------------------
# Get price specifically for HRW wheat in July-August
this_folder <- "C:/Users/bensc/OneDrive/Documents/Agricultural price floors and ceilings revisited/Price floor research/"
this_file <- "Wheat Data-All Years.xls"
this_filePath <- paste0(this_folder, this_file)
df <- readxl::read_excel(this_filePath, sheet = "WheatYearbookTable19-Full")
df <- as.data.frame(df)
#df[, 1] <- as.character(df[, 1])
colnames(df) <- df[1, ]
colnames(df)[2] <- "MY"
ind <- grep("2021", df$MY)[1]
df <- df[2:ind, c("MY", "Jul", "Aug")]
df[, -1] <- as.data.frame(apply(df[, -1], 2, as.numeric))
df$`HRW Jul-Aug avg. price` <- rowMeans(df[, 2:3])
colnames(df)[2:3] <- c("HRW Jul price", "HRW Aug price")
df$`HRW planting year` <- as.integer(gsub("/.*", "", df$MY)) - 1
df_pHRWJulAug <- df
#---------------------------------------------------------------------------
# Get MYA price for all wheat and hrw wheat, plus area, production, and yield data
df_raw <- readxl::read_excel(this_filePath, sheet = "WheatYearbookTable01-Full")
df_raw <- as.data.frame(df_raw)
colnames(df_raw) <- paste(df_raw[1, ], df_raw[2, ])
colnames(df_raw)[2] <- "MY"
df_raw <- df_raw[-c(1, 2), -1]
df_raw$`NA NA` <- NULL
colnames(df_raw)[ncol(df_raw)] <- "MYA price"
df_raw[, -1] <- as.data.frame(apply(df_raw[, -1], 2, as.numeric))
ind <- grep("2021", df_raw$MY)
df_allWheat <- df_raw[1:ind[1], ]
df_hrwWheat <- df_raw[(ind[1] + 2):ind[2], ]
# Note: Prior to 2002, the "MYA price" for HRW wheat is just a simple average,
# so not very useful.
colnames(df_allWheat)[-1] <- paste("All", colnames(df_allWheat)[-1])
colnames(df_hrwWheat)[-1] <- paste("HRW", colnames(df_hrwWheat)[-1])
df_wheat <- merge(df_allWheat, df_hrwWheat, by = "MY")
df_wheat <- merge(df_wheat, df_pHRWJulAug, by = "MY")
df_wheat$`HRW planting year` <- as.integer(gsub("/.*", "", df_wheat$MY)) - 1
#---------------------------------------------------------------------------
# # Look for relationships between HRW and All wheat
df_look <- subset(df_wheat, `HRW planting year` >= 1970)
df_look$pRatio <- df_look$`HRW Jul-Aug avg. price` / df_look$`All MYA price`
hist(df_look$pRatio)
df_look$dPratio <- c(NA, diff(log(df_look$pRatio)))
hist(df_look$dPratio)
df_look$prodRatio <- df_look$`HRW Production (million bushels)` / df_look$`All Production (million bushels)`
df_look$aRatio <- df_look$`HRW Harvested acreage (million acres)` / df_look$`All Harvested acreage (million acres)`
#df_look$aRatio <- df_look$`HRW Planted acreage (million acres)` / df_look$`All Planted acreage (million acres)`
hist(df_look$aRatio, 5)
hist(df_look$prodRatio)
hist(df_look$pRatio / df_look$aRatio)
ccf(df_look$pRatio, df_look$prodRatio)
# df_look$lpRatio <- log(df_look$pRatio)
# df_look$laRatio <- log(df_look$aRatio)
mod <- lm(pRatio ~ prodRatio, df_look)
summary(mod)
plot(mod$fitted.values, mod$residuals)
gg <- ggplot(df_look, aes(x = prodRatio, y = pRatio))
gg <- gg + geom_point()
#gg <- gg + scale_x_log10() + scale_y_log10()
gg
#---------------------------------------------------------------------------
# Get volatility s, m, tau
this_folder <- "C:/Users/bensc/OneDrive/Documents/Agricultural price floors and ceilings revisited/"
this_file <- "Wheat futures KEZ price 2000-2021.csv"
this_filePath <- paste0(this_folder, this_file)
df <- read.csv(this_filePath, stringsAsFactors = F)
colnames(df)[ncol(df)] <- "Price"
colnames(df)[1] <- "Date"
df$Date <- as.Date(df$Date)
df <- df[-which(is.na(df$Price)), ]
df$yr <- year(df$Date)
yr_vec <- unique(df$yr)
n_yrs <- length(yr_vec)
s_vec <- c()
m_vec <- c()
tau_vec <- c()
startCropYr_vec <- c()
endCropYr_vec <- c()
P0_vec <- c()
Ptau_vec <- c()
for(i in 1:n_yrs){
  this_yr <- yr_vec[i]
  startCropYr <- paste0(this_yr, "-09-12")
  endCropYr <- paste0(this_yr + 1, "-08-20")
  this_df <- subset(df[, c("Date", "Price")], Date > startCropYr &
                      Date < endCropYr)
  s <- sd(diff(log(this_df$Price))) # p. 326 Hull 9th edition
  m <- mean(diff(log(this_df$Price)))
  s_vec[i] <- s
  m_vec[i] <- m
  tau_vec[i] <- nrow(this_df)
  startCropYr_vec[i] <- startCropYr
  endCropYr_vec[i] <- endCropYr
  P0_vec[i] <- mean(this_df$Price[which(month(this_df$Date) %in% c(9))]) / 100
  Ptau_vec[i] <- mean(this_df$Price[which(month(this_df$Date) %in% c(7, 8))]) / 100
}

df_mstau <- data.frame(Year = yr_vec, startCropYr_vec,
                       endCropYr_vec, m = m_vec, s = s_vec,
                       tau = tau_vec, P0 = P0_vec,
                       Ptau = Ptau_vec)
colnames(df_mstau)[1:3] <- c("HRW planting year", "Planting date", "Harvest date")
df_mstau$mYr <- df_mstau$m * df_mstau$tau
df_mstau$sYr <- df_mstau$s * sqrt(df_mstau$tau)
df_mstau$muP <- df_mstau$P0 * exp(df_mstau$m * df_mstau$tau)
df_mstau <- df_mstau[-nrow(df_mstau), ]
#---------------------------------------------------------------------------
# How does the futures and theory based expected price muP compare to the
# MYA price forecast?
list_df <- list(df_mstau, df_wheat, df_myaPforecast)
df <- plyr::join_all(list_df, by = "HRW planting year")
df_compareForecast <- df[, c("MY", "HRW planting year",
                             "muP", "Ptau",
                             "MYA price forecast",
                             "HRW Jul-Aug avg. price",
                             "pRatio",
                             "All MYA price")]
# Compare geometric brownian movement forecast at planting
# to the futures price and KC price at harvest. Wow, good prediction!
df_plot <- df_compareForecast[, c("HRW planting year",
                                  "muP", "HRW Jul-Aug avg. price",
                                  "All MYA price", "Ptau")] %>%
  gather(Type, Value, muP:Ptau)
gg <- ggplot(df_plot, aes(x = `HRW planting year`, y = Value,
                                     group = Type, color = Type))
gg <- gg + geom_line()
gg
hist(df_compareForecast$Ptau / df_compareForecast$`All MYA price`)
hist(df_compareForecast$`HRW Jul-Aug avg. price` / df_compareForecast$`All MYA price`)
# But the farmer needs a way to estimate the all wheat MYA price in the
# harvest year in order to calculate the effective reference price, etc.
# Judging by the graphic, the previous harvest year's MYA price
# is a terrible predictor.
# Maybe there is a relation between the HRW harvest (Jul-Aug) price and
# the MYA price, such that the latter can be estimated based on an estimate
# of the former?
# If the ratio of these two prices follows a geometric brownian movement,
# then the ratio can presumably be forecast with accuracy similar to that
# of the HRW Jul-Aug price forecast.
# Or, just look at the monthly all wheat price series. Maybe that follows
# geometric browning movment. Can maybe forecast for each month a
# out to month 12, and then multiply by marketing weights?
# Calculate m, s on basis of previous 12 months (tau = 12).
# OR just use the future price forecast for each month and then dot by
# mkting wgts. I think the USDA uses the HRW wheat future price in its
# (unofficial) MYA price forecasts.
#---------------------------------------------------------------------------
# Create data frame containing the olympic average of the previous 4 yrs  + 
# planting year MYA price.
plantYear1 <- df_myaPforecast$`HRW planting year`[1]
df_w <- subset(df_wheat, `HRW planting year` > plantYear1 - 4)
list_vec <- list()
for(i in 4:nrow(df_w)){
  myaPprev4yrs_vec <- df_w$`All MYA price`[(i - 3):i]
  myaPforecast <- df_myaPforecast$`MYA price forecast`[i - 3]
  this_yr <- df_myaPforecast$`HRW planting year`[i - 3]
  myaPavg_vec <- c(myaPprev4yrs_vec, myaPforecast)
  ind_min <- which(myaPavg_vec == min(myaPavg_vec))
  ind_max <- which(myaPavg_vec == max(myaPavg_vec))
  olympAvg <- mean(myaPavg_vec[-c(ind_min, ind_max)])
  list_vec[[i - 3]] <- c(this_yr, myaPavg_vec, olympAvg)
}
df_myaPriceOlympAvg <- as.data.frame(do.call(rbind, list_vec))
colnames(df_myaPriceOlympAvg) <- c("HRW planting year",
                                   paste("MYA price", -4:0),
                                   "MYA price olympic avg.")
#---------------------------------------------------------------------------
# Get farm bill parameters (loan rate and reference price)
this_folder <- "C:/Users/bensc/OneDrive/Documents/Agricultural price floors and ceilings revisited/Price floor research/Historical USDA wheat MYA price forecasts/"
this_file <- "futmodwheat2021.xlsx"
this_filePath <- paste0(this_folder, this_file)
df <- readxl::read_excel(this_filePath, sheet = "Data")
df <- as.data.frame(df)
df[, 1] <- as.character(df[, 1])
ind <- grep("Table 13", df[, 1])[2]
df <- df[ind:nrow(df), ]
colnames(df) <- df[2, ]
df <- df[-c(1:4, nrow(df)), c(1, 3:6)]
colnames(df)[ncol(df)] <- "Reference price"
colnames(df)[1] <- "MY"
df$MY <- gsub("-", "/", df$MY)
df[, -1] <- as.data.frame(apply(df[, -1], 2, as.numeric))
df$`HRW planting year` <- as.integer(gsub("/.*", "", df$MY)) - 1
df_fbParams <- df
#===========================================================================
# Merge into single decision data frame
list_df <- list(df_mstau, df_fbParams, df_myaPriceOlympAvg)
df_decis <- plyr::join_all(list_df, by = "HRW planting year")
#df_decis$`HRW planting year`
ind1 <- which(df_decis$`HRW planting year` == 2014)
ind2 <- nrow(df_decis)
df_decis <- df_decis[ind1:ind2, ]
Pr <- df_decis$`Reference price`
Pa <- df_decis$`MYA price olympic avg.`
df_decis$`Effective ref. price` <- NA
for(i in 1:nrow(df_decis)){
  df_decis$`Effective ref. price`[i] <- min(1.15 * Pr[i], 0.85 * Pa[i])  
}
EffPr <- df_decis$`Effective ref. price`
Pl <- df_decis$`National average loan rate`
df_decis$`Price floor` <- NA
for(i in 1:nrow(df_decis)){
  df_decis$`Price floor`[i] <- max(EffPr[i], Pl[i])
}
Pf <- df_decis$`Price floor`
P <- df_decis$muP * 0.9

implicitP <- effProdPrice(P = P,
                          pIntervene = Pf,
                          tau = df_decis$tau,
                          s = df_decis$s,
                          type = "floor", output = "Peff")
N2 <- effProdPrice(P = P,
                          pIntervene = Pf,
                          tau = df_decis$tau,
                          s = df_decis$s,
                          type = "floor", output = "N2")
df_decis$`Implicit producer price` <- implicitP
df_decis$`Probability of gov. payments` <- 1 - N2
#---------------------------------------------------------------------------
# df_plot <- subset(df_w, `HRW planting year` >= 2014)
# df_plot$chi <- implicitP
# df_plot$Prob <- 1 - N2
df_plot <- df_compareForecast
df_plot$chi <- c(rep(NA, 14), implicitP)
df_plot$prob <- c(rep(NA, 14), 1 - N2)

df_plot1 <- df_plot[, c("HRW planting year",
                        "All MYA price",
                        "Ptau",
                        "HRW Jul-Aug avg. price",
                        "chi")] %>%
  gather(Type, Value, `All MYA price`:chi)
gg <- ggplot(df_plot1, aes(x = `HRW planting year`,
                           y = Value, group = Type,
                           color = Type))
gg <- gg + geom_line()
gg



# #==========================================================================
# # Price ceiling
# Pceil <- 7
# Peff_vec <- effProdPrice(P_vec, Pceil, tau, s, type = "ceiling")
# 
# QS0 <- theta_S * P_vec^eta_S
# QS <- theta_S * Peff_vec^eta_S
# QD <- theta_D * P_vec^eta_D
# 
# df_plot <- data.frame(Price = P_vec, QS0, QS, QD)
# df_plot <- df_plot %>% gather(Type, Quantity, QS0:QD)
# 
# gg <- ggplot(df_plot, aes(x = Quantity, y = Price, group = Type, color = Type))
# gg <- gg + geom_line(lwd = 1)
# gg <- gg + geom_hline(yintercept = Pceil, linetype = "dashed")
# gg
# 
