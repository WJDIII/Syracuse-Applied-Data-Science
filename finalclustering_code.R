################################################
# IST707, Standard Homework Heading
#
# Student name: Ryan Dean
# Final Project

#accessing libraries
library("Quandl")
library("tidyverse")
library("quantmod")
library("dplyr")
library("ggplot2")
library("xts")
library("quantmod")
library("factoextra")
#renaming dataset from csv generated
eth.ohlcv <- ethOHLCV052121
#removing index column 
eth <- eth.ohlcv[,-c(1:2)]
#was running into issues with the acf and pacf plots, I think they were scaled to seconds
#had to use this approach
dates <- seq(as.Date("2020-01-07"), length = 500, by = "days")
#convert to time series
eth <- xts(x=eth, order.by = dates)
#checking parameters of dataset
length(eth)
head(eth, n = 10)
tail(eth, n = 10)
str(eth)
#extracting core
e_core <- coredata(eth)
class(e_core)
#index
e_index <- index(eth)
class(e_core)
#loading quantmod to plot ema
library(quantmod)
chartSeries(eth$close, theme="black", up.col='white')
addEMA(7, col = "red")
addEMA(14, col = "blue")


#Feature/Signal Engineering
#assigning as time series
eth.ts <- data.frame(eth)
# exponential moving averages (1 week and 2 week)
ema7 <- EMA(eth.ts$close, n = 7)
ema14 <- EMA(eth.ts$close, n = 14)
## adding if/else logic columns
#ema 7 crosses above 14 buy
eth.ts$ema7.greater.ema14 <- ifelse(ema7 > ema14, 1, 0)
#ema  7 crosses below ema 14
eth.ts$ema7.less.ema14 <- ifelse(ema7 < ema14, 1, 0)
#close above ema 7
eth.ts$close.greater.ema7 <- ifelse(eth.ts$close > ema7, 1, 0)
#bull or bear candle
eth.ts$candle.today <- ifelse(eth.ts$close > eth.ts$open, 1, 0)
#bull or bear candle next day
eth.ts$candle.tommorrow <- lead(eth.ts$candle.today, n=1)
#percent change function
pct_delta <- function(x, y) {
  change <- (x - y)
  pct_change <- (change/y)
}
# 1 day return
eth.ts$daily_return <- pct_delta(eth.ts$close, eth.ts$open)
# 1 day lagged return
eth.ts$tommorrow_return <- lead(eth.ts$daily_return, n=1)
#ema close pct diff
eth.ts$close.ema7.diff <- pct_delta(eth.ts$close, ema7)
eth.ts$close.ema14.diff <- pct_delta(eth.ts$close, ema14)
#Omitting NAs
eth.ts <- na.omit(eth.ts)
hist(eth.ts$daily_return, col = "green", breaks = 100)
#k-means cluster
set.seed(1234)
#elbow method for optimal clusters
fviz_nbclust(eth.ts[,12:13], kmeans, method = "wss")
#five clusters of the percent change difference between the close price and 7 day ema and 14 day ema
km1 <- kmeans(eth.ts[,12:13], 6)
#assigning a cluster to the column
eth.ts$cluster <- as.factor(km1$cluster)
#plotting clusters
par(mfrow=c(1,2))
ggplot(eth.ts, aes(close.ema7.diff, close.ema14.diff)) + geom_point() + theme_dark()
cluster <- ggplot(eth.ts, aes(close.ema7.diff, close.ema14.diff, col = cluster)) + geom_point()
cluster + ggtitle("Percent Difference in EMA7 & EMA14")
#ETH behavior over past 486 days
par(mfrow=c(2,1))
eth.ts$eth.return <- cumsum(eth.ts$daily_return)
plot(eth.ts$daily_return, main="Daily Volatility - Buy and Hold", xlab="Volatility", ylab="Variance", type = "l", col = 11)
grid(col="lightgray")
plot(eth.ts$eth.return, main="Cumulative Return - Buy and Hold", xlab="Days Since Position Entrance", ylab="Return", type = "l", col = 11)
grid(col="lightgray")
# trading rules
rule <- 1

if (rule == 1) {
  
  
  #prediction w/ basic technical ema strat - long when price is above ema7
  eth.ts$buy_list <- ifelse(eth.ts$ema7.greater.ema14 == 1, 1, 0)
  eth.ts$sell_list <- ifelse(eth.ts$ema7.less.ema14 == 1, 1, 0)
  
  # predicted Return
  eth.ts$pred_return <- ifelse(eth.ts$candle.tommorrow == 1 & eth.ts$buy_list == 1,
                               eth.ts$tommorrow_return,
                               ifelse(eth.ts$candle.tommorrow == 0 & eth.ts$buy_list == 1,
                                      eth.ts$tommorrow_return,
                                      ifelse(eth.ts$candle.tommorrow == 1 & eth.ts$sell_list == 1,
                                             -eth.ts$tommorrow_return,
                                             ifelse(eth.ts$candle.tommorrow == 0 & eth.ts$sell_list == 0,
                                                    -eth.ts$tommorrow_return, 0))))
  
  #cumulative return
  eth.ts$cumulative_return <- cumsum(eth.ts$pred_return)
  
} else { #long above ema7 when cluster has large differences in ema14, sell when negative changes
  
  #buy_list
  eth.ts$buy_list <- ifelse(eth.ts$ema7.greater.ema14 == 1 & eth.ts$cluster == 2 | eth.ts$cluster == 4 | eth.ts$cluster == 5, 1, 0)
  #sell_list
  eth.ts$sell_list <- ifelse(eth.ts$ema7.less.ema14 == 1 & eth.ts$cluster == 1 | eth.ts$cluster == 3 | eth.ts$cluster == 6, 1, 0)
  # predicted Return
  eth.ts$pred_return2 <- ifelse(eth.ts$candle.tommorrow == 1 & (eth.ts$cluster == 2 | eth.ts$cluster == 4 | eth.ts$cluster == 5) & eth.ts$buy_list == 1,
                               eth.ts$tommorrow_return,
                               ifelse(eth.ts$candle.tommorrow == 0 & (eth.ts$cluster == 2 | eth.ts$cluster == 3 | eth.ts$cluster == 5) & eth.ts$buy_list == 1,
                                      eth.ts$tommorrow_return,
                                      ifelse(eth.ts$candle.tommorrow == 1 & (eth.ts$cluster == 1 | eth.ts$cluster == 4 | eth.ts$cluster == 6) & eth.ts$sell_list == 1,
                                             -eth.ts$tommorrow_return,
                                             ifelse(eth.ts$candle.tommorrow == 0 & (eth.ts$cluster == 1 | eth.ts$cluster == 3 | eth.ts$cluster == 6) & eth.ts$sell_list == 0,
                                                    -eth.ts$tommorrow_return, 0))))
  
  #cumulative return
  eth.ts$cumulative_return2 <- cumsum(eth.ts$pred_return)
  
}
#plots of returns and volatility
par(mfrow=c(2,1))
plot(eth.ts$pred_return, type = "l", col = 11)
title("Volatility")
grid(col="lightgray")
plot(eth.ts$cumulative_return, type = "l", col = 11)
title("Cumulative Return")
grid(col="lightgray")
#for second strategy
#plots of returns and volatility
par(mfrow=c(2,1))
plot(eth.ts$pred_return2, type = "l", col = 11)
title("Volatility")
grid(col="lightgray")
plot(eth.ts$cumulative_return2, type = "l", col = 11)
title("Cumulative Return")
grid(col="lightgray")
#Confusion Matrix and Accuracy Measures - Using Caret
#confusion matrix of buying accuracy
buy_list_cm <- confusionMatrix(factor(eth.ts$candle.tommorrow), factor(eth.ts$buy_list))
#adding a bear candle column with similar logic as to not have to change logic above
#bear candle
eth.ts$candle.todayinv <- ifelse(eth.ts$close < eth.ts$open, 1, 0)
#bull or bear candle next day
eth.ts$candle.tommorrowinv <- lead(eth.ts$candle.today, n=1)
#confusion matrix of selling accuracy - basically saying (If bear candle tomorrow and predicted to sell today, what is the accuracy)
sell_list_cm <- confusionMatrix(factor(eth.ts$candle.tommorrowinv), factor(eth.ts$sell_list))
#output confusion matrix for 
buy_list_cm
sell_list_cm
#cumulative return for a strategy
eth.ts$cumulative_return

view(eth.ts)
returns <- eth.ts[,18:21]
returns$BUYandHold <- eth.ts$daily_return
returns$BUYandHOLD <- cumsum(eth.ts$daily_return)
colnames(returns) <- c("k-means Volatility", "k-means alpha", "EMA Volatility", "EMA alpha", "Buy&Hold Volatility", "Buy&Hold alpha")
view(returns)

volatility <- returns[,c(1,3,5)]
returns <- returns[,c(2,4,6)]
#######
plot(volatility$`k-means Volatility`, main = "k-means Volatility", ylab = "Daily Return", type = "l", col = "#8b0000", ylim = c(-.5,.5))
rect(par("usr")[1], par("usr")[3],
     par("usr")[2], par("usr")[4],
     col = "black") # Color
lines(volatility$`k-means Volatility`, type = "l", col = "#8b0000")
grid(col = "lightgray")
#######
plot(volatility$`EMA Volatility`, main = "EMA Crossover Volatility", ylab = "Daily Return", type = "l", col = "#ff00ff", ylim = c(-.5,.5))
rect(par("usr")[1], par("usr")[3],
     par("usr")[2], par("usr")[4],
     col = "black") # Color
lines(volatility$`EMA Volatility`, type = "l", col = "#ff00ff")
grid(col = "lightgray")
#######
plot(volatility$`Buy&Hold Volatility`, main = "Buy & Hold Volatility", ylab = "Daily Return", type = "l", col = "#90ee90", ylim = c(-.5,.5))
rect(par("usr")[1], par("usr")[3],
     par("usr")[2], par("usr")[4],
     col = "black") # Color
lines(volatility$`Buy&Hold Volatility`, type = "l", col = "#90ee90")
grid(col = "lightgray")
#######

plot(returns$`k-means alpha`, 
     main = "Cumulative Return by Strategy", 
     xlab = "Days Elapsed", ylab = "Percent Return",
     ylim = c(-1,6))
rect(par("usr")[1], par("usr")[3],
     par("usr")[2], par("usr")[4],
     col = "black") # Color
lines(returns$`k-means alpha`, type = "l", col = "#8b0000")
lines(returns$`EMA alpha`, type = "l", col = "#ff00ff")
lines(returns$`Buy&Hold alpha`, type = "l", col = "#90ee90")
grid(col = "lightgray")
legend(x = "topleft",
       legend = c("k-means", "EMA Crossover", "Buy and Hold"),
       col = c("#8b0000", "#ff00ff", "#90ee90"),
       lwd = 2)

kr <- cumsum(returns$`k-means alpha`)
bandhr <- cumsum(returns$`Buy&Hold alpha`)
emar <- cumsum(returns$`EMA alpha`)
tail(kr,1)
