library(MASS)
library(tseries)
library(forecast)
library(xts)
library(ggplot2)
#Renaming dataset from csv
eth.ohlcv <- ethOHLCV052121
#removing index column 
eth <- eth.ohlcv[,-c(1:2)]
#was running into issues with the acf and pacf plots, I think they were scaled to seconds
#had to use this approach as the crypto market technically never "closes". These time periods are 24 hours apart.
dates <- seq(as.Date("2020-01-07"), length = 500, by = "days")
#convert to time series
eth <- xts(x=eth, order.by = dates)
class(eth)
#natural log of eth price (log transformation)
lneth <- log(eth$close[1:450])
lneth
#ACF, PACF, and Dickey-Fuller Test
acf(lneth, lag.max = 50)
#gradual decent of correlation
pacf(lneth, lag.max = 50)
#Simulated random walkto justify using one difference
set.seed(12345)
rw1 <- arima.sim(model = list(order = c(0,1,0)), n = 500)
ts.plot(rw1, main = "Simulated ARIMA Random Walk (0,1,0")
rw_diff <- diff(rw1, n = 1)
ts.plot(rw_diff)
ts.plot(eth$close, main = "ETH Price over Time")
#immediate drop in correlation at lag 0 to 1, it is stationary
#differencing because of PACF plot
difflneth <- diff(lneth, 1)
difflneth
#adf test on logged and logged differenced series
lneth <- na.omit(lneth)
difflneth <- na.omit(difflneth)
acf(difflneth, lag.max = 50)
#gradual decent of correlation
pacf(difflneth, lag.max = 50)
ln <- adf.test(lneth)
ln
#reject null hypothesis of non-stationarity
diff <- adf.test(difflneth)
diff
#Time Series and auto-arima
eth.ARIMA <- ts(lneth)
fitlneth <- auto.arima(eth.ARIMA)
fitlneth
plot(eth.ARIMA, type="l", main = "ETH Log Price Returns")
exp(lneth)
#other models to compare
model2 <- arima(eth.ARIMA, order=c(2,1,1))
summary(model2)
#model (1,1,2)
model3 <- arima(eth.ARIMA, order=c(1,1,2))
summary(model3)
#forecasted values from ARIMA
forecastedvalues <- forecast(fitlneth, h=50)
forecastedvalues
plot(forecastedvalues)

forecastedvalues <- as.numeric(forecastedvalues$mean)
finalforecast <- exp(forecastedvalues)
finalforecast

#Percentage Error
df <- data.frame(eth$close[451:500], finalforecast)
headings <- c("actual price", "forecasted price")
names(df) <- headings
attach(df)
percent_error <- ((df$'actual price'-df$'forecasted price')/(df$'actual price'))
percent_error
mean(percent_error)
view(df)
#make sure residuals are random
checkresiduals(fitlneth)

#naive forecast
neth <- naive(lneth, h=50)
autoplot(neth)

#training and test data sets for different forecasting methods
eth_fc <- eth$close
train <- subset(eth_fc, end = 450)
test <- eth_fc[451:500]
naive_f <- naive(train, h = 50)
mean_f <- meanf(train, h = 50)

accuracy(naive_f, test)
accuracy(mean_f, test)

#time series cross validation - forecasting evaluation on a rolling origin lol
error <- tsCV(train, forecastfunction = naive, h = 10)
mse <- colMeans(error^2, na.rm = TRUE)
#taken directly from datacamp - Forecasting in R
data.frame(h=1:10, MSE = mse) %>% ggplot(aes(x=h, y=MSE)) + geom_point()

library(quantmod)
chartSeries(eth[400:500], theme = "black",
            bar.type = "hlc")

chartSeries(eth$close, theme = "black")
addEMA(7, col = "red")
addEMA(14, col = "white")
