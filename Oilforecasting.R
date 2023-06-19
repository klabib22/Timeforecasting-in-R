#loading data
data <- read.csv("C:\\Users\\Hp\\Downloads\\BrentOilPrices.csv")
data

#converting to a time series data
X <- ts(data[,2])
X

#autoplot for the oil price trends using ggfortify package
install.packages("ggfortify")
library(ggfortify)
autoplot(X)+
  ggtitle("Trends for Brent Oil Prices")+ylab("Oil Prices in dollars")

#differencing and detrending
DX <- diff(X)
autoplot(DX)+
  ggtitle("Detrending Brent Oil Prices")+ylab("Oil Prices in dollars")


install.packages("fpp2")
library(fpp2)

#seasonality analysis
fit<- snaive(DX)
fit
print(summary(fit))
# residual sd = 1.6979 indicates a very good fit since a lower residual sd means a better model fit
checkresiduals(fit)
# distribution of residuals/errors is narrow and variance is low (using Seasonal naive method), bars within the blue dashed lines

#check ARIMA model
arima <- auto.arima(X,d=1)
arima
#sd is 1.214 for the arima, even better fitting
checkresiduals(arima)


#forecast using arima
fcst <- forecast(arima,h = 4800)
autoplot(fcst)
