library(fpp2)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggfortify)

data <- read.csv("C:\\Users\\Hp\\Downloads\\archive (3)\\stocks.csv")
data

column <- data[,2]
column
X <- ts(column)
X

autoplot(X)+
  ggtitle("Trends for Opening Stock Prices")+ylab("Prices")

arimafit <- auto.arima(X,d=1)
arimafit
forecast <- forecast(arimafit,h = 360)
autoplot(forecast)+ggtitle("ARIMA forecast for opening stock prices for Google")
checkresiduals(arimafit)
#sigma squared of 70.72 and p-value of 0.2915 indicating non-stationarity

