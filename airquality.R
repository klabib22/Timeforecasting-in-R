library(tidyverse)
library(ggplot2)
library(tibble)
library(tidyr)
library(readr)

airquality <- as_tibble(airquality)
airquality

#quantile plots and error analysis for fitting
qplot(x = Ozone, y = Wind, data = airquality)
qplot(x = Temp, y = Solar.R, data = airquality)
fit.f <- lm(Ozone~Wind,data = airquality)
layout(matrix(1:4,nrow = 2))
plot(fit.f)
summary(fit.f)
confint(fit.f)

#generalised linear model/regression
library(rstan)
library(tidyr)
library(rstan)
y~int+b*Wind+error (error~Normal(0,sigma))
fit.b<- glm(Temp~Wind,data = airquality)
layout(matrix(1:4,nrow=2))
plot(fit.b)


summary(fit.b)
anova(fit.b, test = "Chisq")
library(rstantools)
library(dplyr)
install.packages("caTools")
install.packages(ROCR)
library(caTools)

#histogram
library(ggplot2)
ggplot(airquality,aes(x = Wind))+geom_density()
ggplot(airquality, aes(x=Temp)) +
  geom_histogram(binwidth=.5, colour="black", fill="white") +
  geom_vline(aes(xintercept=mean(Temp, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1)

#statistical tests
airquality
shapiro.test(airquality$Wind)
ggplot(airquality,aes(x = Ozone))+geom_density(fill="blue")
install.packages("moments")
library(moments)
variance(airquality$Ozone)
kurtosis(airquality$Ozone)
range(airquality$Ozone)
library(tidyverse)
install.packages("caret")
library("caret")

#polynomial regression/fitting
ggplot(airquality, aes(x=Wind, y=Ozone)) + 
  geom_point() +
  stat_smooth(method='glm', formula = y ~ poly(x,3), size = 1) + 
  xlab('Wind') +
  ylab('Ozone')

#sampling from a distribution and fitting a curve to it
reps <- 20000
DF <- 5
Z <- replicate(reps,rnorm(DF))
X <- colSums(Z^2)
hist(X, 
     freq = F, 
     col = "steelblue", 
     breaks = 50, 
     ylab = "Density", 
     main = "")
curve(dchisq(x, df = DF), 
      type = 'l', 
      lwd = 2, 
      col = "red", 
      add = T)


