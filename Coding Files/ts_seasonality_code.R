#***********************************************
# Randall R. Rojas
# Email: rrojas@econ.ucla.edu
# Date: 04/01/2019
# Comment(s): R code example for fitting/forecasting a seasonality to ts data.
# Data File(s): beer.csv, and housing.dat
#***********************************************
# Variable Definitions
# beer = monthly beer production in Australia from Jan 1956 - Aug 1995
# house = monthly housing starts from 1946 to 1993
#************************************************


# Clear all variables and prior sessions
rm(list=ls(all=TRUE))

# Load Libraries
library(lattice)
library(foreign)
library(MASS)
library(car)
require(stats)
require(stats4)
library(KernSmooth)
library(fastICA)
library(cluster)
library(leaps)
library(mgcv)
library(rpart)
library(pan)
library(mgcv)
library(DAAG)
library("TTR")
library(tis)
require("datasets")
require(graphics)
library("forecast")
#install.packages("astsa")
#require(astsa)
library(RColorBrewer)
library(plotrix)
library(nlstools)
library(seasonal)
library(fpp2)

# Basic Seasonality Example using 'tslm'
y =ts(rnorm(120,0,3) + 20*sin(2*pi*(1:120)/12), frequency=12)
fit1=tslm(y ~ trend ) #linear trend - automatically create seasonal dummies. 
fit2=tslm(y ~ season) #only season - have all seasonal factors
fit3=tslm(y ~ trend+ season) #native to the tslm function - 
quartz()
par(mfrow=c(3,1))
plot(y,main="Time Series Data: Trend")
lines(fit1$fitted.values, col="red")

plot(y,main="Time Series Data: Seasonality")
lines(fit2$fitted.values, col="red")

plot(y,main="Time Series Data: Trend + Seasonality")
lines(fit3$fitted.values, col="red")
# Customize to ur own personal trend - tslm function allow that

# Read in the beer data into a data file
beer=read.csv("beer.csv",header=T,dec=",",sep=";")
beer=ts(beer[,1],start=1956,freq=12)

# Lets look at the beer data once again:
beer=read.csv("beer.csv",header=T,dec=",",sep=";")
beer=ts(beer[,1],start=1956,freq=12)
lbeer<-log(beer)

# Compare 3 different fit models:
fit1=tslm(lbeer ~ trend)
fit2=tslm(lbeer ~ season)
fit3=tslm(lbeer ~ trend + season)

quartz()
par(mfrow=c(3,1))
plot(lbeer,main="Time Series Data: Trend")
lines(fit1$fitted.values, col="red")
plot(lbeer,main="Time Series Data: Seasonality")
lines(fit2$fitted.values, col="red")
plot(lbeer,main="Time Series Data: Trend + Seasonality")
lines(fit3$fitted.values, col="red")
AIC(fit1,fit2,fit3)
BIC(fit1,fit2,fit3)

# Compute forecasts based on the 3 fit models:
quartz()
par(mfrow=c(3,1)) #tslm really nice - also works with the forecast function - fit1, 2,3 as objects of tslm - how many steps ahead - 60 steps 
plot(forecast(fit1,h=60),main="Model 1: Forecast Trend")
lines(fit1$fitted.values, col="red")
plot(forecast(fit2,h=60),main="Model 2: Forecast Seasonality")
lines(fit2$fitted.values, col="red")
plot(forecast(fit3,h=60),main="Model 3: Forecast Trend + Seasonality")
lines(fit3$fitted.values, col="red") #automatically overlay model with forecast - see the forecast with the actual data - continuous pattern

# The forecast above can be improved considerably via 'ets'
fit=ets(lbeer)
quartz()
plot(fit)
accuracy(fit)
plot(forecast(fit,level=c(50,80,95)))

#Plot the seasonal factors:
quartz()
fit=tslm(lbeer ~ season+0) #standard syntax to plot seasonal factors - +0 omits the y intercept - seasonal variation
plot(fit$coef,type='l',ylab='Seasonal Factors', xlab="Season",lwd=2, main="Plot of Seasonal Factors")
#Better to omit the y-intercept - disable the y-intercept - +0 to disable y - intercept
# Australia - summer is december, and winter is june - informative - seasonal factors are telling us stories. 
# create a seasonal dummy for that event - plot seasonal dummy for that characteristic - create that behaviour. 
# flat - no change for that particular event. 

#--------Book Example: Housing Starts----------------
house<-read.table("housing.dat")
housets<-ts(house[,1],start=1946,freq=12)
t<-seq(1946,1993.12,length=length(housets))
quartz()
plot(housets)
quartz()
plot(housets[200:256],type="l") #zoom in

# Seasonal Decomposition of Time Series by Loess
quartz()
plot(stl(housets,s.window="periodic"))
forecast(housets)
summary(forecast(housets))

fit1=tslm(housets ~ trend + season+0)
fit2=tslm(housets ~ trend+0)
fit3=tslm(housets ~ season) 

quartz()
par(mfrow=c(3,1))
plot(forecast(fit1,h=24),main="Model 1: Forecast Trend + Seasonality")
lines(fit1$fitted.values,col="red")
plot(forecast(fit2,h=24),main="Model 2: Forecast Trend Only")
lines(fit2$fitted.values,col="red")
plot(forecast(fit3,h=24),main="Model 2: Forecast Seasonality Only")
lines(fit3$fitted.values,col="red")
AIC(fit1,fit2,fit3)
BIC(fit1,fit2,fit3)

#Plot the seasonal factors:
quartz()
par(mfrow=c(2,1))
plot(fit3$coef,type='l',ylab='Seasonal Factors', xlab="Season",lwd=2, main="Plot of Seasonal Factors")
hist(fit3$res,main="Histogram of Residuals",col="skyblue3")

# We can improve the forecast using ets
quartz()
plot(housets,s.window="periodic")
forecast(housets)
plot(forecast(housets))

fit=ets(housets)
quartz()
plot(fit)
accuracy(fit)
plot(forecast(fit,level=c(50,80,95),h=12))

# Seasonality Decomposition using the library 'seasonal'
# See: https://otexts.com/fpp2/components.html
library(seasonal)
library(fpp)
# Classical Decompositions: Multiplicative and Additive
elecequip %>% decompose(type="multiplicative") %>%
  autoplot()
  
  elecequip %>% decompose(type="additive") %>%
  autoplot()

# Seasonal Decomposition using X11
elecequip %>% seas(x11="") -> fit
autoplot(fit) +
  ggtitle("X11 decomposition of electrical equipment index")

# STL
elecequip %>%
  stl(t.window=13, s.window="periodic", robust=TRUE) %>%
  autoplot()
  
plot(elecequip, col="grey", lwd= 2,ylab = "Total")
lines(seasadj(fit),col = "blue",lwd= 2)
lines(trendcycle(fit),col = "red",lwd= 2)
legend(2000,75, c("Data", "Seasonally Adjusted","Trend"),
fill = c("black","blue","red"),cex=1,bty='n')

fit %>% seasonal() %>% ggsubseriesplot() + ylab("Seasonal")

# Forecast:

fit <- stl(elecequip, t.window=13, s.window="periodic",
  robust=TRUE)
  
fit %>% seasadj() %>% naive() %>%
  autoplot() + ylab("New orders index") +
  ggtitle("Naive forecasts of seasonally adjusted data")
  
 fit %>% forecast(method="naive") %>%
  autoplot() + ylab("New orders index")

