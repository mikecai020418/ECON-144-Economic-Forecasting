#***********************************************
# Randall R. Rojas
# Email: rrojas@econ.ucla.edu
# Date: 04/06/2015
# Comment(s): ACF and PACF R code example for Econ 144.
# Data File(s): From Yahoo Finance use and stock/index
#***********************************************


# Clear all variables and prior sessions
rm(list = ls(all = TRUE))

# Load Libraries
library("fImport")
#library(fOptions)
#library(RQuantLib)
library(nlstools)
library(tseries)
library(Quandl)
library(zoo)
library(PerformanceAnalytics)
library(quantmod)
library(car)
library(FinTS)
#library(fOptions)
library(forecast)
require(stats)
#library(stockPortfolio)
library(vars)
library(tseries, quietly = T)
library(forecast, quietly = T)
library(XML)
library(fBasics)
library(timsac)
library(TTR)
library(lattice)
library(foreign)
library(MASS)
require(stats4)
library(KernSmooth)
library(fastICA)
library(cluster)
library(leaps)
library(mgcv)
library(rpart)
require("datasets")
require(graphics)
library(RColorBrewer)
library(dynlm)

# Load the Data
#AAPL <- getReturns("AAPL",freq="day")
getSymbols("AAPL") # no need to download the data
quartz()
#chartSeries(AAPL$R, subset='last 3 months')
chartSeries(AAPL$AAPL.Adjusted,subset='last 3 months')
addBBands()
 
# Compare the following two plot and try to identify what is wrong in the first one.
quartz()
plot(AAPL)

#quartz()
#plot(AAPL$AAPL.Adjusted)

# Let's look at the ACF and PACF of the (a) Stock Prices vs (b) The Returns (using the 1st Difference)

#Note: I need to interpolate because e.g., weekends and holidays return 'NAs' for the stock price.
ts_daily_prices <- get.hist.quote('AAPL', quote = "AdjClose", compression = "d",retclass="ts") #compression - daily,monthly, yearly etc. - retclass - timeseries. 
interp_ts_daily_prices=interpNA(ts_daily_prices,method="linear")

quartz()
par(mfrow=c(2,1))
acf(interp_ts_daily_prices,main="ACF of Apple Prices")
acf(diff(interp_ts_daily_prices),main="ACF of the First Difference of Apple Prices")

quartz()
par(mfrow=c(2,1))
pacf(interp_ts_daily_prices,main="PACF of Apple Prices")
pacf(diff(interp_ts_daily_prices),main="PACF of the First Difference of Apple Prices")

quartz()
tsdisplay(diff(interp_ts_daily_prices))

################### PROBLEM 2 #######################
###################  (2.3)    #######################
z=read.table('P2_3_data.txt',header=T)
# The data are quarterly and start from 1950 Q2, therefore, we need to convert the data.
y=ts(z$gdprowth,start=1950.25, freq=4)
x=ts(z$sp500returns,start=1950.25, freq=4)
# Since there are 4 parts to this question (a-d), I'll lable each model accordingly,
# e.g., ma=model (part a), mb = model (part b), ...



plot(z$sp500returns,z$gdprowth)
abline(ma,lty=1, lwd=2,col ="red3")


# (a) 
ma=lm(y~x)
summary(ma)

# (b) 
mb=dynlm(y~L(x,1))
summary(mb)

# (c) 
mc=dynlm(y~L(x,1)+L(x,2)+L(x,3)+L(x,4)) #xt-1, xt-2, xt-3 etc. 
summary(mc)

# (d) 
md=dynlm(y~L(x,1)+L(x,2)+L(x,3)+L(x,4)+L(y,1)) #also include lack of y
summary(md)

#Note:For model selection we should use AIC & BIC, and look at the residuals.
AIC(ma,mb,mc,md) #ignore warning because they don't have the same observations - AIC with lowest is the best model 
BIC(ma,mb,mc,md)
# It seems that md is the best model from the summaries above.

################### PROBLEM 3 #######################
###################  (3.7)    #######################

# (a) Compute the daily returns
getSymbols("^GSPC", from="2006-01-02")
# To compute the regular returns (i.e., without the log):
returns = diff(GSPC[,6]) / lag(GSPC[,6]) * 100 #(yt-yt-1)/yt-1
#Note: A faster way to it is like this: 
returns=dailyReturn(GSPC[,6])
#hist - show distribution
#summary - do normal five point summary - mean, return etc. 

# Compute the returns according to the book instructions:
# Rt = ln(p_t) - ln(p_t-1) - log returns - more popular - log function used to help volatility, but doesn't do much
log.returns = log(GSPC[,6]) - log(lag(GSPC[,6]))
quartz()
plot(log.returns)

# tsdisplay(log.returns)
# square the return - get some structure - get pretty good volatility model
acf(returns)
pacf(returns)

#(b) Compute the sample moments of the returns: mean, variance, skewness and kurtosis.Plot the historgram.
mean.log.returns = mean(log.returns$GSPC.Adjusted, na.rm=TRUE)
var.log.returns =  var(log.returns$GSPC.Adjusted, na.rm=TRUE)
skew.log.returns = skewness(log.returns$GSPC.Adjusted, na.rm=TRUE) 
kurt.log.returns = kurtosis(log.returns$GSPC.Adjusted, na.rm=TRUE) 
quartz()
truehist(log.returns, col='steelblue3',main="Log of S&P 500 Returns",xlab="log(Returns)")
#dev.print(device=postscript,"returns.eps",width=7,height=7, horizontal=FALSE)
#dev.off()

#(c) Plot Rt against Rt-1, Rt-2, Rt-3, Rt-4
Rt = log.returns$GSPC.Adjusted
Rt_1 = as.numeric(lag(Rt,1))
Rt_2 = as.numeric(lag(Rt,2))
Rt_3 = as.numeric(lag(Rt,3))
Rt_4 = as.numeric(lag(Rt,4))
Rt =as.numeric(Rt)
quartz()
par(mfcol=c(2,2))
plot(Rt_1,Rt,pch=20,col="skyblue4")
plot(Rt_2,Rt,pch=20,col="skyblue4")
plot(Rt_3,Rt,pch=20,col="skyblue4")
plot(Rt_4,Rt,pch=20,col="skyblue4")
#dev.print(device=postscript,"returnst.eps",width=7,height=7, horizontal=FALSE)
#dev.off()



