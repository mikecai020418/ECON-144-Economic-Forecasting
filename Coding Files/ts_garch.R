#***********************************************
# Randall R. Rojas
# Email: rrojas@econ.ucla.edu
# Comment(s): R code for fitting ARMA+GARCH models
# Data File(s): S&P500
#***********************************************

# Set your 'working directory' to the folder where all the data and respective codes are located.
#setwd("...")

# Clear all variables and prior sessions
rm(list=ls(all=TRUE))

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
library(nlstools)
library(fpp)
library(strucchange)
library(Quandl)
library(zoo)
library(PerformanceAnalytics)
library(quantmod)
# library(stockPortfolio)
library(vars)
library(lmtest)
library(dlnm)
library(hts)
library(tseries)
library(rugarch)

# Specify an ARMA + GARCH Model
model=ugarchspec(
variance.model = list(model = "sGARCH", garchOrder = c(2, 2)), #not happy - change it, get AIC and so on
mean.model = list(armaOrder = c(2, 2), include.mean = TRUE),
distribution.model = "sstd") #standard normal

#Sanity check: explore the model parameters - turning on all the switches
model@model$pars #one - parameter gonna be estimated and included in our model
#mxreg-  xreg in arima - trend(quadratic, exponential etc) u can specify

#arfima - aregressive fractional integrated moving average - change in yt = y(t) - y(t-1/2)
#include more than arima

# Get the S&P500 Data (get Adjusted Prices, convert them to rerurns, and then to a data frame)
sp500.prices=get.hist.quote(
instrument = "^GSPC",
quote = "Adj",
provider = c("yahoo"), method = NULL,
origin = "1899-12-30", compression = "d",
retclass = c("zoo"), quiet = FALSE, drop = FALSE
)

sp500=as.data.frame(sp500.prices)
N=length(sp500[,1])
sp500.returns=100*(log(sp500[2:N,])-log(sp500[1:(N-1),]))

plot(sp500.returns, type = 'l')

# Fit the model to the data
modelfit=ugarchfit(spec=model,data=sp500.returns)
modelfit
plot(modelfit) #how to see the model fit
# Note: you can pass the "which=" argument to the plot() function with the number of the graph.

# defaults to ARFIMA model - sstd model
# beta1 - % from yesterday to today
# beta2 - % from two days ago to today
# Second term in sGARCH to increase beta,first term to increase alpha
# robust standard errors - heteroskedasticity - standard errors actually wrong.
# decide which term to keep - beta2 doesn't have good p-value.

# plot modelfit 
#1.blue - data, red-  conditional sd
#4.there is something in the return but not significant
#5.there are some dynamic in the square - volatility modelling that we prefer
#11. Whether the model works or not - if it works, will look like white noise

#Not needed:
# 9 - confirms that everything is normally distributed.  - 1 to 1 - consistent with normal. 
# near the tails - not so good - but doesn't matter since u only need ±3
infocriteria(modelfit)

# Forecast
modelfor = ugarchforecast(modelfit, data = NULL, n.ahead = 10, n.roll = 0, out.sample = 0) 
#forecast - out of sample - specify rolling window average(n.roll) - volatility - noisy - smoothing using rolling window - everything turned off except for n.ahead
# take entire data, forecast out of sample
plot(modelfor) #only have default unconditional forecast - sigma prediction - model for data + model for volatility
#1.zoom in to the data - suggest a slight increase
#3. what's gonna happen for volatility - forecasting first a decrease follow by an increase
# trading on volatility - trade on that point - rely on volatility to increase.




