#### Co-integration and pairs trading

# Source: http://faculty.chicagobooth.edu/ruey.tsay/teaching/bs41202/sp2017/lec10-17.pdf
# good source for financial analysis

# Another good example of pairs trading can be found here: 
# https://www.quantstart.com/articles/Cointegrated-Augmented-Dickey-Fuller-Test-for-Pairs-Trading-Evaluation-in-R

require(MTS)
library(urca)
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
library(TTR)
library(tis)
require("datasets")
require(graphics)
library(forecast)
#install.packages("astsa")
#require(astsa)
library(xtable)
# New libraries added:
library(stats)
library(TSA)
library(timeSeries)
library(fUnitRoots)
library(fBasics)
library(tseries)
library(timsac)
library(TTR)
library(fpp)
library(strucchange)
#library(MSBVAR)
library(vars)
library(lmtest)
library(dlnm)

da=read.table("d-bhp0206.txt",header=T)
da1=read.table("d-vale0206.txt",header=T)
bhp=log(da$adjclose)
vale=log(da1$adjclose)
bhp1=ts(bhp,frequency=252,start=c(2002,127))
vale1=ts(vale,frequency=252,start=c(2002,127))

# Plot the two series
quartz()
plot(bhp1, type ='l', col = "blue",ylim=c(0.45,3.7),ylab="Stock Price ($)", main= "Pairs Trading Example")
lines(vale1, type ='l', col = "red")
legend(2003, 3.5, c("BHP","Vale"), fill=c("blue","red"),cex =1.2,bty='n')

# make sure there is shared dynamics - convert them to z-score unit - in that case the two series would overlap
# trace each other quite well - if they diverge, that's an opportunity for you to go in - one that drops below will be cheaper than the other
# short sell/buy etc. 

# step 1: find two series of stocks that are similar to each other
# step 2: find the cointegrated relationship
# step 3: look at where series departs from each other - away - start - series mean revert - close out
# forecast when that's suppose to happen

# Try a simple regression of S1 on S2
mreg=lm(bhp~vale)
summary(mreg)

# Perform the Johansen Test
x=cbind(bhp,vale)
m1=ar(x)
m1$order
m2=ca.jo(x,K=2)
summary(m2)
m3=ca.jo(x,K=2,type=c("trace"))
summary(m3)

# 1 and -0.7 - buy 1 share of bhp and short sell 0.7 of vale - strategy relative to equilibrium
# all u want - optimal weights and weights that represent # of shares. 

# Compute the co-intergration expression
wt=bhp1-0.718*vale1
quartz()
plot(wt,type='l',ylab="wt = BHP - 0.72Vale",main = "Co-integration Relation")
abline(h=c(1.82),col="red")
legend(2003, 1.95,c("wt","E[wt] = 1.82"),fill=c("black","red"))
#all I want - not on red line - not in equilibrium - makign money
#above equilibrium - making profit

# if the dynamics hover around this - mean revert too often - don't want to be in a position too often
# ar-ish - don't have to incur cost too much time - more viable. 
quartz()
tsdisplay(wt) #AR like
# auto.arima(wt) #ARMA model by auto.arima

library("tseries")
adf.test(wt) #staionary - volatility constant enough for our purposes

# Reject H0: there is a unit root (i.e., not stationary) --> the linear combination series is stationary


# Now that we have μ = 1.82 and 𝝲 = 0.72 -->Trade

# (1) Buy Stock 1 and short 𝝲 shares of Stock 2 when the wt = μ - 𝝳. 𝝳up to u - how far away are u too take that profit
# (2) Sell Stock 1 and buy 𝝲 shares of Stock 2,when w(t+h) = μ + 𝝳
# (3) Profit: r(h) = wt+h  - wt =  2𝝳 - profit double since both short sell and buy

#Some practical considerations: 
#The threshold is chosen so that the profit out-weights the costs of two trading. In high frequency, must be greater than trading slippage, which is the same linear combination of bid-ask spreads of the two stock, i.e. bid-ask spread of Stock 1 + ⇥(bid-ask spread) of Stock 2.  
#Speed of mean-reverting of wt plays an important role as h is directly related to the speed of mean-reverting. 
#There are many ways available to search for co-integrating pairs of stocks. For example, via fundamentals, risk factors, etc. 
#For unit-root and co-integration tests, see the textbook and ref- erences therein. 

# We can now fit a model to wt and forecast or simply forcst using ETS

# Using ETS - monitoring this in real time - impossible to run all pairwise combinations of 5k tickers
# narrow it by sectors - reduces number of combinations - ran all the stuff, test which one are the most significant
# homeruns - survival of cointegrated relationship - find large enough window - how large of window of time do they need to maintain
# co-integrated relationship - a year to 2 years of enough shared dynamics - played around with windows too
# a lot of parameters to optimize - handful of pairs to identify - industrial strength

quartz()
plot(forecast(wt))

quartz()
tsdiag(ets(wt))



