
#***********************************************
# Randall R. Rojas
# Email: rrojas@econ.ucla.edu
# Date: 04/07/2017
# Comment(s): R code example for fitting/forecasting a trend to ts data.
# Data File(s): labordata.dat
#***********************************************
# Variable Definitions
# male = labor force male participate rate = y (response variable)
# female = labor force female participate rate = y (response variable)
# t =  time (monthly observations from 1948-1991)
#************************************************

# Set your 'working directory' to the folder where all the data and respective codes are located.
#setwd("...")

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
library(xtable)
library(stats)


#NOTE: to add recession bands:
# Example: Presidents approval rating
#plot(presidents, type='n', ylab="Presidents approval rating")
#nberShade()
#lines(presidents)

# Read in the data into a data file and attach names:
z=read.table("labordata.dat")
names(z)= c("male","female","total")
attach(z)
# Convert data to time series format:
male_ts<-ts(male,start=1948,freq=12)
t<-seq(1948, 1991,length=length(male_ts)) #create the time dummy variable - starting 1948 and ending in 1991 - make sure it matches optimally. 
#t2 = t^2
#head(t) - treat it accordinlgy - jan 1948 etc. - mapped as time series
#-------------[1] TREND FITTING--------------

#Linear Fit
m1=lm(male_ts~t)
quartz()
par(mfrow=c(2,1))
plot(male_ts,ylab="Participation Rate (Male)", xlab="Time", lwd=2, col='skyblue3',ylim=c(75,84), xlim=c(1968,1995))
#The next commands adds the U.S. recession bands
nberShade()
lines(t,m1$fit,col="red3",lwd=2)
plot(t,m1$res, ylab="Residuals",type='l',xlab="Time")

#Quadratic Fit
m2=lm(male_ts~t+I(t^2))
quartz()
par(mfrow=c(2,1))
plot(male_ts,ylab="Participation Rate (Male)", xlab="Time", lwd=2, col='skyblue3',ylim=c(75,84), xlim=c(1968,1995))
lines(t,m2$fit,col="red3",lwd=2)
plot(t,m2$res, ylab="Residuals",type='l',xlab="Time")

#Log-Linear Fit
m3=lm(log(male_ts) ~ t)  
quartz()
par(mfrow=c(2,1))
plot(log(male_ts),ylab="Participation Rate (Male)", xlab="Time", lwd=2, col='skyblue3', xlim=c(1968,1995))
lines(t,m3$fit,col="red3",lwd=2)
plot(t,m3$res, ylab="Residuals",type='l',xlab="Time")

#Exponential Fit
ds=data.frame(x=t, y=male_ts)
par(mfrow=c(2,1))
plot(male_ts,ylab="Participation Rate (Male)", xlab="Time", lwd=2, col='skyblue3',ylim=c(75,84), xlim=c(1968,1995))
#lines(t,m1$fit,col="green",lwd=2)
m4=nls(y ~ exp(a + b * t),data=ds, start = list(a = 0, b = 0))
lines(ds$x, predict(m4, list(x = ds$x)),col="red3",lwd=2)
plot(t,residuals(m4), ylab="Residuals",type='l',xlab="Time")
summary(m4)


#-------------[2] MODEL SELECTION--------------
# Compare models using AIC and BIC
AIC(m1,m2,m3,m4)
BIC(m1,m2,m3,m4)

quartz()
plot(stl(male_ts,s.window="periodic"))r4


#-------------[3] TREND FORECASTING--------------
tn=data.frame(t=seq(1992,1999)) #beyond 1991
pred=predict(lm(male_ts ~ t), tn, se.fit = TRUE) #linear model - tn - predict with that model, use tn value, se.fit = true - use the whole picture
#show standard errors as well. 
#plot(c(male_ts,pred$fit),type='l',xlim=c(1940,2000))
pred.plim = predict(lm(male_ts ~ t),tn, level =0.95, interval="prediction") #include prediction interval
pred.clim = predict(lm(male_ts ~ t), tn,level=0.95, interval="confidence") #include confidence interval
#typically we see confint. 
matplot(tn$t,cbind(pred.clim, pred.plim[,-1]), 
        lty=c(1,1,1,3,3), type="l", lwd=2, ylab="predicted y",xlab="Time") #overlay many series on the same plot.
# create ur model, plot your data from start to forecasting end: observation followed by prediction. 
# forecast will automatically take care of it for us. 

#dev.print(device=postscript,"tsfig.eps",width=7,height=7, horizontal=FALSE)
#dev.off()

#-------------[4] Holt-Winters Filter--------------
hwfit<-HoltWinters(male_ts) #consulting work - good idea to show the entire picture of everything that's going on
#balck: data, red: fitted observation, others: out of sample forecast - also shows boundary where the data ends and forecast begins. 
#need to have legend so readers know what does each line represents. 
quartz()
hwpred <- predict(hwfit, 60, prediction.interval = TRUE,level=0.5)
plot(hwfit,hwpred,ylab="Participation Rate (Male)", xlab="Time",xlim=c(1948,1999))
#lines(predict(hwfit,n.ahead=60),lwd=1.5, col='blue')
#plot(hwfit,ylab="Participation Rate (Male)", xlab="Time", lwd=1, col='black',xlim=c(1948,1999))
#lines(predict(hwfit,n.ahead=60),lwd=1.5, col='blue')

