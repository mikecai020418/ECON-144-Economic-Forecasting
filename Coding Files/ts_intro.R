#***********************************************
# Randall R. Rojas
# Email: rrojas@econ.ucla.edu
# Date: 12/27/2021
# Comment(s): R code example for fitting/forecasting a trend to ts data.
# Data File(s): beer.csv
#***********************************************
# Variable Definitions
# beer = monthly beer production in Australia from Jan 1956 - Aug 1995
#************************************************

# Set your 'working directory' to the folder where all the data and respective codes are located.
#setwd("...")

# Clear all variables and prior sessions
rm(list=ls(all=TRUE))

# Load libraries
library(tseries)
library(forecast)
library(fpp3)
library(tseries)
library(seasonal)
library(fable)
library(stats)
require(graphics)


# Read in the data into a data file
beer=read.csv("beer.csv",header=T,dec=",",sep=";")
beer=ts(beer[,1],start=1956,freq=12) #,1 to take the values
plot(beer,xlab="Year", ylab="Monthly Beer Production")

#-------------Example 1: TS Anatomy Plot--------------
# STL Decomposition 
quartz()
plot(stl(log(beer),s.window="periodic")) # stl extract all the component
summary(forecast(beer)) # Forecast function - use the current best model ets. 


#-------------Example 2: TS Anatomy Plot--------------
# STL Decomposition 
# Example from Hyndman, Sec 3.2
data(us_employment)
us_retail_employment <- us_employment %>% #pipe operator - way to nest all command in one - try running the code/make sure it works. 
  filter(year(Month) >= 1990, Title == "Retail Trade") %>%
  select(-Series_ID)
autoplot(us_retail_employment, Employed) +
  labs(y = "Persons (thousands)",
       title = "Total employment in US retail")
     
# Decompose the series (extract the components)       
dcmp <- us_retail_employment %>%
  model(stl = STL(Employed))
components(dcmp)

# Plot the components
components(dcmp) %>% autoplot()
# Amplitude seems to be growing with time, so multiplicative. 

# ------------ ADDITIVE ADJUSTMENTS-------------------
#-------------Example 3: TS Anatomy Plot--------------
# STL Additive Decomposition using 'decompose'
# Series (orginal data): Y = T  + S + R

library(fpp)
autoplot(a10)

dcmp_a10 = decompose(ts(a10,frequency=12), "additive")
plot(dcmp_a10)

cost = ts(a10,frequency=12) # Other subseries - won't know which one u're necessary to plot - complain


# Store the inidividual components and plot them:

trend_a10 = dcmp_a10$trend #extracting trend, seasonality, and cycle and plot them
# a10 hold all the column - eatract one by one and plot it
plot(trend_a10)
seasonal_a10 = dcmp_a10$seasonal
plot(seasonal_a10)
random_a10 = dcmp_a10$random #get a sense of any evidence of cycle - most part amplitude same, but remainder part changes quite a bit. 
plot(random_a10) 

# Detrend: Y - T - take y subtract t and plot it. 
detrend_a10 = cost - trend_a10
autoplot(detrend_a10) # suggesting multiplicative adjustment.  - seasonally adjust origimal data, not the detrended one. 

# Seasonally Adjust: Y - S
seasonally_adjusted_a10 = cost - seasonal_a10
autoplot(seasonally_adjusted_a10) # subtract seasonality - trend + remainder. 

# Remove the Random Component: Y - R - remove the noise - very clean series - growth in amplitude not as strong as we saw earlier. 
cycles_adjusted_a10 = cost - random_a10
autoplot(cycles_adjusted_a10)

# Most Popular Adjustment: Y - T - S
# Detrend and Seasonally Adjust the Series #left with the remainder. 
detrend_seas_adj_a10 = cost - trend_a10 - seasonal_a10
autoplot(detrend_seas_adj_a10) 

#Q: Is the remainder consistent with white noise, or can we model it?
ts_display_plot <- tsdisplay(detrend_seas_adj_a10) #ACF and PACF of the remainder

# ------------ MULTIPLICATIVE ADJUSTMENTS-------------------
#-------------Example 3: TS Anatomy Plot--------------
# STL Multiplicative Decomposition using 'decompose'
# Series (orginal data): Y = T x S x R

# Decompose the series (extract the components)  
Mdcmp_a10 = decompose(ts(a10,frequency=12), "multiplicative") #M for multiplicative - good practice
plot(Mdcmp_a10) # sometimes it's not obvious
#autoplot(Mdcmp_a10)

# Store the inidividual components and plot them:

Mtrend_a10 = Mdcmp_a10$trend
plot(Mtrend_a10)
Mseasonal_a10 = Mdcmp_a10$seasonal
plot(Mseasonal_a10)
Mrandom_a10 = Mdcmp_a10$random
plot(Mrandom_a10)

# Detrend: Y / T - divided by the trend - seasonality + remainder. 
Mdetrend_a10 = cost/Mtrend_a10
autoplot(Mdetrend_a10)

# Seasonally Adjust: Y / S - using multiplicative adjustment. 
Mseasonally_adjusted_a10 = cost/Mseasonal_a10
autoplot(Mseasonally_adjusted_a10)

# Remove the Random Component: Y / R - remove the remainder/random aprt. 
Mcycles_adjusted_a10 = cost/Mrandom_a10
autoplot(Mcycles_adjusted_a10) #still has this growth. 

# Most Popular Adjustment: Y / T / S 
# Detrend and Seasonally Adjust the Series
Mdetrend_seas_adj_a10 = (cost/Mtrend_a10)/Mseasonal_a10
autoplot(Mdetrend_seas_adj_a10) #get the remainder. 

#Q: Is the remainder consistent with white noise, or can we model it?
tsdisplay(Mdetrend_seas_adj_a10) #cycles are there since there spikes over the band. 

#Q: Based on the Additive vs. Multiplicative Adjustments, which one is better? Why? 

# Note: Another way to extract the components
stl_a10 = stl(cost, "periodic")
seasonal_stl_a10 = stl_a10$time.series[,1]
trend_stl_a10 = stl_a10$time.series[,2]
random_stl_a10 = stl_a10$time.series[,3]

# Note: For Homework 1, use stats::lag() for computing lags
# For example:
lag2_beer = stats::lag(beer,2)
plot(lag2_beer, beer)
