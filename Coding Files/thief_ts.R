# For details on "thief" see https://robjhyndman.com/hyndsight/thief/
library(forecast)
library(thief)
library(ggplot2)
fc <- thief(USAccDeaths)
autoplot(fc)
total <- AEdemand[,12]

# similar package - thief.  - identical to what mappa is doing, but differnet in the backend.  
# Mappa - combination happen on the very end - forecast happen at very end as well - uses ETS
# THief - forecast happen for each individual time  - combine forecast at the end - uses ARIMA

# unevenly spaced data/very high fequency data we can't forecast - data with multiple seasonalities
# weekly data could have weekly spikes but could also have monthly spikes - seasonality at every single temporal level. 
# complex seasonality - dynamic harmonic series

# blue - reconcile - all forecast combined
# red - only the forecast right now.

# Construct all temporal aggregates
totalagg <- tsaggregates(total)
autoplot(totalagg, main="Total Emergency Admissions")

base <- list()
for(i in seq_along(totalagg))
  base[[i]] <- forecast(auto.arima(totalagg[[i]]),
                 h=2*frequency(totalagg[[i]]), level=80)

# Reconcile forecasts
reconciled <- reconcilethief(base)

#Plot original and reconciled forecasts
par(mfrow=c(2,3), mar=c(3,3,1,0))
for(i in 6:1)
{
  plot(reconciled[[i]], main=names(totalagg)[i],
       xlim=c(2010.5,2017.5), flwd=1)
  lines(base[[i]]$mean, col="red")
}