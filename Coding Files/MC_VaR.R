# https://blog.alookanalytics.com/2017/04/26/monte-carlo-method-in-r/

#' Stock price calculation
#' 
#' Calculates stock price after n periods using standard stock price model
#' @param stock_price original stock price
#' @param n number of periods
#' @param stock_mu expected percentual stock drift over n periods
#' @param stock_sigma expecter percentual stock volatility
#' @return stock price after n periods
f_stock_return <- function(stock_price, n, stock_mu, stock_sigma)
{	
	delta_t <- 1/n # one period
	for (i in seq(n)){
		epsilon <- runif(n=1, min=0, max=1)
		stock_price <- stock_price * (1 + qnorm(epsilon, stock_mu * delta_t, stock_sigma* sqrt(delta_t)))
		}
	return(stock_price)
}

simulations <- 1000 # number of MC simulations
n <- 20 # trading days
stock_price <- 100
stock_mu <- .1 # drift 10%
stock_sigma <- .2 # volatility 20%

# St+1 = St * (1 + μΔt + σε√Δt)

set.seed(42) # for reproducibility
stock_prices <- c()
for (i in seq(simulations)){
	stock_prices <- c(stock_prices, 
	f_stock_return(stock_price=stock_price, 
	n=n, 
	stock_mu=stock_mu, 
	stock_sigma=stock_sigma))
}
quantile(stock_prices, c(.01, .05))
plot(stock_prices,type='l',xlab="Number of Simulations", ylab="Stock Price ($)")
legend('topright', c("MC Approximation","VaR =$66.28"), fill =c("black", "red"),cex=1,bty='n')
abline(h=66.28,col="red",lwd=2)
dev.print(device=postscript,"mc_stock.eps",width=7,height=7, horizontal=FALSE)
dev.off()
