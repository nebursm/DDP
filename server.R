# Developing Data Products - Course Project
# Portfolio analysis of an investment in 2 risky assets
# The asset chosen are: BMV index (Bolsa Mexicana de Valores) and Cemex stock
library(tseries)
library(PerformanceAnalytics)
library(zoo)

library(shiny)
library(UsingR)

# Get the stock prices from Yahoo
bmv_prices = get.hist.quote(instrument="^MXX", start="2012-04-01", end="2015-04-30", quote="AdjClose",provider="yahoo", origin="1970-01-01",compression="m", retclass="zoo")
cemex_prices = get.hist.quote(instrument="CX", start="2012-04-01", end="2015-04-30", quote="AdjClose",provider="yahoo", origin="1970-01-01",compression="m", retclass="zoo")

index(bmv_prices) = as.yearmon(index(bmv_prices))
index(cemex_prices) = as.yearmon(index(cemex_prices))

# Create merged price data
all_prices = merge(bmv_prices, cemex_prices)
# Rename columns
colnames(all_prices) = c("BMV", "CEMEX")

# Calculate monthly returns
simple_returns = diff(all_prices)/lag(all_prices, k=-1);

# The ratio BMV stock vs Cemex stock (adds up to 1)
bmv_weights = seq(from=-1, to=2, by=0.1)
cemex_weights = 1 - bmv_weights

# Portfolio parameters (annualized)
mu_hat_annual = apply(simple_returns,2,mean)*12   
sigma2_annual = apply(simple_returns,2,var)*12
sigma_annual = sigma2_annual^0.5
cov_mat_annual = cov(simple_returns)*12 
cov_hat_annual = cov(simple_returns)[1,2]*12    
rho_hat_annual = cor(simple_returns)[1,2]
# Assets parameters
mu_bmv = mu_hat_annual["BMV"]
mu_cemex = mu_hat_annual["CEMEX"]
sigma2_bmv =  sigma2_annual["BMV"]
sigma2_cemex = sigma2_annual["CEMEX"]
sigma_bmv = sigma_annual["BMV"]
sigma_cemex = sigma_annual["CEMEX"]
sigma_bmv_cemex = cov_hat_annual
rho_bmv_cemex = rho_hat_annual
# Portfolio expected return
mu_portfolio =  bmv_weights*mu_bmv + cemex_weights*mu_cemex
# Portfolio varianza
sigma2_portfolio =  bmv_weights^2 * sigma2_bmv + cemex_weights^2 * sigma2_cemex + 2 * bmv_weights * cemex_weights * sigma_bmv_cemex
# Portfolio standard deviation (risk)
sigma_portfolio = sqrt(sigma2_portfolio)

shinyServer(
  function(input, output) {
    output$myplot <- renderPlot({
      plot(sigma_portfolio, mu_portfolio, type="b", pch=16, ylim=c(0, max(mu_portfolio)), xlim=c(0, max(sigma_portfolio)), xlab=expression(sigma[p]), ylab=expression(mu[p]),col="steelblue")
      bmv_wt = input$bmv_wt
      ix = which(bmv_weights == as.character(round(bmv_wt, 1)))
      bmv_w = bmv_weights[ix]
      cemex_w = cemex_weights[ix]
      mu_w = mu_portfolio[ix]
      sigma_w = sigma_portfolio[ix]
      points(sigma_w, mu_w, col="red", lwd=5, pch=4)
      text(0.00, 0.30, paste("expected return = ", sprintf("%.1f %%", 100*round(mu_w, 2))), pos=4, cex=0.8)
      text(0.00, 0.28, paste("risk = ", sprintf("%.1f %%", 100*round(sigma_w, 2))), pos=4, cex=0.8)
      text(0.00, 0.26, paste("BMV weight = ", sprintf("%.1f %%", 100*round(bmv_w, 1))), pos=4, cex=0.8)
      text(0.00, 0.24, paste("CEMEX weight = ", sprintf("%.1f %%", 100*round(cemex_w, 1))), pos=4, cex=0.8)
    })
  }
)