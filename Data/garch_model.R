# Clear Global Environment
rm(list=ls(all=TRUE))

# please change to your working directory
setwd("~/Ivan/MSc Statistics/Thesis/Code/Final Code")

# -----------------
# Libraries loading
# -----------------

# List of libraries to be used
lib <- list("truncnorm", "MASS", "MCMCpack", "ggplot2", "readr", "Rlab", 
            "shiny", "shinydashboard", "plotly", "rugarch", "forecast")

# Installing or calling the libraries
invisible(lapply(lib, function(x){
  result <- library(x, logical.return=T, character.only =T)
  if(result == F) install.packages(x)
  library(x, character.only =T)
  print(paste0(x, " loaded"))
}))

rm(lib)

# -----------------------------
# Data Loading (crpytos prices)
# -----------------------------

# The following are the actual CRIX index members (last update 09.09.2018)
# for more info about CRIX index please visit: http://thecrix.de/

cryptos_long_name <- c("Cardano (ada)", "Bitcoin Cash (bch)", "Bitcoin (btc)", 
                       "CRIX Index (crix)", "DASH", "EOS", "Ethereum Classic (etc)",
                       "Ethereum (eth)", "Litecoin (ltc)", "IOTA (miota)",
                       "Ontology (ont)", "Tron (trx)", "Tether (usdt)", 
                       "Stellar (xlm)", "Monero (xmr)", "Ripple (xrp)")

cryptos <- c("ada", "bch", "btc", "crix", "dash", "eos",
             "etc", "eth", "ltc", "miota","ont", "trx", 
             "usdt", "xlm", "xmr", "xrp")

prices = list()
for (crypto in cryptos) { 
  temp <- read_csv(paste0(crypto,"_price.csv"))
  if (crypto == "crix") {
    colnames(temp)[1] <- c("snapped_at")
  }
  colnames(temp)[1] <- c("date")
  temp$date <- as.Date(temp$date)
  prices[[crypto]] <- as.data.frame(temp[,c(1,2)])
}
rm(temp, crypto)

# Get btc prices for example
# prices$btc$price

# ---------------------------------
# Create GARCH model for comparison
# ---------------------------------

# Define standard GARCH
garch_model <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)), mean.model = list(armaOrder = c(2,2)), distribution.model = "std")

garch_residuals <- list()
for (i in cryptos) {
  print(i)
  return <- diff(prices[[i]]$price)/prices[[i]]$price[-length(prices[[i]]$price)]*sqrt(250)
  garch_results <- ugarchfit(spec = garch_model, data = return)
  resid <- residuals(garch_results)
  garch_residuals[[i]] <- as.vector(resid) 
}
rm(resid, garch_model,garch_results,i,return)

# Save GARCH residuals
save(garch_residuals, file = "garch_residuals.Rda")
