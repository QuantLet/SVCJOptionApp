# Clear Global Environment
rm(list=ls(all=TRUE))

# please change to your working directory
setwd("~/Ivan/MSc Statistics/Thesis/Code/Final Code/Optionapp/Data")

# -----------------
# Libraries loading
# -----------------

# List of libraries to be used
lib <- list("truncnorm", "MASS", "MCMCpack", "ggplot2", "readr", "Rlab")

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

# The following were CRIX index members on 31.09.2018.
# for more info about CRIX index please visit: http://thecrix.de/

cryptos_long_name <- c("Cardano (ada)", "Bitcoin Cash (bch)", "Bitcoin (btc)",
                        "CRIX Index (crix)", "DASH", "EOS", "Ethereum Classic (etc)",
                        "Ethereum (eth)", "Litecoin (ltc)",
                        "Ontology (ont)", "Tether (usdt)",
                       "Monero (xmr)", "Ripple (xrp)")

cryptos <- c("ada", "bch", "btc", "crix", "dash", "eos",
              "etc", "eth", "ltc","ont",
              "usdt", "xmr", "xrp")

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

# ------------------
# Some Initial Plots
# ------------------

# Summary of data
for (crypto in cryptos) {
  print(paste0("Summary of ", crypto, " with sd of: ",sd(prices[[crypto]]$price)))
  print(summary(prices[[crypto]]$price))
  }
rm(crypto)

# mean(prices$btc$price[255:length(prices$btc$price)])
# sd(prices$btc$price[255:length(prices$btc$price)])
# mean(prices$ltc$price[460:length(prices$ltc$price)])
# sd(prices$ltc$price[460:length(prices$ltc$price)])
# mean(prices$usdt$price[1110:length(prices$usdt$price)])
# sd(prices$usdt$price[1110:length(prices$usdt$price)])


# Time series plots of prices
price_plots <- list()
for (i in 1:length(cryptos)) {
  plot(prices[[cryptos[i]]]$price ~ prices[[cryptos[i]]]$date, type = "l",xaxt = "n", col = "blue", lwd = 1, cex.axis = .8, xlab = "Date", ylab = "Daily price", main = cryptos_long_name[i])
  axis.Date(1, at=seq(min(prices[[cryptos[i]]]$date), max(prices[[cryptos[i]]]$date), by="2 mon"), format="%m-%y", cex.axis = .8)
  price_plots[[cryptos[i]]] <- recordPlot()
  }
rm(i)

# Plot btc price for example
# price_plots$btc

# Price plot (4 cryptos with highest markcap)
par(mfrow = c(2,2), mar = c(3,3,2,1))
plot(prices[["crix"]]$price ~ prices[["crix"]]$date, type = "l",xaxt = "n", col = "blue", lwd = 1, cex.axis = .8, xlab = "", ylab = "", main = "CRIX Price")
axis.Date(1, at=seq(min(prices[["crix"]]$date), max(prices[["crix"]]$date), by="2 mon"), format="%m-%y", cex.axis = .8)
mtext(text = "Date", side = 1,line = 2, cex = 0.8)
mtext(text = "Daily Price", side = 2,line = 2, cex = 0.8)
plot(prices[["btc"]]$price ~ prices[["btc"]]$date, type = "l",xaxt = "n", col = "blue", lwd = 1, cex.axis = .8, xlab = "", ylab = "", main = "BTC Price")
axis.Date(1, at=seq(min(prices[["btc"]]$date), max(prices[["btc"]]$date), by="2 mon"), format="%m-%y", cex.axis = .8)
mtext(text = "Date", side = 1,line = 2, cex = 0.8)
mtext(text = "Daily Price", side = 2,line = 2, cex = 0.8)
plot(prices[["eth"]]$price ~ prices[["eth"]]$date, type = "l",xaxt = "n", col = "blue", lwd = 1, cex.axis = .8, xlab = "", ylab = "", main = "ETH Price")
axis.Date(1, at=seq(min(prices[["eth"]]$date), max(prices[["eth"]]$date), by="2 mon"), format="%m-%y", cex.axis = .8)
mtext(text = "Date", side = 1,line = 2, cex = 0.8)
mtext(text = "Daily Price", side = 2,line = 2, cex = 0.8)
plot(prices[["xrp"]]$price ~ prices[["xrp"]]$date, type = "l",xaxt = "n", col = "blue", lwd = 1, cex.axis = .8, xlab = "", ylab = "", main = "XRP Price")
axis.Date(1, at=seq(min(prices[["xrp"]]$date), max(prices[["xrp"]]$date), by="2 mon"), format="%m-%y", cex.axis = .8)
mtext(text = "Date", side = 1,line = 2, cex = 0.8)
mtext(text = "Daily Price", side = 2,line = 2, cex = 0.8)

# Date maximum prices
for (i in cryptos) {
  print(paste0("Maximum price of ", i, " on", prices[[i]]$date[which.max(prices[[i]]$price)]))
}
rm(i)

# Price plot (4 cryptos with lowest markcap)
par(mfrow = c(2,2), mar = c(3,3,3,1))
plot(prices[["ont"]]$price ~ prices[["ont"]]$date, type = "l",xaxt = "n", col = "blue", lwd = 1, cex.axis = .8, xlab = "Date", ylab = "Daily price", main = "ONT Price")
axis.Date(1, at=seq(min(prices[["ont"]]$date), max(prices[["ont"]]$date), by="2 mon"), format="%m-%y", cex.axis = .8)
mtext(text = "Date", side = 1,line = 2, cex = 0.8)
mtext(text = "Daily Price", side = 2,line = 2, cex = 0.8)
plot(prices[["etc"]]$price ~ prices[["etc"]]$date, type = "l",xaxt = "n", col = "blue", lwd = 1, cex.axis = .8, xlab = "Date", ylab = "Daily price", main = "ETC Price")
axis.Date(1, at=seq(min(prices[["etc"]]$date), max(prices[["etc"]]$date), by="2 mon"), format="%m-%y", cex.axis = .8)
mtext(text = "Date", side = 1,line = 2, cex = 0.8)
mtext(text = "Daily Price", side = 2,line = 2, cex = 0.8)
plot(prices[["dash"]]$price ~ prices[["dash"]]$date, type = "l",xaxt = "n", col = "blue", lwd = 1, cex.axis = .8, xlab = "Date", ylab = "Daily price", main = "DASH Price")
axis.Date(1, at=seq(min(prices[["dash"]]$date), max(prices[["dash"]]$date), by="2 mon"), format="%m-%y", cex.axis = .8)
mtext(text = "Date", side = 1,line = 2, cex = 0.8)
mtext(text = "Daily Price", side = 2,line = 2, cex = 0.8)
plot(prices[["xmr"]]$price ~ prices[["xmr"]]$date, type = "l",xaxt = "n", col = "blue", lwd = 1, cex.axis = .8, xlab = "Date", ylab = "Daily price", main = "XMR Price")
axis.Date(1, at=seq(min(prices[["xmr"]]$date), max(prices[["xmr"]]$date), by="2 mon"), format="%m-%y", cex.axis = .8)
mtext(text = "Date", side = 1,line = 2, cex = 0.8)
mtext(text = "Daily Price", side = 2,line = 2, cex = 0.8)

# Time series plots of returns
par(mfrow = c(1,1))
returns_plots <- list()
for (i in 1:length(cryptos)) {
  Y <- diff(prices[[cryptos[i]]]$price)/prices[[cryptos[i]]]$price[-length(prices[[cryptos[i]]]$price)]*sqrt(250)
  dat <- prices[[cryptos[i]]]$date[-1]
  plot(Y ~ dat, type = "l",xaxt = "n", col = "blue", lwd = 1, cex.axis = .8, xlab = "Date", ylab = "Daily return", main = cryptos_long_name[i])
  axis.Date(1, at=seq(min(dat), max(dat), by="2 mon"), format="%m-%y", cex.axis = .8)
  returns_plots[[cryptos[i]]] <- recordPlot()
  }
rm(Y, dat, i)

# Plot btc returns for example
# returns_plots$btc

# Returns plot (4 cryptos with highest markcap)
par(mfrow = c(2,2), mar = c(3,3,2.5,1))
for (i in c("crix", "btc", "eth", "xrp")) {
  Y <- diff(prices[[i]]$price)/prices[[i]]$price[-length(prices[[i]]$price)]*sqrt(250)
  dat <- prices[[i]]$date[-1]
  plot(Y ~ dat, type = "l",xaxt = "n", col = "blue", lwd = 1, cex.axis = .8, xlab = "", ylab = "", main = paste0(toupper(i)," Return"))
  axis.Date(1, at=seq(min(dat), max(dat), by="2 mon"), format="%m-%y", cex.axis = .8)
  mtext(text = "Date", side = 1,line = 2, cex = 0.8)
  mtext(text = "Daily return", side = 2,line = 2, cex = 0.8)
  recordPlot()
}
rm(i, Y,dat)

# Returns plot (Some other 4 cryptos)
par(mfrow = c(2,2), mar = c(3,3,2.5,1))
for (i in c("bch", "etc", "ont", "usdt")) {
  Y <- diff(prices[[i]]$price)/prices[[i]]$price[-length(prices[[i]]$price)]*sqrt(250)
  dat <- prices[[i]]$date[-1]
  plot(Y ~ dat, type = "l",xaxt = "n", col = "blue", lwd = 1, cex.axis = .8, xlab = "", ylab = "", main = paste0(toupper(i)," Return"))
  axis.Date(1, at=seq(min(dat), max(dat), by="2 mon"), format="%m-%y", cex.axis = .8)
  mtext(text = "Date", side = 1,line = 2, cex = 0.8)
  mtext(text = "Daily return", side = 2,line = 2, cex = 0.8)
  recordPlot()
}
rm(i, Y,dat)

# ---------------
# SVCJ Estimation
# ---------------

# Due to minimal variaton in the prices of some cryptos during certain periods,
# for the following crpytos, a smaller number of observations is taken into acccount 
# (after visual inspection): 

# btc from 01/08/2014 on 
# ltc from 01/08/2014 on
# usdt from 01/04/2018 on

# Load function to estimate SVCJ model
source("svcj_model.R")
# svcj_model(prices, N = # Iterations, n = # burn in)

# Model results are stored a separate object for each crypto  
ada_svcj <- svcj_model(prices$ada$price, N = 5000, n = 1000)
ada_svcj$parameters
save(ada_svcj,file="ada_svcj.Rda")

bch_svcj <- svcj_model(prices$bch$price, N = 5000, n = 1000)
bch_svcj$parameters
save(bch_svcj,file="bch_svcj.Rda")

which(prices$btc$date == "2014-08-01")
btc_svcj <- svcj_model(prices$btc$price[460:length(prices$btc$price)], N = 5000, n = 1000)
btc_svcj$parameters
save(btc_svcj,file="btc_svcj.Rda")

crix_svcj <- svcj_model(prices$crix$price, N = 5000, n = 1000)
crix_svcj$parameters
save(crix_svcj,file="crix_svcj.Rda")

dash_svcj <- svcj_model(prices$dash$price, N = 5000, n = 1000)
dash_svcj$parameters
save(dash_svcj,file="dash_svcj.Rda")

eos_svcj <- svcj_model(prices$eos$price, N = 5000, n = 1000)
eos_svcj$parameters
save(eos_svcj,file="eos_svcj.Rda")

etc_svcj <- svcj_model(prices$etc$price, N = 5000, n = 1000)
etc_svcj$parameters
save(etc_svcj,file="etc_svcj.Rda")

eth_svcj <- svcj_model(prices$eth$price, N = 5000, n = 1000)
eth_svcj$parameters
save(eth_svcj,file="eth_svcj.Rda")

which(prices$ltc$date == "2014-08-01")
ltc_svcj <- svcj_model(prices$ltc$price[460:length(prices$ltc$price)], N = 5000, n = 1000)
ltc_svcj$parameters
save(ltc_svcj,file="ltc_svcj.Rda")

miota_svcj <- svcj_model(prices$miota$price, N = 5000, n = 1000)
miota_svcj$parameters
save(miota_svcj,file="miota_svcj.Rda")

ont_svcj <- svcj_model(prices$ont$price, N = 5000, n = 1000)
ont_svcj$parameters
save(ont_svcj,file="ont_svcj.Rda")

trx_svcj <- svcj_model(prices$trx$price, N = 5000, n = 1000)
trx_svcj$parameters
save(trx_svcj,file="trx_svcj.Rda")

which(prices$usdt$date == "2018-04-01")
usdt_svcj <- svcj_model(prices$usdt$price[1110:length(prices$usdt$price)], N = 5000, n = 1000)
usdt_svcj$parameters
save(usdt_svcj,file="usdt_svcj.Rda")

which(prices$xlm$date == "2017-04-01")
xlm_svcj <- svcj_model(prices$xlm$price[964:length(prices$xlm$price)], N = 5000, n = 1000)
xlm_svcj$parameters
save(xlm_svcj,file="xlm_svcj.Rda")

xmr_svcj <- svcj_model(prices$xmr$price, N = 5000, n = 1000)
xmr_svcj$parameters
save(xmr_svcj,file="xmr_svcj.Rda")

xrp_svcj <- svcj_model(prices$xrp$price, N = 5000, n = 1000)
xrp_svcj$parameters
save(xrp_svcj,file="xrp_svcj.Rda")

svcj_results <- list(ada = ada_svcj, bch = bch_svcj, btc = btc_svcj,
                     crix = crix_svcj, dash = dash_svcj, eos = eos_svcj,
                     etc = etc_svcj, eth = eth_svcj, ltc = ltc_svcj,
                     miota = miota_svcj, ont = ont_svcj, trx = trx_svcj,
                     usdt = usdt_svcj, xlm = xlm_svcj, xmr = xmr_svcj,
                     xrp = xrp_svcj)
save(svcj_results, file = "svcj_results.Rda")

#load("svcj_results.Rda")

# Trace plot of mu
par(mfrow = c(6,2), mar = c(2,2,1.5,1))
for (i in cryptos[-13]) {
  plot(svcj_results[[i]]$param_evolution$mu, type = "l", col = "blue", lwd = 1, cex.axis = .8, xlab = "Iteration", main = toupper(i))
}
rm(i)

# Trace plot of lambda
par(mfrow = c(6,2), mar = c(2,2,1.5,1))
for (i in cryptos[-13]) {
  plot(svcj_results[[i]]$param_evolution$lambda, type = "l", col = "blue", lwd = 1, cex.axis = .8, xlab = "Iteration", main = toupper(i))
}
rm(i)

# Trace plot of rho
par(mfrow = c(6,2), mar = c(2,2,1.5,1))
for (i in cryptos[-13]) {
  plot(svcj_results[[i]]$param_evolution$rho, type = "l", col = "blue", lwd = 1, cex.axis = .8, xlab = "Iteration", main = toupper(i))
}
rm(i)

# SVCJ parameters
for (i in cryptos) {
  print(paste0("SVCJ Parameters of ", i, ":"))
  print(svcj_results[[i]]$parameters)
}
rm(i)

# SVCJ MSE
for (i in cryptos) {
  print(paste0("MSE of ", i, " = ", round(mean((svcj_results[[i]]$residuals)^2),3)))
}
rm(i)

# SVCJ Volatility
par(mfrow = c(2,2), mar = c(2.5,2.5,2.5,1))
for (i in c("crix", "dash", "eth", "xrp")) {
  plot(svcj_results[[i]]$volatility ~ prices[[i]]$date[-1], type = "l", xaxt = "n", col = "blue", lwd = 1, cex.axis = .8, xlab = "Date", ylab = "Volatility", main = toupper(i))
  axis.Date(1, at=seq(min(prices[[i]]$date[-1]), max(prices[[i]]$date[-1]), by="2 mon"), format="%m-%y", cex.axis = .8)
}
rm(i)

# Jumps in returns and volatility
par(mfrow = c(8,2), mar = c(2,2.5,2,0.3))
for (i in c("btc", "bch", "crix", "dash", "eth", "ltc", "xmr", "xrp")) {
  l1 <- length(svcj_results[[i]]$jumps_price)
  l2 <- length(prices[[i]]$date)
  plot(svcj_results[[i]]$jumps_price ~ prices[[i]]$date[(l2-l1+1):l2], type = "l", xaxt = "n", col = "blue", xlab = "Date", ylab = "", main = toupper(i))
  axis.Date(1, at=seq(min(prices[[i]]$date[(l2-l1+1):l2]), max(prices[[i]]$date[(l2-l1+1):l2]), by="2 mon"), format="%m-%y", cex.axis = .9)
  plot(svcj_results[[i]]$jumps_volatility ~ prices[[i]]$date[(l2-l1+1):l2], type = "l", xaxt = "n", col = "blue", xlab = "Date", ylab = "", main = toupper(i))
  axis.Date(1, at=seq(min(prices[[i]]$date[(l2-l1+1):l2]), max(prices[[i]]$date[(l2-l1+1):l2]), by="2 mon"), format="%m-%y", cex.axis = .9)
}
rm(i,l1,l2)

# QQ-plot of residuals
par(mfrow = c(2,2), mar = c(3,3,3,3))
par(pty="s")
for (i in c("crix", "btc", "eth", "usdt")) {
  qqnorm(y = svcj_results[[i]]$residuals, xlim = c(-4, 4), ylim = c(-4, 4), col = "blue", cex = 0.8, main = toupper(i), ylab="", xlab = "")
  abline(a = 0, b = 1, col = "red", lwd = 2)
  title(ylab = "Quantiles of Input Sample", line=2)
  title(xlab = "Standard Normal Quantiles", line=2)
}

# Default values
# par(mfrow = c(1,1))
# par(pty="m")

