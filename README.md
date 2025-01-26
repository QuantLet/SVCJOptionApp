<div style="margin: 0; padding: 0; text-align: center; border: none;">
<a href="https://quantlet.com" target="_blank" style="text-decoration: none; border: none;">
<img src="https://github.com/StefanGam/test-repo/blob/main/quantlet_design.png?raw=true" alt="Header Image" width="100%" style="margin: 0; padding: 0; display: block; border: none;" />
</a>
</div>

```
Name of QuantLet: SVCJOptionApp

Published in: Master Thesis

Description: Shiny App using the SVCJ (Stochastic Volatility with Correlated Jumps) model to estimate Option Prices for some cryptocurrencies and the CRIX Index

Keywords: CRIX, Bitcoin, cryptocurrency, option pricing, risk neutral density, SVCJ

Author: Ivan Perez

Submitted: 18.10.2018

Datafile: SVCJOptionApp.R

Input: 
- svcj_results.Rda: Calibrated parameters of the SVCJ for different cryptos
- simulated_returns.Rda: Simulated crypto returns using the SVCJ calibrated parameters
- garch_residuals.Rda: GARCH model to compare vs SVCJ model
- www: folder containing jpeg and png files required for the app
- Data: folder containing crypto prices (csv files) and some intermediate R files

Output: 
- Option Price table (csv file) after running the App (online accesible via https://svcjoptionpricing.shinyapps.io/optionapp/)

Example: Call Option prices for Ethereum with respect to different strikes K and time to maturity t

```
