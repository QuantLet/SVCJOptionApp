[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **SVCJOptionApp** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of QuantLet : SVCJOptionApp

Published in : Master Thesis

Description : 'Shiny App using the SVCJ (Stochastic Volatility with Correlated Jumps) model to estimate Option Prices for some cryptocurrencies and the CRIX Index'

Keywords : 'CRIX, Bitcoin, cryptocurrency, option pricing, risk neutral density, SVCJ'

Author : Ivan Perez

Submitted : 18.10.2018

Datafile : SVCJOptionApp.R

Input: 
- svcj_results.Rda: Calibrated parameters of the SVCJ for different cryptos
- simulated_returns.Rda: Simulated crypto returns using the SVCJ calibrated parameters
- garch_residuals.Rda: GARCH model to compare vs SVCJ model
- www: folder containing jpeg and png files required for the app
- Data: folder containing crypto prices (csv files) and some intermediate R files 

Output: 
- Option Price table (csv file) after running the App (online accesible via https://svcjoptionpricing.shinyapps.io/optionapp/)  

Example : 'Call Option prices for Ethereum with respect to different strikes K and time to maturity t'

```

### R Code
```r

# Clear Global Environment
rm(list=ls(all=TRUE))

# please change to your working directory
#setwd("~/Ivan/MSc Statistics/Thesis/Code/Final Code/Optionapp")

# -----------------
# Libraries loading
# -----------------


# List of libraries to be used
lib <- list("MASS", "ggplot2", "ggplot2", "readr", "Rlab", 
            "shiny", "shinydashboard", "plotly", "rugarch", 
            "forecast", "DT")

# Installing or calling the libraries
invisible(lapply(lib, function(x){
  result <- library(x, logical.return=T, character.only =T)
  if(result == F) install.packages(x)
  library(x, character.only =T)
  print(paste0(x, " loaded"))
}))

rm(lib)

#library(MASS)
#library(ggplot2)
#library(readr)
#library(Rlab)
#library(shiny)
#library(shinydashboard)
#library(plotly)
#library(rugarch)
#library(forecast)
#library(DT)


# -----------------------------
# Data Loading (crpytos prices)
# -----------------------------

load("prices.Rda")

# ------------------------------------------------------
# Load SVCJ parameters (comming from svcj_all_cryptos.R)
# ------------------------------------------------------

load("svcj_results.Rda")

# ---------------------------------------------------------------------
# Load GARCH model residuals for comparison (coming from garch_model.R)
# ---------------------------------------------------------------------

load("garch_residuals.Rda")

# --------------------------------------------------------
# Load Simulated Returns (comming from simulated_returns.R
# --------------------------------------------------------

load("simulated_returns.Rda")

# --------------------------------
# Create Shiny UI (User Interface)
# --------------------------------

convertMenuItem <- function(mi,tabName) {
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  mi
}

ui <- shinyUI(
  dashboardPage(
    dashboardHeader(title = "Cryptocurrency Option Pricing",
                    titleWidth = 300),
    dashboardSidebar(
      sidebarMenu(
        convertMenuItem(
          menuItem("Welcome", tabName = "welcome"), tabName = "welcome"
        ),
        convertMenuItem(
          menuItem("Step 1 - Select Crypto", tabName = "step1",
                   selectInput(inputId = "crypto",
                               label = "Select Crypto",
                               choices = c("crix", "btc", "eth", "xrp", "bch", "eos", "ltc", "ada", "usdt", "xmr", "etc", "dash", "ont"),
                               selected = "crix")
          ), tabName = "step1"),
        convertMenuItem(
          menuItem("Step 2 - SVCJ Parameters", tabName = "step2",
                   uiOutput('resetable_input'),
                   fluidRow(column(12,align = "center", actionButton(inputId = "resparam", "Reset Parameters")))
          ), tabName = "step2"),
        convertMenuItem(
          menuItem("Step 3 - Option Type", tabName = "step3",
                   selectInput(inputId = "optiontype",
                               label = "Select option type",
                               choices = c("Call Option", "Put Option"),
                               selected = "Call Option"),
                   textInput(inputId="interest", label = "Interest Rate r (in decimals)", value = 0.03),
                   uiOutput("initial_price")
          ), tabName = "step3"),
        convertMenuItem(
          menuItem("Step 4 - Start Simulation", tabName = "step4",
                   actionButton(inputId = "startsimul", 
                                label = HTML("Click to start simulation <br/> (will only change tabs c and d)"))
          ), tabName = "step4"),
        convertMenuItem(
          menuItem("Code and References", tabName = "references"
          ), tabName = "references")),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      h6(align = "center", span("This app is intended for educational", style="color:grey"), br(),
         span("purposes only. None of the content", style="color:grey"), br(),
         span("should be taken in part or in whole as", style="color:grey"), br(),
         span("financial advice to buy, hold or sell", style="color:grey"), br(),
         span("any security, product or service,", style="color:grey"), br(), 
         span("not even cryptocurrencies", style="color:grey"))
      
    ),
    dashboardBody(
      tags$head(tags$style(HTML('
                                
                                /* logo */
                                .skin-blue .main-header .logo {
                                background-color: #F3F1E4;
                                color: #000000;
                                }
                                
                                /* logo when hovered */
                                .skin-blue .main-header .logo:hover {
                                background-color: #F3F1E4;
                                }
                                
                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                background-color: #F3F1E4;
                                }
                                
                                # /* main sidebar */
                                # .skin-blue .main-sidebar {
                                # background-color: #F3F1E4;
                                # }
                                # 
                                # /* active selected tab in the sidebarmenu */
                                # .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                # background-color: #F3F1E4;
                                # }
                                # 
                                # /* other links in the sidebarmenu */
                                # .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                # background-color: #F3F1E4;
                                # color: #000000;
                                # }
                                # 
                                # /* other links in the sidebarmenu when hovered */
                                # .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                                # background-color: #F3F1E4;
                                # }
                                
                                /* toggle button when hovered  */
                                .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                background-color: #F3F1E4;
                                }
                                
                                /* body */
                                .content-wrapper, .right-side {
                                background-color: #FFFFFF;
                                }
                                
                                '))),
      
      tabItems(
        tabItem(tabName = "welcome", class = "active",
                h1("Cryptocurrency Option Pricing Using SVCJ model"),
                br(),
                br(),
                p("The cryptocurrency market is a practically new and very interesting market with increasing market capitalization. Due to its early stages of development, a market for contingent claims hat not been built up yet. One of the reasons, among others, is the lack of pricing tools based on solid econometric models. Here the SVCJ (Stochastic Volatility with Correlated Jumps) is used to simulate the price of several cryptocurrencies, most specifically the ones that compose the CRIX index, in order to get option prices. The SVCJ parameters were calibrated in a previous MCMC (Markov Chain Monte Carlo) algorithm. The code and relevant information can be found in the documentation section"),
                br(),
                h5("Do not forget to visit thecrix.de (click on the following image):"),
                br(),
                tags$a(img(src = "crix.PNG", height = 100, width = 200), href = "http://thecrix.de/"),
                br(),
                br(),
                br(),
                br(),
                h5("To start follow the steps on the left side panel"),
                br()
        ),
        tabItem(tabName = "step1",
                h2("You can select from the following cryptos"),
                br(),
                br(),
                fluidRow(column(3, align = "center", 
                                img(src = "crix.PNG", height = 50, width = 90),
                                h4("CRIX Index (crix)"),
                                br(),
                                img(src = "btc.PNG", height = 50, width = 50),
                                h4("Bitcoin (btc)"),
                                br(),
                                img(src = "eth.PNG", height = 50, width = 50),
                                h4("Ethereum (eth)"),
                                br(),
                                img(src = "xrp.PNG", height = 50, width = 50),
                                h4("Ripple (xrp)")
                ),
                column(3, align = "center", 
                       img(src = "bch.PNG", height = 50, width = 50),
                       h4("Bitcoin Cash (bch)"),
                       br(),
                       img(src = "eos.PNG", height = 50, width = 50),
                       h4("EOS (eos)"),
                       br(),
                       img(src = "ltc.PNG", height = 50, width = 50),
                       h4("Litecoin (ltc)")
                ),
                column(3, align = "center", 
                       img(src = "ada.PNG", height = 50, width = 50),
                       h4("Cardano (ada)"),
                       br(),
                       img(src = "usdt.PNG", height = 50, width = 50),
                       h4("Tether (usdt)"),
                       br(),
                       img(src = "xmr.PNG", height = 50, width = 50),
                       h4("Monero (xmr)")
                ),
                column(3, align = "center", 
                       img(src = "etc.PNG", height = 50, width = 50),
                       h4("Ethereum Classic (etc)"),
                       br(),
                       img(src = "dash.PNG", height = 50, width = 50),
                       h4("DASH (dash)"),
                       br(),
                       img(src = "ont.PNG", height = 50, width = 50),
                       h4("Ontology (ont)")
                )
                )
        ),
        tabItem(tabName = "step2",
                h1("The SVCJ Model"),
                h4("Stochastic Volatility with Correlated Jumps"),
                br(),
                p("Let \\(S_t\\) be the price process, \\(d\\log(S_t)\\) the log return and \\(V_t\\) the volatility process, then the SVCJ dynamic are the following:"),
                br(),
                withMathJax("$$d\\log(S_t) = \\mu dt + \\sqrt{V_t} dW_t^{(s)} + Z_t^{(y)}dN_t  $$"),
                withMathJax("$$dV_t = \\kappa(\\theta - V_t)dt + \\sigma_v\\sqrt{V_t} dW_t^{(v)} + Z_t^{(v)}dN_t$$"),
                withMathJax("$$Cov(dW_t^{(s)}, dW_t^{(v)} = \\rho dt$$"),
                withMathJax("$$P(dN_t = 1) = \\lambda dt$$"),
                br(),
                p('The parameters \\(\\kappa\\) and \\(\\theta\\) are the volatility mean reversion rate
                  and mean reversion level respectively. The parameter \\(\\theta\\)
                  is the long run mean of \\(V_t\\) and the process reverts to this level
                  at a speed governed by the parameter \\(\\kappa\\). The parameter 
                  \\(\\sigma_v\\) is referred to as the volatility in volatility. 
                  \\(W^{(s)}\\) and \\(W^{(v)}\\)
                  are two correlated standard Brownian motions with correlation \\(\\rho\\).
                  \\(N_t\\) is a pure jump process with a constant mean-jump arrival
                  rate \\(\\lambda\\). The random jump sizes are determined by \\(Z_t^{y}\\)
                  and \\(Z_t^{v}\\). The distribution of the random jump sizes are:'),
                withMathJax("$$Z_t^{y}|Z_t^{v} \\sim N(\\mu_y + \\rho_j Z_t^{v}, \\sigma_y^2)$$"),
                withMathJax("$$Z_t^{v} \\sim Exp(\\mu_v)$$"),
                p("The empirical calibration of parameters is based on the following 
                  Euler discretization:"),
                withMathJax("$$Y_t = \\mu + \\sqrt{V_{t-1}} \\varepsilon_t^y + Z_t^yJ_t $$"),
                withMathJax("$$V_t = \\alpha + \\beta V_{t-1} + \\sigma_v \\sqrt{V_{t-1}} \\varepsilon_t^v + Z_t^vJ_t $$"),
                p("Where \\(Y_{t+1}\\) = \\(\\log(S_{t+1}/S_t)\\) is the log return, 
                  \\(\\alpha = \\kappa\\theta\\), \\(\\beta = 1 - \\kappa\\) and 
                  \\(\\varepsilon_t^y\\), \\(\\varepsilon_t^v\\) are the \\(N(0,1)\\) variables
                  with correlation \\(\\rho\\). \\(J_t\\) is a Bernoulli random variable
                  with \\(P(J_t = 1) = \\lambda \\) and the jump sizes \\(Z_t^y\\) and 
                  \\(Z_t^v\\) are distributed as specified before."),
                br(),
                p("The parameters were calibrated using a MCMC (Markov Chain Monte Carlo) simulation. For more details about the calibration process
                  please refer to the documentation.")
                ),
        tabItem(tabName = "step3",
                h1("Option Pricing set up"),
                br(),
                h3("A brief reminder to start"),
                br(),
                h5("A European Option can only be exercised once the option has reached the expiry date, which is denoted with the letter \\(T\\). A Call Option gives the holder the right to buy at a specific price, known as exercise price and denoted by \\(K\\). The Call Option pay-off is determined by the following:"),
                withMathJax("$$C_T = \\max(0, S_t - K) = \\phi(S_t)$$"),
                h5("Where \\(S_T\\) represents the price of the asset at maturity. Similarly, a Put Option gives the holder the right to sell an asset. The Put Option pay-off is given by the following formula:"),
                withMathJax("$$P_T = \\max(0, K - S_T) = \\phi(S_t)$$"),
                h3("Risk neutral valuation"),
                br(),
                h5("For the present Option Price simulation I use a risk neutral valuation, where the fair value of the derivative is the expected value of its present pay-off under a risk neutral density (RND) \\(Q\\). In that way for any date before expiration, denoted by \\(t\\), the option price is the expected value its pay-off discounted by a factor, which in the case of a Call Option will be:"),
                withMathJax("$$C_t = E^Q[\\exp^{-r(T-t)} \\phi(S_t)]$$") ,
                h5("There are several ways to compute the RND but due to the lack of real option prices to calibrate a proper model, parametric or not, I make use of the Girsanov Theorem which relates the RND \\(Q\\) with the real probability \\(P\\) in the following way:"),
                withMathJax("$$C_t = E^Q[\\exp^{-r(T-t)} \\phi(S_t)] = E^P[\\exp^{-(r + \\frac{\\lambda^2}{2} + \\frac{\\lambda}{T}W_t^P)T} \\phi(S_t)]$$"),
                h5("As you can notice from the previous equation, the only new variable is \\(\\lambda\\) which is defined as the risk premim (i.e sharpe ratio). For the present simulation, the risk premium is assumed to be 0. This could be debatable, but since there are no real options prices is one way to avoid further assumptions that could also be debatable. Additionally, the long term return \\(\\mu\\) for several cryptos is between 0.02 and 0.03 which is also close to the 3 months Treasury Bond rate."),
                br(),
                h3("Simulating Asset Price"),
                br(),
                h5("To simulate the asset price, \\(S_T\\), we will make use of our already introduced SVCJ model. For every crypto 5000 different price paths are going to be simulated, each containing 1000 observations. As a reminder the SVCJ parameters were calculated for the returns, instead of prices, so the real simulation will be for 5000 return paths. Then, using an initial price, the prices are obtained from the simulated returns. To better understand the process the steps are summarized:"),
                br(),
                h5("1) Calibrate the SVCJ parameters for the returns (already done and parameters can be found in Step 2)"),
                h5("2) Simulate 5000 return paths and using an initial price transform them in 5000 price paths"),
                h5("3) Compute the pay-offs, for a given set of \\(K\\) and \\(\\tau = T-t\\)"),
                h5("4) Average the 5000 pay-offs, for a given set of \\(K\\) and \\(\\tau = T-t\\)"),
                h5("5) Discount the pay-offs to get the final price"),
                h5("6) Compute the implied volatility by using the Black-Scholes formula"),
                br(),
                h5("To proceed please select the option type (call or put), the annual interest free rate \\(r\\) and the Initial Price (default is the average price of the selected crypto)")
        ),
        tabItem(tabName = "step4",
                fluidRow(
                  tabBox(width = 12,
                         tabPanel(title = "a) Simulated Jumps from SVCJ MCMC",
                                  splitLayout(cellWidths = c("50%","50%"),
                                              plotOutput("jump_returns"),
                                              plotOutput("jump_vol"))),
                         tabPanel(title = "b) Model Residuals",
                                  splitLayout(cellWidths = c("50%","50%"),
                                              plotOutput("residuals"),
                                              fluidRow(column(10,align = "left", verbatimTextOutput("diebold"),
                                                              h5("Null Hypothesis: the two methods have the same forecast accuracy")))  
                                  )),
                         tabPanel(title = "c) Option Price Table",
                                  helpText("Please do not click Simulate Button until actual simulation is complete"),
                                  uiOutput("table_title"),
                                  tableOutput("option_price_table"),
                                  downloadButton(outputId = "download_csv", label = "Download Table.csv")),
                         tabPanel(title = "d) Implied Volatility",
                                  plotOutput("imp_vol"))
                  )
                )
        ),
        tabItem(tabName = "references",
                h2("Follow the link column to access the documents"),
                br(),
                dataTableOutput("refer"),
                br(),
                p("Last update: 19.10.2018")
        )
                )
                )
      )
      )

# -------------------
# Create Shiny server
# -------------------

server <- shinyServer(function(input, output) {
  
  output$resetable_input <- renderUI({
    parameters = svcj_results[[input$crypto]]$parameters[,2]
    times <- input$resparam
    shiny::div(id=letters[(times %% length(letters)) + 1],
               fluidRow(column(6, align = "center",
                               textInput(inputId = "mu", label =  "\u03BC", value = parameters[1]),
                               textInput(inputId = "mu_y", label = HTML("&mu;<sub>y</"), value = parameters[2]),
                               textInput(inputId = "sigma_y", label = HTML("&sigma;<sub>y</"), parameters[3]),
                               textInput(inputId = "lambda", label = HTML("&lambda;"), parameters[4]),
                               textInput(inputId = "alpha", label = HTML("&alpha;"), parameters[5])),
                        column(6, align = "center",
                               textInput(inputId = "beta", label = HTML("&beta;"), parameters[6]),
                               textInput(inputId = "rho", label = HTML("&rho;"), parameters[7]),
                               textInput(inputId = "sigma_v", label = HTML("&sigma;<sub>v</"), parameters[8]),
                               textInput(inputId = "rho_j", label = HTML("&rho;<sub>j</"), parameters[9]),
                               textInput(inputId = "mu_v", label = HTML("&mu;<sub>v</"), parameters[10]))))
  })
  
  output$initial_price <- renderUI({
    textInput(inputId = "init_price", label = "Initial Price (USD)", value = round(mean(prices[[input$crypto]]$price, na.rm = TRUE)))
  })
  
  output$jump_returns <- renderPlot({
    l1 <- length(svcj_results[[input$crypto]]$jumps_price)
    l2 <- length(prices[[input$crypto]]$date)
    plot(svcj_results[[input$crypto]]$jumps_price ~ prices[[input$crypto]]$date[(l2-l1+1):l2], type = "l", xaxt = "n", xlab = "Date", ylab = "", main = paste0("Jumps in returns for ", toupper(input$crypto)))
    axis.Date(1, at=seq(min(prices[[input$crypto]]$date[(l2-l1+1):l2]), max(prices[[input$crypto]]$date[(l2-l1+1):l2]), by="2 mon"), format="%m-%y", cex.axis = .8)
    
  })
  
  output$jump_vol <- renderPlot({
    l1 <- length(svcj_results[[input$crypto]]$jumps_volatility)
    l2 <- length(prices[[input$crypto]]$date)
    plot(svcj_results[[input$crypto]]$jumps_volatility ~ prices[[input$crypto]]$date[(l2-l1+1):l2], type = "l", xaxt = "n", xlab = "Date", ylab = "", main = paste0("Jumps in volatility for ", toupper(input$crypto)))
    axis.Date(1, at=seq(min(prices[[input$crypto]]$date[(l2-l1+1):l2]), max(prices[[input$crypto]]$date[(l2-l1+1):l2]), by="2 mon"), format="%m-%y", cex.axis = .8)
  })
  
  output$residuals <- renderPlot({
    resid <- garch_residuals[[input$crypto]]
    # Lenght of SVCJ residuals and GARCH residuals could differ (input prices for SVCJ was shorter in some cases)
    l1 <- length(svcj_results[[input$crypto]]$residuals)
    l2 <- length(resid)
    # Take only last residuals from GARCH equal to SVCJ residuals length
    resid <- resid[(l2-l1+1):l2]
    # Plot
    par(pty="s")
    qqnorm(y = svcj_results[[input$crypto]]$residuals, xlim = c(-4, 4), ylim = c(-4, 4), col = "blue", cex = 0.8, main = paste0("Residuals for ", toupper(input$crypto)), 
           ylab = "Quantiles of Input Sample", xlab = "Standard Normal Quantiles")
    par(new=T)
    qqnorm(y = resid, xlim = c(-4, 4), ylim = c(-4, 4), col = "green", cex = 0.8, main = "", ylab = "", xlab = "")
    abline(a = 0, b = 1, col = "red", lwd = 2)
    legend(x=1.8,y=-3,c("SVCJ","GARCH"),cex=.8,col=c("blue","green"), lty = c(1,1), bty = "n", y.intersp=1.2)
  })
  
  output$diebold <- renderPrint({
    resid <- garch_residuals[[input$crypto]]
    # Lenght of SVCJ residuals and GARCH residuals could differ (input prices for SVCJ was shorter in some cases)
    l1 <- length(svcj_results[[input$crypto]]$residuals)
    l2 <- length(resid)
    # Take only last residuals from GARCH equal to SVCJ residuals length
    resid <- resid[(l2-l1+1):l2]
    svcj_residuals <- svcj_results[[input$crypto]]$residuals
    garch_residuals <- resid
    dm.test(svcj_residuals,garch_residuals,h=1)
  })
  
  output$table_title <- renderText({
    paste0(input$optiontype, " prices for ", toupper(input$crypto))
  })
  
  
  simulated_prices <- reactive({
    
    if (input$startsimul == 0)
      return()
    
    
    isolate({
      
      withProgress(message = "Simulating Option Prices", value = 0, {
        validate(
          need(input$init_price, "Please verify the Step 3 - Option type")
        )
        init_price <- as.numeric(input$init_price) 
        n <- 361
        iter2 <- 5000
        sim_price <- data.frame(matrix(NA, nrow = iter2, ncol = n))
        sim_price[,1] <- init_price
        
        for (i in 2:n) {
          sim_price[,i] <- (simulated_returns[[input$crypto]][,i]/sqrt(250) + 1)*sim_price[,i-1] 
        }
        
        return(sim_price)
        
      })
      
      
    })
    
  })
  
  option_price_df <- reactive({
    
    if (input$startsimul == 0)
      return()
    
    # Get simulated prices
    sim_price <- simulated_prices()
    init_price <- as.numeric(input$init_price)
    
    # Create time to maturity vector
    tau <- c(1,7,30,60,90,180,360)
    # Create exercise price K according to initial price
    seq1 <- seq(from = 0.85, to = 1.15, 0.015) 
    Ex_K <- vector()
    for (i in 1:length(seq1)) {
      Ex_K[i] <- round(init_price*seq1[i],0) 
    }
    r <- as.numeric(input$interest)
    option_price <- data.frame(matrix(NA, nrow = length(Ex_K), ncol = length(tau)))
    
    for (i in 1:length(Ex_K)){
      payoffs <- as.matrix(sim_price[,c(tau+1)])
      for (j in 1:length(tau)) {
        if (input$optiontype == "Call Option") { 
          payoffs[,j] <- pmax(0,payoffs[,j]-Ex_K[i])
        } else {
          payoffs[,j] <- pmax(0,Ex_K[i]-payoffs[,j])
        }
      } 
      option_price[i,] <- colMeans(payoffs, na.rm = TRUE)
    }
    
    for (i in 1:length(tau)) {
      discount_factor <- exp(-r*tau[i]/365)
      option_price[,i] <- option_price[,i]*discount_factor
    }
    
    # Organize table 
    option_price$K <- Ex_K
    option_price <- option_price[,c(8,1,2,3,4,5,6,7)]
    tau_symbol <- intToUtf8(0x03C4) 
    names(option_price)[-1] <- paste0(tau_symbol,"_", tau)
    
    # return table
    return(option_price)
    
  })  
  
  output$option_price_table <- renderTable({
    option_price_df()
  })
  
  output$download_csv <- downloadHandler(
    filename = function(){
      paste0(input$optiontype," Prices for ", toupper(input$crypto), ".csv")
    },
    content = function(file) {
      write.csv(option_price_df(), file)
    }
  )
  
  output$imp_vol <- renderPlot({
    
    if (input$startsimul == 0)
      return()
    
      sim_price <- simulated_prices()
      
      bsValue <- function(S, r, sig, K, T, type) {
        d1 = (log(S/K)+(r+0.5*sig^2)*T) / (sig*sqrt(T))
        d2 = d1-sig*sqrt(T)
        
        if (type == "Call") {
          value = S*pnorm(d1) - K*exp(-r*T)*pnorm(d2)
          return(value)
        } 
        if (type == "Put") {
          value = K*exp(-r*T)*pnorm(-d2) - S*pnorm(-d1)
          return(value)
        }
      }
      
      bsVega <- function(S,r,sig,K,T,type) {
        d1 = (log(S/K)+(r+0.5*sig^2)*T)/(sig*sqrt(T))
        return(S*sqrt(T)*dnorm(d1))
      }
      
      findvola <- function(C, S, r, K, T, type) {
        maxIterations <- 100
        precision <- 1.0e-10
        
        sig <- 0.2
        for (i in 1:maxIterations) {
          value <- bsValue(S,r,sig,K,T,type)
          vega <- bsVega(S,r,sig,K,T,type)
          
          
          if(vega==0) {
            sig <- 1e-300
            return(sig)
          }
          
          diff <- C - value
          ifelse(abs(diff) < precision, 
                 return(sig), 
                 sig <- sig + diff/vega) 
          
        }
        return(sig)
        
      }
      
      V_market_1 = unname(unlist(option_price_df()[,2]))
      V_market_30 = unname(unlist(option_price_df()[,4]))
      S_1 = mean(sim_price[,2], na.rm = TRUE)
      S_30 = mean(sim_price[,31], na.rm = TRUE)
      r = as.numeric(input$interest)/365
      strike = unname(unlist(option_price_df()[,1]))
      T_1 = 1
      T_30 = 30
      if (input$optiontype == "Call Option") {
        type = "Call" 
      } else {
        type = "Put"
      }
      
      implied_volatility_1 <- vector()
      implied_volatility_30 <- vector()
      for (i in 1:length(strike)) {
        implied_volatility_1[i] <- findvola(V_market_1[i], S_1, r,strike[i], T_1, type)
        implied_volatility_30[i] <- findvola(V_market_30[i], S_30, r,strike[i], T_30, type)
        
      } 
      
      par(pty="s")
      plot(implied_volatility_1 ~ strike, type = "l", xlab = "Strike Price", ylab = "Implied Volatility",lwd = 2, cex.axis = .8, main =paste0("Implied Volatiliy for ", toupper(input$crypto), " using Black-Scholes"))
      par(new=TRUE)
      plot(implied_volatility_30 ~ strike, type = "l", xlab = "", ylab = "",lwd = 2, yaxt = "n",xaxt = "n",col = "blue", cex.axis = .8)
      legend(x = "top", legend = c("t = 1","t = 30"),cex=.8,col=c("black","blue"), lty = c(1,1), bty = "n", y.intersp=1.2)
    
    
  })
  
  
  
  output$refer <- renderDataTable({
    ref <- data.frame(
      Title <- c("Code", "Master Thesis", "Discussion Paper 1", "Discussion Paper 2", "CRIX Index"),
      Description <- c("Quanlet under the name SVCJOptionAPP",
                       "Perez, I. Graphical User Interface for pricing Cryptocurrency Options. October 2018. Will be available soon on the Ladislaus von Bortkiewicz Chair of Statistics",
                       "Chen, C. Y., W. K. Haerdle, A. J. Hou, and W. Wang (2018). Pricing cryptocurrency options: the case of crix and bitcoin. Discussion Paper 2018-004",
                       "Chen, S., C. Y.-H. Chen, W. Haerdle, T. Lee, and B. Ong (2016). A first econometric analysis of the crix family. Discussion Paper 2016-031",
                       "Website of the Cryptocurrency Index"),
      link <- c('<a href="http://www.quantlet.de/">
                <img src="quanlet.PNG" height="20"></img>',
                '<a href="https://www.wiwi.hu-berlin.de/de/professuren/vwl/statistik/research/dmb">
                <img src="pdf_logo.png" height="20"></img>',
                '<a href="https://www.wiwi.hu-berlin.de/de/forschung/irtg/results/discussion-papers/discussion-papers-2017-1/irtg1792dp2018-004.pdf">
                <img src="pdf_logo.png" height="20"></img>',
                '<a href="http://sfb649.wiwi.hu-berlin.de/papers/pdf/SFB649DP2016-031.pdf">
                <img src="pdf_logo.png" height="20"></img>',
                '<a href="http://thecrix.de/">
                <img src="crix.PNG" height="20"></img>')
    )
    DT::datatable(ref, 
                  options = list(dom = 't',
                                 autoWidth = TRUE,
                                 columnDefs = list(list(width = '200px', targets = "_all"),
                                                   list(className = 'dt-center', targets = 1))), 
                  escape = FALSE, 
                  class = "compact", 
                  rownames = FALSE,
                  colnames = c("Title", "link"))
    
    
    
  })
  
})


# -------
# Run App
# -------

app <- shinyApp(ui, server)

# ----------
# Deploy App
# ----------

# library(devtools)
# #install.packages("rsconnect")
# library(rsconnect)
# rsconnect::setAccountInfo(XXXX)
# deployApp()




```

automatically created on 2018-10-18