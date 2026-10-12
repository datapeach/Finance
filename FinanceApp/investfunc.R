# Author: Rosie Schmitz 

#' invest.output()
#' 
#' @description 
#' returns a dataframe for budget to be used in other places of the app
#' loaded in the main application
# this is set with the goal to familiarize myself with using s4 classes 
# investfunc.R

 
#setting the investment (sub)class (for larger portfolio later) ----
setClass(
  "Investment", 
  slots = c(
    ticker = "character", 
    share = "numeric", 
    share_price = "numeric", 
    ttl_value = "numeric"
  )
)

#Getter and Setter for Ticker ----
setGeneric("getTicker", function(x) standardGeneric("getTicker"))

setMethod("getTicker", "Investment", function(x){
  x@ticker #we don't need to return x since x is declared and isn't being changed
})

setGeneric("setTicker", function(x, ...) standardGeneric("setTicker"))

setMethod("setTicker", "Investment", function(x, user_ticker){
  x@ticker = user_ticker
  x
})

#Getter and Setter for Share
setGeneric("getShare", function(x) standardGeneric("getShare"))

setMethod("getShare", "Investment", function(x){
  x@share 
})

setGeneric("setShare", function(x, ...) standardGeneric("setShare"))

setMethod("setShare", "Investment", function(x, user_share){
  x@share = user_share
  x
})

#updater for updateSharePrice ----
setGeneric("updateSharePrice", function(x) standardGeneric("updateSharePrice"))

setMethod("updateSharePrice", "Investment", function(x){
  x@share_price = quantmod::getQuote(x@ticker)$Last
  x #we call x here since we want to retrieve the operated upon value
}) 

#updater for total value ----
setGeneric("updateTotalValue", function(x) standardGeneric("updateTotalValue"))

setMethod("updateTotalValue", "Investment", function(x){
  x@ttl_value = x@share*x@share_price
  x
})

# Define print function for investment class 
setGeneric("printInvest", function(x) standardGeneric("printInvest"))

setMethod("printInvest", "Investment", function(x){
  print(paste0(x@share, " share(s) of '", x@ticker, "' at $", x@share_price, ". Total value = $", x@ttl_value))
})

# setting class for Portfolio class ----

setClass(
  "Portfolio", 
  slots = c(
    investment_list = "list",
    ttl_value = "numeric"
  )
)

# Define a function to update all investments; refresh = FALSE re-totals with the
# prices already fetched instead of requesting new quotes
setGeneric("updatePortfolio", function(x, ...) standardGeneric("updatePortfolio"))

setMethod("updatePortfolio", "Portfolio", function(x, refresh = TRUE){
  for (i in 1:length(x@investment_list)) {
    if (refresh) x@investment_list[[i]] <- updateSharePrice(x@investment_list[[i]])
    x@investment_list[[i]] <- updateTotalValue(x@investment_list[[i]])
  }
  
  cumulative_ttl = 0
  for (investment in x@investment_list) {
    cumulative_ttl = cumulative_ttl + investment@ttl_value
  }
  x@ttl_value = cumulative_ttl
  
  x
}) 

# Define a print function that will print out a portfolio
setGeneric("printPortfolio", function(x) standardGeneric("printPortfolio"))

setMethod("printPortfolio", "Portfolio", function(x){
  x = updatePortfolio(x)
  for (investment in x@investment_list) {
    printInvest(investment)
  }
  print(paste0("Total Portfolio = $", x@ttl_value))
  
}) 

# Define a function to add investment to portfolio and remove investment to portfolio
setGeneric("addInvestToPortfolio", function(x, ...) standardGeneric("addInvestToPortfolio"))

setMethod("addInvestToPortfolio", "Portfolio", function(x, invest){
  #if statement for if it's a valid investment class object
  if(class(invest) != "Investment") {
    warning("The object provided is not of class 'Investment'.")
    return(x)
  }
  
  # check that num of shares and ticker are not null
  if(is.null(invest@share) || is.null(invest@ticker)) {
    warning("Investment must have both 'num_shares' and 'ticker' defined!")
  }
  
  # check that ticker is not already in list
  existing_tickers <- sapply(x@investment_list, function(i) i@ticker) # retrieve a list of existing tickers in investment_list
  if(invest@ticker %in% existing_tickers) {
    warning("Investment with this ticker is already in the portfolio!")
    return(x)
  }
  
  x@investment_list = append(x@investment_list, invest)
  x 
}) 

setGeneric("removeInvestFromPortfolio", function(x, ...) standardGeneric("removeInvestFromPortfolio"))

setMethod("removeInvestFromPortfolio", "Portfolio", function(x, invest){
  #if statement for if it's a valid investment class object 
  if(class(invest) != "Investment") {
    warning("The object provided is not of class 'Investment'.")
    return(x)
  }
  
  # check that num of shares and ticker are not null
  if(is.null(invest@ticker)) {
    warning("Investment must have 'ticker' defined to remove it!")
  }
  
  # check that ticker is not already in list
  existing_tickers <- sapply(x@investment_list, function(i) i@ticker) # retrieve a list of existing tickers in investment_list
  if(!(invest@ticker %in% existing_tickers)) {
    warning("Investment with this ticker is not in the portfolio!")
    return(x)
  }
  tickers_kept <- setdiff(existing_tickers, invest@ticker)
  temp_list <- list()
  
  for (i in tickers_kept) {
    temp_list <- append(temp_list, list(getInvestmentFromPortolio(x, i)))
  }
  x@investment_list <- temp_list
  x 
}) 

#next steps:define 2 func
#get invest from portfolio (pass in ticker and it returns the investment) 
setGeneric("getInvestmentFromPortolio", function(x, ...) standardGeneric("getInvestmentFromPortolio"))
setMethod("getInvestmentFromPortolio", "Portfolio", function(x, ticker){
  for(i in x@investment_list) {
    if(ticker == i@ticker){
      return(i)
    }
  }
  warning("Check again, no matching investment associated with ticker")
  return(NULL)
})
#next func would be replace investment in portfolio (pass in investment)
setGeneric("replaceInvestmentInPortolio", function(x, ...) standardGeneric("replaceInvestmentInPortolio"))
setMethod("replaceInvestmentInPortolio", "Portfolio", function(x, investment){
  existing_tickers <- sapply(x@investment_list, function(i) i@ticker)
  if(!(investment@ticker %in% existing_tickers)) {
    warning("Investment with this ticker is not in the portfolio!")
    return(x)
  }
  x <-removeInvestFromPortfolio(x, investment)
  x <-addInvestToPortfolio(x, investment)
  x
})

#setting the investment manager class ----
setClass(
  "PortfolioManager", 
  slots = c(
    portfolio = "Portfolio",
    money_to_invest = "numeric",
    current_perc = "list",
    target_perc = "list", 
    shares_needed = "list", 
    dollars_needed = "list"
  )
)

# function to set money user wants to invest
setGeneric("setMoneyInvest", function(x, ...) standardGeneric("setMoneyInvest"))

setMethod("setMoneyInvest", "PortfolioManager", function(x, user_money){
  x@money_to_invest = user_money
  x
})

# function to get percentages of investments in portfolio
setGeneric("updateInvestPerc", function(x) standardGeneric("updateInvestPerc"))

setMethod("updateInvestPerc", "PortfolioManager", function(x){
  p = updatePortfolio(x@portfolio, refresh = FALSE)
  ttl_val = p@ttl_value
  temp_list = list()
  for (investment in p@investment_list) {
    temp_list = append(temp_list, list((investment@ttl_value/ttl_val)*100))
  }
  x@current_perc = temp_list
  return (x)
})

# function to set desired percentages of investments in portfolio
setGeneric("setDesiredInvestPerc", function(x, ...) standardGeneric("setDesiredInvestPerc"))

setMethod("setDesiredInvestPerc", "PortfolioManager", function(x, user_desire_perc){
  #as a future improvement, validation check: 1) make sure they sum to 100, 2) make sure length of list = length of current_perc
  #3) no individual entry should be negative
  x@target_perc = user_desire_perc 
  return (x)
})

# func for shares need and dollars needed 
setGeneric("updateFutureNeeds", function(x) standardGeneric("updateFutureNeeds"))

setMethod("updateFutureNeeds", "PortfolioManager", function(x){
 # as a future improvement, validate to make sure target_perc is not empty
 #basically get total pool of money plus extra money I want to invest then I basically get dollars needed then shares
  dollar_temp = list() #this is for dollars needed
  shares_temp = list() #this is for shares needed
  p = updatePortfolio(x@portfolio, refresh = FALSE)
  target_value = p@ttl_value + x@money_to_invest #denominator for everything
  
  cnt = 1
  for (investment in p@investment_list){
    target_dollar = (x@target_perc[[cnt]]*target_value)/100
    current_dollar = investment@ttl_value
    delta_dollar = target_dollar - current_dollar
    delta_shares = (delta_dollar/investment@share_price)
    
    dollar_temp = append(dollar_temp, list(delta_dollar))
    shares_temp[cnt] = list(delta_shares) #since we have cnt we don't need to use [length(shares_temp)+1]
    cnt = cnt+1
  }
  x@dollars_needed = dollar_temp
  x@shares_needed = shares_temp
  
  return (x)
})

# function to return a dataframe in format of the one in the google sheets
setGeneric("returnPortfolioManagerTable", function(x) standardGeneric("returnPortfolioManagerTable"))

setMethod("returnPortfolioManagerTable", "PortfolioManager", function(x){
  p = updatePortfolio(x@portfolio, refresh = FALSE)
  ticker_list = list()
  shares_list = list()
  dollars_list = list()
  for (investment in p@investment_list){
    ticker_list = append(ticker_list, list(investment@ticker))
    shares_list = append(shares_list, list(investment@share))
    dollars_list = append(dollars_list, list(investment@ttl_value))
  }
    
  df_temp = data.frame(`Ticker` = unlist(ticker_list),
                       `Shares` = unlist(shares_list),
                       `Dollars` = unlist(dollars_list), 
                       `Desired Percent` = unlist(x@target_perc), 
                       `Shares Needed` = unlist(x@shares_needed), 
                       `Dollars Needed` = unlist(x@dollars_needed), 
                       check.names = FALSE
                      )

return (df_temp)
})


# func top print this 
setGeneric("printPortfolioManager", function(x) standardGeneric("printPortfolioManager"))

setMethod("printPortfolioManager", "PortfolioManager", function(x){
  print(glue("Money to Invest: ${round(x@money_to_invest,2)}"))
  df_temp = returnPortfolioManagerTable(x) %>%
    mutate(Dollars = glue("${round(Dollars, 2)}"), 
           `Desired Percent` = glue("{`Desired Percent`}%"), 
           `Shares Needed` = round(`Shares Needed`, 2), 
           `Dollars Needed` = glue("${round(`Dollars Needed`, 2)}"))
  print(df_temp)
})





