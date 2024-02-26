library(tidyverse)
library(tidyquant)
library(GA)
library(timetk)


#selecting stocks

selectedStocks <-
  c('MSFT', 'PEP', 'LLY', 'WMT', 'JPM', 'DD', 'CBRE', 'XOM', 'F' , 'T')

#getting data

price_data <-
  tq_get(selectedStocks,
         from = '2017-01-01',
         to = '2018-12-31',
         get = 'stock.prices')

#daily returns

log_ret_tidy <-
  price_data %>% group_by(symbol) %>% tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = 'daily',
    col_rename = 'ret'
  )

#convert to wide format

ret_xts <- log_ret_tidy %>%
  spread(symbol, value = ret) %>%
  tk_xts()

# mean daily returns

mean_ret <- colMeans(ret_xts)

# get covariance

cov_mat <- cov(ret_xts) * 252

# fitness function

suggested1 <- runif(10)
suggested2 <- runif(10)
suggested3 <- runif(10)
suggested4 <- runif(10)
suggested5 <- runif(10)
suggested1 <- suggested1/sum(suggested1)
suggested2 <- suggested2/sum(suggested2)
suggested3 <- suggested3/sum(suggested3)
suggested4 <- suggested4/sum(suggested4)
suggested2 <- suggested5/sum(suggested5)
suggested6 <- rep(0.1,10)

maximizeRatio <- function(x) {
  if (sum(x) > 1) {
    result <- -sum(x)
  }
  else {
    result <-
      (((sum(x * mean_ret) + 1) ^ 252 - 1) - 0.025) / sqrt(t(x) %*% (cov_mat %*% x))
  }
}


GA <- ga(
  type = "real-valued",
  fitness = maximizeRatio,
  lower = rep(0, 10),
  upper = rep(1, 10),
  suggestions = suggestedSol,
  pmutation = 0.4,
  maxiter = 1000,
  popSize = 100,
  seed = 123
)

solution <- GA@solution

#random portfolios

balancedPortfolio <- rep(0.1, 10)
random1 <- runif(n = 10)
random2 <- runif(n = 10)
random3 <- runif(n = 10)
random4 <- runif(n = 10)
random5 <- runif(n = 10)
random1 <- random1 / sum(random1)
random2 <- random2 / sum(random2)
random3 <- random3 / sum(random3)
random4 <- random4 / sum(random4)
random5 <- random5 / sum(random5)

# stock data for next time

price_data_2 <-
  tq_get(selectedStocks,
         from = '2019-01-01',
         to = '2020-12-31',
         get = 'stock.prices')

#daily returns

log_ret_tidy_2 <-
  price_data_2 %>% group_by(symbol) %>% tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = 'daily',
    col_rename = 'ret'
  )

#convert to wide format

ret_xts_2 <- log_ret_tidy_2 %>%
  spread(symbol, value = ret) %>%
  tk_xts()

# mean daily returns

mean_ret_2 <- colMeans(ret_xts_2)

#comparison with test data

GAPortfolioReturn = ((sum((GA@solution) * mean_ret_2) + 1) ^ 252 - 1)
balancedPortfolioReturn <-
  ((sum((balancedPortfolio) * mean_ret_2) + 1) ^ 252 - 1)
random1Return <- ((sum((random1) * mean_ret_2) + 1) ^ 252 - 1)
random2Return <- ((sum((random2) * mean_ret_2) + 1) ^ 252 - 1)
random3Return <- ((sum((random3) * mean_ret_2) + 1) ^ 252 - 1)
random4Return <- ((sum((random4) * mean_ret_2) + 1) ^ 252 - 1)
random5Return <- ((sum((random5) * mean_ret_2) + 1) ^ 252 - 1)

barplot(
  c(
    GAPortfolioReturn,
    balancedPortfolioReturn,
    random1Return,
    random2Return,
    random3Return,
    random4Return,
    random5Return
  ),
  main = "Return Comparison",
  ylab = "Annualised Return",
  space = 0.2,
  names.arg = c(
    "Evolved",
    "Balanced",
    "Random 1",
    "Random 2",
    "Random 3",
    "Random 4",
    "Random 5"
  )
)

#minimize risk

minimizeRisk <- function(x) {
  if (sum(x) > 1) {
    result <- -sum(x)
  }
  else {
    result <-
      result <-
      (((sum(x * mean_ret) + 1) ^ 252 - 1) - 0.1) / sqrt(t(x) %*% (cov_mat %*% x))
  }
}

GArisk <- ga(
  type = "real-valued",
  fitness = minimizeRisk,
  lower = rep(0, 10),
  upper = rep(1, 10),
  suggestions = suggestedSol,
  pmutation = 0.4,
  maxiter = 1000,
  popSize = 100,
  seed = 123
)

#maximize return

maximizeReturn <- function(x) {
  if (sum(x) > 1) {
    result <- -sum(x)
  }
  else {
    result <- ((sum(x * mean_ret) + 1) ^ 252 - 1)
  }
}

GAreturn <- ga(
  type = "real-valued",
  fitness = maximizeReturn,
  lower = rep(0, 10),
  upper = rep(1, 10),
  suggestions = suggestedSol,
  pmutation = 0.4,
  maxiter = 1000,
  popSize = 100,
  seed = 123
)

#balance return/risk

balanceRisk <- function(x) {
  if (sum(x) > 1) {
    result <- -sum(x)
  }
  else {
    result <-
      result <-
      (((sum(x * mean_ret) + 1) ^ 252 - 1) - 0.05) / sqrt(t(x) %*% (cov_mat %*% x))
  }
}

GAbalance <- ga(
  type = "real-valued",
  fitness = balanceRisk,
  lower = rep(0, 10),
  upper = rep(1, 10),
  suggestions = suggestedSol,
  pmutation = 0.4,
  maxiter = 1000,
  popSize = 100,
  seed = 123
)

#zero risk free rate
balanceRisk2 <- function(x) {
  if (sum(x) > 1) {
    result <- -sum(x)
  }
  else {
    result <-
      result <-
      ((sum(x * mean_ret) + 1) ^ 252 - 1) / sqrt(t(x) %*% (cov_mat %*% x))
  }
}

GAbalance2 <- ga(
  type = "real-valued",
  fitness = balanceRisk2,
  lower = rep(0, 10),
  upper = rep(1, 10),
  suggestions = suggestedSol,
  pmutation = 0.4,
  maxiter = 1000,
  popSize = 100,
  seed = 123
)

#comparison with test data

GAPortfolioReturn = ((sum((GA@solution) * mean_ret_2) + 1) ^ 252 - 1)
GAriskReturn <- ((sum((GArisk@solution) * mean_ret_2) + 1) ^ 252 - 1)
GAreturnReturn <- ((sum((GAreturn@solution) * mean_ret_2) + 1) ^ 252 - 1)
GAbalance <- ((sum((GAbalance@solution) * mean_ret_2) + 1) ^ 252 - 1)
GAbalance2 <- ((sum((
  GAbalance2@solution
) * mean_ret_2) + 1) ^ 252 - 1)

barplot(
  c(
    GAPortfolioReturn,
    GAriskReturn,
    GAreturnReturn,
    GAbalance,
    GAbalance2
  ),
  main = "Return Comparison",
  ylab = "Annualised Return",
  space = 0.2,
  names.arg = c("Evolved", "Risk Averse", "Aggressive", "Balanced", "0% RRR")
)

#part2

stocksPool <- c(
  'AAPL',
  'ABBV',
  'ABT',
  'ACN',
  'ADBE',
  'AIG',
  'AMD',
  'AMGN',
  'AMT',
  'AMZN',
  'AVGO',
  'AXP',
  'BA',
  'BAC',
  'BK',
  'BKNG',
  'BLK',
  'BMY',
  'BRK.B',
  'c',
  'CAT',
  'CHTR',
  'CL',
  'CMCSA',
  'COF',
  'COP',
  'COST',
  'CRM',
  'CSCO',
  'CVS',
  'CVX',
  'DHR',
  'DIS',
  'DUK',
  'EMR',
  'EXC',
  'F',
  'FDX',
  'GD',
  'GE',
  'GILD',
  'GM',
  'GOOG',
  'GOOGL',
  'GS',
  'HD',
  'HON',
  'IBM',
  'INTC',
  'JNJ',
  'JPM',
  'KHC',
  'KO',
  'LIN',
  'LLY',
  'LMT',
  'LOW',
  'MA',
  'MCD',
  'MDLZ',
  'MDT',
  'MET',
  'META',
  'MMM',
  'MO',
  'MRK',
  'MS',
  'MSFT',
  'NEE',
  'NFLX',
  'NKE',
  'NVDA',
  'ORCL',
  'PEP',
  'PFE',
  'PG',
  'PM',
  'PYPL',
  'QCOM',
  'RTX',
  'SBUX',
  'SCHW',
  'SO',
  'SPG',
  'T',
  'TGT',
  'TMO',
  'TMUS',
  'TSLA',
  'TXN',
  'UNH',
  'UNP',
  'UPS',
  'USB',
  'V',
  'VZ',
  'WBA',
  'WFC',
  'WMT',
  'XOM'
)
price_data_pool <-
  tq_get(stocksPool,
         from = '2017-01-01',
         to = '2018-12-31',
         get = 'stock.prices')
pool_ret_tidy <- price_data_pool %>%
  group_by(symbol) %>%
  tq_transmute(
    select = adjusted,mutate_fun = periodReturn,period = 'daily',col_rename = 'ret'
  )
pool_ret_xts <- pool_ret_tidy %>%
  spread(symbol, value = ret) %>%
  tk_xts()
pool_mean_ret <- colMeans(pool_ret_xts)

poolSelectionFitness <- function(x) {
  if (sum(x) > 10) {
    result <- -sum(x)
  } else { result <- sum(x*pool_mean_ret) }
 
}

GAPoolSelection <-
  ga(
    type = 'binary',
    fitness = poolSelectionFitness,
    nBits = length(pool_mean_ret),
    maxiter = 500,
    popSize = 50
  )

StockIndices <- which(GAPoolSelection@solution == 1)

GASelectedStocks <- c()
GASelectedStocksReturns <- c()
selectedStock_mean_ret <- c()
for (i in temp) {
       GASelectedStocks <- append(GASelectedStocks, stocksPool[i])
       GASelectedStocksReturns <- cbind(GASelectedStocksReturns,pool_ret_xts[,i])
       selectedStock_mean_ret <- append(selectedStock_mean_ret, pool_mean_ret[i])
       
 }
selectedStock_cov_mat <- cov(GASelectedStocksReturns) * 252

maximizeRatio2 <- function(x) {
  if (sum(x) > 1) {
    result <- -sum(x)
  }
  else {
    result <-
      (((sum(x * selectedStock_mean_ret) + 1) ^ 252 - 1) - 0.025) / sqrt(t(x) %*% (selectedStock_cov_mat %*% x))
  }
}


GA2 <- ga(
  type = "real-valued",
  fitness = maximizeRatio,
  lower = rep(0, 10),
  upper = rep(1, 10),
  suggestions = suggestedSol,
  pmutation = 0.4,
  maxiter = 1000,
  popSize = 100,
  seed = 123
)

GA2PortfolioReturn = ((sum((GA2@solution) * selectedStock_mean_ret) + 1) ^ 252 - 1)

barplot(
  c(
    GAPortfolioReturn,
    GA2PortfolioReturn
  ),
  main = "Return Comparison",
  ylab = "Annualised Return",
  space = 0.2
)
