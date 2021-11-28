# install.packages("tidyr")
# install.packages("devtools")
# install.packages("timetk")
# install.packages("tidyquant")
# install.packages("forcats")
library(tidyquant) # To download the data
library(plotly) # To create interactive charts
library(timetk) # To manipulate the data series
library(devtools)
library(tidyr)
library(forcats)


# Download the price data.
tick <- c('AMZN', 'AAPL', 'NFLX', 'XOM', 'T')
price_data <- tq_get(tick,
                     from = '2014-01-01',
                     to = '2018-05-31',
                     get = 'stock.prices')

# Calculate the daily returns for these stocks.
# We use the logarithmic returns.
log_ret_tidy <- price_data %>%
  group_by(symbol) %>%
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = 'daily',
    col_rename = 'ret',
    type = 'log'
  )
head(log_ret_tidy)

# Convert data to a wide format.
## Convert it into a time series object using xts() function.
### Using column `date` for date_var.
log_ret_xts <- log_ret_tidy %>%
  spread(symbol, value = ret) %>%
  tk_xts()

# Calculate the mean daily returns for each asset.
mean_ret <- colMeans(log_ret_xts)
print(round(mean_ret, 5))

# Calculate the covariance matrix for all stocks.
# We annualize it by multiplying by 252.
cov_mat <- cov(log_ret_xts) * 252
print(round(cov_mat, 4))


## To calculate the portfolio returns and risk (standard deviation) we will us need
# Mean assets returns
# Portfolio weights
# Covariance matrix of all assets
# Random weights

# Create random weights
wts <- runif(n = length(tick))
print(wts)
print(sum(wts))

# Fix that sum is more than 1 problem
wts <- wts / sum(wts)
print(wts)
print(sum(wts))

# Calculate the annualized portfolio returns.
port_returns <- (sum(wts * mean_ret) + 1) ^ 252 - 1

# Calculate the portfolio risk (Standard deviation).
# This will be annualized Standard deviation for the portfolio.
port_risk <- sqrt(t(wts) %*% (cov_mat %*% wts))
print(port_risk)

# Sharp ratio Calculation with 0% Risk free rate.
sharpe_ratio <- port_returns / port_risk
print(sharpe_ratio)


# 5000 random portfolios
num_port <- 5000

# Creating a matrix to store the weights
all_wts <- matrix(nrow = num_port,
                  ncol = length(tick))

# Creating an empty vector to store
# Portfolio returns
port_returns <- vector('numeric', length = num_port)

# Creating an empty vector to store
# Portfolio Standard deviation
port_risk <- vector('numeric', length = num_port)

# Creating an empty vector to store
# Portfolio Sharpe Ratio
sharpe_ratio <- vector('numeric', length = num_port)


##### MONTE CARLO #####
for (i in seq_along(port_returns)) {
  wts <- runif(length(tick))
  wts <- wts / sum(wts)
  
  # Storing weight in the matrix
  all_wts[i,] <- wts
  
  # Portfolio returns
  
  port_ret <- sum(wts * mean_ret)
  port_ret <- ((port_ret + 1) ^ 252) - 1
  
  # Storing Portfolio Returns values
  port_returns[i] <- port_ret
  
  
  # Creating and storing portfolio risk
  port_sd <- sqrt(t(wts) %*% (cov_mat  %*% wts))
  port_risk[i] <- port_sd
  
  # Creating and storing Portfolio Sharpe Ratios
  # Assuming 0% Risk free rate
  
  sr <- port_ret / port_sd
  sharpe_ratio[i] <- sr
  
} ##### MONTE CARLO #####


# Storing the values in the table
portfolio_values <- tibble(Return = port_returns,
                           Risk = port_risk,
                           SharpeRatio = sharpe_ratio)


# Converting matrix to a tibble and changing column names
all_wts <- tk_tbl(all_wts)

colnames(all_wts) <- colnames(log_ret_xts)

# Combing all the values together
portfolio_values <- tk_tbl(cbind(all_wts, portfolio_values))


# The minimum variance portfolio
min_var <- portfolio_values[which.min(portfolio_values$Risk), ]

# The tangency portfolio
max_sr <- portfolio_values[which.max(portfolio_values$SharpeRatio), ]


# The minimum variance portfolio plot
mvp <- min_var %>%
  gather(AAPL:XOM, key = Asset,
         value = Weights) %>%
  mutate(Asset = as.factor(Asset)) %>%
  ggplot(aes(
    x = fct_reorder(Asset, Weights),
    y = Weights,
    fill = Asset
  )) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(x = 'Assets', y = 'Weights', title = "Minimum Variance Portfolio Weights") +
  scale_y_continuous(labels = scales::percent)

ggplotly(mvp)

# The tangency portfolio
tp <- max_sr %>%
  gather(AAPL:XOM, key = Asset,
         value = Weights) %>%
  mutate(Asset = as.factor(Asset)) %>%
  ggplot(aes(
    x = fct_reorder(Asset, Weights),
    y = Weights,
    fill = Asset
  )) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(x = 'Assets', y = 'Weights', title = "Tangency Portfolio Weights") +
  scale_y_continuous(labels = scales::percent)

ggplotly(tp)

# Efficient frontier of all random portfolios
eff <- portfolio_values %>%
  ggplot(aes(x = Risk, y = Return, color = SharpeRatio)) +
  geom_point() +
  theme_classic() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = 'Annualized Risk',
       y = 'Annualized Returns',
       title = "Portfolio Optimization & Efficient Frontier") +
  geom_point(aes(x = Risk,
                 y = Return), data = min_var, color = 'red') +
  geom_point(aes(x = Risk,
                 y = Return), data = max_sr, color = 'red') +
  annotate('text',
           x = 0.20,
           y = 0.42,
           label = "Tangency Portfolio") +
  annotate('text',
           x = 0.18,
           y = 0.01,
           label = "Minimum variance portfolio") +
  annotate(
    geom = 'segment',
    x = 0.14,
    xend = 0.135,
    y = 0.01,
    yend = 0.06,
    color = 'red',
    arrow = arrow(type = "open")
  ) +
  annotate(
    geom = 'segment',
    x = 0.22,
    xend = 0.2275,
    y = 0.405,
    yend = 0.365,
    color = 'red',
    arrow = arrow(type = "open")
  )


ggplotly(eff)
