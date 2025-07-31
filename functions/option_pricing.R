# option_pricing.R - Black-Scholes Option Pricing

# Function for Black-Scholes European Call Option Pricing
bs_call_price <- function(S, X, rf, T, sigma) {
  d1 <- (log(S/X) + (rf + sigma^2 / 2) * T) / (sigma * sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  call_price <- S * pnorm(d1) - X * exp(-rf * T) * pnorm(d2)
  return(call_price)
}

# Function for Black-Scholes European Put Option Pricing
bs_put_price <- function(S, X, rf, T, sigma) {
  d1 <- (log(S/X) + (rf + sigma^2 / 2) * T) / (sigma * sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  put_price <- X * exp(-rf * T) * pnorm(-d2) - S * pnorm(-d1)
  return(put_price)
}

# Function to find Implied Volatility using Black-Scholes
find_implied_volatility <- function(option_type, market_price, S, X, rf, T) {
  # Objective function to find the root, market price - model price
  objective_function <- function(sigma) {
    if (option_type == "call") {
      return(bs_call_price(S, X, rf, T, sigma) - market_price)
    } else if (option_type == "put") {
      return(bs_put_price(S, X, rf, T, sigma) - market_price)
    } else {
      stop("Invalid option type.")
    }
  }
  
  # Use uniroot to solve for implied volatility
  implied_vol <- uniroot(objective_function, interval = c(0.01, 3), tol = 1e-8)$root
  return(implied_vol)
}

# Example for pricing and implied volatility
# S = Spot price = 2500
# X = Strike price = 2600
# rf = Risk-free rate = 0.0658/252
# T = Time to maturity = 30/252
# sigma = Volatility = 0.2
# market_price = 50
example_black_scholes <- function() {
  S <- 2500
  X <- 2600
  rf <- 0.0658/252
  T <- 30/252
  sigma <- 0.2
  market_price <- 50
  
  cat("Call Price:", bs_call_price(S, X, rf, T, sigma), "\n")
  cat("Put Price:", bs_put_price(S, X, rf, T, sigma), "\n")
  cat("Implied Volatility (Call):", find_implied_volatility("call", market_price, S, X, rf, T), "\n")
}
