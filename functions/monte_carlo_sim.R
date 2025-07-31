# monte_carlo_sim.R - Monte Carlo Simulation for Stock Price Forecasting

# Function to run Monte Carlo simulation for stock price path
monte_carlo_simulation <- function(S0, mu, sigma, T, steps, paths, save_plot = TRUE) {
  cat("Running Monte Carlo simulation...\n")
  cat("Initial Price:", S0, ", Drift:", mu, ", Volatility:", sigma, "\n")
  cat("Time Horizon:", T, "years, Steps:", steps, ", Paths:", paths, "\n")
  
  # Time increment
  dt <- T / steps
  
  # Initialize price matrix
  price_paths <- matrix(nrow = steps + 1, ncol = paths)
  price_paths[1, ] <- S0
  
  # Generate random shocks
  set.seed(123)
  
  for (path in 1:paths) {
    for (step in 2:(steps + 1)) {
      # Geometric Brownian Motion: dS = S * (mu * dt + sigma * sqrt(dt) * Z)
      Z <- rnorm(1)
      price_paths[step, path] <- price_paths[step - 1, path] * exp((mu - 0.5 * sigma^2) * dt + sigma * sqrt(dt) * Z)
    }
  }
  
  # Calculate statistics
  final_prices <- price_paths[steps + 1, ]
  mean_final_price <- mean(final_prices)
  std_final_price <- sd(final_prices)
  
  # Calculate percentiles
  percentiles <- quantile(final_prices, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
  
  # Probability of price falling below a threshold (e.g., 80% of initial price)
  threshold <- 0.8 * S0
  prob_below_threshold <- mean(final_prices < threshold)
  
  cat("Simulation Results:\n")
  cat("Mean Final Price:", round(mean_final_price, 2), "\n")
  cat("Std Dev Final Price:", round(std_final_price, 2), "\n")
  cat("5th Percentile:", round(percentiles[1], 2), "\n")
  cat("95th Percentile:", round(percentiles[5], 2), "\n")
  cat("Probability of falling below", threshold, ":", round(prob_below_threshold * 100, 2), "%\n")
  
  if (save_plot) {
    par(mfrow = c(1, 2))
    
    # Plot sample paths
    time_grid <- seq(0, T, length.out = steps + 1)
    plot(time_grid, price_paths[, 1], type = "l", col = "blue", ylim = range(price_paths[, 1:min(50, paths)]),
         xlab = "Time (Years)", ylab = "Price", main = "Sample Monte Carlo Paths")
    
    for (i in 2:min(50, paths)) {
      lines(time_grid, price_paths[, i], col = sample(colors(), 1))
    }
    
    # Plot histogram of final prices
    hist(final_prices, breaks = 50, col = "lightblue", border = "black",
         xlab = "Final Price", main = "Distribution of Final Prices", prob = TRUE)
    abline(v = mean_final_price, col = "red", lwd = 2, lty = 2)
    abline(v = percentiles[c(1, 5)], col = "orange", lwd = 2, lty = 3)
    legend("topright", legend = c("Mean", "5th & 95th Percentiles"), 
           col = c("red", "orange"), lty = c(2, 3), lwd = 2)
    
    dev.copy(png, filename = "outputs/monte_carlo_simulation.png", width = 1200, height = 600)
    dev.off()
    cat("Monte Carlo plots saved to outputs/\n")
  }
  
  return(list(
    price_paths = price_paths,
    final_prices = final_prices,
    statistics = list(
      mean = mean_final_price,
      std = std_final_price,
      percentiles = percentiles,
      prob_below_threshold = prob_below_threshold
    )
  ))
}

# Function to estimate parameters from historical data
estimate_gbm_parameters <- function(price_data) {
  # Calculate log returns
  log_returns <- diff(log(as.numeric(price_data)))
  
  # Estimate drift (mu) and volatility (sigma)
  mu <- mean(log_returns) * 252  # Annualized
  sigma <- sd(log_returns) * sqrt(252)  # Annualized
  
  cat("Estimated Parameters:\n")
  cat("Drift (μ):", round(mu, 4), "\n")
  cat("Volatility (σ):", round(sigma, 4), "\n")
  
  return(list(mu = mu, sigma = sigma))
}
