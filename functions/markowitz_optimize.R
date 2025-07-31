# markowitz_optimize.R - Markowitz Portfolio Optimization

# Function to optimize a portfolio using Markowitz's Theory
optimize_portfolio <- function(expected_returns, cov_matrix, rf_rate, num_portfolios = 5000, save_plot = TRUE) {
  library(quadprog)
  cat("Running Markowitz portfolio optimization...\n")

  # Number of assets
  if (is.vector(expected_returns)) {
    num_assets <- length(expected_returns)
  } else {
    num_assets <- ncol(expected_returns)
  }

  # Generate random portfolios
  results_matrix <- matrix(nrow=num_portfolios, ncol=3+num_assets)
  colnames(results_matrix) <- c("Return", "StdDev", "Sharpe", paste0("Weight", 1:num_assets))

  for (i in 1:num_portfolios) {
    weights <- runif(num_assets)
    weights <- weights / sum(weights)

    if (is.vector(expected_returns)) {
      portfolio_return <- sum(weights * expected_returns)
    } else {
      portfolio_return <- sum(weights * colMeans(expected_returns))
    }
    portfolio_stddev <- sqrt(t(weights) %*% cov_matrix %*% weights)
    portfolio_sharpe <- (portfolio_return - rf_rate) / portfolio_stddev

    results_matrix[i,] <- c(portfolio_return, portfolio_stddev, portfolio_sharpe, weights)
  }

  # Optimal portfolios
  max_sharpe_idx <- which.max(results_matrix[, "Sharpe"])
  min_vol_idx <- which.min(results_matrix[, "StdDev"])

  cat("Maximum Sharpe Ratio Portfolio:\n")
  print(results_matrix[max_sharpe_idx,])

  cat("Minimum Volatility Portfolio:\n")
  print(results_matrix[min_vol_idx,])

  # Plot if needed
  if (save_plot) {
    plot(results_matrix[, "StdDev"], results_matrix[, "Return"], col="grey", xlab="Risk (StdDev)", ylab="Return", main="Efficient Frontier")
    points(results_matrix[max_sharpe_idx, "StdDev"], results_matrix[max_sharpe_idx, "Return"], col="red", pch=19)
    points(results_matrix[min_vol_idx, "StdDev"], results_matrix[min_vol_idx, "Return"], col="blue", pch=19)
    legend("topright", legend=c("Max Sharpe", "Min Volatility"), col=c("red", "blue"), pch=19)
    cat("Efficient frontier plot generated.\n")
  }

  return(list(
    max_sharpe_portfolio = results_matrix[max_sharpe_idx,],
    min_vol_portfolio = results_matrix[min_vol_idx,]
  ))
}

