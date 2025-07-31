# calculate_risk.R - Risk and performance metrics calculation

# Function to create equal-weighted portfolio
create_equal_weighted_portfolio <- function(returns_data) {
  
  # Exclude benchmark (NSEI) from portfolio if present
  portfolio_returns <- returns_data[, !grepl("NSEI", colnames(returns_data))]
  
  n_assets <- ncol(portfolio_returns)
  equal_weights <- rep(1/n_assets, n_assets)
  
  cat("Creating equal-weighted portfolio with", n_assets, "assets\n")
  cat("Equal weight per asset:", round(equal_weights[1] * 100, 2), "%\n")
  
  # Calculate portfolio returns
  portfolio_ret <- Return.portfolio(portfolio_returns, weights = equal_weights)
  colnames(portfolio_ret) <- "Portfolio"
  
  return(list(
    returns = portfolio_ret,
    weights = equal_weights,
    assets = colnames(portfolio_returns)
  ))
}

# Function to calculate comprehensive risk metrics
calculate_risk_metrics <- function(returns_data, portfolio_returns = NULL, rf_rate = 0.0658/252) {
  
  cat("Calculating comprehensive risk metrics...\n")
  
  # If portfolio returns not provided, calculate for all assets
  if (is.null(portfolio_returns)) {
    data_to_analyze <- returns_data
  } else {
    # Combine portfolio with individual assets for comparison
    data_to_analyze <- merge(portfolio_returns, returns_data)
  }
  
  # Initialize results dataframe
  results <- data.frame(
    Asset = colnames(data_to_analyze),
    Mean_Return = numeric(ncol(data_to_analyze)),
    Volatility = numeric(ncol(data_to_analyze)),
    Sharpe_Ratio = numeric(ncol(data_to_analyze)),
    Sortino_Ratio = numeric(ncol(data_to_analyze)),
    VaR_95 = numeric(ncol(data_to_analyze)),
    CVaR_95 = numeric(ncol(data_to_analyze)),
    Max_Drawdown = numeric(ncol(data_to_analyze)),
    stringsAsFactors = FALSE
  )
  
  for (i in 1:ncol(data_to_analyze)) {
    asset_returns <- data_to_analyze[, i]
    
    # Basic metrics
    results$Mean_Return[i] <- mean(asset_returns, na.rm = TRUE) * 252  # Annualized
    results$Volatility[i] <- sd(asset_returns, na.rm = TRUE) * sqrt(252)  # Annualized
    
    # Sharpe Ratio
    excess_returns <- asset_returns - rf_rate
    results$Sharpe_Ratio[i] <- mean(excess_returns, na.rm = TRUE) / sd(excess_returns, na.rm = TRUE) * sqrt(252)
    
    # Sortino Ratio
    downside_returns <- excess_returns[excess_returns < 0]
    if (length(downside_returns) > 0) {
      downside_dev <- sd(downside_returns, na.rm = TRUE)
      results$Sortino_Ratio[i] <- mean(excess_returns, na.rm = TRUE) / downside_dev * sqrt(252)
    } else {
      results$Sortino_Ratio[i] <- NA
    }
    
    # VaR and CVaR (95% confidence)
    results$VaR_95[i] <- VaR(asset_returns, p = 0.95, method = "historical")
    results$CVaR_95[i] <- ES(asset_returns, p = 0.95, method = "historical")
    
    # Maximum Drawdown
    tryCatch({
      results$Max_Drawdown[i] <- maxDrawdown(asset_returns)
    }, error = function(e) {
      results$Max_Drawdown[i] <- NA
    })
  }
  
  # Convert percentages for better readability
  results$Mean_Return <- round(results$Mean_Return * 100, 2)
  results$Volatility <- round(results$Volatility * 100, 2)
  results$Sharpe_Ratio <- round(results$Sharpe_Ratio, 3)
  results$Sortino_Ratio <- round(results$Sortino_Ratio, 3)
  results$VaR_95 <- round(results$VaR_95 * 100, 2)
  results$CVaR_95 <- round(results$CVaR_95 * 100, 2)
  results$Max_Drawdown <- round(results$Max_Drawdown * 100, 2)
  
  cat("Risk metrics calculated for", nrow(results), "assets/portfolios\n")
  
  return(results)
}

# Function to create risk metrics visualization
plot_risk_metrics <- function(risk_metrics, save_plot = TRUE) {
  
  # Risk-Return scatter plot
  p1 <- ggplot(risk_metrics, aes(x = Volatility, y = Mean_Return)) +
    geom_point(aes(color = Sharpe_Ratio, size = abs(VaR_95)), alpha = 0.7) +
    geom_text(aes(label = Asset), vjust = -0.5, size = 3) +
    scale_color_gradient2(low = "red", mid = "yellow", high = "green", 
                         midpoint = median(risk_metrics$Sharpe_Ratio, na.rm = TRUE)) +
    labs(title = "Risk-Return Profile",
         subtitle = "Size = VaR (95%), Color = Sharpe Ratio",
         x = "Volatility (%)",
         y = "Expected Return (%)") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # Sharpe Ratio bar chart
  p2 <- ggplot(risk_metrics, aes(x = reorder(Asset, Sharpe_Ratio), y = Sharpe_Ratio)) +
    geom_col(aes(fill = Sharpe_Ratio > 0), alpha = 0.8) +
    scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "red")) +
    coord_flip() +
    labs(title = "Sharpe Ratios",
         x = "Asset",
         y = "Sharpe Ratio") +
    theme_minimal() +
    theme(legend.position = "none")
  
  if (save_plot) {
    ggsave("outputs/risk_return_profile.png", p1, width = 12, height = 8, dpi = 300)
    ggsave("outputs/sharpe_ratios.png", p2, width = 10, height = 8, dpi = 300)
    cat("Risk metric plots saved to outputs/\n")
  }
  
  return(list(risk_return = p1, sharpe_ratios = p2))
}

# Function to calculate correlation matrix and plot
analyze_correlations <- function(returns_data, save_plot = TRUE) {
  
  cat("Analyzing correlations...\n")
  
  # Calculate correlation matrix
  cor_matrix <- cor(returns_data, use = "complete.obs")
  
  if (save_plot) {
    # Save correlation plot
    png("outputs/correlation_matrix.png", width = 12, height = 10, units = "in", res = 300)
    corrplot(cor_matrix, method = "color", type = "upper", 
             order = "hclust", tl.cex = 0.8, tl.col = "black")
    title("Asset Correlation Matrix")
    dev.off()
    cat("Correlation matrix plot saved to outputs/\n")
  }
  
  return(cor_matrix)
}
