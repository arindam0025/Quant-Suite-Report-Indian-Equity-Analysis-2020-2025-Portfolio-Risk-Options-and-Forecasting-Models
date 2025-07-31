# capm_model.R - Capital Asset Pricing Model implementation

# Function to run CAPM analysis
run_capm_analysis <- function(portfolio_returns, market_returns, rf_rate = 0.0658/252, save_plot = TRUE) {
  
  cat("Running CAPM analysis...\n")
  
  # Prepare data
  portfolio_col <- colnames(portfolio_returns)[1]
  market_col <- colnames(market_returns)[1]
  
  # Merge portfolio and market returns
  capm_data <- merge(portfolio_returns, market_returns)
  capm_data <- na.omit(capm_data)
  
  # Calculate excess returns
  portfolio_excess <- capm_data[, 1] - rf_rate
  market_excess <- capm_data[, 2] - rf_rate
  
  # Create dataframe for regression
  regression_data <- data.frame(
    Portfolio_Excess = as.numeric(portfolio_excess),
    Market_Excess = as.numeric(market_excess)
  )
  
  # Run CAPM regression: Rp - Rf = α + β(Rm - Rf) + ε
  capm_model <- lm(Portfolio_Excess ~ Market_Excess, data = regression_data)
  
  # Extract results
  alpha <- coef(capm_model)[1]
  beta <- coef(capm_model)[2]
  r_squared <- summary(capm_model)$r.squared
  alpha_pvalue <- summary(capm_model)$coefficients[1, 4]
  beta_pvalue <- summary(capm_model)$coefficients[2, 4]
  
  # Annualize alpha
  alpha_annual <- alpha * 252
  
  # Calculate tracking error
  residuals_data <- residuals(capm_model)
  tracking_error <- sd(residuals_data) * sqrt(252)
  
  # Information ratio
  information_ratio <- alpha_annual / tracking_error
  
  # Create results summary
  capm_results <- data.frame(
    Metric = c("Alpha (Annual)", "Beta", "R-Squared", "Alpha p-value", 
               "Beta p-value", "Tracking Error", "Information Ratio"),
    Value = c(round(alpha_annual * 100, 3), round(beta, 3), round(r_squared, 3),
              round(alpha_pvalue, 4), round(beta_pvalue, 4), 
              round(tracking_error * 100, 3), round(information_ratio, 3)),
    Unit = c("%", "", "", "", "", "%", ""),
    stringsAsFactors = FALSE
  )
  
  # Interpretation
  interpretation <- list(
    alpha_interpretation = ifelse(alpha > 0, "Outperforming market", "Underperforming market"),
    beta_interpretation = case_when(
      beta > 1.2 ~ "High volatility (Aggressive)",
      beta > 0.8 ~ "Moderate volatility",
      beta > 0 ~ "Low volatility (Defensive)",
      TRUE ~ "Negative beta (Hedge)"
    ),
    significance = ifelse(alpha_pvalue < 0.05, "Alpha is statistically significant", 
                         "Alpha is not statistically significant")
  )
  
  cat("CAPM Analysis Results:\n")
  cat("Alpha (Annual):", round(alpha_annual * 100, 2), "%\n")
  cat("Beta:", round(beta, 3), "\n")
  cat("R-Squared:", round(r_squared, 3), "\n")
  cat("Interpretation:", interpretation$alpha_interpretation, "\n")
  cat("Risk Profile:", interpretation$beta_interpretation, "\n")
  
  # Create CAPM plot
  if (save_plot) {
    capm_plot <- ggplot(regression_data, aes(x = Market_Excess * 100, y = Portfolio_Excess * 100)) +
      geom_point(alpha = 0.6, color = "steelblue") +
      geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +
      labs(
        title = "CAPM Regression Analysis",
        subtitle = paste0("α = ", round(alpha_annual * 100, 2), "%, β = ", round(beta, 3), 
                         ", R² = ", round(r_squared, 3)),
        x = "Market Excess Return (%)",
        y = "Portfolio Excess Return (%)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12)
      )
    
    ggsave("outputs/capm_regression.png", capm_plot, width = 10, height = 8, dpi = 300)
    cat("CAPM plot saved to outputs/\n")
  }
  
  return(list(
    model = capm_model,
    results = capm_results,
    interpretation = interpretation,
    plot = if(save_plot) capm_plot else NULL
  ))
}

# Function to calculate beta for multiple assets
calculate_asset_betas <- function(returns_data, market_returns, rf_rate = 0.0658/252) {
  
  cat("Calculating betas for all assets...\n")
  
  # Exclude market index from returns_data if present
  asset_returns <- returns_data[, !grepl("NSEI", colnames(returns_data))]
  
  # Merge with market returns
  merged_data <- merge(asset_returns, market_returns)
  merged_data <- na.omit(merged_data)
  
  n_assets <- ncol(asset_returns)
  market_col <- ncol(merged_data)
  
  # Initialize results
  beta_results <- data.frame(
    Asset = colnames(asset_returns),
    Beta = numeric(n_assets),
    Alpha_Annual = numeric(n_assets),
    R_Squared = numeric(n_assets),
    Risk_Profile = character(n_assets),
    stringsAsFactors = FALSE
  )
  
  for (i in 1:n_assets) {
    # Calculate excess returns
    asset_excess <- merged_data[, i] - rf_rate
    market_excess <- merged_data[, market_col] - rf_rate
    
    # Run regression
    model <- lm(asset_excess ~ market_excess)
    
    # Extract metrics
    beta_results$Beta[i] <- coef(model)[2]
    beta_results$Alpha_Annual[i] <- coef(model)[1] * 252
    beta_results$R_Squared[i] <- summary(model)$r.squared
    
    # Risk profile classification
    beta <- beta_results$Beta[i]
    beta_results$Risk_Profile[i] <- case_when(
      beta > 1.2 ~ "Aggressive",
      beta > 0.8 ~ "Moderate",
      beta > 0 ~ "Defensive",
      TRUE ~ "Hedge"
    )
  }
  
  # Round values
  beta_results$Beta <- round(beta_results$Beta, 3)
  beta_results$Alpha_Annual <- round(beta_results$Alpha_Annual * 100, 2)
  beta_results$R_Squared <- round(beta_results$R_Squared, 3)
  
  cat("Beta analysis completed for", n_assets, "assets\n")
  
  return(beta_results)
}
