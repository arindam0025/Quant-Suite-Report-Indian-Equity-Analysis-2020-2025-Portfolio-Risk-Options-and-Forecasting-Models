# main.R - Master script for Quantitative Finance Suite
# Complete Indian Market Analysis with Risk Models and Portfolio Optimization

# Clear environment
rm(list = ls())

# Set working directory to script location
tryCatch({
  if (!interactive() && exists("sys.frame")) {
    script_dir <- dirname(sys.frame(1)$ofile)
    setwd(script_dir)
  }
}, error = function(e) {
  # If running from command line, assume we're in the right directory
  cat("Working directory:", getwd(), "\n")
})

start_time <- Sys.time()
cat("=== QUANTITATIVE FINANCE SUITE ===\n")
cat("Advanced Indian Market Analysis\n")
cat("Starting analysis at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# ================================================
# STEP 1: LOAD PACKAGES AND FUNCTIONS
# ================================================

cat("STEP 1: Loading packages and functions...\n")
source("requirements.R")

# Source all function files
function_files <- list.files("functions", pattern = "\\.R$", full.names = TRUE)
invisible(sapply(function_files, source))

cat("Functions loaded successfully!\n\n")

# ================================================
# STEP 2: FETCH AND PREPARE DATA
# ================================================

cat("STEP 2: Fetching Indian stock market data...\n")

# Set date range for analysis
start_date <- "2020-01-01"
end_date <- Sys.Date()

# Fetch stock data
stock_prices <- fetch_indian_stocks(start_date, end_date)

# Calculate returns
stock_returns <- calculate_returns(stock_prices, method = "log")

# Get risk-free rate
rf_info <- get_risk_free_rate()

# Save raw data
save_data(stock_prices, "stock_prices", "data")
save_data(stock_returns, "stock_returns", "data")

cat("Data preparation completed!\n\n")

# ================================================
# STEP 3: PORTFOLIO CREATION AND RISK ANALYSIS
# ================================================

cat("STEP 3: Creating equal-weighted portfolio and analyzing risk...\n")

# Create equal-weighted portfolio
portfolio <- create_equal_weighted_portfolio(stock_returns)

# Calculate comprehensive risk metrics
risk_metrics <- calculate_risk_metrics(stock_returns, portfolio$returns, rf_info$daily)

# Display risk metrics
cat("\nRisk Metrics Summary:\n")
print(risk_metrics)

# Create risk visualizations
risk_plots <- plot_risk_metrics(risk_metrics, save_plot = TRUE)

# Analyze correlations
correlation_matrix <- analyze_correlations(stock_returns, save_plot = TRUE)

# Save results
write.csv(risk_metrics, "outputs/risk_metrics.csv", row.names = FALSE)
write.csv(correlation_matrix, "outputs/correlation_matrix.csv", row.names = TRUE)

cat("Risk analysis completed!\n\n")

# ================================================
# STEP 4: CAPM ANALYSIS
# ================================================

cat("STEP 4: Running CAPM analysis...\n")

# Extract market returns (Nifty 50)
market_returns <- stock_returns[, grepl("NSEI", colnames(stock_returns))]

# Run CAPM analysis on portfolio
capm_results <- run_capm_analysis(portfolio$returns, market_returns, rf_info$daily, save_plot = TRUE)

# Calculate individual asset betas
asset_betas <- calculate_asset_betas(stock_returns, market_returns, rf_info$daily)

cat("\nCAPM Results Summary:\n")
print(capm_results$results)

cat("\nAsset Betas:\n")
print(asset_betas)

# Save CAPM results
write.csv(capm_results$results, "outputs/capm_results.csv", row.names = FALSE)
write.csv(asset_betas, "outputs/asset_betas.csv", row.names = FALSE)

cat("CAPM analysis completed!\n\n")

# ================================================
# STEP 5: FAMA-FRENCH ANALYSIS
# ================================================

cat("STEP 5: Running Fama-French analysis...\n")

# Simulate or fetch Fama-French factors for analysis
ff_factors <- prepare_ff_factors(start_date, end_date, nrow(stock_returns))

# Run Fama-French analysis
ff_results <- run_fama_french_analysis(portfolio$returns - rf_info$daily, ff_factors, save_plot = TRUE)

# Save Fama-French results
write.csv(ff_results, "outputs/ff_results.csv", row.names = FALSE)

cat("Fama-French analysis completed!\n\n")

# ================================================
# STEP 6: BLACK-SCHOLES OPTION PRICING
# ================================================

cat("STEP 6: Running Black-Scholes option pricing...\n")

# Example option pricing for RELIANCE.NS
S <- 2500  # Spot price
X <- 2600  # Strike price
market_vol <- 0.2  # Market estimated volatility
call_price <- bs_call_price(S, X, rf_info$daily, T = 30/252, sigma = market_vol)
put_price <- bs_put_price(S, X, rf_info$daily, T = 30/252, sigma = market_vol)

cat("Call Price:", round(call_price, 2), "\n")
cat("Put Price:", round(put_price, 2), "\n")

cat("Black-Scholes option pricing completed!\n\n")

# ================================================
# STEP 7: MARKOWITZ PORTFOLIO OPTIMIZATION
# ================================================

cat("STEP 7: Optimizing portfolio using Markowitz theory...\n")

# Estimate expected returns and covariance matrix (exclude market index)
portfolio_stock_returns <- stock_returns[, !grepl("NSEI", colnames(stock_returns))]
expected_returns <- apply(portfolio_stock_returns, 2, mean) * 252
cov_matrix <- cov(portfolio_stock_returns) * 252

# Optimize the portfolio
optimal_portfolios <- optimize_portfolio(expected_returns, cov_matrix, rf_info$annual, save_plot = TRUE)

# Save optimal portfolios
write.csv(optimal_portfolios$max_sharpe_portfolio, "outputs/max_sharpe_portfolio.csv", row.names = FALSE)
write.csv(optimal_portfolios$min_vol_portfolio, "outputs/min_vol_portfolio.csv", row.names = FALSE)

cat("Markowitz optimization completed!\n\n")

# ================================================
# STEP 8: MONTE CARLO SIMULATION
# ================================================

cat("STEP 8: Running Monte Carlo simulation for RELIANCE.NS...\n")

# Estimate parameters
gbm_params <- estimate_gbm_parameters(stock_prices$RELIANCE)

# Run simulation
mc_results <- monte_carlo_simulation(S0 = 2500, mu = gbm_params$mu, sigma = gbm_params$sigma, T = 1, steps = 252, paths = 1000)

# Save Monte Carlo results
write.csv(mc_results$final_prices, "outputs/monte_carlo_final_prices.csv", row.names = FALSE)

cat("Monte Carlo simulation completed!\n\n")

# ================================================
# STEP 9: SUMMARY REPORT
# ================================================

cat("STEP 9: Generating summary report...\n")

# Create summary statistics
summary_stats <- list(
  analysis_date = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  data_period = paste(start_date, "to", end_date),
  total_assets = ncol(stock_returns) - 1,  # Exclude benchmark
  total_observations = nrow(stock_returns),
  portfolio_sharpe = risk_metrics$Sharpe_Ratio[risk_metrics$Asset == "Portfolio"],
  portfolio_alpha = capm_results$results$Value[capm_results$results$Metric == "Alpha (Annual)"],
  portfolio_beta = capm_results$results$Value[capm_results$results$Metric == "Beta"],
  fattest_alpha = ff_results$Value[ff_results$Factor == "Alpha (Annual)"],
  best_performing_asset = risk_metrics$Asset[which.max(risk_metrics$Sharpe_Ratio)],
  most_aggressive_asset = asset_betas$Asset[which.max(asset_betas$Beta)],
  most_defensive_asset = asset_betas$Asset[which.min(asset_betas$Beta)]
)

# Print summary
cat("\n=== ANALYSIS SUMMARY ===\n")
cat("Analysis Date:", summary_stats$analysis_date, "\n")
cat("Data Period:", summary_stats$data_period, "\n")
cat("Total Assets Analyzed:", summary_stats$total_assets, "\n")
cat("Total Observations:", summary_stats$total_observations, "\n")
cat("Portfolio Sharpe Ratio:", summary_stats$portfolio_sharpe, "\n")
cat("Portfolio Alpha (%):", summary_stats$portfolio_alpha, "\n")
cat("Portfolio Beta:", summary_stats$portfolio_beta, "\n")
cat("Fama-French Alpha (%):", summary_stats$fattest_alpha, "\n")
cat("Best Performing Asset (Sharpe):", summary_stats$best_performing_asset, "\n")
cat("Most Aggressive Asset (Beta):", summary_stats$most_aggressive_asset, "\n")
cat("Most Defensive Asset (Beta):", summary_stats$most_defensive_asset, "\n")

capture.output(
  {
    cat("=== QUANTITATIVE FINANCE SUITE - ANALYSIS SUMMARY ===\n")
    cat("Generated:", summary_stats$analysis_date, "\n\n")
    cat("DATA OVERVIEW:\n")
    cat("Period:", summary_stats$data_period, "\n")
    cat("Assets:", summary_stats$total_assets, "\n")
    cat("Observations:", summary_stats$total_observations, "\n\n")
    cat("PORTFOLIO PERFORMANCE:\n")
    cat("Sharpe Ratio:", summary_stats$portfolio_sharpe, "\n")
    cat("Alpha (%):", summary_stats$portfolio_alpha, "\n")
    cat("Beta:\n", summary_stats$portfolio_beta, "\n\n")
    cat("FAMA-FRENCH ANALYSIS:\n")
    cat("Alpha (%):\n", summary_stats$fattest_alpha, "\n\n")
    cat("TOP PERFORMERS:\n")
    cat("Best Sharpe Ratio:\n", summary_stats$best_performing_asset, "\n")
    cat("Most Aggressive (High Beta):\n", summary_stats$most_aggressive_asset, "\n")
    cat("Most Defensive (Low Beta):\n", summary_stats$most_defensive_asset, "\n\n")
    cat("FILES GENERATED:\n")
    cat("- outputs/risk_metrics.csv\n")
    cat("- outputs/capm_results.csv\n")
    cat("- outputs/asset_betas.csv\n")
    cat("- outputs/ff_results.csv\n")
    cat("- outputs/max_sharpe_portfolio.csv\n")
    cat("- outputs/min_vol_portfolio.csv\n")
    cat("- outputs/monte_carlo_final_prices.csv\n")
    cat("- outputs/correlation_matrix.csv\n")
    cat("- outputs/risk_return_profile.png\n")
    cat("- outputs/sharpe_ratios.png\n")
    cat("- outputs/correlation_matrix.png\n")
    cat("- outputs/capm_regression.png\n")
    cat("- outputs/Fama_French_Factor_Exposure.png\n")
  },
  file = "outputs/analysis_summary.txt"
)

cat("\n=== ANALYSIS COMPLETED SUCCESSFULLY ===\n")
cat("All results saved to 'outputs/' directory\n")
cat("Summary report: outputs/analysis_summary.txt\n")
cat("Runtime:\n", round(difftime(Sys.time(), start_time, units = "mins"), 2), "minutes\n")
