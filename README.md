# README.md

## Overview
This project is a comprehensive analysis suite for the Indian stock market, utilizing R programming for fetching data, running financial models, and optimizing portfolios.

## Project Structure
- **data/**: Folder to store fetched data for analysis.
- **functions/**: Directory containing separate logic in R scripts.
  - `fetch_data.R`: Functions to fetch Indian stock data from Yahoo Finance.
  - `calculate_risk.R`: Risk and performance metrics calculations.
  - `capm_model.R`: Implement CAPM for portfolio risk-return analysis.
  - `fama_french_model.R`: Fama-French 3-Factor model analysis.
  - `option_pricing.R`: Black-Scholes option pricing functions.
  - `markowitz_optimize.R`: Markowitz optimization for the efficient frontier.
  - `monte_carlo_sim.R`: Monte Carlo simulation for price forecasting.
- **outputs/**: Repository for generated reports, graphs, and CSV outputs.
- **main.R**: Master script to run the complete suite of analyses.
- **requirements.R**: Script to install and load required packages.

## How to Run
1. Clone the repository to your local machine.
2. Navigate to the project directory.
3. Run `requirements.R` to install all necessary R packages.
4. Execute the main script: `Rscript main.R`.
5. Review results in the `outputs/` directory.

## Tools Used
- Data Fetching: `quantmod`
- Statistical Analysis: `PerformanceAnalytics`, `tseries`, `zoo`
- Visualization: `ggplot2`, `plotly`, `corrplot`
- Portfolio Optimization: `ROI`, `quadprog`

## Features
- Fetch historical price data for major Indian stocks from Yahoo Finance.
- Calculate risk metrics including VaR, CVaR, Sharpe, and Sortino Ratios.
- Perform CAPM and Fama-French factor analysis on portfolio returns.
- Black-Scholes models for option pricing and implied volatility.
- Use Markowitz theory for optimal portfolio allocation.
- Forecast stock price paths using Monte Carlo simulations.

## Results
Results and graphs are automatically saved to the `outputs/` directory. Check the `analysis_summary.txt` for a comprehensive report.
