# fetch_data.R - Data fetching functions for Indian stock market

# Function to fetch Indian stock data
fetch_indian_stocks <- function(start_date = "2020-01-01", end_date = Sys.Date()) {
  
  # Indian stock universe
  indian_stocks <- c(
    "HDFCBANK.NS",    # HDFC Bank
    "ICICIBANK.NS",   # ICICI Bank
    "INFY.NS",        # Infosys
    "TCS.NS",         # Tata Consultancy Services
    "RELIANCE.NS",    # Reliance Industries
    "ONGC.NS",        # Oil and Natural Gas Corporation
    "ITC.NS",         # ITC Limited
    "HINDUNILVR.NS",  # Hindustan Unilever
    "TATAMOTORS.NS",  # Tata Motors
    "M&M.NS",         # Mahindra & Mahindra
    "WIPRO.NS",       # Wipro
    "BAJFINANCE.NS",  # Bajaj Finance
    "MARUTI.NS",      # Maruti Suzuki
    "KOTAKBANK.NS",   # Kotak Mahindra Bank
    "BHARTIARTL.NS"   # Bharti Airtel
  )
  
  # Market benchmark
  benchmark <- "^NSEI"  # Nifty 50
  
  cat("Fetching data for", length(indian_stocks), "Indian stocks and Nifty 50...\n")
  
  # Fetch stock data
  stock_data <- list()
  
  for (symbol in c(indian_stocks, benchmark)) {
    tryCatch({
      cat("Fetching:", symbol, "\n")
      data <- getSymbols(symbol, src = "yahoo", 
                        from = start_date, to = end_date, 
                        auto.assign = FALSE, warnings = FALSE)
      
      # Extract adjusted closing prices
      adj_close <- Ad(data)
      colnames(adj_close) <- gsub("\\.NS\\.Adjusted|\\.Adjusted|\\^", "", symbol)
      
      stock_data[[symbol]] <- adj_close
      
    }, error = function(e) {
      cat("Error fetching", symbol, ":", e$message, "\n")
    })
  }
  
  # Merge all stock data
  if (length(stock_data) > 0) {
    merged_data <- do.call(merge, stock_data)
    merged_data <- na.omit(merged_data)  # Remove rows with missing data
    
    cat("Successfully fetched data for", ncol(merged_data), "assets\n")
    cat("Date range:", as.character(start(merged_data)), "to", as.character(end(merged_data)), "\n")
    cat("Total observations:", nrow(merged_data), "\n")
    
    return(merged_data)
  } else {
    stop("No data could be fetched!")
  }
}

# Function to calculate returns
calculate_returns <- function(price_data, method = "log") {
  
  cat("Calculating", method, "returns...\n")
  
  if (method == "log") {
    returns <- Return.calculate(price_data, method = "log")
  } else {
    returns <- Return.calculate(price_data, method = "discrete")
  }
  
  # Remove first row (NA values)
  returns <- returns[-1, ]
  
  cat("Returns calculated. Shape:", dim(returns), "\n")
  
  return(returns)
}

# Function to get risk-free rate (approximation for India)
get_risk_free_rate <- function() {
  # Using 10-year Indian Government Bond yield approximation
  # In practice, you'd fetch this from RBI or other sources
  rf_annual <- 0.0658  # 6.58% annual
  rf_daily <- rf_annual / 252  # Convert to daily
  
  cat("Using risk-free rate:", round(rf_annual * 100, 2), "% annually\n")
  
  return(list(
    annual = rf_annual,
    daily = rf_daily
  ))
}

# Function to save data
save_data <- function(data, filename, folder = "data") {
  if (!dir.exists(folder)) {
    dir.create(folder, recursive = TRUE)
  }
  
  filepath <- file.path(folder, paste0(filename, ".csv"))
  write.csv(data, filepath, row.names = TRUE)
  cat("Data saved to:", filepath, "\n")
}
