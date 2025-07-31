# requirements.R - Package installer and loader
# Install and load all required packages for quantitative finance analysis

# Set CRAN mirror
options(repos = c(CRAN = "https://cran.rstudio.com/"))

# Core required packages (simplified for reliability)
required_packages <- c(
  "quantmod",           # Financial data fetching and modeling
  "PerformanceAnalytics", # Performance and risk analytics
  "ROI",                # R Optimization Infrastructure
  "ROI.plugin.quadprog", # Quadratic programming solver
  "ggplot2",            # Advanced plotting
  "tidyverse",          # Data manipulation
  "xts",                # Time series objects
  "zoo",                # Time series data
  "quadprog",           # Quadratic programming
  "tseries",            # Time series analysis
  "scales",             # Scale functions for ggplot2
  "plotly",             # Interactive plots
  "corrplot",           # Correlation plots
  "MASS",               # Statistical functions
  "gridExtra",          # Arrange multiple plots
  "knitr",              # Dynamic report generation
  "dplyr"               # Data manipulation
)

# Function to install packages if not already installed
install_if_missing <- function(package) {
  tryCatch({
    if (!require(package, character.only = TRUE, quietly = TRUE)) {
      cat("Installing", package, "...\n")
      install.packages(package, dependencies = TRUE, quiet = TRUE)
      library(package, character.only = TRUE, quietly = TRUE)
      cat("✓", package, "loaded successfully\n")
    } else {
      cat("✓", package, "already available\n")
    }
  }, error = function(e) {
    cat("⚠ Warning: Could not install", package, "-", e$message, "\n")
  })
}

# Install and load all packages
cat("Installing and loading required packages...\n")
cat("Using CRAN mirror:", getOption("repos")["CRAN"], "\n\n")

invisible(sapply(required_packages, install_if_missing))

cat("\n=== Package Installation Complete ===\n")
cat("Core packages loaded successfully!\n")
