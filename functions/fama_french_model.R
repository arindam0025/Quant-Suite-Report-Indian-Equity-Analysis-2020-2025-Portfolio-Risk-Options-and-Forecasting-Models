# fama_french_model.R - Fama-French 3-Factor Model Implementation

# Function to run Fama-French analysis
run_fama_french_analysis <- function(portfolio_excess, factor_data, save_plot = TRUE) {
    cat("Running Fama-French 3-Factor analysis...\n")
    
    # Convert portfolio_excess to numeric if it's an xts object
    if (is.xts(portfolio_excess) || is.zoo(portfolio_excess)) {
        portfolio_excess_df <- data.frame(
            Date = index(portfolio_excess),
            Portfolio_Excess = as.numeric(portfolio_excess)
        )
    } else {
        portfolio_excess_df <- data.frame(
            Portfolio_Excess = as.numeric(portfolio_excess)
        )
    }
    
    # Create combined data frame
    ff_data <- cbind(portfolio_excess_df, factor_data)
    ff_data <- na.omit(ff_data)
    
    # Run Fama-French regression: Excess = α + β1(Mkt-RF) + β2(SMB) + β3(HML) + ε
    ff_model <- lm(Portfolio_Excess ~ Market_RF + SMB + HML, data = ff_data)
    
    # Extract results
    coeffs <- summary(ff_model)$coefficients
    alphas <- coeffs[1,1] * 252 # Annualize
    betas <- coeffs[-1,1]
    pvalues <- coeffs[,4]
    r_squared <- summary(ff_model)$r.squared
    
    # Create results summary
    ff_results <- data.frame(
        Factor = c("Alpha (Annual)", "Market Beta", "SMB Beta", "HML Beta", "R-Squared"),
        Value = c(alphas, betas, r_squared),
        pvalue = c(pvalues, NA),
        stringsAsFactors = FALSE
    )
    
    # Create factor exposure plots
    if(save_plot) {
        par(mfrow = c(1, 3))
        
        plot(ff_data$Market_RF, portfolio_excess * 100, 
             main = "Market Factor", xlab = "Market Excess", ylab = "Portfolio Excess",
             col = "blue", pch = 20)
        abline(lm(portfolio_excess ~ ff_data$Market_RF), col = "red")
        
        plot(ff_data$SMB, portfolio_excess * 100, 
             main = "Size Factor", xlab = "SMB", ylab = "Portfolio Excess",
             col = "blue", pch = 20)
        abline(lm(portfolio_excess ~ ff_data$SMB), col = "red")
        
        plot(ff_data$HML, portfolio_excess * 100, 
             main = "Value Factor", xlab = "HML", ylab = "Portfolio Excess",
             col = "blue", pch = 20)
        abline(lm(portfolio_excess ~ ff_data$HML), col = "red")
        
        dev.copy(png, filename = "outputs/Fama_French_Factor_Exposure.png")
        dev.off()
        cat("Fama-French factor exposure plots saved to outputs/\n")
    }
    
    cat("Fama-French analysis completed. Results:\n")
    print(ff_results)
    
    return(ff_results)
}

# Function to prepare Fama-French factors (example simulation)
prepare_ff_factors <- function(start_date, end_date, num_days) {
    # Simulate factors for demonstration (or fetch real ones)
    set.seed(123)
    ff_factors <- data.frame(
        Market_RF = rnorm(num_days, 0.0005, 0.01),
        SMB = rnorm(num_days, 0.0003, 0.005),
        HML = rnorm(num_days, -0.0002, 0.007)
    )
    
    cat("Simulated Fama-French factors prepared from", start_date, "to", end_date, "\n")
    return(ff_factors)
}
