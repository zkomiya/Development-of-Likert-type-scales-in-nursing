# ===================================================
# Factor Suitability Main (Controller Layer)
# Version: 2.0
# Description: Main controller for FA suitability check
# Changes from v1.0:
#   - Added show_fa_evaluation function
#   - Separated result display and evaluation
# ===================================================

# Main function to check factor analysis suitability
check_fa_suitability <- function(data_obj) {
  
  # Extract data from keyed structure
  source("data_structure.R")
  data <- get_data(data_obj)
  keys <- get_keys(data_obj)
  
  # Data preprocessing
  source("data_preprocessor.R")
  data_fa <- preprocess_for_fa(data, method = "listwise", verbose = FALSE)
  
  # Calculate both correlation matrices
  source("factor_suitability_calculator.R")
  correlations <- calculate_both_correlations(data_fa)
  
  # Calculate suitability for both methods
  results <- calculate_fa_suitability_both(
    data_fa,
    correlations$polychoric,
    correlations$pearson
  )
  
  # Display results only
  source("factor_suitability_display.R")
  fs_display_results(results)
  
  # Return results invisibly
  invisible(results)
}

# Function to display evaluation and suggestions
show_fa_evaluation <- function(results) {
  source("factor_suitability_display.R")
  fs_display_evaluation(results)
}