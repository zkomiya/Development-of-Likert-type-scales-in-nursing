# ===================================================
# Factor Number Main (Controller Layer)
# Version: 2.0
# Description: Main controller for factor number determination
# ===================================================

source("data_structure.R")
source("data_preprocessor.R")
source("factor_number_calculator.R")
source("factor_number_display.R")

# Main function to determine number of factors
determine_factors <- function(data_obj,
                              n_iterations = 1000,
                              percentile = 99,
                              seed = NULL) {
  
  # Extract data from keyed structure
  data <- get_data(data_obj)
  
  # Data preprocessing
  data_fa <- preprocess_for_fa(data, method = "listwise", verbose = FALSE)
  
  # Run factor number determination
  results <- determine_n_factors(
    data_fa,
    n_iterations = n_iterations,
    percentile = percentile,
    seed = seed
  )
  
  # Display results
  display_factor_number_results(
    results,
    n_obs = results$n_obs,
    n_vars = results$n_vars
  )
  
  invisible(results)
}

# Show evaluation separately
show_fn_evaluation <- function(data_obj,
                               n_iterations = 1000,
                               percentile = 99,
                               seed = NULL) {
  
  # Extract data from keyed structure
  data <- get_data(data_obj)
  
  # Data preprocessing
  data_fa <- preprocess_for_fa(data, method = "listwise", verbose = FALSE)
  
  # Run factor number determination
  results <- determine_n_factors(
    data_fa,
    n_iterations = n_iterations,
    percentile = percentile,
    seed = seed
  )
  
  # Display evaluation
  show_factor_number_evaluation(results)
  
  invisible(results)
}