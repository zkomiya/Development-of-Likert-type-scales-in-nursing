# ===================================================
# Factor Number Main (Controller Layer)
# Version: 4.1 (YAML global scale/pattern integration)
# Description: Main controller for factor number determination
# ===================================================

source("data_structure.R")
source("data_preprocessor.R")
source("factor_number_calculator.R")
source("factor_number_display.R")
source("config_loader.R")

# Main function to determine number of factors
determine_factors <- function(data_obj,
                              n_iterations = NULL,
                              percentile = NULL,
                              seed = NULL,
                              verbose = TRUE) {
  
  # Load config
  config <- load_config()
  
  # Global settings (scale range / item pattern)
  global_config <- config$analysis$global
  if (is.null(global_config$item_pattern)) {
    stop("Global item_pattern not found in analysis_config.yaml")
  }
  if (is.null(global_config$scale$min) || is.null(global_config$scale$max)) {
    stop("Global scale min/max not found in analysis_config.yaml")
  }
  item_pattern <- global_config$item_pattern
  scale_min <- global_config$scale$min
  scale_max <- global_config$scale$max
  
  # Get parameters from config (use argument if provided)
  if (is.null(n_iterations)) {
    n_iterations <- config$analysis$factor_number$parallel_analysis_iterations
  }
  
  if (is.null(percentile)) {
    percentile <- config$analysis$factor_number$percentile
  }
  
  # Validate required parameters
  if (is.null(n_iterations)) {
    stop("parallel_analysis_iterations not found in analysis_config.yaml under factor_number section")
  }
  
  if (is.null(percentile)) {
    stop("percentile not found in analysis_config.yaml under factor_number section")
  }
  
  # Extract data from keyed structure
  data <- get_data(data_obj)
  
  # Data preprocessing
  data_fa <- preprocess_for_fa(data,
                               method = "listwise",
                               verbose = verbose,
                               item_pattern = item_pattern,
                               scale_min = scale_min,
                               scale_max = scale_max)
  
  # Run factor number determination
  results <- determine_n_factors(
    data_fa,
    n_iterations = n_iterations,
    percentile = percentile,
    seed = seed,
    verbose = verbose
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
                               n_iterations = NULL,
                               percentile = NULL,
                               seed = NULL,
                               verbose = TRUE) {
  
  # Load config
  config <- load_config()
  
  # Global settings (scale range / item pattern)
  global_config <- config$analysis$global
  if (is.null(global_config$item_pattern)) {
    stop("Global item_pattern not found in analysis_config.yaml")
  }
  if (is.null(global_config$scale$min) || is.null(global_config$scale$max)) {
    stop("Global scale min/max not found in analysis_config.yaml")
  }
  item_pattern <- global_config$item_pattern
  scale_min <- global_config$scale$min
  scale_max <- global_config$scale$max
  
  # Get parameters from config (use argument if provided)
  if (is.null(n_iterations)) {
    n_iterations <- config$analysis$factor_number$parallel_analysis_iterations
  }
  
  if (is.null(percentile)) {
    percentile <- config$analysis$factor_number$percentile
  }
  
  # Validate required parameters
  if (is.null(n_iterations)) {
    stop("parallel_analysis_iterations not found in analysis_config.yaml under factor_number section")
  }
  
  if (is.null(percentile)) {
    stop("percentile not found in analysis_config.yaml under factor_number section")
  }
  
  # Extract data from keyed structure
  data <- get_data(data_obj)
  
  # Data preprocessing
  data_fa <- preprocess_for_fa(data,
                               method = "listwise",
                               verbose = verbose,
                               item_pattern = item_pattern,
                               scale_min = scale_min,
                               scale_max = scale_max)
  
  # Run factor number determination
  results <- determine_n_factors(
    data_fa,
    n_iterations = n_iterations,
    percentile = percentile,
    seed = seed,
    verbose = verbose
  )
  
  # Display evaluation
  show_factor_number_evaluation(results)
  
  invisible(results)
}
