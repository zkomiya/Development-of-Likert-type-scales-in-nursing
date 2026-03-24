# ===================================================
# Factor Number Main (Controller Layer)
# ===================================================

source("data_structure.R")
source("data_preprocessor.R")
source("factor_number_calculator.R")
source("factor_number_display.R")
source("config_loader.R")
source("sensitivity_utils.R")

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
  
  # Read sensitivity_exclude_items
  sensitivity_exclude_items <- unlist(config$analysis$factor_number$sensitivity_exclude_items)
  exclude_subsets <- generate_exclude_subsets(sensitivity_exclude_items)
  
  if (verbose) {
    if (!is.null(sensitivity_exclude_items) && length(sensitivity_exclude_items) > 0) {
      cat("  Sensitivity exclude items:", paste(sensitivity_exclude_items, collapse = ", "), "\n")
      cat("  Number of subsets:", length(exclude_subsets), "\n")
    } else {
      cat("  Sensitivity analysis: disabled\n")
    }
  }
  
  # Extract data from keyed structure
  data <- get_data(data_obj)
  
  # Outer loop: subsets (sensitivity analysis)
  all_subset_results <- list()
  
  for (subset_info in exclude_subsets) {
    
    subset_key <- subset_info$key
    exclude_items <- subset_info$items
    
    cat("\n################################################################\n")
    cat(sprintf("SENSITIVITY SUBSET: %s\n", subset_key))
    if (length(exclude_items) > 0) {
      cat(sprintf("  Excluded items: %s\n", paste(exclude_items, collapse = ", ")))
    } else {
      cat("  Excluded items: none (full dataset)\n")
    }
    cat("################################################################\n")
    
    # Prepare data for this subset: remove excluded columns
    data_subset <- data
    if (length(exclude_items) > 0) {
      cols_to_remove <- intersect(exclude_items, names(data_subset))
      if (length(cols_to_remove) != length(exclude_items)) {
        missing_cols <- setdiff(exclude_items, names(data_subset))
        stop(sprintf("Exclude items not found in data: %s",
                     paste(missing_cols, collapse = ", ")))
      }
      data_subset <- data_subset[, !names(data_subset) %in% exclude_items, drop = FALSE]
    }
    
    # Data preprocessing
    data_fa <- preprocess_for_fa(data_subset,
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
      verbose = verbose,
      missing_method = "listwise",
      scale_min = scale_min,
      scale_max = scale_max
    )
    
    # Display results
    display_factor_number_results(
      results,
      n_obs = results$n_obs,
      n_vars = results$n_vars
    )
    
    all_subset_results[[subset_key]] <- results
  }
  
  # Final summary
  cat("\n========================================\n")
  cat("FACTOR NUMBER DETERMINATION COMPLETE\n")
  cat(sprintf("  Subsets analyzed: %s\n",
              paste(sapply(exclude_subsets, function(x) x$key), collapse = ", ")))
  cat("========================================\n")
  
  all_subset_results$exclude_subsets <- exclude_subsets
  
  invisible(all_subset_results)
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
  
  # Read sensitivity_exclude_items
  sensitivity_exclude_items <- unlist(config$analysis$factor_number$sensitivity_exclude_items)
  exclude_subsets <- generate_exclude_subsets(sensitivity_exclude_items)
  
  if (verbose) {
    if (!is.null(sensitivity_exclude_items) && length(sensitivity_exclude_items) > 0) {
      cat("  Sensitivity exclude items:", paste(sensitivity_exclude_items, collapse = ", "), "\n")
      cat("  Number of subsets:", length(exclude_subsets), "\n")
    } else {
      cat("  Sensitivity analysis: disabled\n")
    }
  }
  
  # Extract data from keyed structure
  data <- get_data(data_obj)
  
  # Outer loop: subsets (sensitivity analysis)
  for (subset_info in exclude_subsets) {
    
    subset_key <- subset_info$key
    exclude_items <- subset_info$items
    
    cat("\n################################################################\n")
    cat(sprintf("SENSITIVITY SUBSET: %s\n", subset_key))
    if (length(exclude_items) > 0) {
      cat(sprintf("  Excluded items: %s\n", paste(exclude_items, collapse = ", ")))
    } else {
      cat("  Excluded items: none (full dataset)\n")
    }
    cat("################################################################\n")
    
    # Prepare data for this subset: remove excluded columns
    data_subset <- data
    if (length(exclude_items) > 0) {
      cols_to_remove <- intersect(exclude_items, names(data_subset))
      if (length(cols_to_remove) != length(exclude_items)) {
        missing_cols <- setdiff(exclude_items, names(data_subset))
        stop(sprintf("Exclude items not found in data: %s",
                     paste(missing_cols, collapse = ", ")))
      }
      data_subset <- data_subset[, !names(data_subset) %in% exclude_items, drop = FALSE]
    }
    
    # Data preprocessing
    data_fa <- preprocess_for_fa(data_subset,
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
      verbose = verbose,
      missing_method = "listwise",
      scale_min = scale_min,
      scale_max = scale_max
    )
    
    # Display evaluation
    show_factor_number_evaluation(results)
  }
  
  invisible(NULL)
}