# ===================================================
# Response Styles Main (Controller Layer)
# Version: 1.1
# Description: Main controller for response styles analysis
# Changes from v1.0:
#   - Pass row names (IDs) to calculator
# ===================================================

analyze_response_styles <- function(data_obj) {
  
  # Extract data from keyed structure
  source("data_structure.R")
  data <- get_data(data_obj)
  
  # Get row IDs
  row_ids <- rownames(data)
  if (is.null(row_ids)) {
    row_ids <- as.character(1:nrow(data))
  }
  
  # Load configuration
  config <- load_config()
  
  # Get scale range from global settings
  min_cat <- config$analysis$global$scale$min
  max_cat <- config$analysis$global$scale$max
  
  # Get response styles settings
  rs_config <- config$analysis$response_styles
  extreme_percentile <- rs_config$extreme_percentile
  
  # Display header
  response_styles_display_header()
  
  # Display mathematical definitions
  response_styles_display_definitions(min_cat, max_cat)
  
  # Calculate indices
  cat("Calculating response style indices...\n\n")
  
  indices <- calculate_response_styles(data, min_cat, max_cat, row_ids)
  
  # Calculate descriptive statistics
  stats <- calculate_response_styles_descriptives(indices)
  
  # Flag extreme users
  flag_results <- flag_response_styles(indices, extreme_percentile)
  
  # Calculate correlations with total score
  correlations <- calculate_rs_correlations(indices, data)
  
  # Display results
  response_styles_display_descriptives(stats)
  response_styles_display_thresholds(flag_results$thresholds)
  response_styles_display_summary(flag_results$summary)
  response_styles_display_correlations(correlations)
  response_styles_display_flagged_cases(indices, flag_results$flags)
  
  # Footer
  cat("========================================\n")
  cat("Analysis Complete\n")
  cat("========================================\n\n")
  
  cat("Interpretation:\n")
  cat("- ERS: Proportion of extreme category responses (1 or 4)\n")
  cat("- MRS: Proportion of middle category responses (2 or 3)\n")
  cat("- High ERS may inflate variance estimates\n")
  cat("- High MRS may attenuate correlations\n")
  cat("- Use flagged cases for sensitivity analysis\n")
  
  # Return results invisibly
  results <- list(
    indices = indices,
    descriptives = stats,
    flags = flag_results$flags,
    thresholds = flag_results$thresholds,
    summary = flag_results$summary,
    correlations = correlations,
    scale_range = list(min = min_cat, max = max_cat)
  )
  
  invisible(results)
}

# Get flagged case IDs for sensitivity analysis
get_rs_flagged_ids <- function(rs_results, data_obj, type = "ers") {
  source("data_structure.R")
  data <- get_data(data_obj)
  
  if (type == "ers") {
    flagged_rows <- which(rs_results$flags$ers_flag)
  } else if (type == "mrs") {
    flagged_rows <- which(rs_results$flags$mrs_flag)
  } else {
    stop("type must be 'ers' or 'mrs'")
  }
  
  flagged_ids <- rs_results$flags$id[flagged_rows]
  
  return(list(
    indices = flagged_rows,
    ids = flagged_ids,
    n_flagged = length(flagged_rows),
    n_total = nrow(data)
  ))
}

# Create dataset excluding high ERS or MRS cases
create_rs_clean_dataset <- function(data_obj, rs_results, exclude_type = "ers") {
  source("data_structure.R")
  data <- get_data(data_obj)
  keys <- get_keys(data_obj)
  
  if (exclude_type == "ers") {
    clean_indices <- which(!rs_results$flags$ers_flag)
    n_excluded <- sum(rs_results$flags$ers_flag, na.rm = TRUE)
    label <- "high ERS"
  } else if (exclude_type == "mrs") {
    clean_indices <- which(!rs_results$flags$mrs_flag)
    n_excluded <- sum(rs_results$flags$mrs_flag, na.rm = TRUE)
    label <- "high MRS"
  } else if (exclude_type == "both") {
    clean_indices <- which(!rs_results$flags$ers_flag & !rs_results$flags$mrs_flag)
    n_excluded <- sum(rs_results$flags$ers_flag | rs_results$flags$mrs_flag, na.rm = TRUE)
    label <- "high ERS or MRS"
  } else {
    stop("exclude_type must be 'ers', 'mrs', or 'both'")
  }
  
  # Subset data
  clean_data <- data[clean_indices, , drop = FALSE]
  
  cat(sprintf("Original cases: %d\n", nrow(data)))
  cat(sprintf("Excluded (%s): %d\n", label, n_excluded))
  cat(sprintf("Clean cases: %d\n", nrow(clean_data)))
  
  # Return as keyed data structure
  return(create_data_with_keys(keys = keys, data = clean_data))
}