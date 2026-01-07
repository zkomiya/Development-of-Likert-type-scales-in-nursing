# ===================================================
# Response Styles Main (Controller Layer)
# Version: 2.0
# Description: Main controller for response styles analysis
# Changes from v1.2:
#   - Accept entire data object from prepare_data()
#   - Analyze both target and rehab datasets automatically
# ===================================================

# Internal function to analyze single dataset
analyze_response_styles_single <- function(data, dataset_name, key_name, config) {
  
  # Get row IDs
  row_ids <- rownames(data)
  if (is.null(row_ids)) {
    row_ids <- as.character(1:nrow(data))
  }
  
  # Get scale range from global settings
  min_cat <- config$analysis$global$scale$min
  max_cat <- config$analysis$global$scale$max
  
  # Get response styles settings
  rs_config <- config$analysis$response_styles
  extreme_percentile <- rs_config$extreme_percentile
  
  # Display header
  response_styles_display_header(dataset_name)
  
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
  response_styles_display_flagged_cases(indices, flag_results$flags, key_name)
  
  # Footer
  cat("========================================\n")
  cat("Analysis Complete\n")
  cat("========================================\n\n")
  
  # Return results
  results <- list(
    indices = indices,
    descriptives = stats,
    flags = flag_results$flags,
    thresholds = flag_results$thresholds,
    summary = flag_results$summary,
    correlations = correlations,
    scale_range = list(min = min_cat, max = max_cat)
  )
  
  return(results)
}

# Main function to analyze all datasets
analyze_response_styles <- function(data) {
  
  source("data_structure.R")
  
  # Load configuration
  config <- load_config()
  
  # Get target dataset name from config
  target_dataset_name <- config$analysis$data_source$dataset
  target_key_name <- config$analysis$data_keys[[target_dataset_name]]
  rehab_key_name <- config$analysis$data_keys$rehab
  
  # Build display name for target
  if (target_dataset_name == "items_30") {
    target_display_name <- "30-item Scale"
  } else if (target_dataset_name == "items_28") {
    target_display_name <- "28-item Scale"
  } else {
    target_display_name <- target_dataset_name
  }
  
  # Initialize results list
  all_results <- list()
  
  # Analyze target dataset
  cat("##################################################\n")
  cat("## PART 1: TARGET SCALE\n")
  cat("##################################################\n\n")
  
  target_data <- get_data(data$target)
  all_results$target <- analyze_response_styles_single(
    data = target_data,
    dataset_name = target_display_name,
    key_name = target_key_name,
    config = config
  )
  
  # Analyze REHAB dataset
  cat("\n##################################################\n")
  cat("## PART 2: REHAB SCALE\n")
  cat("##################################################\n\n")
  
  rehab_data <- get_data(data$rehab)
  all_results$rehab <- analyze_response_styles_single(
    data = rehab_data,
    dataset_name = "REHAB Scale",
    key_name = rehab_key_name,
    config = config
  )
  
  # Final interpretation
  cat("\n##################################################\n")
  cat("Interpretation:\n")
  cat("- ERS: Proportion of extreme category responses (1 or 4)\n")
  cat("- MRS: Proportion of middle category responses (2 or 3)\n")
  cat("- High ERS may inflate variance estimates\n")
  cat("- High MRS may attenuate correlations\n")
  cat("- Use flagged cases for sensitivity analysis\n")
  cat("##################################################\n")
  
  invisible(all_results)
}

# Get flagged case IDs for sensitivity analysis
get_rs_flagged_ids <- function(rs_results, dataset = "target", type = "ers") {
  
  results <- rs_results[[dataset]]
  
  if (type == "ers") {
    flagged_rows <- which(results$flags$ers_flag)
  } else if (type == "mrs") {
    flagged_rows <- which(results$flags$mrs_flag)
  } else {
    stop("type must be 'ers' or 'mrs'")
  }
  
  flagged_ids <- results$flags$id[flagged_rows]
  
  return(list(
    indices = flagged_rows,
    ids = flagged_ids,
    n_flagged = length(flagged_rows),
    n_total = nrow(results$flags)
  ))
}

# Create dataset excluding high ERS or MRS cases
create_rs_clean_dataset <- function(data_obj, rs_results, dataset = "target", exclude_type = "ers") {
  source("data_structure.R")
  data <- get_data(data_obj)
  keys <- get_keys(data_obj)
  
  results <- rs_results[[dataset]]
  
  if (exclude_type == "ers") {
    clean_indices <- which(!results$flags$ers_flag)
    n_excluded <- sum(results$flags$ers_flag, na.rm = TRUE)
    label <- "high ERS"
  } else if (exclude_type == "mrs") {
    clean_indices <- which(!results$flags$mrs_flag)
    n_excluded <- sum(results$flags$mrs_flag, na.rm = TRUE)
    label <- "high MRS"
  } else if (exclude_type == "both") {
    clean_indices <- which(!results$flags$ers_flag & !results$flags$mrs_flag)
    n_excluded <- sum(results$flags$ers_flag | results$flags$mrs_flag, na.rm = TRUE)
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