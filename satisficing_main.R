# ===================================================
# Satisficing Main (Controller Layer)
# Version: 2.0
# Description: Main controller for satisficing analysis
# Changes from v1.2:
#   - Accept entire data object from prepare_data()
#   - Analyze both target and rehab datasets automatically
# ===================================================

# Internal function to analyze single dataset
analyze_satisficing_single <- function(data, dataset_name, key_name, config) {
  
  # Get row IDs
  row_ids <- rownames(data)
  if (is.null(row_ids)) {
    row_ids <- as.character(1:nrow(data))
  }
  
  # Get thresholds from config
  satisficing_config <- config$analysis$satisficing
  ls_threshold <- satisficing_config$longstring$threshold
  irv_percentile <- satisficing_config$irv$percentile_threshold
  mahad_p_threshold <- satisficing_config$mahalanobis$p_threshold
  
  # Display header
  satisficing_display_header(dataset_name)
  
  # Display mathematical definitions
  satisficing_display_definitions()
  
  # Calculate indices and flags
  cat("Calculating satisficing indices...\n\n")
  
  results <- flag_careless_responders(
    data = data,
    ls_threshold = ls_threshold,
    irv_percentile = irv_percentile,
    mahad_p_threshold = mahad_p_threshold,
    row_ids = row_ids
  )
  
  # Calculate descriptive statistics
  stats <- calculate_satisficing_descriptives(results$indices)
  
  # Display results
  satisficing_display_descriptives(stats)
  satisficing_display_thresholds(results$thresholds)
  satisficing_display_summary(results$summary)
  satisficing_display_flagged_cases(results$indices, results$flags, key_name)
  
  # Footer
  cat("========================================\n")
  cat("Analysis Complete\n")
  cat("========================================\n\n")
  
  # Return results
  results$descriptives <- stats
  return(results)
}

# Main function to analyze all datasets
analyze_satisficing <- function(data) {
  
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
  all_results$target <- analyze_satisficing_single(
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
  all_results$rehab <- analyze_satisficing_single(
    data = rehab_data,
    dataset_name = "REHAB Scale",
    key_name = rehab_key_name,
    config = config
  )
  
  # Final interpretation
  cat("\n##################################################\n")
  cat("Interpretation:\n")
  cat("- Longstring: High values indicate straightlining\n")
  cat("- IRV: Low values indicate lack of response variability\n")
  cat("- Mahalanobis: Low p-values indicate multivariate outliers\n")
  cat("- Flagged cases should be examined for sensitivity analysis\n")
  cat("##################################################\n")
  
  invisible(all_results)
}

# Get flagged case IDs for sensitivity analysis
get_flagged_ids <- function(satisficing_results, dataset = "target") {
  
  results <- satisficing_results[[dataset]]
  
  flagged_rows <- which(results$flags$any_flag)
  flagged_ids <- results$flags$id[flagged_rows]
  
  return(list(
    indices = flagged_rows,
    ids = flagged_ids,
    n_flagged = length(flagged_rows),
    n_total = nrow(results$flags)
  ))
}

# Create clean dataset excluding flagged cases
create_clean_dataset <- function(data_obj, satisficing_results, dataset = "target") {
  source("data_structure.R")
  data <- get_data(data_obj)
  keys <- get_keys(data_obj)
  
  results <- satisficing_results[[dataset]]
  
  # Get non-flagged indices
  clean_indices <- which(!results$flags$any_flag)
  
  # Subset data
  clean_data <- data[clean_indices, , drop = FALSE]
  
  cat(sprintf("Original cases: %d\n", nrow(data)))
  cat(sprintf("Flagged cases removed: %d\n", sum(results$flags$any_flag)))
  cat(sprintf("Clean cases: %d\n", nrow(clean_data)))
  
  # Return as keyed data structure
  return(create_data_with_keys(keys = keys, data = clean_data))
}