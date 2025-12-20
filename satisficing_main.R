# ===================================================
# Satisficing Main (Controller Layer)
# Version: 1.1
# Description: Main controller for satisficing analysis
# Changes from v1.0:
#   - Pass row names (IDs) to calculator
# ===================================================

analyze_satisficing <- function(data_obj) {
  
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
  satisficing_config <- config$analysis$satisficing
  
  # Get thresholds from config
  ls_threshold <- satisficing_config$longstring$threshold
  irv_percentile <- satisficing_config$irv$percentile_threshold
  mahad_p_threshold <- satisficing_config$mahalanobis$p_threshold
  
  # Display header
  satisficing_display_header()
  
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
  satisficing_display_flagged_cases(results$indices, results$flags)
  
  # Footer
  cat("========================================\n")
  cat("Analysis Complete\n")
  cat("========================================\n\n")
  
  cat("Interpretation:\n")
  cat("- Longstring: High values indicate straightlining\n")
  cat("- IRV: Low values indicate lack of response variability\n")
  cat("- Mahalanobis: Low p-values indicate multivariate outliers\n")
  cat("- Flagged cases should be examined for sensitivity analysis\n")
  
  # Return results invisibly
  results$descriptives <- stats
  invisible(results)
}

# Get flagged case IDs for sensitivity analysis
get_flagged_ids <- function(satisficing_results, data_obj) {
  source("data_structure.R")
  data <- get_data(data_obj)
  
  flagged_rows <- which(satisficing_results$flags$any_flag)
  flagged_ids <- satisficing_results$flags$id[flagged_rows]
  
  return(list(
    indices = flagged_rows,
    ids = flagged_ids,
    n_flagged = length(flagged_rows),
    n_total = nrow(data)
  ))
}

# Create clean dataset excluding flagged cases
create_clean_dataset <- function(data_obj, satisficing_results) {
  source("data_structure.R")
  data <- get_data(data_obj)
  keys <- get_keys(data_obj)
  
  # Get non-flagged indices
  clean_indices <- which(!satisficing_results$flags$any_flag)
  
  # Subset data
  clean_data <- data[clean_indices, , drop = FALSE]
  
  cat(sprintf("Original cases: %d\n", nrow(data)))
  cat(sprintf("Flagged cases removed: %d\n", sum(satisficing_results$flags$any_flag)))
  cat(sprintf("Clean cases: %d\n", nrow(clean_data)))
  
  # Return as keyed data structure
  return(create_data_with_keys(keys = keys, data = clean_data))
}