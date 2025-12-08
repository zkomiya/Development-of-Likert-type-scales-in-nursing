# ===================================================
# GP Main (Controller Layer)
# Version: 7.0
# Changes from v6.0:
#   - Removed cutoff_percentile and scale_range from arguments
#   - Now reads these parameters from YAML configuration
# Description: Main controller for GP analysis
# ===================================================

# Main GP analysis function
analyze_gp <- function(data_obj) {
  
  # Extract data from keyed structure
  data <- get_data(data_obj)
  keys <- get_keys(data_obj)
  
  # Load configuration for parameters
  config <- load_config()
  cutoff_percentile <- config$analysis$gp_analysis$cutoff_percentile
  scale_min <- config$analysis$global$scale$min
  scale_max <- config$analysis$global$scale$max
  scale_range <- c(scale_min, scale_max)
  
  # Display header
  gp_display_header()
  
  # Display data info - data is already Q columns only
  n_items <- ncol(data)
  item_cols <- names(data)
  gp_display_data_info(item_cols, n_items)
  
  # Step 2: Calculate total scores (direct rowSums call)
  total_scores <- rowSums(data, na.rm = TRUE)
  
  # Display total score info
  gp_display_total_score_info(
    min(total_scores, na.rm = TRUE),
    max(total_scores, na.rm = TRUE)
  )
  
  # Step 3: Calculate cutoffs and assign groups
  cutoffs <- calculate_cutoffs(total_scores, cutoff_percentile)
  gp_group <- assign_gp_groups(total_scores, cutoffs)
  
  # Count group sizes
  n_good <- sum(gp_group == "Good", na.rm = TRUE)
  n_poor <- sum(gp_group == "Poor", na.rm = TRUE)
  
  # Display group info
  gp_display_group_info(cutoff_percentile, n_good, n_poor, cutoffs$upper, cutoffs$lower)
  
  # Step 4: Calculate discrimination indices (now with 4 effect sizes)
  discrimination_results <- calculate_gp_indices(data, gp_group, scale_range)
  
  # Display results
  gp_display_results(discrimination_results)
  
  # Step 5: Calculate summary statistics
  summary_stats <- calculate_summary_stats(discrimination_results, cutoffs, total_scores)
  
  # Display summary
  gp_display_summary(summary_stats)
  
  # Return results as invisible list
  invisible(list(
    discrimination_indices = discrimination_results,
    summary = summary_stats,
    gp_group = gp_group,
    total_scores = total_scores,
    item_cols = item_cols
  ))
}