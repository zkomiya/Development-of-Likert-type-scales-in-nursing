# ===================================================
# GP Main (Controller Layer)
# Version: 9.2
# Changes from v9.1:
#   - Updated CSV export column order
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
  
  # Step 4: Calculate discrimination indices
  discrimination_results <- calculate_gp_indices(data, gp_group, scale_range)
  
  # Display results
  gp_display_results(discrimination_results)
  
  # Step 5: Calculate summary statistics
  summary_stats <- calculate_summary_stats(discrimination_results, cutoffs, total_scores)
  
  # Display summary
  gp_display_summary(summary_stats)
  
  # Export results to CSV
  output_dir <- "output"
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }
  output_file <- file.path(output_dir, "gp_analysis_results.csv")
  col_order <- c("item", "n_poor", "n_good", "M_poor", "SD_poor", "M_good", "SD_good", "D_star", "Cohens_d", "Hedges_g")
  write.csv(discrimination_results[, col_order], output_file, row.names = FALSE)
  cat(sprintf("\nResults exported to: %s\n", output_file))
  
  # Return results as invisible list
  invisible(list(
    discrimination_indices = discrimination_results,
    summary = summary_stats,
    gp_group = gp_group,
    total_scores = total_scores,
    item_cols = item_cols
  ))
}

# Function to display GP evaluation separately
show_gp_evaluation <- function(gp_results) {
  gp_display_evaluation(gp_results$discrimination_indices)
}