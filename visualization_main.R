# ===================================================
# Visualization Main (Controller Layer)
# Version: 1.0 - Data visualization controller
# ===================================================

visualize_data <- function(data_obj) {
  
  # Display header
  visualization_display_header()
  
  # Load config
  config <- load_config()
  scale_min <- config$analysis$global$scale$min
  scale_max <- config$analysis$global$scale$max
  use <- config$analysis$correlation_analysis$use
  
  # Extract data
  source("data_structure.R")
  data <- get_data(data_obj)
  
  cat(sprintf("Data: %d cases x %d items\n", nrow(data), ncol(data)))
  cat(sprintf("Scale range: %d to %d\n\n", scale_min, scale_max))
  
  # Step 1: Response Distribution
  cat("Step 1: Response Distribution\n")
  cat("-----------------------------\n")
  dist_data <- calc_response_distribution(data, scale_min, scale_max)
  display_response_histograms(dist_data, scale_min, scale_max)
  
  # Step 2: Total Score Distribution
  cat("\nStep 2: Total Score Distribution\n")
  cat("---------------------------------\n")
  total_data <- calc_total_score_distribution(data)
  display_total_score_histogram(total_data)
  
  # Step 3: Correlation Matrix
  cat("\nStep 3: Correlation Matrix\n")
  cat("---------------------------\n")
  cor_matrix <- calc_correlation_matrix(data, use)
  display_correlation_heatmap(cor_matrix)
  
  cat("\n========================================\n")
  cat("Visualization Complete\n")
  cat("========================================\n")
  
  invisible(list(
    response_dist = dist_data,
    total_score = total_data,
    correlation = cor_matrix
  ))
}