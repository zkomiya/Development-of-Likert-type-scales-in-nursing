# ===================================================
# Ceiling-Floor Effect Display (View Layer)
# Version: 9.0 - Added kurtosis with specified order
# ===================================================

# Unified display function for all ceiling-floor results
display_ceiling_floor_results <- function(results, parameters) {
  
  # Header
  cat("========================================\n")
  cat("Ceiling-Floor Effect Analysis\n")
  cat("========================================\n\n")
  
  # Parameters
  cat(sprintf("Scale range: %d-%d\n", parameters$scale_min, parameters$scale_max))
  cat(sprintf("Items: %d, Cases: %d\n\n", parameters$n_items, parameters$n_cases))
  
  # Format results for display with specified order
  display_df <- data.frame(
    Item = results$item,
    Mean = sprintf("%.2f", results$mean),
    SD = sprintf("%.2f", results$sd),
    Skewness = sprintf("%+.2f", results$skewness),  # + sign for positive values
    Kurtosis = sprintf("%+.2f", results$kurtosis),  # + sign for positive values
    `Mean+SD` = sprintf("%.2f", results$mean_plus_sd),
    `Mean-SD` = sprintf("%.2f", results$mean_minus_sd),
    `Ceiling(%)` = sprintf("%.1f", results$ceiling_pct),
    `Floor(%)` = sprintf("%.1f", results$floor_pct),
    check.names = FALSE
  )
  
  print(display_df, row.names = FALSE)
}