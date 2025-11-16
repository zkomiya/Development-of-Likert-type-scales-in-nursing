# ===================================================
# Item-Total Correlation Display (View Layer)
# Version: 4.0 - Pearson + Polyserial concurrent display (4-column)
# Description: Display functions for I-T correlation analysis with both methods
# Changes from v2.0:
#   - Updated it_display_calculation_info() to show both methods
#   - Updated it_display_results() for 4-column display
#   - Updated it_display_summary() to show both methods + comparison
#   - Updated subscale display functions for 4-column support
# ===================================================

# Display header
it_display_header <- function() {
  cat("========================================\n")
  cat("Item-Total Correlation Analysis\n")
  cat("========================================\n\n")
}

# Display data information
it_display_data_info <- function(n_items, item_names) {
  cat("Step 1: Data detection\n")
  cat(sprintf("  Found %d items: %s ... %s\n", 
              n_items, item_names[1], item_names[n_items]))
}

# Display calculation info
it_display_calculation_info <- function() {
  cat("\nStep 2: Calculating correlations\n")
  cat("  Method 1: Pearson (via psych::alpha)\n")
  cat("  Method 2: Polyserial (via polycor::polyserial)\n")
  cat("  Type: Both corrected and uncorrected\n")
}

# Display main results (4-column structure)
it_display_results <- function(it_results) {
  cat("\n========================================\n")
  cat("RESULTS\n")
  cat("========================================\n\n")
  
  # Format for display
  formatted <- data.frame(
    Item = it_results$item,
    Pear_Corr = sprintf("%.3f", it_results$cor_corrected_pearson),
    Pear_Uncorr = sprintf("%.3f", it_results$cor_uncorrected_pearson),
    Poly_Corr = sprintf("%.3f", it_results$cor_corrected_polyserial),
    Poly_Uncorr = sprintf("%.3f", it_results$cor_uncorrected_polyserial),
    stringsAsFactors = FALSE
  )
  
  # Replace NA with "  NA" for alignment
  formatted[formatted == "NA"] <- "  NA"
  
  print(formatted, row.names = FALSE)
  
  cat("\nColumn Legend:\n")
  cat("  Pear_Corr   : Pearson corrected (item-rest)\n")
  cat("  Pear_Uncorr : Pearson uncorrected (item-total)\n")
  cat("  Poly_Corr   : Polyserial corrected (item-rest)\n")
  cat("  Poly_Uncorr : Polyserial uncorrected (item-total)\n")
}

# Display summary statistics
it_display_summary <- function(summary_stats) {
  cat("\n----------------------------------------\n")
  cat("SUMMARY\n")
  cat("----------------------------------------\n")
  
  # Pearson
  cat("\nPearson Correlations:\n")
  cat(sprintf("  Corrected:   Mean = %.3f (SD = %.3f), Range = [%.3f, %.3f]\n",
              summary_stats$mean_corrected_pearson,
              summary_stats$sd_corrected_pearson,
              summary_stats$min_corrected_pearson,
              summary_stats$max_corrected_pearson))
  cat(sprintf("  Uncorrected: Mean = %.3f (SD = %.3f)\n",
              summary_stats$mean_uncorrected_pearson,
              summary_stats$sd_uncorrected_pearson))
  
  # Polyserial
  cat("\nPolyserial Correlations:\n")
  cat(sprintf("  Corrected:   Mean = %.3f (SD = %.3f), Range = [%.3f, %.3f]\n",
              summary_stats$mean_corrected_polyserial,
              summary_stats$sd_corrected_polyserial,
              summary_stats$min_corrected_polyserial,
              summary_stats$max_corrected_polyserial))
  cat(sprintf("  Uncorrected: Mean = %.3f (SD = %.3f)\n",
              summary_stats$mean_uncorrected_polyserial,
              summary_stats$sd_uncorrected_polyserial))
  
  # Comparison
  cat("\nDifference (Polyserial - Pearson):\n")
  cat(sprintf("  Corrected:   Mean = %+.3f (SD = %.3f)\n",
              summary_stats$mean_diff_corrected,
              summary_stats$sd_diff_corrected))
  cat(sprintf("  Uncorrected: Mean = %+.3f (SD = %.3f)\n",
              summary_stats$mean_diff_uncorrected,
              summary_stats$sd_diff_uncorrected))
  
  # Quality check
  if (summary_stats$n_na_corrected_polyserial > 0 || 
      summary_stats$n_na_uncorrected_polyserial > 0) {
    cat("\nWarning: Polyserial computation failed for some items:\n")
    cat(sprintf("  Corrected: %d NAs\n", summary_stats$n_na_corrected_polyserial))
    cat(sprintf("  Uncorrected: %d NAs\n", summary_stats$n_na_uncorrected_polyserial))
  }
}

# Display subscale header
it_display_subscale_header <- function(subscale_name, n_items) {
  cat("\n----------------------------------------\n")
  cat(sprintf("SUBSCALE: %s (%d items)\n", subscale_name, n_items))
  cat("----------------------------------------\n")
}

# Display subscale results (4-column structure)
it_display_subscale_results <- function(subscale_results) {
  formatted <- data.frame(
    Item = subscale_results$item,
    Pear_Corr = sprintf("%.3f", subscale_results$cor_corrected_pearson),
    Pear_Uncorr = sprintf("%.3f", subscale_results$cor_uncorrected_pearson),
    Poly_Corr = sprintf("%.3f", subscale_results$cor_corrected_polyserial),
    Poly_Uncorr = sprintf("%.3f", subscale_results$cor_uncorrected_polyserial),
    stringsAsFactors = FALSE
  )
  
  formatted[formatted == "NA"] <- "  NA"
  
  print(formatted, row.names = FALSE)
}