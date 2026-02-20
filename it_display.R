# ===================================================
# Item-Total Correlation Display (View Layer)
# Version: 5.2 - Removed judgment from comparison display
# Description: Display functions for I-T correlation analysis with both methods
# Changes from v5.1:
#   - Removed Flag column and Legend from it_display_comparison()
#   - Removed Summary count of atypical pattern
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

# Display I-T evaluation results
display_it_evaluation <- function(eval_results) {
  cat("\n========================================\n")
  cat("Item-Total Correlation Evaluation\n")
  cat("========================================\n\n")
  
  # Display thresholds
  cat("Evaluation Criteria (Corrected I-T Correlation):\n")
  cat(sprintf("  Poor:      r < %.2f (consider deletion)\n", eval_results$thresholds$poor))
  cat(sprintf("  Marginal:  %.2f <= r < %.2f (caution)\n", 
              eval_results$thresholds$poor, eval_results$thresholds$marginal))
  cat(sprintf("  Good:      %.2f <= r < %.2f (acceptable)\n",
              eval_results$thresholds$marginal, eval_results$thresholds$good))
  cat(sprintf("  Excellent: r >= %.2f (good)\n", eval_results$thresholds$good))
  
  # Display item-level evaluation
  cat("\n----------------------------------------\n")
  cat("Item-Level Evaluation\n")
  cat("----------------------------------------\n\n")
  
  eval_df <- eval_results$evaluation
  formatted <- data.frame(
    Item = eval_df$item,
    Pear_r = sprintf("%.3f", eval_df$pearson_r),
    Pear_Eval = eval_df$pearson_eval,
    Poly_r = sprintf("%.3f", eval_df$polyserial_r),
    Poly_Eval = eval_df$polyserial_eval,
    stringsAsFactors = FALSE
  )
  
  print(formatted, row.names = FALSE)
  
  # Display summary counts
  cat("\n----------------------------------------\n")
  cat("Summary Counts\n")
  cat("----------------------------------------\n\n")
  
  cat("Pearson:\n")
  cat(sprintf("  Excellent: %d, Good: %d, Marginal: %d, Poor: %d\n",
              eval_results$pearson_counts["Excellent"],
              eval_results$pearson_counts["Good"],
              eval_results$pearson_counts["Marginal"],
              eval_results$pearson_counts["Poor"]))
  
  cat("\nPolyserial:\n")
  cat(sprintf("  Excellent: %d, Good: %d, Marginal: %d, Poor: %d\n",
              eval_results$polyserial_counts["Excellent"],
              eval_results$polyserial_counts["Good"],
              eval_results$polyserial_counts["Marginal"],
              eval_results$polyserial_counts["Poor"]))
  
  # Display problem items
  if (length(eval_results$problem_items) > 0) {
    cat("\n----------------------------------------\n")
    cat("Problem Items (Poor or Marginal)\n")
    cat("----------------------------------------\n")
    cat(paste(eval_results$problem_items, collapse = ", "), "\n")
  } else {
    cat("\nNo problem items detected.\n")
  }
}

# Display Total vs Subscale comparison
it_display_comparison <- function(comparison) {
  cat("\n========================================\n")
  cat("RESULTS: Total vs Subscale Comparison\n")
  cat("========================================\n\n")
  
  # Format for display
  formatted <- data.frame(
    Item = comparison$item,
    Subscale = substr(comparison$subscale, 1, 20),
    Total_Corr = sprintf("%.3f", comparison$total_corr),
    Subscale_Corr = sprintf("%.3f", comparison$subscale_corr),
    Diff = sprintf("%+.3f", comparison$diff),
    stringsAsFactors = FALSE
  )
  
  # Handle NA subscale
  formatted$Subscale[is.na(comparison$subscale)] <- "(none)"
  formatted$Subscale_Corr[is.na(comparison$subscale_corr)] <- "   NA"
  formatted$Diff[is.na(comparison$diff)] <- "    NA"
  
  print(formatted, row.names = FALSE)
}