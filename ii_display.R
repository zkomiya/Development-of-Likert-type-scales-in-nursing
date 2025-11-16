# ===================================================
# Item-Item Correlation Display (View Layer)
# Version: 5.0 - Polychoric + Pearson comparison display
# Description: Display functions for item-item correlation analysis
# Changes from v4.0:
#   - Added ii_display_miic_comparison() without Difference column
#   - Added ii_display_correlation_comparison()
#   - Removed old summary statistics display
# ===================================================

# Display header
ii_display_header <- function() {
  cat("========================================\n")
  cat("Item-Item Correlation Analysis\n")
  cat("========================================\n\n")
}

# Display data information
ii_display_data_info <- function(item_cols, n_items, complete_info = NULL) {
  cat("Step 1: Data detection\n")
  cat(sprintf("  Found %d item columns: %s ... %s\n", 
              n_items, item_cols[1], item_cols[n_items]))
  
  if (!is.null(complete_info)) {
    cat(sprintf("  Complete cases: %d out of %d (%.1f%%)\n",
                complete_info$n_complete,
                complete_info$n_total,
                complete_info$percent_complete))
  }
}

# Display MIIC comparison (without Difference column)
ii_display_miic_comparison <- function(miic_poly, miic_pear) {
  cat("\n========================================\n")
  cat("MIIC COMPARISON\n")
  cat("========================================\n\n")
  
  # Header
  cat(sprintf("%-25s %12s %12s\n", "", "Polychoric", "Pearson"))
  cat(paste(rep("-", 50), collapse = ""), "\n")
  
  # MIIC values
  cat(sprintf("%-25s %12.3f %12.3f\n", 
              "MIIC:", miic_poly$miic, miic_pear$miic))
  
  cat(sprintf("%-25s %12d %12d\n", 
              "Item pairs:", miic_poly$n_pairs, miic_pear$n_pairs))
  
  # Item-level mean correlations
  cat("\nItem-level mean correlations:\n")
  cat(sprintf("%-8s %12s %12s\n", "Item", "Polychoric", "Pearson"))
  cat(paste(rep("-", 35), collapse = ""), "\n")
  
  items <- names(miic_poly$item_mean_correlations)
  for (item in items) {
    poly_val <- miic_poly$item_mean_correlations[[item]]
    pear_val <- miic_pear$item_mean_correlations[[item]]
    
    cat(sprintf("%-8s %12.3f %12.3f\n", item, poly_val, pear_val))
  }
}

# Display correlation matrix comparison
ii_display_correlation_comparison <- function(cor_poly, cor_pear) {
  cat("\n========================================\n")
  cat("CORRELATION MATRIX COMPARISON\n")
  cat("========================================\n\n")
  
  cat("Polychoric Correlation Matrix:\n")
  print(round(cor_poly, 3))
  
  cat("\n----------------------------------------\n")
  cat("Pearson Correlation Matrix:\n")
  print(round(cor_pear, 3))
  
  cat("\n----------------------------------------\n")
  cat("Difference (Polychoric - Pearson):\n")
  cor_diff <- cor_poly - cor_pear
  print(round(cor_diff, 3))
}