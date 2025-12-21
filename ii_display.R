# ===================================================
# Item-Item Correlation Display (View Layer)
# Version: 6.0 - Added evaluation display
# Description: Display functions for item-item correlation analysis
# Changes from v5.0:
#   - Added ii_display_evaluation()
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

# Display evaluation results
ii_display_evaluation <- function(miic_poly, miic_pear, cor_poly, cor_pear) {
  cat("\n========================================\n")
  cat("II CORRELATION EVALUATION\n")
  cat("========================================\n\n")
  
  # MIIC evaluation
  cat("MIIC:\n")
  
  # Polychoric MIIC
  poly_range <- if (miic_poly$miic < 0.15) {
    "< 0.15"
  } else if (miic_poly$miic > 0.50) {
    "> 0.50"
  } else {
    "0.15-0.50"
  }
  cat(sprintf("  Polychoric: %.3f [%s]\n", miic_poly$miic, poly_range))
  
  # Pearson MIIC
  pear_range <- if (miic_pear$miic < 0.15) {
    "< 0.15"
  } else if (miic_pear$miic > 0.50) {
    "> 0.50"
  } else {
    "0.15-0.50"
  }
  cat(sprintf("  Pearson:    %.3f [%s]\n", miic_pear$miic, pear_range))
  
  # Item pairs with r > 0.70 (Polychoric)
  cat("\nItem Pairs with r > 0.70 (Polychoric):\n")
  high_pairs <- which(cor_poly > 0.70 & upper.tri(cor_poly), arr.ind = TRUE)
  if (nrow(high_pairs) > 0) {
    for (i in seq_len(nrow(high_pairs))) {
      row_idx <- high_pairs[i, 1]
      col_idx <- high_pairs[i, 2]
      cat(sprintf("  %s-%s: %.2f\n", 
                  rownames(cor_poly)[row_idx],
                  colnames(cor_poly)[col_idx],
                  cor_poly[row_idx, col_idx]))
    }
  } else {
    cat("  None\n")
  }
  
  # Item pairs with r < 0.10 (Polychoric)
  cat("\nItem Pairs with r < 0.10 (Polychoric):\n")
  low_pairs <- which(cor_poly < 0.10 & upper.tri(cor_poly), arr.ind = TRUE)
  if (nrow(low_pairs) > 0) {
    for (i in seq_len(nrow(low_pairs))) {
      row_idx <- low_pairs[i, 1]
      col_idx <- low_pairs[i, 2]
      cat(sprintf("  %s-%s: %.2f\n",
                  rownames(cor_poly)[row_idx],
                  colnames(cor_poly)[col_idx],
                  cor_poly[row_idx, col_idx]))
    }
  } else {
    cat("  None\n")
  }
  
  # Pairs with large difference
  cat("\nPairs with |Polychoric - Pearson| > 0.15:\n")
  cor_diff <- abs(cor_poly - cor_pear)
  diff_pairs <- which(cor_diff > 0.15 & upper.tri(cor_diff), arr.ind = TRUE)
  if (nrow(diff_pairs) > 0) {
    for (i in seq_len(nrow(diff_pairs))) {
      row_idx <- diff_pairs[i, 1]
      col_idx <- diff_pairs[i, 2]
      cat(sprintf("  %s-%s: Poly=%.2f, Pear=%.2f, Diff=%.2f\n",
                  rownames(cor_poly)[row_idx],
                  colnames(cor_poly)[col_idx],
                  cor_poly[row_idx, col_idx],
                  cor_pear[row_idx, col_idx],
                  cor_diff[row_idx, col_idx]))
    }
  } else {
    cat("  None\n")
  }
}