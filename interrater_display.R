# ===================================================
# Interrater Reliability Display (View Layer)
# Version: 3.0 - Added Gwet's AC2 display functionality
# ===================================================

# Display header
ir_display_header <- function() {
  cat("========================================\n")
  cat("INTERRATER RELIABILITY ANALYSIS\n")
  cat("========================================\n\n")
}

# Display item-level results with AC2
ir_display_item_results <- function(item_results, scale_range = c(1, 4)) {
  cat("Item-level Reliability\n")
  cat("----------------------\n\n")
  
  # Header with AC2 columns in specified order
  cat(sprintf("%-6s %7s %7s %-16s %7s %-16s %8s\n",
              "Item", "p_o(%)", "κ_w", "κ_w CI", "AC2", "AC2 CI", "n_pairs"))
  cat(paste(rep("-", 75), collapse = ""), "\n")
  
  # Results for each item
  for (i in 1:nrow(item_results)) {
    row <- item_results[i, ]
    
    # Format values
    p_o_str <- ifelse(is.na(row$p_o), "   NA", sprintf("%6.1f", row$p_o))
    kappa_str <- ifelse(is.na(row$kappa_w), "   NA", sprintf("%6.3f", row$kappa_w))
    ac2_str <- ifelse(is.na(row$ac2_value), "   NA", sprintf("%6.3f", row$ac2_value))
    
    # Format confidence intervals
    if (is.na(row$ci_lower) || is.na(row$ci_upper)) {
      kappa_ci_str <- "       NA       "
    } else {
      kappa_ci_str <- sprintf("[%5.3f,%5.3f]", row$ci_lower, row$ci_upper)
    }
    
    if (is.na(row$ac2_ci_lower) || is.na(row$ac2_ci_upper)) {
      ac2_ci_str <- "       NA       "
    } else {
      ac2_ci_str <- sprintf("[%5.3f,%5.3f]", row$ac2_ci_lower, row$ac2_ci_upper)
    }
    
    n_pairs_str <- sprintf("%6d", row$n_pairs)
    
    # Print row in specified order: Item | p_o(%) | κ_w | κ_w CI | AC2 | AC2 CI | n_pairs
    cat(sprintf("%-6s %7s %7s %-16s %7s %-16s %8s\n",
                row$item, p_o_str, kappa_str, kappa_ci_str, ac2_str, ac2_ci_str, n_pairs_str))
  }
  
  # Summary statistics
  cat("\nSummary Statistics\n")
  cat("------------------\n")
  
  valid_p_o <- item_results$p_o[!is.na(item_results$p_o)]
  valid_kappa <- item_results$kappa_w[!is.na(item_results$kappa_w)]
  valid_ac2 <- item_results$ac2_value[!is.na(item_results$ac2_value)]
  
  cat(sprintf("Mean p_o:    %.1f%%\n", mean(valid_p_o)))
  cat(sprintf("Mean κ_w:    %.3f\n", mean(valid_kappa)))
  cat(sprintf("Mean AC2:    %.3f\n", mean(valid_ac2)))
}

# Display scale-level results with AC2
ir_display_scale_results <- function(scale_results) {
  cat("\nScale-level Reliability\n")
  cat("------------------------\n")
  cat("Total Score:\n\n")
  
  # ICC results
  cat(sprintf("  ICC(2,1) = %.3f [%.3f, %.3f]  # Single rater\n",
              scale_results$icc_2_1$value,
              scale_results$icc_2_1$ci[1],
              scale_results$icc_2_1$ci[2]))
  
  cat(sprintf("  ICC(2,2) = %.3f [%.3f, %.3f]  # Average of 2 raters\n",
              scale_results$icc_2_2$value,
              scale_results$icc_2_2$ci[1],
              scale_results$icc_2_2$ci[2]))
  
  # Weighted kappa (if available in scale_results)
  if (!is.null(scale_results$kappa_w)) {
    cat(sprintf("  κ_w      = %.3f [%.3f, %.3f]  # Weighted kappa\n",
                scale_results$kappa_w$value,
                scale_results$kappa_w$ci[1],
                scale_results$kappa_w$ci[2]))
  }
  
  # AC2 results
  if (!is.null(scale_results$ac2)) {
    cat(sprintf("  %s      = %.3f [%.3f, %.3f]  # Gwet's %s\n",
                scale_results$ac2$type,
                scale_results$ac2$value,
                scale_results$ac2$ci_lower,
                scale_results$ac2$ci_upper,
                scale_results$ac2$type))
  }
  
  # Bland-Altman
  cat("\nBland-Altman Statistics:\n")
  cat(sprintf("  Mean difference:  %.2f (SD = %.2f)\n", 
              scale_results$bland_altman$mean_diff,
              scale_results$bland_altman$sd_diff))
  cat(sprintf("  95%% Limits:      [%.2f, %.2f]\n",
              scale_results$bland_altman$limits[1],
              scale_results$bland_altman$limits[2]))
  
}