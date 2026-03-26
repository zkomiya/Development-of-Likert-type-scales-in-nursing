# ===================================================
# Validity Display (View Layer)
# Version: 6.0 - No load_config() in display
# Changes from v5.0:
#   - Removed load_config() calls
#   - Subscale config received as arguments from controller
# ===================================================

# Display header
validity_display_header <- function() {
  cat("========================================\n")
  cat("REHAB External Validity Analysis\n")
  cat("========================================\n\n")
}

# Total score level display
validity_display_total <- function(results) {
  cat("=== Total Score Correlations ===\n")
  cat(sprintf("Sample size: n = %d\n\n", results$total_level$gb$n))
  
  cat("              r        95% CI           Power\n")
  cat("-------------------------------------------------------\n")
  
  # GB
  cat(sprintf("REHAB-GB   %6.3f   [%6.3f, %6.3f]   %5.3f\n",
              results$total_level$gb$r,
              results$total_level$gb$ci_lower,
              results$total_level$gb$ci_upper,
              results$total_level$gb$power))
  
  # DB
  cat(sprintf("REHAB-DB   %6.3f   [%6.3f, %6.3f]   %5.3f\n",
              results$total_level$db$r,
              results$total_level$db$ci_lower,
              results$total_level$db$ci_upper,
              results$total_level$db$power))
  
  cat("\nNote: Pearson correlation (appropriate for sum scores)\n")
}

# Subscale level display
validity_display_subscales <- function(results, target_subscales) {
  cat("\n=== Subscale Correlations ===\n\n")
  
  # Get subscale names
  subscale_names <- names(results$subscale_level$gb)
  
  cat("Correlations with REHAB-GB:\n")
  cat("                                           r        95% CI           Power\n")
  cat("--------------------------------------------------------------------------------\n")
  
  for (subscale in subscale_names) {
    # Get name from subscale config
    display_name <- target_subscales[[subscale]]$name
    if (nchar(display_name) > 40) {
      display_name <- paste0(substr(display_name, 1, 37), "...")
    }
    
    result <- results$subscale_level$gb[[subscale]]
    cat(sprintf("%-40s  %6.3f   [%6.3f, %6.3f]   %5.3f\n",
                display_name,
                result$r,
                result$ci_lower,
                result$ci_upper,
                result$power))
  }
  
  cat("\nCorrelations with REHAB-DB:\n")
  cat("                                           r        95% CI           Power\n")
  cat("--------------------------------------------------------------------------------\n")
  
  for (subscale in subscale_names) {
    # Get name from subscale config
    display_name <- target_subscales[[subscale]]$name
    if (nchar(display_name) > 40) {
      display_name <- paste0(substr(display_name, 1, 37), "...")
    }
    
    result <- results$subscale_level$db[[subscale]]
    cat(sprintf("%-40s  %6.3f   [%6.3f, %6.3f]   %5.3f\n",
                display_name,
                result$r,
                result$ci_lower,
                result$ci_upper,
                result$power))
  }
}

# Subscale x subscale matrix display
validity_display_subscale_matrix <- function(results, target_subscales, rehab_gb_subscales) {
  cat("\n=== 30items Subscales x REHAB GB Subscales Correlation Matrix ===\n\n")
  
  # Get subscale names
  target_names <- names(results$subscale_x_subscale)
  gb_names <- names(results$subscale_x_subscale[[target_names[1]]])
  
  # Pearson correlation matrix
  cat("Pearson Correlations (r):\n")
  cat(sprintf("%-45s", ""))
  for (gb_name in gb_names) {
    display_name <- rehab_gb_subscales[[gb_name]]$name
    if (nchar(display_name) > 14) {
      display_name <- substr(display_name, 1, 14)
    }
    cat(sprintf("%16s", display_name))
  }
  cat("\n")
  cat(paste(rep("-", 45 + 16 * length(gb_names)), collapse=""), "\n")
  
  for (target_name in target_names) {
    display_name <- target_subscales[[target_name]]$name
    if (nchar(display_name) > 43) {
      display_name <- paste0(substr(display_name, 1, 40), "...")
    }
    cat(sprintf("%-45s", display_name))
    
    for (gb_name in gb_names) {
      r <- results$subscale_x_subscale[[target_name]][[gb_name]]$r
      cat(sprintf("%16.3f", r))
    }
    cat("\n")
  }
  
  cat("\nNote: All correlations are Pearson (appropriate for sum scores)\n")
}