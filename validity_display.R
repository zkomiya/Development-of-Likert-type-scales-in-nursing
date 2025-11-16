# ===================================================
# Validity Display (View Layer)
# Version: 5.0 - Pearson only (statistically appropriate)
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
validity_display_subscales <- function(results) {
  cat("\n=== Subscale Correlations ===\n\n")
  
  # Load config to get proper names from YAML
  config <- load_config()
  subscale_config <- config$analysis$item_total_analysis
  
  # Get subscale names
  subscale_names <- names(results$subscale_level$gb)
  
  cat("Correlations with REHAB-GB:\n")
  cat("                                           r        95% CI           Power\n")
  cat("--------------------------------------------------------------------------------\n")
  
  for (subscale in subscale_names) {
    # Get name from YAML config
    display_name <- subscale_config[[subscale]]$name
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
    # Get name from YAML config
    display_name <- subscale_config[[subscale]]$name
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

# Subscale × subscale matrix display
validity_display_subscale_matrix <- function(results) {
  cat("\n=== 30items Subscales × REHAB GB Subscales Correlation Matrix ===\n\n")
  
  # Load configs for names
  config <- load_config()
  items30_config <- config$analysis$item_total_analysis
  gb_config <- config$analysis$rehab_subscales$gb_subscales
  
  # Get subscale names
  items30_names <- names(results$subscale_x_subscale)
  gb_names <- names(results$subscale_x_subscale[[items30_names[1]]])
  
  # Pearson correlation matrix
  cat("Pearson Correlations (r):\n")
  cat(sprintf("%-45s", ""))
  for (gb_name in gb_names) {
    display_name <- gb_config[[gb_name]]$name
    if (nchar(display_name) > 14) {
      display_name <- substr(display_name, 1, 14)
    }
    cat(sprintf("%16s", display_name))
  }
  cat("\n")
  cat(paste(rep("-", 45 + 16 * length(gb_names)), collapse=""), "\n")
  
  for (items30_name in items30_names) {
    display_name <- items30_config[[items30_name]]$name
    if (nchar(display_name) > 43) {
      display_name <- paste0(substr(display_name, 1, 40), "...")
    }
    cat(sprintf("%-45s", display_name))
    
    for (gb_name in gb_names) {
      r <- results$subscale_x_subscale[[items30_name]][[gb_name]]$r
      cat(sprintf("%16.3f", r))
    }
    cat("\n")
  }
  
  cat("\nNote: All correlations are Pearson (appropriate for sum scores)\n")
}