# ===================================================
# McDonald's Omega Display (View Layer)
# Version: 2.0 - omega_h NA handling
# Changes from v1.0:
#   - omega_h displayed only when not NA
#   - nfactors displayed in overall output
# ===================================================

# Display header
omega_display_header <- function() {
  cat("========================================\n")
  cat("McDonald's Omega Analysis\n")
  cat("========================================\n\n")
}

# Display overall omega results
omega_display_overall <- function(overall_results) {
  cat("OVERALL RELIABILITY (McDonald's omega)\n")
  cat("----------------------------------------\n")
  cat(sprintf("Sample size:             %d / %d (%.1f%% complete)\n", 
              overall_results$n_cases,
              overall_results$n_total_cases,
              overall_results$percent_complete))
  cat(sprintf("Number of items:         %d\n", overall_results$n_items))
  cat(sprintf("nfactors:                %d\n", overall_results$nfactors))
  cat(sprintf("omega_total:             %.3f\n", overall_results$omega_total))
  if (!is.na(overall_results$omega_hierarchical)) {
    cat(sprintf("omega_hierarchical:      %.3f\n", overall_results$omega_hierarchical))
  }
  cat(sprintf("Cronbach's alpha (ref):  %.3f\n", overall_results$alpha))
}

# Display subscale omega results
omega_display_subscales <- function(subscale_results) {
  if (is.null(subscale_results) || length(subscale_results) == 0) {
    return()
  }
  
  cat("\n========================================\n")
  cat("SUBSCALE RELIABILITY (McDonald's omega)\n")
  cat("========================================\n\n")
  
  # Create summary table
  subscale_names <- character()
  n_items <- numeric()
  omega_totals <- numeric()
  alphas <- numeric()
  
  for (subscale_id in names(subscale_results)) {
    subscale <- subscale_results[[subscale_id]]
    subscale_names <- c(subscale_names, subscale$name)
    n_items <- c(n_items, subscale$n_items)
    omega_totals <- c(omega_totals, subscale$omega_total)
    alphas <- c(alphas, subscale$alpha)
  }
  
  display_df <- data.frame(
    Subscale = subscale_names,
    Items = n_items,
    omega_total = sprintf("%.3f", omega_totals),
    alpha = sprintf("%.3f", alphas),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  print(display_df, row.names = FALSE)
}