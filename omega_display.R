# ===================================================
# McDonald's Omega Display (View Layer)
# Version: 3.0 - display improvements
# Changes from v2.0:
#   - Sample size -> Complete cases
#   - nfactors -> Group factors specified
#   - Cronbach's alpha (ref) -> Cronbach's alpha (for comparison)
# ===================================================

# Display header
omega_display_header <- function() {
  cat("========================================\n")
  cat("McDonald's omega Analysis\n")
  cat("========================================\n\n")
}

# Display overall omega results
omega_display_overall <- function(overall_results) {
  cat("OVERALL RELIABILITY (McDonald's omega)\n")
  cat("----------------------------------------\n")
  cat(sprintf("Complete cases:          %d / %d (%.1f%%)\n", 
              overall_results$n_cases,
              overall_results$n_total_cases,
              overall_results$percent_complete))
  cat(sprintf("Number of items:         %d\n", overall_results$n_items))
  cat(sprintf("Group factors specified: %d\n", overall_results$nfactors))
  cat(sprintf("McDonald's omega_total:  %.3f\n", overall_results$omega_total))
  if (!is.na(overall_results$omega_hierarchical)) {
    cat(sprintf("McDonald's omega_h:      %.3f\n", overall_results$omega_hierarchical))
  }
  cat(sprintf("Cronbach's alpha (for comparison): %.3f\n", overall_results$alpha))
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