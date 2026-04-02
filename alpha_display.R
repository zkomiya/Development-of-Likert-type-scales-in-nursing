# ===================================================
# Cronbach's Alpha Display (View Layer)
# Version: 2.0 - display improvements
# Changes from v1.0:
#   - Sample size -> Complete cases
#   - Cronbach's Alpha -> Cronbach's alpha (lowercase)
#   - Added delta_alpha column to if_deleted display
# ===================================================

# Display header
alpha_display_header <- function() {
  cat("========================================\n")
  cat("Cronbach's alpha Analysis\n")
  cat("========================================\n\n")
}

# Display overall alpha results
alpha_display_overall <- function(overall_results) {
  cat("OVERALL RELIABILITY\n")
  cat("----------------------------------------\n")
  cat(sprintf("Complete cases:          %d / %d (%.1f%%)\n", 
              overall_results$n_cases,
              overall_results$n_total_cases,
              overall_results$percent_complete))
  cat(sprintf("Number of items:         %d\n", overall_results$n_items))
  cat(sprintf("Cronbach's alpha (raw):  %.3f\n", overall_results$raw_alpha))
  cat(sprintf("Cronbach's alpha (std):  %.3f\n", overall_results$std_alpha))
}

# Display alpha if deleted results
alpha_display_if_deleted <- function(if_deleted_results) {
  cat("\n========================================\n")
  cat("RELIABILITY IF ITEM DELETED\n")
  cat("========================================\n\n")
  
  # Format for display
  display_df <- data.frame(
    Item = if_deleted_results$item,
    `alpha if deleted` = sprintf("%.3f", if_deleted_results$raw_alpha_if_deleted),
    `delta alpha` = sprintf("%+.3f", if_deleted_results$delta_alpha),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  print(display_df, row.names = FALSE)
}

# Display subscale results
alpha_display_subscales <- function(subscale_results) {
  if (is.null(subscale_results) || length(subscale_results) == 0) {
    return()
  }
  
  cat("\n========================================\n")
  cat("SUBSCALE RELIABILITY\n")
  cat("========================================\n\n")
  
  # Create summary table
  subscale_names <- character()
  n_items <- numeric()
  raw_alphas <- numeric()
  std_alphas <- numeric()
  
  for (subscale_id in names(subscale_results)) {
    subscale <- subscale_results[[subscale_id]]
    subscale_names <- c(subscale_names, subscale$name)
    n_items <- c(n_items, subscale$n_items)
    raw_alphas <- c(raw_alphas, subscale$raw_alpha)
    std_alphas <- c(std_alphas, subscale$std_alpha)
  }
  
  display_df <- data.frame(
    Subscale = subscale_names,
    Items = n_items,
    `alpha (raw)` = sprintf("%.3f", raw_alphas),
    `alpha (std)` = sprintf("%.3f", std_alphas),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  print(display_df, row.names = FALSE)
}