# ===================================================
# Ceiling-Floor Effect Display (View Layer)
# Version: 12.0 - Fixed evaluation display order
# ===================================================

# Unified display function for all ceiling-floor results
display_ceiling_floor_results <- function(results, parameters) {
  
  # Header
  cat("========================================\n")
  cat("Ceiling-Floor Effect Analysis\n")
  cat("========================================\n\n")
  
  # Parameters
  cat(sprintf("Scale range: %d-%d\n", parameters$scale_min, parameters$scale_max))
  cat(sprintf("Items: %d, Cases: %d\n\n", parameters$n_items, parameters$n_cases))
  
  # Format results for display with specified order
  display_df <- data.frame(
    Item = results$item,
    Mean = sprintf("%.2f", results$mean),
    SD = sprintf("%.2f", results$sd),
    Skewness = sprintf("%+.2f", results$skewness),
    Kurtosis = sprintf("%+.2f", results$kurtosis),
    `Mean-SD` = sprintf("%.2f", results$mean_minus_sd),
    `Mean+SD` = sprintf("%.2f", results$mean_plus_sd),
    `Floor(%)` = sprintf("%.1f", results$floor_pct),
    `Ceiling(%)` = sprintf("%.1f", results$ceiling_pct),
    check.names = FALSE
  )
  
  print(display_df, row.names = FALSE)
}

# Evaluate ceiling-floor results against criteria
evaluate_ceiling_floor_results <- function(results, scale_min, scale_max) {
  
  evaluation <- data.frame(
    item = results$item,
    ceiling_flag = case_when(
      results$ceiling_pct >= 25 ~ "25%",
      results$ceiling_pct >= 20 ~ "20%",
      TRUE ~ "-"
    ),
    floor_flag = case_when(
      results$floor_pct >= 25 ~ "25%",
      results$floor_pct >= 20 ~ "20%",
      TRUE ~ "-"
    ),
    skewness_flag = ifelse(abs(results$skewness) > 1.0, "FLAG", "-"),
    kurtosis_flag = ifelse(abs(results$kurtosis) > 2.0, "FLAG", "-"),
    mean_plus_sd_flag = ifelse(results$mean_plus_sd > scale_max, "FLAG", "-"),
    mean_minus_sd_flag = ifelse(results$mean_minus_sd < scale_min, "FLAG", "-"),
    stringsAsFactors = FALSE
  )
  
  evaluation
}

# Display evaluation results
display_ceiling_floor_evaluation <- function(evaluation) {
  
  cat("\n========================================\n")
  cat("Evaluation Results\n")
  cat("========================================\n\n")
  
  # Display evaluation table (same order as results: Skewness, Kurtosis, Mean-SD, Mean+SD, Floor, Ceiling)
  cat(sprintf("%-6s %10s %10s %9s %9s %8s %8s\n",
              "Item", "Skewness", "Kurtosis", "Mean-SD", "Mean+SD", "Floor", "Ceiling"))
  cat(paste(rep("-", 62), collapse = ""), "\n")
  
  for (i in seq_len(nrow(evaluation))) {
    cat(sprintf("%-6s %10s %10s %9s %9s %8s %8s\n",
                evaluation$item[i],
                evaluation$skewness_flag[i],
                evaluation$kurtosis_flag[i],
                evaluation$mean_minus_sd_flag[i],
                evaluation$mean_plus_sd_flag[i],
                evaluation$floor_flag[i],
                evaluation$ceiling_flag[i]))
  }
  
  # Summary (Floor before Ceiling)
  cat("\nSummary:\n")
  
  # Floor effect
  floor_25 <- evaluation$item[evaluation$floor_flag == "25%"]
  floor_20 <- evaluation$item[evaluation$floor_flag == "20%"]
  cat(sprintf("  Floor effect (>=25%%): %s\n", 
              ifelse(length(floor_25) > 0, paste(floor_25, collapse = ", "), "(none)")))
  cat(sprintf("  Floor effect (>=20%%): %s\n", 
              ifelse(length(floor_20) > 0, paste(floor_20, collapse = ", "), "(none)")))
  
  # Ceiling effect
  ceiling_25 <- evaluation$item[evaluation$ceiling_flag == "25%"]
  ceiling_20 <- evaluation$item[evaluation$ceiling_flag == "20%"]
  cat(sprintf("  Ceiling effect (>=25%%): %s\n", 
              ifelse(length(ceiling_25) > 0, paste(ceiling_25, collapse = ", "), "(none)")))
  cat(sprintf("  Ceiling effect (>=20%%): %s\n", 
              ifelse(length(ceiling_20) > 0, paste(ceiling_20, collapse = ", "), "(none)")))
  
  # Skewness
  skew_flag <- evaluation$item[evaluation$skewness_flag == "FLAG"]
  cat(sprintf("  High skewness (|value|>1.0): %s\n", 
              ifelse(length(skew_flag) > 0, paste(skew_flag, collapse = ", "), "(none)")))
  
  # Kurtosis
  kurt_flag <- evaluation$item[evaluation$kurtosis_flag == "FLAG"]
  cat(sprintf("  High kurtosis (|value|>2.0): %s\n", 
              ifelse(length(kurt_flag) > 0, paste(kurt_flag, collapse = ", "), "(none)")))
  
  # Mean-SD / Mean+SD
  minus_flag <- evaluation$item[evaluation$mean_minus_sd_flag == "FLAG"]
  plus_flag <- evaluation$item[evaluation$mean_plus_sd_flag == "FLAG"]
  cat(sprintf("  Mean-SD < scale_min: %s\n", 
              ifelse(length(minus_flag) > 0, paste(minus_flag, collapse = ", "), "(none)")))
  cat(sprintf("  Mean+SD > scale_max: %s\n", 
              ifelse(length(plus_flag) > 0, paste(plus_flag, collapse = ", "), "(none)")))
}