# ===================================================
# Factor Number Display
# Version: 2.0 (Output order: PA -> MAP -> Kaiser)
# Description: Display functions for factor number determination
# ===================================================

# Display factor number results (facts only)
display_factor_number_results <- function(results, n_obs, n_vars) {
  
  cat("\n========================================\n")
  cat("FACTOR NUMBER DETERMINATION\n")
  cat("========================================\n\n")
  
  # Sample info
  cat("Sample Information\n")
  cat("------------------\n")
  cat("  Variables:", n_vars, "\n")
  cat("  Observations:", n_obs, "\n\n")
  
  # Parallel Analysis (FIRST)
  cat("Parallel Analysis (Factor Analysis)\n")
  cat("------------------------------------\n")
  cat("  Suggested:", results$pa$n_factors, "factors\n\n")
  
  pa_df <- results$pa$eigen_table
  cat("  Comparison of Eigenvalues:\n")
  for (i in 1:nrow(pa_df)) {
    marker <- if (pa_df$Retain[i]) " *" else ""
    cat(sprintf("    F%02d: Real=%6.3f | Simulated=%6.3f%s\n",
                pa_df$Factor[i],
                pa_df$Real[i],
                pa_df$Simulated[i],
                marker))
  }
  cat("\n")
  
  # MAP test (SECOND)
  cat("MAP Test (Velicer's MAP)\n")
  cat("------------------------\n")
  cat("  Suggested:", results$map$n_factors, "factors\n\n")
  
  map_df <- results$map$map_table
  cat("  MAP values:\n")
  for (i in 1:nrow(map_df)) {
    marker <- if (map_df$Factors[i] == results$map$n_factors) " *" else ""
    cat(sprintf("    %d factors: %.6f%s\n", 
                map_df$Factors[i], 
                map_df$MAP[i],
                marker))
  }
  cat("\n")
  
  # Kaiser's criterion (THIRD)
  cat("Kaiser's Criterion\n")
  cat("------------------\n")
  cat("  Eigenvalues > 1:", results$kaiser$n_factors, "\n\n")
  
  eigen_df <- results$kaiser$eigen_table
  cat("  Eigenvalues:\n")
  cumvar <- cumsum(eigen_df$Eigenvalue) / sum(eigen_df$Eigenvalue) * 100
  for (i in 1:nrow(eigen_df)) {
    cat(sprintf("    F%02d: %6.3f (cum: %5.1f%%)", 
                i, 
                eigen_df$Eigenvalue[i], 
                cumvar[i]))
    if (eigen_df$Eigenvalue[i] > 1) cat(" *")
    cat("\n")
  }
  cat("\n")
  
  # Summary
  cat("========================================\n")
  cat("SUMMARY\n")
  cat("========================================\n")
  cat("Parallel Analysis (FA):  ", results$pa$n_factors, "factors\n")
  cat("MAP test:                ", results$map$n_factors, "factors\n")
  cat("Kaiser's criterion:      ", results$kaiser$n_factors, "factors\n")
  cat("========================================\n")
  
  invisible(NULL)
}

# Show evaluation and suggestions
show_factor_number_evaluation <- function(results) {
  
  cat("\n========================================\n")
  cat("FACTOR NUMBER EVALUATION\n")
  cat("========================================\n\n")
  
  kaiser_n <- results$kaiser$n_factors
  pa_n <- results$pa$n_factors
  map_n <- results$map$n_factors
  
  # Agreement check
  all_values <- c(pa_n, map_n, kaiser_n)
  unique_values <- unique(all_values)
  
  cat("Method Agreement\n")
  cat("----------------\n")
  
  if (length(unique_values) == 1) {
    cat("  All three methods agree:", unique_values[1], "factors\n")
  } else if (length(unique_values) == 2) {
    mode_val <- as.numeric(names(sort(table(all_values), decreasing = TRUE)[1]))
    cat("  Two methods agree:", mode_val, "factors\n")
    cat("  Range:", min(all_values), "-", max(all_values), "\n")
  } else {
    cat("  No agreement among methods\n")
    cat("  Range:", min(all_values), "-", max(all_values), "\n")
  }
  
  cat("\n")
  cat("Method Characteristics\n")
  cat("----------------------\n")
  cat("  Parallel Analysis: Recommended for ordinal data\n")
  cat("  MAP: Minimizes partial correlations\n")
  cat("  Kaiser: Tends to overextract\n")
  cat("========================================\n")
  
  invisible(NULL)
}