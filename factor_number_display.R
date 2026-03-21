# ===================================================
# Factor Number Display
# Description: Display functions for factor number determination
# ===================================================

# Display factor number results (facts only)
display_factor_number_results <- function(results, n_obs, n_vars) {
  
  cat("\n========================================\n")
  cat("FACTOR NUMBER DETERMINATION\n")
  cat("========================================\n\n")
  
  # Calculation conditions
  cond <- results$conditions
  cat("Calculation Conditions\n")
  cat("----------------------\n")
  cat("  Correlation method:        ", cond$correlation_method, "\n")
  cat("  Extraction method (MRFA):  ", cond$extraction_method_pa_mrfa, "\n")
  cat("  Extraction method (PA):    ", cond$extraction_method_pa, "\n")
  cat("  PA-MRFA datasets:          ", cond$pa_mrfa_datasets, "\n")
  cat("  PA-MRFA random method:     ", cond$pa_mrfa_random_method, "\n")
  cat("  PA iterations:             ", cond$pa_iterations, "\n")
  cat("  PA percentile:             ", cond$pa_percentile, "\n")
  if (!is.null(cond$pa_fa_reference)) {
    cat("  PA FA reference:           ", cond$pa_fa_reference, "\n")
  }
  if (!is.null(cond$pa_pca_reference)) {
    cat("  PA PCA reference:          ", cond$pa_pca_reference, "\n")
  }
  if (!is.null(cond$missing_method)) {
    cat("  Missing data handling:     ", cond$missing_method, "\n")
  }
  if (!is.null(cond$scale_min) && !is.null(cond$scale_max)) {
    cat("  Scale range:               ", cond$scale_min, "-", cond$scale_max, "\n")
  }
  cat("  Kaiser eigenvalue source:  ", cond$kaiser_eigenvalue_source, "\n")
  cat("\n")
  
  # Sample info
  cat("Sample Information\n")
  cat("------------------\n")
  cat("  Variables:", n_vars, "\n")
  cat("  Observations:", n_obs, "\n\n")
  
  # ========================================
  # Parallel Analysis - MRFA (Polychoric)
  # ========================================
  if (!is.null(results$pa_mrfa)) {
    cat("Parallel Analysis (MRFA) - Polychoric\n")
    cat("--------------------------------------\n")
    cat("  Suggested:", results$pa_mrfa$n_factors, "factors\n")
    if (!is.null(results$pa_mrfa$n_factors_mean)) {
      cat("  (Mean reference):", results$pa_mrfa$n_factors_mean, "factors\n")
    }
    cat("\n")
    
    mrfa_df <- results$pa_mrfa$eigen_table
    cat("  Comparison of % Explained Common Variance:\n")
    for (i in 1:nrow(mrfa_df)) {
      marker <- if (mrfa_df$Retain[i]) " *" else ""
      cat(sprintf("    F%02d: Real=%6.3f%% | Percentile=%6.3f%% | Mean=%6.3f%%%s\n",
                  mrfa_df$Factor[i],
                  mrfa_df$Real_Pct[i],
                  mrfa_df$Simulated_Pct[i],
                  mrfa_df$Simulated_Mean_Pct[i],
                  marker))
    }
    cat("\n")
  }
  
  # ========================================
  # Parallel Analysis - MRFA (Pearson)
  # ========================================
  if (!is.null(results$pa_mrfa_pearson)) {
    cat("Parallel Analysis (MRFA) - Pearson\n")
    cat("-----------------------------------\n")
    cat("  Suggested:", results$pa_mrfa_pearson$n_factors, "factors\n")
    if (!is.null(results$pa_mrfa_pearson$n_factors_mean)) {
      cat("  (Mean reference):", results$pa_mrfa_pearson$n_factors_mean, "factors\n")
    }
    cat("\n")
    
    mrfa_pear_df <- results$pa_mrfa_pearson$eigen_table
    cat("  Comparison of % Explained Common Variance:\n")
    for (i in 1:nrow(mrfa_pear_df)) {
      marker <- if (mrfa_pear_df$Retain[i]) " *" else ""
      cat(sprintf("    F%02d: Real=%6.3f%% | Percentile=%6.3f%% | Mean=%6.3f%%%s\n",
                  mrfa_pear_df$Factor[i],
                  mrfa_pear_df$Real_Pct[i],
                  mrfa_pear_df$Simulated_Pct[i],
                  mrfa_pear_df$Simulated_Mean_Pct[i],
                  marker))
    }
    cat("\n")
  }
  
  # ========================================
  # Parallel Analysis - FA (Polychoric)
  # ========================================
  cat("Parallel Analysis (Factor Analysis) - Polychoric\n")
  cat("--------------------------------------------------\n")
  cat("  Suggested:", results$pa$n_factors, "factors\n")
  if (!is.null(results$pa$n_factors_mean)) {
    cat("  (Mean reference):", results$pa$n_factors_mean, "factors\n")
  }
  cat("\n")
  
  pa_df <- results$pa$eigen_table
  cat("  Comparison of Eigenvalues:\n")
  for (i in 1:nrow(pa_df)) {
    marker <- if (pa_df$Retain[i]) " *" else ""
    if (!is.null(pa_df$Simulated_mean)) {
      cat(sprintf("    F%02d: Real=%6.3f | Simulated=%6.3f | Mean=%6.3f%s\n",
                  pa_df$Factor[i],
                  pa_df$Real[i],
                  pa_df$Simulated[i],
                  pa_df$Simulated_mean[i],
                  marker))
    } else {
      cat(sprintf("    F%02d: Real=%6.3f | Simulated=%6.3f%s\n",
                  pa_df$Factor[i],
                  pa_df$Real[i],
                  pa_df$Simulated[i],
                  marker))
    }
  }
  cat("\n")
  
  # ========================================
  # Parallel Analysis - FA (Pearson)
  # ========================================
  if (!is.null(results$pa_pearson)) {
    cat("Parallel Analysis (Factor Analysis) - Pearson\n")
    cat("-----------------------------------------------\n")
    cat("  Suggested:", results$pa_pearson$n_factors, "factors\n")
    if (!is.null(results$pa_pearson$n_factors_mean)) {
      cat("  (Mean reference):", results$pa_pearson$n_factors_mean, "factors\n")
    }
    cat("\n")
    
    pa_pear_df <- results$pa_pearson$eigen_table
    cat("  Comparison of Eigenvalues:\n")
    for (i in 1:nrow(pa_pear_df)) {
      marker <- if (pa_pear_df$Retain[i]) " *" else ""
      if (!is.null(pa_pear_df$Simulated_mean)) {
        cat(sprintf("    F%02d: Real=%6.3f | Simulated=%6.3f | Mean=%6.3f%s\n",
                    pa_pear_df$Factor[i],
                    pa_pear_df$Real[i],
                    pa_pear_df$Simulated[i],
                    pa_pear_df$Simulated_mean[i],
                    marker))
      } else {
        cat(sprintf("    F%02d: Real=%6.3f | Simulated=%6.3f%s\n",
                    pa_pear_df$Factor[i],
                    pa_pear_df$Real[i],
                    pa_pear_df$Simulated[i],
                    marker))
      }
    }
    cat("\n")
  }
  
  # ========================================
  # Parallel Analysis - PCA (Polychoric)
  # ========================================
  if (!is.null(results$pa_pca)) {
    cat("Parallel Analysis (PCA) - Polychoric\n")
    cat("--------------------------------------\n")
    cat("  Suggested:", results$pa_pca$n_factors, "components\n")
    if (!is.null(results$pa_pca$n_factors_mean)) {
      cat("  (Mean reference):", results$pa_pca$n_factors_mean, "components\n")
    }
    cat("\n")
    
    pca_df <- results$pa_pca$eigen_table
    cat("  Comparison of Eigenvalues:\n")
    for (i in 1:nrow(pca_df)) {
      marker <- if (pca_df$Retain[i]) " *" else ""
      if (!is.null(pca_df$Simulated_mean)) {
        cat(sprintf("    C%02d: Real=%6.3f | Simulated=%6.3f | Mean=%6.3f%s\n",
                    pca_df$Component[i],
                    pca_df$Real[i],
                    pca_df$Simulated[i],
                    pca_df$Simulated_mean[i],
                    marker))
      } else {
        cat(sprintf("    C%02d: Real=%6.3f | Simulated=%6.3f%s\n",
                    pca_df$Component[i],
                    pca_df$Real[i],
                    pca_df$Simulated[i],
                    marker))
      }
    }
    cat("\n")
  }
  
  # ========================================
  # Parallel Analysis - PCA (Pearson)
  # ========================================
  if (!is.null(results$pa_pca_pearson)) {
    cat("Parallel Analysis (PCA) - Pearson\n")
    cat("-----------------------------------\n")
    cat("  Suggested:", results$pa_pca_pearson$n_factors, "components\n")
    if (!is.null(results$pa_pca_pearson$n_factors_mean)) {
      cat("  (Mean reference):", results$pa_pca_pearson$n_factors_mean, "components\n")
    }
    cat("\n")
    
    pca_pear_df <- results$pa_pca_pearson$eigen_table
    cat("  Comparison of Eigenvalues:\n")
    for (i in 1:nrow(pca_pear_df)) {
      marker <- if (pca_pear_df$Retain[i]) " *" else ""
      if (!is.null(pca_pear_df$Simulated_mean)) {
        cat(sprintf("    C%02d: Real=%6.3f | Simulated=%6.3f | Mean=%6.3f%s\n",
                    pca_pear_df$Component[i],
                    pca_pear_df$Real[i],
                    pca_pear_df$Simulated[i],
                    pca_pear_df$Simulated_mean[i],
                    marker))
      } else {
        cat(sprintf("    C%02d: Real=%6.3f | Simulated=%6.3f%s\n",
                    pca_pear_df$Component[i],
                    pca_pear_df$Real[i],
                    pca_pear_df$Simulated[i],
                    marker))
      }
    }
    cat("\n")
  }
  
  # ========================================
  # MAP test (Polychoric)
  # ========================================
  cat("MAP Test (Velicer's MAP) - Polychoric\n")
  cat("--------------------------------------\n")
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
  
  # ========================================
  # MAP test (Pearson)
  # ========================================
  if (!is.null(results$map_pearson)) {
    cat("MAP Test (Velicer's MAP) - Pearson\n")
    cat("------------------------------------\n")
    cat("  Suggested:", results$map_pearson$n_factors, "factors\n\n")
    
    map_pear_df <- results$map_pearson$map_table
    cat("  MAP values:\n")
    for (i in 1:nrow(map_pear_df)) {
      marker <- if (map_pear_df$Factors[i] == results$map_pearson$n_factors) " *" else ""
      cat(sprintf("    %d factors: %.6f%s\n", 
                  map_pear_df$Factors[i], 
                  map_pear_df$MAP[i],
                  marker))
    }
    cat("\n")
  }
  
  # ========================================
  # Kaiser's criterion (Polychoric)
  # ========================================
  cat("Kaiser's Criterion - Polychoric\n")
  cat("--------------------------------\n")
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
  
  # ========================================
  # Kaiser's criterion (Pearson)
  # ========================================
  if (!is.null(results$kaiser_pearson)) {
    cat("Kaiser's Criterion - Pearson\n")
    cat("------------------------------\n")
    cat("  Eigenvalues > 1:", results$kaiser_pearson$n_factors, "\n\n")
    
    eigen_pear_df <- results$kaiser_pearson$eigen_table
    cat("  Eigenvalues:\n")
    cumvar_pear <- cumsum(eigen_pear_df$Eigenvalue) / sum(eigen_pear_df$Eigenvalue) * 100
    for (i in 1:nrow(eigen_pear_df)) {
      cat(sprintf("    F%02d: %6.3f (cum: %5.1f%%)", 
                  i, 
                  eigen_pear_df$Eigenvalue[i], 
                  cumvar_pear[i]))
      if (eigen_pear_df$Eigenvalue[i] > 1) cat(" *")
      cat("\n")
    }
    cat("\n")
  }
  
  # ========================================
  # Summary
  # ========================================
  cat("========================================\n")
  cat("SUMMARY\n")
  cat("========================================\n")
  
  # Header
  cat(sprintf("%-30s %12s %9s\n", "", "Polychoric", "Pearson"))
  
  # PA MRFA
  if (!is.null(results$pa_mrfa)) {
    poly_val <- results$pa_mrfa$n_factors
    pear_val <- if (!is.null(results$pa_mrfa_pearson)) results$pa_mrfa_pearson$n_factors else NA
    cat(sprintf("%-30s %10d %9d\n", "PA (MRFA):", poly_val, pear_val))
  }
  if (!is.null(results$pa_mrfa) && !is.null(results$pa_mrfa$n_factors_mean)) {
    poly_val <- results$pa_mrfa$n_factors_mean
    pear_val <- if (!is.null(results$pa_mrfa_pearson)) results$pa_mrfa_pearson$n_factors_mean else NA
    cat(sprintf("%-30s %10d %9d\n", "PA MRFA (mean reference):", poly_val, pear_val))
  }
  
  # PA FA
  poly_val <- results$pa$n_factors
  pear_val <- if (!is.null(results$pa_pearson)) results$pa_pearson$n_factors else NA
  cat(sprintf("%-30s %10d %9d\n", "PA (FA):", poly_val, pear_val))
  
  if (!is.null(results$pa$n_factors_mean)) {
    poly_val <- results$pa$n_factors_mean
    pear_val <- if (!is.null(results$pa_pearson)) results$pa_pearson$n_factors_mean else NA
    cat(sprintf("%-30s %10d %9d\n", "PA FA (mean reference):", poly_val, pear_val))
  }
  
  # PA PCA
  if (!is.null(results$pa_pca)) {
    poly_val <- results$pa_pca$n_factors
    pear_val <- if (!is.null(results$pa_pca_pearson)) results$pa_pca_pearson$n_factors else NA
    cat(sprintf("%-30s %10d %9d\n", "PA (PCA):", poly_val, pear_val))
  }
  if (!is.null(results$pa_pca) && !is.null(results$pa_pca$n_factors_mean)) {
    poly_val <- results$pa_pca$n_factors_mean
    pear_val <- if (!is.null(results$pa_pca_pearson)) results$pa_pca_pearson$n_factors_mean else NA
    cat(sprintf("%-30s %10d %9d\n", "PA PCA (mean reference):", poly_val, pear_val))
  }
  
  # MAP
  poly_val <- results$map$n_factors
  pear_val <- if (!is.null(results$map_pearson)) results$map_pearson$n_factors else NA
  cat(sprintf("%-30s %10d %9d\n", "MAP test:", poly_val, pear_val))
  
  # Kaiser
  poly_val <- results$kaiser$n_factors
  pear_val <- if (!is.null(results$kaiser_pearson)) results$kaiser_pearson$n_factors else NA
  cat(sprintf("%-30s %10d %9d\n", "Kaiser's criterion:", poly_val, pear_val))
  
  cat("========================================\n")
  
  invisible(NULL)
}

# Show evaluation and suggestions
show_factor_number_evaluation <- function(results) {
  
  cat("\n========================================\n")
  cat("FACTOR NUMBER EVALUATION\n")
  cat("========================================\n\n")
  
  # --- Polychoric ---
  kaiser_n <- results$kaiser$n_factors
  pa_n <- results$pa$n_factors
  map_n <- results$map$n_factors
  pa_mean_n <- results$pa$n_factors_mean
  pa_pca_n <- if (!is.null(results$pa_pca)) results$pa_pca$n_factors else NULL
  pa_pca_mean_n <- if (!is.null(results$pa_pca)) results$pa_pca$n_factors_mean else NULL
  pa_mrfa_n <- if (!is.null(results$pa_mrfa)) results$pa_mrfa$n_factors else NULL
  pa_mrfa_mean_n <- if (!is.null(results$pa_mrfa)) results$pa_mrfa$n_factors_mean else NULL
  
  # Agreement check (PA MRFA quantile, PA FA quantile, PA PCA quantile, MAP, Kaiser)
  all_values <- c(pa_n, map_n, kaiser_n)
  if (!is.null(pa_mrfa_n)) {
    all_values <- c(all_values, pa_mrfa_n)
  }
  if (!is.null(pa_pca_n)) {
    all_values <- c(all_values, pa_pca_n)
  }
  unique_values <- unique(all_values)
  
  cat("Method Agreement - Polychoric\n")
  cat("------------------------------\n")
  
  if (length(unique_values) == 1) {
    cat("  All methods agree:", unique_values[1], "factors\n")
  } else {
    mode_val <- as.numeric(names(sort(table(all_values), decreasing = TRUE)[1]))
    agree_count <- max(table(all_values))
    cat("  Most agreed:", mode_val, "factors (", agree_count, "methods)\n")
    cat("  Range:", min(all_values), "-", max(all_values), "\n")
  }
  
  if (!is.null(pa_mrfa_mean_n)) {
    cat("  PA MRFA (mean reference):", pa_mrfa_mean_n, "factors\n")
  }
  if (!is.null(pa_mean_n)) {
    cat("  PA FA (mean reference):", pa_mean_n, "factors\n")
  }
  if (!is.null(pa_pca_mean_n)) {
    cat("  PA PCA (mean reference):", pa_pca_mean_n, "components\n")
  }
  
  cat("\n")
  
  # --- Pearson ---
  kaiser_n_pear <- if (!is.null(results$kaiser_pearson)) results$kaiser_pearson$n_factors else NULL
  pa_n_pear <- if (!is.null(results$pa_pearson)) results$pa_pearson$n_factors else NULL
  map_n_pear <- if (!is.null(results$map_pearson)) results$map_pearson$n_factors else NULL
  pa_mean_n_pear <- if (!is.null(results$pa_pearson)) results$pa_pearson$n_factors_mean else NULL
  pa_pca_n_pear <- if (!is.null(results$pa_pca_pearson)) results$pa_pca_pearson$n_factors else NULL
  pa_pca_mean_n_pear <- if (!is.null(results$pa_pca_pearson)) results$pa_pca_pearson$n_factors_mean else NULL
  pa_mrfa_n_pear <- if (!is.null(results$pa_mrfa_pearson)) results$pa_mrfa_pearson$n_factors else NULL
  pa_mrfa_mean_n_pear <- if (!is.null(results$pa_mrfa_pearson)) results$pa_mrfa_pearson$n_factors_mean else NULL
  
  if (!is.null(pa_n_pear) && !is.null(map_n_pear) && !is.null(kaiser_n_pear)) {
    all_values_pear <- c(pa_n_pear, map_n_pear, kaiser_n_pear)
    if (!is.null(pa_mrfa_n_pear)) {
      all_values_pear <- c(all_values_pear, pa_mrfa_n_pear)
    }
    if (!is.null(pa_pca_n_pear)) {
      all_values_pear <- c(all_values_pear, pa_pca_n_pear)
    }
    unique_values_pear <- unique(all_values_pear)
    
    cat("Method Agreement - Pearson\n")
    cat("---------------------------\n")
    
    if (length(unique_values_pear) == 1) {
      cat("  All methods agree:", unique_values_pear[1], "factors\n")
    } else {
      mode_val_pear <- as.numeric(names(sort(table(all_values_pear), decreasing = TRUE)[1]))
      agree_count_pear <- max(table(all_values_pear))
      cat("  Most agreed:", mode_val_pear, "factors (", agree_count_pear, "methods)\n")
      cat("  Range:", min(all_values_pear), "-", max(all_values_pear), "\n")
    }
    
    if (!is.null(pa_mrfa_mean_n_pear)) {
      cat("  PA MRFA (mean reference):", pa_mrfa_mean_n_pear, "factors\n")
    }
    if (!is.null(pa_mean_n_pear)) {
      cat("  PA FA (mean reference):", pa_mean_n_pear, "factors\n")
    }
    if (!is.null(pa_pca_mean_n_pear)) {
      cat("  PA PCA (mean reference):", pa_pca_mean_n_pear, "components\n")
    }
    
    cat("\n")
  }
  
  cat("Method Characteristics\n")
  cat("----------------------\n")
  cat("  Parallel Analysis (MRFA): MRFA extraction, % common variance comparison\n")
  cat("  Parallel Analysis (FA):   minres extraction, eigenvalue comparison\n")
  cat("  Parallel Analysis (PCA):  Component-based comparison\n")
  cat("  MAP: Minimizes partial correlations\n")
  cat("  Kaiser: Tends to overextract\n")
  cat("========================================\n")
  
  invisible(NULL)
}