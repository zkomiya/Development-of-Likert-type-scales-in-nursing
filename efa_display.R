# ===================================================
# EFA Display
# Version: 6.0 - Single extraction method structure
# Description: Display EFA results only
# Changes from v5.2:
#   - Updated to single extraction method result structure
#   - Removed method parameter from display functions
# ===================================================

# Display pattern matrix
display_pattern_matrix <- function(pattern) {
  
  p <- nrow(pattern)
  m <- ncol(pattern)
  
  if (is.null(colnames(pattern))) {
    colnames(pattern) <- paste0("F", 1:m)
  }
  if (is.null(rownames(pattern))) {
    rownames(pattern) <- paste0("Q", sprintf("%02d", 1:p))
  }
  
  pattern_display <- round(pattern, 3)
  return(pattern_display)
}

# Display specific gamma result
display_specific_result <- function(results, gamma) {
  
  gamma_key <- paste0("gamma_", gsub("-", "neg", as.character(gamma)))
  
  poly_efa <- results$polychoric$efa
  
  if (!gamma_key %in% names(poly_efa$rotations)) {
    stop("Gamma ", gamma, " not found")
  }
  
  cat("\nRESULTS FOR: gamma =", gamma, "\n")
  cat("----------------------------------------\n")
  
  ext_result <- poly_efa$extraction
  rot_result <- poly_efa$rotations[[gamma_key]]
  
  cat("\nPattern Matrix:\n")
  print(round(rot_result$pattern, 3))
  
  cat("\nStructure Matrix:\n")
  print(round(rot_result$structure, 3))
  
  cat("\nFactor Correlation Matrix:\n")
  print(round(rot_result$factor_correlation, 3))
  
  cat("\nCommunalities:\n")
  comm_df <- data.frame(
    Variable = names(ext_result$communalities),
    Communality = round(ext_result$communalities, 3)
  )
  print(comm_df)
  
  ss_loadings <- colSums(rot_result$pattern^2)
  prop_var <- ss_loadings / nrow(rot_result$pattern)
  cum_var <- cumsum(prop_var)
  
  var_df <- data.frame(
    Factor = paste0("F", 1:length(ss_loadings)),
    SS_Loadings = round(ss_loadings, 3),
    Prop_Var = round(prop_var, 3),
    Cum_Var = round(cum_var, 3)
  )
  
  cat("\nVariance Explained:\n")
  print(var_df)
}

# ===================================================
# Display matrix comparison (Polychoric vs Pearson)
# ===================================================

display_matrix_comparison <- function(matrix_poly, matrix_pear) {
  
  n_items <- nrow(matrix_poly)
  n_factors <- ncol(matrix_poly)
  
  cat(sprintf("%-8s", ""))
  for (f in 1:n_factors) {
    cat(sprintf("  Poly_F%d Pear_F%d", f, f))
  }
  cat("\n")
  
  separator_length <- 8 + n_factors * 18
  cat(paste(rep("-", separator_length), collapse = ""), "\n")
  
  for (i in 1:n_items) {
    item_name <- rownames(matrix_poly)[i]
    cat(sprintf("%-8s", item_name))
    
    for (f in 1:n_factors) {
      cat(sprintf("  %7.3f %7.3f", matrix_poly[i, f], matrix_pear[i, f]))
    }
    cat("\n")
  }
}

# ===================================================
# Display single gamma comparison
# ===================================================

display_gamma_comparison <- function(results, gamma) {
  
  gamma_key <- paste0("gamma_", gsub("-", "neg", as.character(gamma)))
  
  cat("\n========================================\n")
  cat(sprintf("GAMMA: %.1f\n", gamma))
  cat("========================================\n\n")
  
  poly_sol <- results$polychoric$efa$rotations[[gamma_key]]
  pear_sol <- results$pearson$efa$rotations[[gamma_key]]
  
  cat("PATTERN MATRIX\n")
  cat("--------------\n\n")
  display_matrix_comparison(poly_sol$pattern, pear_sol$pattern)
  
  cat("\nSTRUCTURE MATRIX\n")
  cat("----------------\n\n")
  display_matrix_comparison(poly_sol$structure, pear_sol$structure)
  
  cat("\nFACTOR CORRELATION\n")
  cat("------------------\n\n")
  
  cat("Polychoric:\n")
  print(round(poly_sol$factor_correlation, 3))
  
  cat("\nPearson:\n")
  print(round(pear_sol$factor_correlation, 3))
  
  cat("\nCOMMUNALITIES\n")
  cat("-------------\n\n")
  
  poly_comm <- results$polychoric$efa$extraction$communalities
  pear_comm <- results$pearson$efa$extraction$communalities
  
  comm_df <- data.frame(
    Item = names(poly_comm),
    Polychoric = sprintf("%.3f", poly_comm),
    Pearson = sprintf("%.3f", pear_comm),
    stringsAsFactors = FALSE
  )
  
  print(comm_df, row.names = FALSE)
}

# ===================================================
# Display EFA comparison (main function)
# ===================================================

display_efa_comparison <- function(results) {
  
  cat("\n========================================\n")
  cat("EFA COMPARISON: Polychoric vs Pearson\n")
  cat("========================================\n\n")
  
  cat("Number of factors:", results$n_factors, "\n")
  cat("Extraction method:", results$config_used$extraction_method, "\n")
  cat("Gamma values:", paste(results$config_used$gamma_values, collapse = ", "), "\n")
  cat("\nNote: Pearson solution aligned to Polychoric\n")
  cat("      (factor order and signs adjusted)\n")
  
  for (gamma in results$config_used$gamma_values) {
    display_gamma_comparison(results, gamma)
  }
  
  cat("\n========================================\n")
  cat("EFA COMPARISON COMPLETE\n")
  cat("========================================\n")
}

# ===================================================
# Display EFA Evaluation (single gamma)
# ===================================================

display_efa_evaluation_single <- function(results, gamma, thresholds) {
  
  gamma_key <- paste0("gamma_", gsub("-", "neg", as.character(gamma)))
  poly_efa <- results$polychoric$efa
  pattern <- poly_efa$rotations[[gamma_key]]$pattern
  communalities <- poly_efa$extraction$communalities
  factor_cor <- poly_efa$rotations[[gamma_key]]$factor_correlation
  fa_object <- poly_efa$extraction$fa_object
  
  n_items <- nrow(pattern)
  n_factors <- ncol(pattern)
  
  cat("\n----------------------------------------\n")
  cat(sprintf("GAMMA: %.1f\n", gamma))
  cat("----------------------------------------\n")
  
  # 1. Pattern Matrix
  cat("\n1. PATTERN MATRIX\n")
  cat(sprintf("   Threshold: primary >= %.2f (*), cross-loading >= %.2f (x)\n\n", 
              thresholds$primary_loading, thresholds$cross_loading))
  
  cat(sprintf("   %-6s", "Item"))
  for (f in 1:n_factors) {
    cat(sprintf(" %7s", paste0("F", f)))
  }
  cat("  Status\n")
  
  cat("   ", paste(rep("-", 8 + n_factors * 8 + 20), collapse = ""), "\n", sep = "")
  
  problematic_items <- character(0)
  
  for (i in 1:n_items) {
    item_name <- rownames(pattern)[i]
    loadings <- pattern[i, ]
    abs_loadings <- abs(loadings)
    sorted_idx <- order(abs_loadings, decreasing = TRUE)
    primary <- abs_loadings[sorted_idx[1]]
    primary_factor <- sorted_idx[1]
    cross <- if (n_factors > 1) abs_loadings[sorted_idx[2]] else 0
    
    has_primary <- primary >= thresholds$primary_loading
    has_cross <- cross >= thresholds$cross_loading
    
    cat(sprintf("   %-6s", item_name))
    
    for (f in 1:n_factors) {
      val <- loadings[f]
      abs_val <- abs(val)
      
      if (f == primary_factor && abs_val >= thresholds$primary_loading) {
        marker <- "*"
      } else if (abs_val >= thresholds$cross_loading) {
        marker <- "x"
      } else {
        marker <- " "
      }
      
      cat(sprintf(" %6.3f%s", val, marker))
    }
    
    if (!has_primary || has_cross) {
      problematic_items <- c(problematic_items, item_name)
      status <- if (!has_primary && has_cross) {
        "[!] No primary + Cross"
      } else if (!has_primary) {
        "[!] No primary"
      } else {
        "[!] Cross-loading"
      }
      cat(sprintf("  %s", status))
    }
    cat("\n")
  }
  
  n_simple <- n_items - length(problematic_items)
  pct_simple <- round(100 * n_simple / n_items, 1)
  cat(sprintf("\n   Simple structure: %d/%d items (%.1f%%)\n", n_simple, n_items, pct_simple))
  cat(sprintf("   Legend: * = primary loading (>=%.2f), x = cross-loading (>=%.2f)\n",
              thresholds$primary_loading, thresholds$cross_loading))
  
  # 2. Factor Correlations
  cat("\n2. FACTOR CORRELATIONS\n")
  cat(sprintf("   Threshold: < %.2f for discriminant validity\n\n", thresholds$factor_cor_max))
  
  high_cor_pairs <- list()
  
  if (n_factors > 1) {
    cat("   ")
    for (f in 1:n_factors) {
      cat(sprintf(" %6s", paste0("F", f)))
    }
    cat("\n")
    
    for (i in 1:n_factors) {
      cat(sprintf("   F%d", i))
      for (j in 1:n_factors) {
        if (j < i) {
          r <- factor_cor[i, j]
          marker <- if (abs(r) >= thresholds$factor_cor_max) "!" else " "
          cat(sprintf(" %5.3f%s", r, marker))
          if (abs(r) >= thresholds$factor_cor_max) {
            high_cor_pairs[[length(high_cor_pairs) + 1]] <- c(j, i, r)
          }
        } else if (j == i) {
          cat("  1.000 ")
        } else {
          cat("       ")
        }
      }
      cat("\n")
    }
    
    off_diag <- factor_cor[lower.tri(factor_cor)]
    cat(sprintf("\n   Max |r|: %.3f, Mean |r|: %.3f\n", 
                max(abs(off_diag)), mean(abs(off_diag))))
    cat(sprintf("   Discriminant validity: %s\n", 
                if (length(high_cor_pairs) == 0) "OK" else "CONCERN"))
  } else {
    cat("   Single factor solution - not applicable\n")
  }
  
  return(list(
    gamma = gamma,
    n_simple = n_simple,
    pct_simple = pct_simple,
    n_problematic = length(problematic_items),
    problematic_items = problematic_items,
    n_high_cor = length(high_cor_pairs)
  ))
}

# ===================================================
# Display EFA Evaluation (all gamma values)
# ===================================================

display_efa_evaluation <- function(results) {
  
  THRESH <- list(
    primary_loading = 0.40,
    cross_loading = 0.30,
    communality_min = 0.30,
    communality_good = 0.50,
    rmsr_acceptable = 0.08,
    tli_acceptable = 0.90,
    rmsea_acceptable = 0.08,
    factor_cor_max = 0.85
  )
  
  gamma_values <- results$config_used$gamma_values
  poly_efa <- results$polychoric$efa
  communalities <- poly_efa$extraction$communalities
  fa_object <- poly_efa$extraction$fa_object
  extraction_method <- results$config_used$extraction_method
  
  cat("\n========================================\n")
  cat("EFA EVALUATION\n")
  cat(sprintf("Extraction method: %s\n", extraction_method))
  cat("========================================\n")
  
  # Communalities
  cat("\nCOMMUNALITIES\n")
  cat("-------------\n")
  cat(sprintf("Threshold: good >= %.2f, acceptable >= %.2f\n\n", 
              THRESH$communality_good, THRESH$communality_min))
  
  low_comm_items <- character(0)
  
  for (i in seq_along(communalities)) {
    item_name <- names(communalities)[i]
    h2 <- communalities[i]
    
    if (h2 < THRESH$communality_min) {
      low_comm_items <- c(low_comm_items, item_name)
      cat(sprintf("  [!] %s: %.3f (Low)\n", item_name, h2))
    } else if (h2 < THRESH$communality_good) {
      cat(sprintf("  [?] %s: %.3f (Acceptable)\n", item_name, h2))
    }
  }
  
  cat(sprintf("\nMean: %.3f, Min: %.3f, Max: %.3f\n", 
              mean(communalities), min(communalities), max(communalities)))
  cat(sprintf("Low communality items: %d\n", length(low_comm_items)))
  
  # Model Fit
  cat("\nMODEL FIT\n")
  cat("---------\n")
  
  if (!is.null(fa_object)) {
    rmsr <- if (!is.null(fa_object$rms)) fa_object$rms else NA
    tli <- if (!is.null(fa_object$TLI)) fa_object$TLI else NA
    rmsea <- if (!is.null(fa_object$RMSEA)) fa_object$RMSEA[1] else NA
    
    if (!is.na(rmsr)) {
      status <- if (rmsr <= THRESH$rmsr_acceptable) "OK" else "!"
      cat(sprintf("  RMSR:  %.4f [%s] (threshold: < %.2f)\n", 
                  rmsr, status, THRESH$rmsr_acceptable))
    }
    
    if (!is.na(tli)) {
      status <- if (tli >= THRESH$tli_acceptable) "OK" else "!"
      cat(sprintf("  TLI:   %.4f [%s] (threshold: >= %.2f)\n", 
                  tli, status, THRESH$tli_acceptable))
    }
    
    if (!is.na(rmsea)) {
      status <- if (rmsea <= THRESH$rmsea_acceptable) "OK" else "!"
      cat(sprintf("  RMSEA: %.4f [%s] (threshold: < %.2f)\n", 
                  rmsea, status, THRESH$rmsea_acceptable))
    }
  } else {
    cat("  Fit indices not available\n")
  }
  
  # Rotation-specific evaluation
  cat("\nROTATION-SPECIFIC EVALUATION\n")
  cat("============================\n")
  
  gamma_summaries <- list()
  
  for (gamma in gamma_values) {
    summary <- display_efa_evaluation_single(results, gamma, THRESH)
    gamma_summaries[[as.character(gamma)]] <- summary
  }
  
  # Comparison Summary
  cat("\n========================================\n")
  cat("COMPARISON ACROSS GAMMA VALUES\n")
  cat("========================================\n\n")
  
  cat(sprintf("%-10s %15s %15s %15s\n", "Gamma", "Simple Structure", "Problematic", "High Cor"))
  cat(paste(rep("-", 55), collapse = ""), "\n")
  
  for (gamma in gamma_values) {
    s <- gamma_summaries[[as.character(gamma)]]
    cat(sprintf("%-10.1f %12d/%d (%4.1f%%) %10d %15d\n", 
                s$gamma, s$n_simple, s$n_simple + s$n_problematic, 
                s$pct_simple, s$n_problematic, s$n_high_cor))
  }
  
  # Overall Summary
  cat("\nOVERALL SUMMARY\n")
  cat("---------------\n")
  
  issues <- character(0)
  
  if (length(low_comm_items) > 0) {
    issues <- c(issues, sprintf("Low communalities: %s", paste(low_comm_items, collapse = ", ")))
  }
  
  any_high_cor <- any(sapply(gamma_summaries, function(x) x$n_high_cor > 0))
  if (any_high_cor) {
    issues <- c(issues, "High factor correlations detected in some rotations")
  }
  
  if (length(issues) == 0) {
    cat("  [OK] No major issues detected.\n")
  } else {
    for (issue in issues) {
      cat(sprintf("  [!] %s\n", issue))
    }
  }
  
  cat("\n========================================\n")
}