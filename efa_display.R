# ===================================================
# EFA Display
# Version: 7.0 - Eligible parameter sets only
# Description: Display EFA results only for parameter sets where ALL items meet criteria
# Changes from v6.0:
#   - Added check_all_items_eligible() function
#   - Rewrote display_efa_evaluation() to show only eligible sets
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
# Evaluate items and return failure details
# ===================================================

evaluate_items <- function(pattern, 
                           primary_threshold = 0.4,
                           cross_threshold = 0.3,
                           diff_threshold = 0.1) {
  
  n_items <- nrow(pattern)
  n_factors <- ncol(pattern)
  
  failed_items <- list()
  
  for (i in 1:n_items) {
    item_name <- rownames(pattern)[i]
    abs_loadings <- abs(pattern[i, ])
    sorted_idx <- order(abs_loadings, decreasing = TRUE)
    
    primary <- abs_loadings[sorted_idx[1]]
    second <- if (n_factors > 1) abs_loadings[sorted_idx[2]] else 0
    
    # Check conditions
    has_primary <- primary >= primary_threshold
    is_cross_loading <- (second >= cross_threshold) && 
      ((primary - second) <= diff_threshold)
    
    # Record failure details
    if (!has_primary) {
      failed_items[[item_name]] <- list(
        reason = "low_primary",
        primary = primary,
        second = second,
        diff = primary - second
      )
    } else if (is_cross_loading) {
      failed_items[[item_name]] <- list(
        reason = "cross_loading",
        primary = primary,
        second = second,
        diff = primary - second
      )
    }
  }
  
  return(list(
    n_failed = length(failed_items),
    n_total = n_items,
    pass_rate = (n_items - length(failed_items)) / n_items,
    failed_items = failed_items
  ))
}

# ===================================================
# Display EFA Evaluation (all parameter sets with details)
# ===================================================

display_efa_evaluation <- function(results) {
  
  gamma_values <- results$config_used$gamma_values
  extraction_method <- results$config_used$extraction_method
  
  cat("\n========================================\n")
  cat("EFA EVALUATION SUMMARY\n")
  cat(sprintf("Extraction method: %s\n", extraction_method))
  cat("========================================\n")
  cat("\nCriteria for ALL items:\n")
  cat("  1. Primary loading >= 0.40\n")
  cat("  2. NOT cross-loading (2nd < 0.30 OR diff > 0.10)\n")
  cat("\n")
  
  # Collect evaluation results for all sets
  all_evaluations <- list()
  
  # Evaluate Polychoric solutions
  for (gamma in gamma_values) {
    gamma_key <- paste0("gamma_", gsub("-", "neg", as.character(gamma)))
    pattern <- results$polychoric$efa$rotations[[gamma_key]]$pattern
    
    eval_result <- evaluate_items(pattern)
    
    all_evaluations[[paste0("Polychoric_gamma_", gamma)]] <- list(
      cor_type = "Polychoric",
      gamma = gamma,
      evaluation = eval_result,
      results = results$polychoric
    )
  }
  
  # Evaluate Pearson solutions
  for (gamma in gamma_values) {
    gamma_key <- paste0("gamma_", gsub("-", "neg", as.character(gamma)))
    pattern <- results$pearson$efa$rotations[[gamma_key]]$pattern
    
    eval_result <- evaluate_items(pattern)
    
    all_evaluations[[paste0("Pearson_gamma_", gamma)]] <- list(
      cor_type = "Pearson",
      gamma = gamma,
      evaluation = eval_result,
      results = results$pearson
    )
  }
  
  # Display each parameter set
  eligible_sets <- character(0)
  
  for (set_name in names(all_evaluations)) {
    set_info <- all_evaluations[[set_name]]
    eval <- set_info$evaluation
    
    cat("----------------------------------------\n")
    cat(sprintf("PARAMETER SET: %s, gamma = %.1f\n", 
                set_info$cor_type, set_info$gamma))
    cat("----------------------------------------\n")
    cat(sprintf("Items failing criteria: %d/%d (%.1f%% pass)\n", 
                eval$n_failed, eval$n_total, eval$pass_rate * 100))
    
    if (eval$n_failed == 0) {
      cat("\n*** ELIGIBLE SET ***\n\n")
      eligible_sets <- c(eligible_sets, sprintf("%s, gamma = %.1f", 
                                                set_info$cor_type, 
                                                set_info$gamma))
      
      # Display full results for eligible sets
      gamma_key <- paste0("gamma_", gsub("-", "neg", as.character(set_info$gamma)))
      rot_result <- set_info$results$efa$rotations[[gamma_key]]
      ext_result <- set_info$results$efa$extraction
      
      cat("PATTERN MATRIX:\n")
      print(round(rot_result$pattern, 3))
      
      cat("\nSTRUCTURE MATRIX:\n")
      print(round(rot_result$structure, 3))
      
      cat("\nFACTOR CORRELATION:\n")
      print(round(rot_result$factor_correlation, 3))
      
      cat("\nCOMMUNALITIES:\n")
      comm_df <- data.frame(
        Variable = names(ext_result$communalities),
        Communality = round(ext_result$communalities, 3)
      )
      print(comm_df, row.names = FALSE)
      
      ss_loadings <- colSums(rot_result$pattern^2)
      prop_var <- ss_loadings / nrow(rot_result$pattern)
      cum_var <- cumsum(prop_var)
      
      var_df <- data.frame(
        Factor = paste0("F", 1:length(ss_loadings)),
        SS_Loadings = round(ss_loadings, 3),
        Prop_Var = round(prop_var, 3),
        Cum_Var = round(cum_var, 3)
      )
      
      cat("\nVARIANCE EXPLAINED:\n")
      print(var_df, row.names = FALSE)
      
    } else {
      cat("\nFailed items:\n")
      
      for (item_name in names(eval$failed_items)) {
        item_info <- eval$failed_items[[item_name]]
        
        if (item_info$reason == "low_primary") {
          cat(sprintf("  %s: primary=%.2f (< 0.40)\n", 
                      item_name, item_info$primary))
        } else if (item_info$reason == "cross_loading") {
          cat(sprintf("  %s: cross-loading (primary=%.2f, 2nd=%.2f, diff=%.2f)\n",
                      item_name, item_info$primary, item_info$second, item_info$diff))
        }
      }
    }
    
    cat("\n")
  }
  
  # Display summary
  cat("========================================\n")
  cat("SUMMARY\n")
  cat("========================================\n")
  
  if (length(eligible_sets) > 0) {
    cat(sprintf("Eligible sets (0 failed items): %d/%d\n", 
                length(eligible_sets), length(all_evaluations)))
    for (set_name in eligible_sets) {
      cat(sprintf("  - %s\n", set_name))
    }
  } else {
    cat("Eligible sets (0 failed items): 0/", length(all_evaluations), "\n", sep = "")
    
    # Show best non-eligible sets (fewest failures)
    failure_counts <- sapply(all_evaluations, function(x) x$evaluation$n_failed)
    sorted_idx <- order(failure_counts)
    
    cat("\nBest non-eligible sets (fewest failed items):\n")
    for (i in 1:min(3, length(sorted_idx))) {
      idx <- sorted_idx[i]
      set_info <- all_evaluations[[idx]]
      cat(sprintf("  - %s, gamma = %.1f (%d failed items)\n",
                  set_info$cor_type, set_info$gamma, 
                  set_info$evaluation$n_failed))
    }
  }
  
  cat("========================================\n")
  
  invisible(NULL)
}