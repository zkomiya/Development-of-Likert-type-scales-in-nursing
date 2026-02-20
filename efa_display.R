# ===================================================
# EFA Display
# Version: 12.1 - Added display_heywood_check
# Description: Display EFA results with configurable evaluation thresholds
# Changes from v12.0:
#   - Added display_heywood_check() function
# ===================================================

# ===================================================
# Display Heywood Check
# ===================================================

display_heywood_check <- function(communalities, label) {
  cat(sprintf("\n--- HEYWOOD CHECK: %s ---\n", label))
  
  heywood_items <- communalities[communalities >= 1.0]
  
  if (length(heywood_items) == 0) {
    cat("Heywood cases (communality >= 1.000): NONE\n")
  } else {
    cat(sprintf("Heywood cases (communality >= 1.000): %d items detected\n", length(heywood_items)))
    cat(sprintf("  %-8s %s\n", "Item", "Communality"))
    for (item_name in names(heywood_items)) {
      cat(sprintf("  %-8s %.3f        [WARNING]\n", item_name, heywood_items[[item_name]]))
    }
    cat("  ** Solution may be unreliable. Interpret with caution. **\n")
  }
}

# Display pattern matrix
display_pattern_matrix <- function(pattern, display_cutoff) {
  
  p <- nrow(pattern)
  m <- ncol(pattern)
  
  if (is.null(colnames(pattern))) {
    colnames(pattern) <- paste0("F", 1:m)
  }
  if (is.null(rownames(pattern))) {
    rownames(pattern) <- paste0("Q", sprintf("%02d", 1:p))
  }
  
  if (is.null(display_cutoff)) {
    # Show all values
    pattern_display <- round(pattern, 3)
    return(pattern_display)
  }
  
  # Apply cutoff: suppress absolute values below threshold
  pattern_rounded <- round(pattern, 3)
  pattern_char <- matrix("", nrow = p, ncol = m)
  rownames(pattern_char) <- rownames(pattern)
  colnames(pattern_char) <- colnames(pattern)
  
  for (i in 1:p) {
    for (j in 1:m) {
      if (abs(pattern_rounded[i, j]) >= display_cutoff) {
        pattern_char[i, j] <- sprintf("%.3f", pattern_rounded[i, j])
      }
    }
  }
  
  return(noquote(pattern_char))
}

# ===================================================
# Display matrix comparison (Polychoric vs Pearson)
# ===================================================

display_matrix_comparison <- function(matrix_poly, matrix_pear, display_cutoff) {
  
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
      val_poly <- matrix_poly[i, f]
      val_pear <- matrix_pear[i, f]
      
      if (!is.null(display_cutoff)) {
        str_poly <- if (abs(val_poly) >= display_cutoff) sprintf("%7.3f", val_poly) else "       "
        str_pear <- if (abs(val_pear) >= display_cutoff) sprintf("%7.3f", val_pear) else "       "
        cat(sprintf("  %s %s", str_poly, str_pear))
      } else {
        cat(sprintf("  %7.3f %7.3f", val_poly, val_pear))
      }
    }
    cat("\n")
  }
}

# ===================================================
# Display single gamma comparison
# ===================================================

display_gamma_comparison <- function(results, gamma, display_cutoff) {
  
  gamma_key <- paste0("gamma_", gsub("-", "neg", as.character(gamma)))
  
  cat("\n========================================\n")
  cat(sprintf("GAMMA: %.2f\n", gamma))
  cat("========================================\n\n")
  
  poly_sol <- results$polychoric$efa$rotations[[gamma_key]]
  pear_sol <- results$pearson$efa$rotations[[gamma_key]]
  
  cat("PATTERN MATRIX\n")
  cat("--------------\n\n")
  display_matrix_comparison(poly_sol$pattern, pear_sol$pattern, display_cutoff)
  
  cat("\nSTRUCTURE MATRIX\n")
  cat("----------------\n\n")
  display_matrix_comparison(poly_sol$structure, pear_sol$structure, display_cutoff)
  
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
# Display single kappa comparison (Promax)
# ===================================================

display_kappa_comparison <- function(results, kappa, display_cutoff) {
  
  kappa_key <- paste0("kappa_", kappa)
  
  cat("\n========================================\n")
  cat(sprintf("PROMAX KAPPA: %d\n", kappa))
  cat("========================================\n\n")
  
  poly_sol <- results$polychoric$efa$rotations_promax[[kappa_key]]
  pear_sol <- results$pearson$efa$rotations_promax[[kappa_key]]
  
  cat("PATTERN MATRIX\n")
  cat("--------------\n\n")
  display_matrix_comparison(poly_sol$pattern, pear_sol$pattern, display_cutoff)
  
  cat("\nSTRUCTURE MATRIX\n")
  cat("----------------\n\n")
  display_matrix_comparison(poly_sol$structure, pear_sol$structure, display_cutoff)
  
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

display_efa_comparison <- function(results, display_cutoff) {
  
  cat("\n========================================\n")
  cat("EFA COMPARISON: Polychoric vs Pearson\n")
  cat("========================================\n\n")
  
  cat("Number of factors:", results$n_factors, "\n")
  cat("Extraction method:", results$config_used$extraction_method, "\n")
  cat("Gamma values:", paste(results$config_used$gamma_values, collapse = ", "), "\n")
  if (!is.null(results$config_used$promax_kappa_values)) {
    cat("Promax kappa values:", paste(results$config_used$promax_kappa_values, collapse = ", "), "\n")
  }
  if (!is.null(display_cutoff)) {
    cat("Display cutoff:", display_cutoff, "(absolute values below this are suppressed)\n")
  }
  cat("\nNote: Pearson solution aligned to Polychoric\n")
  cat("      (factor order and signs adjusted)\n")
  
  # Display oblimin results
  cat("\n\n### OBLIMIN ROTATION RESULTS ###\n")
  for (gamma in results$config_used$gamma_values) {
    display_gamma_comparison(results, gamma, display_cutoff)
  }
  
  # Display promax results
  if (!is.null(results$config_used$promax_kappa_values)) {
    cat("\n\n### PROMAX ROTATION RESULTS ###\n")
    for (kappa in results$config_used$promax_kappa_values) {
      display_kappa_comparison(results, kappa, display_cutoff)
    }
  }
  
  cat("\n========================================\n")
  cat("EFA COMPARISON COMPLETE\n")
  cat("========================================\n")
}

# ===================================================
# Evaluate items and return failure details
# ===================================================

evaluate_items <- function(pattern, 
                           primary_threshold,
                           cross_threshold,
                           diff_threshold) {
  
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
    is_cross_loading <- (second >= cross_threshold) || 
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
# Display single evaluation set (helper function)
# ===================================================

display_evaluation_set <- function(set_info, eval, primary_threshold,
                                   eligible_sets, display_cutoff) {
  
  cat(sprintf("Items failing criteria: %d/%d (%.1f%% pass)\n", 
              eval$n_failed, eval$n_total, eval$pass_rate * 100))
  
  if (eval$n_failed == 0) {
    cat("\n*** ELIGIBLE SET ***\n\n")
    
    # Get rotation result based on type
    if (set_info$rotation_type == "oblimin") {
      gamma_key <- paste0("gamma_", gsub("-", "neg", as.character(set_info$param_value)))
      rot_result <- set_info$results$efa$rotations[[gamma_key]]
    } else {
      kappa_key <- paste0("kappa_", set_info$param_value)
      rot_result <- set_info$results$efa$rotations_promax[[kappa_key]]
    }
    ext_result <- set_info$results$efa$extraction
    
    cat("PATTERN MATRIX:\n")
    print(display_pattern_matrix(rot_result$pattern, display_cutoff))
    
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
        cat(sprintf("  %s: primary=%.2f (< %.2f)\n", 
                    item_name, item_info$primary, primary_threshold))
      } else if (item_info$reason == "cross_loading") {
        cat(sprintf("  %s: cross-loading (primary=%.2f, 2nd=%.2f, diff=%.2f)\n",
                    item_name, item_info$primary, item_info$second, item_info$diff))
      }
    }
  }
  
  cat("\n")
}

# ===================================================
# Display EFA Evaluation (all parameter sets with details)
# ===================================================

display_efa_evaluation <- function(results,
                                   primary_threshold,
                                   cross_threshold,
                                   diff_threshold,
                                   display_cutoff) {
  
  gamma_values <- results$config_used$gamma_values
  promax_kappa_values <- results$config_used$promax_kappa_values
  extraction_method <- results$config_used$extraction_method
  
  cat("\n========================================\n")
  cat("EFA EVALUATION SUMMARY\n")
  cat(sprintf("Extraction method: %s\n", extraction_method))
  cat("========================================\n")
  
  # --- Heywood check ---
  display_heywood_check(results$polychoric$efa$extraction$communalities, "Polychoric")
  display_heywood_check(results$pearson$efa$extraction$communalities, "Pearson")
  cat("\n")
  # ----------------------
  
  cat("\nCriteria for ALL items:\n")
  cat(sprintf("  1. Primary loading >= %.2f\n", primary_threshold))
  cat(sprintf("  2. NOT cross-loading (2nd < %.2f OR diff > %.2f)\n", 
              cross_threshold, diff_threshold))
  if (!is.null(display_cutoff)) {
    cat(sprintf("  Display cutoff: %.2f (absolute values below this are suppressed)\n",
                display_cutoff))
  }
  cat("\n")
  
  # Collect evaluation results for all sets
  all_evaluations <- list()
  
  # Evaluate Polychoric Oblimin solutions
  for (gamma in gamma_values) {
    gamma_key <- paste0("gamma_", gsub("-", "neg", as.character(gamma)))
    pattern <- results$polychoric$efa$rotations[[gamma_key]]$pattern
    
    eval_result <- evaluate_items(pattern,
                                  primary_threshold = primary_threshold,
                                  cross_threshold = cross_threshold,
                                  diff_threshold = diff_threshold)
    
    all_evaluations[[paste0("Polychoric_Oblimin_gamma_", gamma)]] <- list(
      cor_type = "Polychoric",
      rotation_type = "oblimin",
      param_name = "gamma",
      param_value = gamma,
      evaluation = eval_result,
      results = results$polychoric
    )
  }
  
  # Evaluate Pearson Oblimin solutions
  for (gamma in gamma_values) {
    gamma_key <- paste0("gamma_", gsub("-", "neg", as.character(gamma)))
    pattern <- results$pearson$efa$rotations[[gamma_key]]$pattern
    
    eval_result <- evaluate_items(pattern,
                                  primary_threshold = primary_threshold,
                                  cross_threshold = cross_threshold,
                                  diff_threshold = diff_threshold)
    
    all_evaluations[[paste0("Pearson_Oblimin_gamma_", gamma)]] <- list(
      cor_type = "Pearson",
      rotation_type = "oblimin",
      param_name = "gamma",
      param_value = gamma,
      evaluation = eval_result,
      results = results$pearson
    )
  }
  
  # Evaluate Polychoric Promax solutions
  if (!is.null(promax_kappa_values)) {
    for (kappa in promax_kappa_values) {
      kappa_key <- paste0("kappa_", kappa)
      pattern <- results$polychoric$efa$rotations_promax[[kappa_key]]$pattern
      
      eval_result <- evaluate_items(pattern,
                                    primary_threshold = primary_threshold,
                                    cross_threshold = cross_threshold,
                                    diff_threshold = diff_threshold)
      
      all_evaluations[[paste0("Polychoric_Promax_kappa_", kappa)]] <- list(
        cor_type = "Polychoric",
        rotation_type = "promax",
        param_name = "kappa",
        param_value = kappa,
        evaluation = eval_result,
        results = results$polychoric
      )
    }
    
    # Evaluate Pearson Promax solutions
    for (kappa in promax_kappa_values) {
      kappa_key <- paste0("kappa_", kappa)
      pattern <- results$pearson$efa$rotations_promax[[kappa_key]]$pattern
      
      eval_result <- evaluate_items(pattern,
                                    primary_threshold = primary_threshold,
                                    cross_threshold = cross_threshold,
                                    diff_threshold = diff_threshold)
      
      all_evaluations[[paste0("Pearson_Promax_kappa_", kappa)]] <- list(
        cor_type = "Pearson",
        rotation_type = "promax",
        param_name = "kappa",
        param_value = kappa,
        evaluation = eval_result,
        results = results$pearson
      )
    }
  }
  
  # Display each parameter set
  eligible_sets <- character(0)
  
  for (set_name in names(all_evaluations)) {
    set_info <- all_evaluations[[set_name]]
    eval <- set_info$evaluation
    
    cat("----------------------------------------\n")
    if (set_info$rotation_type == "oblimin") {
      cat(sprintf("PARAMETER SET: %s, Oblimin, gamma = %.2f\n", 
                  set_info$cor_type, set_info$param_value))
    } else {
      cat(sprintf("PARAMETER SET: %s, Promax, kappa = %d\n", 
                  set_info$cor_type, set_info$param_value))
    }
    cat("----------------------------------------\n")
    
    display_evaluation_set(set_info, eval, primary_threshold, eligible_sets,
                           display_cutoff)
    
    if (eval$n_failed == 0) {
      if (set_info$rotation_type == "oblimin") {
        eligible_sets <- c(eligible_sets, sprintf("%s, Oblimin, gamma = %.2f", 
                                                  set_info$cor_type, 
                                                  set_info$param_value))
      } else {
        eligible_sets <- c(eligible_sets, sprintf("%s, Promax, kappa = %d", 
                                                  set_info$cor_type, 
                                                  set_info$param_value))
      }
    }
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
    for (i in 1:min(5, length(sorted_idx))) {
      idx <- sorted_idx[i]
      set_info <- all_evaluations[[idx]]
      if (set_info$rotation_type == "oblimin") {
        cat(sprintf("  - %s, Oblimin, gamma = %.2f (%d failed items)\n",
                    set_info$cor_type, set_info$param_value, 
                    set_info$evaluation$n_failed))
      } else {
        cat(sprintf("  - %s, Promax, kappa = %d (%d failed items)\n",
                    set_info$cor_type, set_info$param_value, 
                    set_info$evaluation$n_failed))
      }
    }
  }
  
  cat("========================================\n")
  
  # Return eligible sets information
  invisible(all_evaluations)
}