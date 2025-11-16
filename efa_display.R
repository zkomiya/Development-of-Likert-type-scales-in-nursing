# ===================================================
# EFA Display
# Version: 4.0 - Added Polychoric + Pearson comparison display
# Description: Display EFA results only
# Changes from v3.0:
#   - Added display_matrix_comparison()
#   - Added display_solution_comparison()
#   - Added display_efa_comparison()
# ===================================================

# Display pattern matrix
display_pattern_matrix <- function(pattern) {
  
  p <- nrow(pattern)
  m <- ncol(pattern)
  
  # Set column names if missing
  if (is.null(colnames(pattern))) {
    colnames(pattern) <- paste0("F", 1:m)
  }
  if (is.null(rownames(pattern))) {
    rownames(pattern) <- paste0("Q", sprintf("%02d", 1:p))
  }
  
  # Round for display
  pattern_display <- round(pattern, 3)
  
  return(pattern_display)
}

# Display all results
display_efa_results <- function(efa_results) {
  
  cat("\nEFA RESULTS\n")
  cat("-----------\n")
  
  # Extract metadata
  n_factors <- efa_results$metadata$n_factors
  methods <- efa_results$metadata$extraction_methods
  gamma_values <- efa_results$metadata$gamma_values
  
  cat("Number of factors:", n_factors, "\n")
  cat("Extraction methods:", paste(methods, collapse = ", "), "\n")
  cat("Gamma values:", paste(gamma_values, collapse = ", "), "\n")
  cat("Kaiser normalization:", efa_results$metadata$kaiser_normalize, "\n\n")
  
  # For each method and gamma combination
  for (method in methods) {
    
    cat("\nEXTRACTION METHOD:", method, "\n")
    cat("------------------\n")
    
    # Extraction info
    ext_result <- efa_results[[method]]$extraction
    cat("Converged:", ext_result$converged, "\n")
    cat("Iterations:", ext_result$iterations, "\n")
    cat("Communalities range:", 
        sprintf("%.3f - %.3f", 
                min(ext_result$communalities), 
                max(ext_result$communalities)), "\n")
    
    # For each gamma
    for (gamma in gamma_values) {
      gamma_key <- paste0("gamma_", gsub("-", "neg", as.character(gamma)))
      rot_result <- efa_results[[method]]$rotations[[gamma_key]]
      
      cat("\nGamma:", gamma, "\n")
      cat("Rotation converged:", rot_result$converged, "\n")
      cat("Rotation iterations:", rot_result$iterations, "\n")
      
      # Pattern matrix
      cat("\nPattern Matrix:\n")
      print(display_pattern_matrix(rot_result$pattern))
      
      # Structure matrix
      cat("\nStructure Matrix:\n")
      print(display_pattern_matrix(rot_result$structure))
      
      # Factor correlations
      cat("\nFactor Correlation Matrix:\n")
      print(round(rot_result$factor_correlation, 3))
    }
  }
}

# Display specific combination
display_specific_result <- function(efa_results, method, gamma) {
  
  gamma_key <- paste0("gamma_", gsub("-", "neg", as.character(gamma)))
  
  if (!method %in% names(efa_results)) {
    stop("Method ", method, " not found")
  }
  
  if (!gamma_key %in% names(efa_results[[method]]$rotations)) {
    stop("Gamma ", gamma, " not found")
  }
  
  cat("\nRESULTS FOR:", method, "with gamma =", gamma, "\n")
  cat("----------------------------------------\n")
  
  ext_result <- efa_results[[method]]$extraction
  rot_result <- efa_results[[method]]$rotations[[gamma_key]]
  
  # Pattern Matrix
  cat("\nPattern Matrix:\n")
  print(round(rot_result$pattern, 3))
  
  # Structure Matrix
  cat("\nStructure Matrix:\n")
  print(round(rot_result$structure, 3))
  
  # Factor Correlations
  cat("\nFactor Correlation Matrix:\n")
  print(round(rot_result$factor_correlation, 3))
  
  # Communalities
  cat("\nCommunalities:\n")
  comm_df <- data.frame(
    Variable = names(ext_result$communalities),
    Communality = round(ext_result$communalities, 3)
  )
  print(comm_df)
  
  # Variance explained
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
# Version: 1.0
# Description: Display two matrices side-by-side for comparison
# ===================================================

display_matrix_comparison <- function(matrix_poly, matrix_pear) {
  
  n_items <- nrow(matrix_poly)
  n_factors <- ncol(matrix_poly)
  
  # Header
  cat(sprintf("%-8s", ""))
  for (f in 1:n_factors) {
    cat(sprintf("  Poly_F%d Pear_F%d", f, f))
  }
  cat("\n")
  
  # Separator
  separator_length <- 8 + n_factors * 18
  cat(paste(rep("-", separator_length), collapse = ""), "\n")
  
  # Each item
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
# Display single solution comparison
# Version: 1.0
# Description: Display one extraction method × gamma combination
# ===================================================

display_solution_comparison <- function(results, method, gamma) {
  
  gamma_key <- paste0("gamma_", gsub("-", "neg", as.character(gamma)))
  
  cat("\n========================================\n")
  cat(sprintf("METHOD: %s | GAMMA: %.1f\n", method, gamma))
  cat("========================================\n\n")
  
  # Get solutions
  poly_sol <- results$polychoric$efa[[method]]$rotations[[gamma_key]]
  pear_sol <- results$pearson$efa[[method]]$rotations[[gamma_key]]
  
  # Pattern Matrix
  cat("PATTERN MATRIX\n")
  cat("--------------\n\n")
  display_matrix_comparison(poly_sol$pattern, pear_sol$pattern)
  
  # Structure Matrix
  cat("\nSTRUCTURE MATRIX\n")
  cat("----------------\n\n")
  display_matrix_comparison(poly_sol$structure, pear_sol$structure)
  
  # Factor Correlation
  cat("\nFACTOR CORRELATION\n")
  cat("------------------\n\n")
  
  cat("Polychoric:\n")
  print(round(poly_sol$factor_correlation, 3))
  
  cat("\nPearson:\n")
  print(round(pear_sol$factor_correlation, 3))
  
  # Communalities
  cat("\nCOMMUNALITIES\n")
  cat("-------------\n\n")
  
  poly_comm <- results$polychoric$efa[[method]]$extraction$communalities
  pear_comm <- results$pearson$efa[[method]]$extraction$communalities
  
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
# Version: 1.0
# Description: Display all EFA results comparing Polychoric vs Pearson
# ===================================================

display_efa_comparison <- function(results) {
  
  cat("\n========================================\n")
  cat("EFA COMPARISON: Polychoric vs Pearson\n")
  cat("========================================\n\n")
  
  # Metadata
  cat("Number of factors:", results$n_factors, "\n")
  cat("Extraction methods:", paste(results$config_used$extraction, collapse = ", "), "\n")
  cat("Gamma values:", paste(results$config_used$gamma_values, collapse = ", "), "\n")
  cat("\nNote: Pearson solution aligned to Polychoric\n")
  cat("      (factor order and signs adjusted)\n")
  
  # Display each solution
  for (method in results$config_used$extraction) {
    for (gamma in results$config_used$gamma_values) {
      display_solution_comparison(results, method, gamma)
    }
  }
  
  cat("\n========================================\n")
  cat("EFA COMPARISON COMPLETE\n")
  cat("========================================\n")
}