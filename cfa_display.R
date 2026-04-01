# ===================================================
# CFA Display (View Layer)
# Description: CFA-specific display functions.
#              Generic lavaan display functions are in lavaan_utils_display.R
# ===================================================

# Display header
cfa_display_header <- function(model_name) {
  cat("========================================\n")
  cat("CONFIRMATORY FACTOR ANALYSIS\n")
  cat("Model:", model_name, "\n")
  cat("========================================\n\n")
}

# Display CFA reliability and validity (CFA-specific)
# CFA assumes each item loads on exactly one factor.
cfa_display_reliability_validity <- function(rel_val, display_names) {
  
  cat("\nRELIABILITY AND VALIDITY\n")
  cat("------------------------\n")
  
  factors <- names(rel_val)
  
  # Create summary table
  summary_df <- data.frame(
    Factor = character(),
    Items = integer(),
    CR = numeric(),
    AVE = numeric(),
    MSV = numeric(),
    ASV = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (factor in factors) {
    summary_df <- rbind(summary_df, data.frame(
      Factor = translate_factor_name(factor, display_names),
      Items = rel_val[[factor]]$n_items,
      CR = rel_val[[factor]]$cr,
      AVE = rel_val[[factor]]$ave,
      MSV = ifelse(is.null(rel_val[[factor]]$msv), NA, rel_val[[factor]]$msv),
      ASV = ifelse(is.null(rel_val[[factor]]$asv), NA, rel_val[[factor]]$asv),
      stringsAsFactors = FALSE
    ))
  }
  
  print(summary_df, row.names = FALSE, digits = 3)
  
  # Display loading ranges
  cat("\nFactor Loading Ranges:\n")
  for (factor in factors) {
    cat(sprintf("%s: %.3f - %.3f (Mean = %.3f)\n",
                translate_factor_name(factor, display_names),
                rel_val[[factor]]$min_loading,
                rel_val[[factor]]$max_loading,
                rel_val[[factor]]$mean_loading))
  }
  
  # Discriminant validity matrix (if multiple factors)
  if (length(factors) > 1) {
    cat("\nDiscriminant Validity (sqrt(AVE) vs correlations):\n")
    
    # Translated factor names for display
    display_factor_names <- translate_factor_names(factors, display_names)
    
    # Create matrix
    disc_matrix <- matrix(NA, length(factors), length(factors))
    rownames(disc_matrix) <- display_factor_names
    colnames(disc_matrix) <- display_factor_names
    
    for (i in 1:length(factors)) {
      disc_matrix[i, i] <- rel_val[[factors[i]]]$sqrt_ave
      if (i < length(factors)) {
        for (j in (i+1):length(factors)) {
          cor_value <- rel_val[[factors[i]]]$correlations_with_others[factors[j]]
          disc_matrix[i, j] <- abs(cor_value)
          disc_matrix[j, i] <- abs(cor_value)
        }
      }
    }
    
    print(round(disc_matrix, 3))
    cat("Note: Diagonal = sqrt(AVE), Off-diagonal = |correlation|\n")
  }
}