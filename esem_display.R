# ===================================================
# ESEM Display (View Layer)
# Description: ESEM-specific display functions.
#              Generic lavaan display functions are in lavaan_utils_display.R
# ===================================================

# Display ESEM header
esem_display_header <- function(model_name) {
  cat("========================================\n")
  cat("EXPLORATORY STRUCTURAL EQUATION MODELING\n")
  cat("Model:", model_name, "\n")
  cat("========================================\n\n")
}

# Display ESEM factor loading matrix
# Shows all loadings (primary and cross-loadings) with primary marked
esem_display_loadings <- function(esem_loadings, display_names) {
  
  loading_matrix <- esem_loadings$matrix
  primary_factor <- esem_loadings$primary_factor
  
  cat("\nESEM FACTOR LOADING MATRIX\n")
  cat("--------------------------\n")
  
  # Translate column names for display
  display_colnames <- translate_factor_names(colnames(loading_matrix), display_names)
  
  # Create display matrix with formatting
  display_matrix <- matrix("", nrow = nrow(loading_matrix), ncol = ncol(loading_matrix))
  rownames(display_matrix) <- rownames(loading_matrix)
  colnames(display_matrix) <- display_colnames
  
  for (i in seq_len(nrow(loading_matrix))) {
    for (j in seq_len(ncol(loading_matrix))) {
      val <- loading_matrix[i, j]
      if (is.na(val)) {
        display_matrix[i, j] <- "  NA"
      } else if (colnames(loading_matrix)[j] == primary_factor[i]) {
        # Mark primary loading
        display_matrix[i, j] <- sprintf("[%5.3f]", val)
      } else if (abs(val) >= 0.30) {
        # Notable cross-loading
        display_matrix[i, j] <- sprintf("*%5.3f*", val)
      } else {
        display_matrix[i, j] <- sprintf(" %5.3f ", val)
      }
    }
  }
  
  print(noquote(display_matrix))
  
  cat("\nLegend: [value] = primary loading, *value* = cross-loading >= 0.30\n")
  
  # Summary: items per factor
  cat("\nItems per factor (by primary loading):\n")
  factor_names <- colnames(loading_matrix)
  for (factor in factor_names) {
    assigned <- rownames(loading_matrix)[primary_factor == factor]
    cat(sprintf("  %s: %d items (%s)\n",
                translate_factor_name(factor, display_names),
                length(assigned),
                paste(assigned, collapse = ", ")))
  }
}

# Display ESEM reliability and validity (ESEM-specific)
# Uses primary-loading-based item assignment
esem_display_reliability_validity <- function(rel_val, display_names) {
  
  cat("\nESEM RELIABILITY AND VALIDITY\n")
  cat("------------------------------\n")
  cat("Note: Items assigned to factors by highest absolute loading\n\n")
  
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
  cat("\nPrimary Loading Ranges:\n")
  for (factor in factors) {
    if (rel_val[[factor]]$n_items > 0) {
      cat(sprintf("  %s: %.3f - %.3f (Mean = %.3f)\n",
                  translate_factor_name(factor, display_names),
                  rel_val[[factor]]$min_loading,
                  rel_val[[factor]]$max_loading,
                  rel_val[[factor]]$mean_loading))
    }
  }
  
  # Discriminant validity matrix (if multiple factors)
  if (length(factors) > 1) {
    cat("\nDiscriminant Validity (sqrt(AVE) vs correlations):\n")
    
    display_factor_names <- translate_factor_names(factors, display_names)
    
    disc_matrix <- matrix(NA, length(factors), length(factors))
    rownames(disc_matrix) <- display_factor_names
    colnames(disc_matrix) <- display_factor_names
    
    for (i in 1:length(factors)) {
      if (!is.na(rel_val[[factors[i]]]$ave)) {
        disc_matrix[i, i] <- rel_val[[factors[i]]]$sqrt_ave
      }
      if (i < length(factors)) {
        for (j in (i+1):length(factors)) {
          if (!is.null(rel_val[[factors[i]]]$correlations_with_others)) {
            cor_value <- rel_val[[factors[i]]]$correlations_with_others[factors[j]]
            disc_matrix[i, j] <- abs(cor_value)
            disc_matrix[j, i] <- abs(cor_value)
          }
        }
      }
    }
    
    print(round(disc_matrix, 3))
    cat("Note: Diagonal = sqrt(AVE), Off-diagonal = |correlation|\n")
  }
}