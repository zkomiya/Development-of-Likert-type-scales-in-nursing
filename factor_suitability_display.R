# ===================================================
# Factor Suitability Display (View Layer)
# Version: 3.0
# Description: Display FA suitability results
# Changes from v2.0:
#   - Added fs_display_evaluation function
# Changes from v1.0:
#   - Order: Bartlett -> KMO
#   - Bartlett: single display (Pearson only)
# ===================================================

# Display all results
fs_display_results <- function(results) {
  
  # Header
  cat("========================================\n")
  cat("Factor Analysis Suitability Check\n")
  cat("========================================\n\n")
  
  # Sample Size
  cat("Sample Size\n")
  cat("-----------\n")
  cat(sprintf("N: %d\n", results$sample_size$n))
  cat(sprintf("p: %d\n", results$sample_size$p))
  cat(sprintf("N:p ratio: %.1f:1\n", results$sample_size$ratio))
  
  # Bartlett's Test (Pearson only)
  cat("\n========================================\n")
  cat("BARTLETT'S TEST\n")
  cat("========================================\n\n")
  
  cat(sprintf("%-12s %12.2f\n", "chi^2", results$bartlett$statistic))
  cat(sprintf("%-12s %12d\n", "df", results$bartlett$df))
  
  if (results$bartlett$p.value < 0.001) {
    cat(sprintf("%-12s %12s\n", "p-value", "< 0.001"))
  } else {
    cat(sprintf("%-12s %12.6f\n", "p-value", results$bartlett$p.value))
  }
  
  # KMO Comparison
  cat("\n========================================\n")
  cat("KMO TEST COMPARISON\n")
  cat("========================================\n\n")
  
  cat(sprintf("%-20s %12s %12s\n", "", "Polychoric", "Pearson"))
  cat(sprintf("%-20s %12.4f %12.4f\n", 
              "Overall KMO",
              results$kmo$polychoric$overall,
              results$kmo$pearson$overall))
  
  # Item-level MSA Comparison
  cat("\nItem-level MSA Comparison\n")
  cat("-------------------------\n")
  cat(sprintf("%-8s %12s %12s\n", "Item", "Polychoric", "Pearson"))
  
  items <- names(results$kmo$polychoric$MSA)
  for (item in items) {
    poly_msa <- results$kmo$polychoric$MSA[[item]]
    pear_msa <- results$kmo$pearson$MSA[[item]]
    
    cat(sprintf("%-8s %12.4f %12.4f\n", item, poly_msa, pear_msa))
  }
}

# ===================================================
# Evaluate FA Suitability
# ===================================================

fs_display_evaluation <- function(results) {
  
  cat("\n========================================\n")
  cat("FA SUITABILITY EVALUATION\n")
  cat("========================================\n\n")
  
  issues <- character(0)
  
  # --- Sample Size Evaluation ---
  cat("SAMPLE SIZE\n")
  cat("-----------\n")
  
  ratio <- results$sample_size$ratio
  
  if (ratio >= 10) {
    cat(sprintf("  N:p ratio: %.1f:1 [OK] (>= 10:1)\n", ratio))
  } else if (ratio >= 5) {
    cat(sprintf("  N:p ratio: %.1f:1 [?] (5:1 - 10:1, acceptable)\n", ratio))
  } else {
    cat(sprintf("  N:p ratio: %.1f:1 [!] (< 5:1, insufficient)\n", ratio))
    issues <- c(issues, "Sample size ratio is insufficient (< 5:1)")
  }
  
  # --- Bartlett's Test Evaluation ---
  cat("\nBARTLETT'S TEST\n")
  cat("---------------\n")
  
  p_value <- results$bartlett$p.value
  
  if (p_value < 0.05) {
    cat(sprintf("  p-value: < 0.05 [OK] Correlation matrix is not identity\n"))
  } else {
    cat(sprintf("  p-value: %.4f [!] Correlation matrix may be identity\n", p_value))
    issues <- c(issues, "Bartlett's test not significant (p >= 0.05)")
  }
  
  # --- KMO Evaluation ---
  cat("\nKMO TEST\n")
  cat("--------\n")
  
  # KMO interpretation function
  interpret_kmo <- function(value) {
    if (value >= 0.90) return(list(label = "Marvelous", status = "OK"))
    if (value >= 0.80) return(list(label = "Meritorious", status = "OK"))
    if (value >= 0.70) return(list(label = "Middling", status = "OK"))
    if (value >= 0.60) return(list(label = "Mediocre", status = "?"))
    if (value >= 0.50) return(list(label = "Miserable", status = "!"))
    return(list(label = "Unacceptable", status = "!"))
  }
  
  kmo_poly <- results$kmo$polychoric$overall
  kmo_pear <- results$kmo$pearson$overall
  
  interp_poly <- interpret_kmo(kmo_poly)
  interp_pear <- interpret_kmo(kmo_pear)
  
  cat(sprintf("  Polychoric: %.4f [%s] %s\n", 
              kmo_poly, interp_poly$status, interp_poly$label))
  cat(sprintf("  Pearson:    %.4f [%s] %s\n", 
              kmo_pear, interp_pear$status, interp_pear$label))
  
  if (kmo_poly < 0.50) {
    issues <- c(issues, "KMO (Polychoric) is unacceptable (< 0.50)")
  }
  if (kmo_pear < 0.50) {
    issues <- c(issues, "KMO (Pearson) is unacceptable (< 0.50)")
  }
  
  # --- Item-level MSA Check ---
  cat("\nITEM-LEVEL MSA\n")
  cat("--------------\n")
  
  msa_poly <- unlist(results$kmo$polychoric$MSA)
  msa_pear <- unlist(results$kmo$pearson$MSA)
  
  low_msa_poly <- names(msa_poly)[msa_poly < 0.50]
  low_msa_pear <- names(msa_pear)[msa_pear < 0.50]
  
  if (length(low_msa_poly) == 0 && length(low_msa_pear) == 0) {
    cat("  All items have MSA >= 0.50 [OK]\n")
  } else {
    if (length(low_msa_poly) > 0) {
      cat(sprintf("  Polychoric: Low MSA items: %s [!]\n", 
                  paste(low_msa_poly, collapse = ", ")))
      issues <- c(issues, sprintf("Low MSA items (Polychoric): %s", 
                                  paste(low_msa_poly, collapse = ", ")))
    }
    if (length(low_msa_pear) > 0) {
      cat(sprintf("  Pearson: Low MSA items: %s [!]\n", 
                  paste(low_msa_pear, collapse = ", ")))
    }
  }
  
  # --- Overall Summary ---
  cat("\n========================================\n")
  cat("SUMMARY\n")
  cat("========================================\n\n")
  
  if (length(issues) == 0) {
    cat("  [OK] Data is suitable for factor analysis.\n")
  } else {
    cat("  Issues detected:\n")
    for (issue in issues) {
      cat(sprintf("  [!] %s\n", issue))
    }
    cat("\n  Suggestion:\n")
    if (any(grepl("Sample size", issues))) {
      cat("  - Consider collecting more data\n")
    }
    if (any(grepl("Bartlett", issues))) {
      cat("  - Variables may not be correlated; factor analysis may not be appropriate\n")
    }
    if (any(grepl("KMO.*unacceptable", issues))) {
      cat("  - Consider removing items with low MSA or reviewing item selection\n")
    }
    if (any(grepl("Low MSA", issues))) {
      cat("  - Consider removing items with MSA < 0.50\n")
    }
  }
  
  cat("\n")
}