# ===================================================
# Factor Suitability Display (View Layer)
# Version: 2.0
# Description: Display FA suitability results
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