# ===================================================
# Factor Suitability Display (View Layer)
# Version: 1.0
# Description: Display FA suitability results
# ===================================================

# Display all results in comparison format
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
  
  # KMO Comparison
  cat("\n========================================\n")
  cat("KMO TEST COMPARISON\n")
  cat("========================================\n\n")
  
  cat(sprintf("%-20s %12s %12s\n", "", "Polychoric", "Pearson"))
  cat(sprintf("%-20s %12.4f %12.4f\n", 
              "Overall KMO",
              results$polychoric$kmo$overall,
              results$pearson$kmo$overall))
  
  # Item-level MSA Comparison
  cat("\nItem-level MSA Comparison\n")
  cat("-------------------------\n")
  cat(sprintf("%-8s %12s %12s\n", 
              "Item", "Polychoric", "Pearson"))
  
  items <- names(results$polychoric$kmo$MSA)
  for (item in items) {
    poly_msa <- results$polychoric$kmo$MSA[[item]]
    pear_msa <- results$pearson$kmo$MSA[[item]]
    
    cat(sprintf("%-8s %12.4f %12.4f\n",item,poly_msa,pear_msa))
  }
  
  # Bartlett's Test Comparison
  cat("\n========================================\n")
  cat("BARTLETT'S TEST COMPARISON\n")
  cat("========================================\n\n")
  
  cat(sprintf("%-20s %12s %12s\n", "", "Polychoric", "Pearson"))
  
  cat(sprintf("%-20s %12.2f %12.2f\n",
              "chi^2",
              results$polychoric$bartlett$statistic,
              results$pearson$bartlett$statistic))
  
  cat(sprintf("%-20s %12d %12d\n",
              "df",
              results$polychoric$bartlett$df,
              results$pearson$bartlett$df))
  
  cat(sprintf("%-20s %16.8f %16.8f\n",
              "p-value",
              results$polychoric$bartlett$p.value,
              results$pearson$bartlett$p.value))
}