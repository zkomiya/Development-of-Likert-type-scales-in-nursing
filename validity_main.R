# ===================================================
# Validity Main (Controller Layer)
# Version: 6.0 - Pearson only analysis
# ===================================================

# Main validity analysis function
analyze_validity <- function(target_obj, rehab_obj) {
  
  # Display header
  validity_display_header()
  
  # Step 1: Data overview
  cat("Step 1: Data Overview\n")
  cat("----------------------\n")
  
  # Extract data for summary only
  source("data_structure.R")
  target_data <- get_data(target_obj)
  target_keys <- get_keys(target_obj)
  rehab_data <- get_data(rehab_obj)
  rehab_keys <- get_keys(rehab_obj)
  
  cat(sprintf("30items dataset:\n"))
  cat(sprintf("  Data: %d rows x %d columns\n", 
              nrow(target_data), ncol(target_data)))
  cat(sprintf("  Keys: %s\n", paste(target_keys, collapse = ", ")))
  
  cat(sprintf("REHAB dataset:\n"))
  cat(sprintf("  Data: %d rows x %d columns\n", 
              nrow(rehab_data), ncol(rehab_data)))
  cat(sprintf("  Keys: %s\n\n", paste(rehab_keys, collapse = ", ")))
  
  # Step 2: Calculate correlations with ID matching
  cat("Step 2: Correlation Analysis\n")
  cat("-----------------------------\n")
  
  # Pass entire objects (including keys) to calculator
  results <- analyze_validity_correlations(target_obj, rehab_obj)
  
  # Step 3: Display results
  cat("Step 3: Results\n")
  cat("---------------\n\n")
  
  # Total level
  validity_display_total(results)
  
  # Subscale level
  validity_display_subscales(results)
  
  # Subscale x subscale matrix
  validity_display_subscale_matrix(results)
  
  cat("\n========================================\n")
  cat("Analysis Complete\n")
  cat("========================================\n")
  
  cat("\nInterpretation Guide:\n")
  cat("- Pearson correlation is appropriate for sum scores (continuous variables)\n")
  cat("- Raw REHAB scores used (no reversal)\n")
  cat("- High REHAB scores indicate worse functioning (both GB and DB)\n")
  cat("- Negative correlations with 30items indicate convergent validity\n")
  cat("  (better function on 30items = lower dysfunction on REHAB)\n")
  cat("- Positive correlations with REHAB-DB expected if 30items measures stability\n")
  
  invisible(results)
}