# ===================================================
# Lavaan Utilities Main (Controller Layer)
# Description: Neutral comparison functions across lavaan-based analyses.
#              Does not belong to CFA or ESEM; sits above both.
# ===================================================

# Compare CFA and ESEM for the same model definition
compare_cfa_esem <- function(data_obj, model_name, display_individual = FALSE) {
  
  cat("========================================\n")
  cat("CFA vs ESEM COMPARISON\n")
  cat(sprintf("Model: %s\n", model_name))
  cat("========================================\n\n")
  
  # Run CFA
  cat("--- Running CFA ---\n")
  cfa_results <- analyze_cfa(data_obj, model_name, display_results = display_individual)
  
  if (is.null(cfa_results)) {
    stop("CFA estimation failed")
  }
  
  # Run ESEM
  cat("\n--- Running ESEM ---\n")
  esem_results <- analyze_esem(data_obj, model_name, display_results = display_individual)
  
  if (is.null(esem_results)) {
    stop("ESEM estimation failed")
  }
  
  # Display comparison
  display_cfa_esem_comparison(cfa_results, esem_results)
  
  invisible(list(
    cfa = cfa_results,
    esem = esem_results
  ))
}