# ===================================================
# Reliability Analysis Main (Unified Controller)
# Version: 3.1 - nfactors from scale_structure, omega_h NA handling
# Changes from v3.0:
#   - Fixed config path (config$analysis$...)
#   - Derives nfactors from length(subscale_defs)
#   - Passes nfactors to calculate_omega
#   - omega_h displayed only when not NA
# ===================================================

analyze_reliability <- function(data_obj) {
  
  # Extract data from keyed structure
  source("data_structure.R")
  data <- get_data(data_obj)
  keys <- get_keys(data_obj)
  
  # Load configuration from YAML
  config <- load_config()
  dataset_name <- config$analysis$data_source$dataset
  
  # Get subscale definitions from scale_structure
  subscale_defs <- config$analysis$scale_structure[[dataset_name]]
  
  # Derive nfactors from subscale definitions
  nfactors <- if (!is.null(subscale_defs)) length(subscale_defs) else 1
  
  cat("========================================\n")
  cat("RELIABILITY ANALYSIS\n")
  cat("========================================\n\n")
  
  # Calculate Cronbach's Alpha
  cat("--- Cronbach's Alpha ---\n")
  alpha_results <- calculate_cronbach_alpha(data, subscale_defs = subscale_defs)
  alpha_display_overall(alpha_results$overall)
  
  # Calculate McDonald's Omega
  cat("\n--- McDonald's Omega ---\n")
  omega_results <- calculate_omega(data, nfactors = nfactors, subscale_defs = subscale_defs)
  omega_display_overall(omega_results$overall)
  
  # Display subscales if available
  if (!is.null(alpha_results$subscales) || !is.null(omega_results$subscales)) {
    cat("\n========================================\n")
    cat("SUBSCALE RELIABILITY\n")
    cat("========================================\n")
    
    # Get all subscale names
    subscale_names <- names(alpha_results$subscales)
    
    for (name in subscale_names) {
      alpha_sub <- alpha_results$subscales[[name]]
      omega_sub <- omega_results$subscales[[name]]
      
      cat(sprintf("\nSubscale: %s (%d items)\n", alpha_sub$name, alpha_sub$n_items))
      cat("----------------------------------------\n")
      cat(sprintf("Cronbach's Alpha (raw):  %.3f\n", alpha_sub$raw_alpha))
      cat(sprintf("Cronbach's Alpha (std):  %.3f\n", alpha_sub$std_alpha))
      
      if (!is.null(omega_sub)) {
        cat(sprintf("omega_total:             %.3f\n", omega_sub$omega_total))
      }
    }
  }
  
  # Display comparison table
  cat("\n========================================\n")
  cat("RELIABILITY COMPARISON\n")
  cat("========================================\n\n")
  
  # Create comparison table
  cat("Overall Scale:\n")
  cat(sprintf("nfactors:                %d\n", nfactors))
  cat("Method                   Value\n")
  cat("-------------------------------\n")
  cat(sprintf("Cronbach's alpha (raw)   %.3f\n", alpha_results$overall$raw_alpha))
  cat(sprintf("McDonald's omega_total   %.3f\n", omega_results$overall$omega_total))
  if (!is.na(omega_results$overall$omega_hierarchical)) {
    cat(sprintf("McDonald's omega_h       %.3f\n", omega_results$overall$omega_hierarchical))
  }
  
  # Display if-deleted results
  alpha_display_if_deleted(alpha_results$if_deleted)
  
  # Return both results
  results <- list(
    alpha = alpha_results,
    omega = omega_results,
    dataset = dataset_name,
    nfactors = nfactors,
    subscale_defs_used = subscale_defs
  )
  
  invisible(results)
}