# ===================================================
# Reliability Analysis Main (Unified Controller)
# Version: 3.2 - display improvements
# Changes from v3.1:
#   - nfactors -> Group factors specified
#   - Comparison table layout improved
#   - Lowercase notation unified (Cronbach's alpha, McDonald's omega)
#   - Subscale display notation unified
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
  
  # Calculate Cronbach's alpha
  cat("--- Cronbach's alpha ---\n")
  alpha_results <- calculate_cronbach_alpha(data, subscale_defs = subscale_defs)
  alpha_display_overall(alpha_results$overall)
  
  # Calculate McDonald's omega
  cat("\n--- McDonald's omega ---\n")
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
      cat(sprintf("Cronbach's alpha (raw):  %.3f\n", alpha_sub$raw_alpha))
      cat(sprintf("Cronbach's alpha (std):  %.3f\n", alpha_sub$std_alpha))
      
      if (!is.null(omega_sub)) {
        cat(sprintf("McDonald's omega_total:  %.3f\n", omega_sub$omega_total))
      }
    }
  }
  
  # Display comparison table
  cat("\n========================================\n")
  cat(sprintf("RELIABILITY COMPARISON (%d items; %d group factors specified)\n",
              alpha_results$overall$n_items, nfactors))
  cat("========================================\n\n")
  
  cat("Coefficient              Value\n")
  cat("-------------------------------\n")
  cat(sprintf("Cronbach's alpha         %.3f\n", alpha_results$overall$raw_alpha))
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