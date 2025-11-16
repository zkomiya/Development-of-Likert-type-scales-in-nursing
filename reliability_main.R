# ===================================================
# Reliability Analysis Main (Unified Controller)
# Version: 2.0 - YAML-based configuration only
# Changes from v1.0:
#   - Removed enable_subscales and subscale_config from arguments
#   - Now reads all configuration from YAML only
# ===================================================

analyze_reliability <- function(data_obj) {
  
  # Extract data from keyed structure
  source("data_structure.R")
  data <- get_data(data_obj)
  keys <- get_keys(data_obj)
  
  # Load configuration from YAML
  config <- load_config()
  enable_subscales <- config$analysis$item_total_analysis$enable_subscales
  subscale_config <- config$analysis$item_total_analysis
  
  cat("========================================\n")
  cat("RELIABILITY ANALYSIS\n")
  cat("========================================\n\n")
  
  # Create config for calculations
  config_for_analysis <- NULL
  if (enable_subscales && !is.null(subscale_config)) {
    config_for_analysis <- list(
      analysis = list(
        item_total_analysis = subscale_config
      )
    )
    config_for_analysis$analysis$item_total_analysis$enable_subscales <- enable_subscales
  }
  
  # Calculate Cronbach's Alpha
  cat("--- Cronbach's Alpha ---\n")
  alpha_results <- calculate_cronbach_alpha(data, config = config_for_analysis)
  alpha_display_overall(alpha_results$overall)
  
  # Calculate McDonald's Omega
  cat("\n--- McDonald's Omega ---\n")
  omega_results <- calculate_omega(data, config = config_for_analysis)
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
        cat(sprintf("ω_total:                 %.3f\n", omega_sub$omega_total))
        cat(sprintf("ω_hierarchical:          %.3f\n", omega_sub$omega_hierarchical))
      }
    }
  }
  
  # Display comparison table
  cat("\n========================================\n")
  cat("RELIABILITY COMPARISON\n")
  cat("========================================\n\n")
  
  # Create comparison table
  cat("Overall Scale:\n")
  cat("Method                   Value\n")
  cat("-------------------------------\n")
  cat(sprintf("Cronbach's α (raw)       %.3f\n", alpha_results$overall$raw_alpha))
  cat(sprintf("McDonald's ω_total       %.3f\n", omega_results$overall$omega_total))
  cat(sprintf("McDonald's ω_h           %.3f\n", omega_results$overall$omega_hierarchical))
  
  # Display if-deleted results
  alpha_display_if_deleted(alpha_results$if_deleted)
  
  # Return both results
  results <- list(
    alpha = alpha_results,
    omega = omega_results,
    config_used = list(
      enable_subscales = enable_subscales,
      subscale_config = subscale_config
    )
  )
  
  invisible(results)
}