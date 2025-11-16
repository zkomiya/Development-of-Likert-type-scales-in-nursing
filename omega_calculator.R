# ===================================================
# McDonald's Omega Calculator (Model Layer)
# Version: 1.0 - psych package implementation
# ===================================================

library(psych)

# Main omega calculation function
calculate_omega <- function(data, config = NULL) {
  
  # Complete cases info - inline
  complete_rows <- complete.cases(data)
  n_complete <- sum(complete_rows)
  n_total <- nrow(data)
  percent_complete <- (n_complete / n_total) * 100
  
  # Calculate McDonald's omega using psych::omega
  # suppressWarnings to avoid convergence warnings during iteration
  omega_result <- suppressWarnings(
    psych::omega(data, 
                 nfactors = 1,  # For omega_h, single general factor
                 fm = "minres",  # Extraction method
                 plot = FALSE,
                 flip = FALSE)
  )
  
  # Format results to maintain consistency with alpha structure
  results <- list(
    overall = list(
      omega_total = omega_result$omega.tot,
      omega_hierarchical = omega_result$omega_h,
      omega_subscale = if(!is.null(omega_result$omega.lim)) omega_result$omega.lim else NA,
      alpha = omega_result$alpha,  # Cronbach's alpha for comparison
      n_items = ncol(data),
      n_cases = n_complete,
      n_total_cases = n_total,
      percent_complete = percent_complete
    ),
    factor_loadings = omega_result$schmid$sl,  # Schmid-Leiman solution
    item_data = data
  )
  
  # Handle subscales if configured
  if (!is.null(config) && 
      !is.null(config$analysis$item_total_analysis$enable_subscales) &&
      config$analysis$item_total_analysis$enable_subscales == TRUE) {
    
    subscale_results <- list()
    subscale_configs <- config$analysis$item_total_analysis
    
    for (name in names(subscale_configs)) {
      if (name != "enable_subscales" && !is.null(subscale_configs[[name]]$items)) {
        subscale_info <- subscale_configs[[name]]
        available_items <- subscale_info$items[subscale_info$items %in% names(data)]
        
        if (length(available_items) >= 3) {  # Need at least 3 items for omega
          subscale_data <- data[, available_items, drop = FALSE]
          
          subscale_omega_result <- suppressWarnings(
            psych::omega(subscale_data, 
                         nfactors = 1,
                         fm = "minres",
                         plot = FALSE,
                         flip = FALSE)
          )
          
          subscale_results[[name]] <- list(
            omega_total = subscale_omega_result$omega.tot,
            omega_hierarchical = subscale_omega_result$omega_h,
            alpha = subscale_omega_result$alpha,
            n_items = length(available_items),
            items = available_items,
            name = subscale_info$name
          )
        }
      }
    }
    
    results$subscales <- subscale_results
  }
  
  return(results)
}