# ===================================================
# McDonald's Omega Calculator (Model Layer)
# Version: 3.0 - nfactors argument, omega_h handling
# Changes from v2.0:
#   - Added nfactors parameter for overall calculation
#   - omega_h set to NA when nfactors = 1
#     (psych documentation: "Omega_h for 1 factor is not meaningful")
#   - Subscale calculations always use nfactors = 1 (unidimensional)
# ===================================================

library(psych)

# Main omega calculation function
calculate_omega <- function(data, nfactors, subscale_defs = NULL) {
  
  # Complete cases info - inline
  complete_rows <- complete.cases(data)
  n_complete <- sum(complete_rows)
  n_total <- nrow(data)
  percent_complete <- (n_complete / n_total) * 100
  
  # Calculate McDonald's omega using psych::omega
  # suppressWarnings to avoid convergence warnings during iteration
  omega_result <- suppressWarnings(
    psych::omega(data, 
                 nfactors = nfactors,
                 fm = "minres",
                 plot = FALSE,
                 flip = FALSE)
  )
  
  # omega_h is not meaningful when nfactors = 1
  omega_h_value <- if (nfactors == 1) NA else omega_result$omega_h
  
  # Format results
  results <- list(
    overall = list(
      omega_total = omega_result$omega.tot,
      omega_hierarchical = omega_h_value,
      omega_subscale = if(!is.null(omega_result$omega.lim)) omega_result$omega.lim else NA,
      alpha = omega_result$alpha,
      nfactors = nfactors,
      n_items = ncol(data),
      n_cases = n_complete,
      n_total_cases = n_total,
      percent_complete = percent_complete
    ),
    factor_loadings = omega_result$schmid$sl,
    item_data = data
  )
  
  # Handle subscales if definitions provided
  # Each subscale is unidimensional -> nfactors = 1 -> omega_h = NA
  if (!is.null(subscale_defs) && length(subscale_defs) > 0) {
    
    subscale_results <- list()
    
    for (name in names(subscale_defs)) {
      subscale_info <- subscale_defs[[name]]
      available_items <- subscale_info$items[subscale_info$items %in% names(data)]
      
      if (length(available_items) >= 3) {
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
          omega_hierarchical = NA,
          alpha = subscale_omega_result$alpha,
          n_items = length(available_items),
          items = available_items,
          name = subscale_info$name
        )
      }
    }
    
    results$subscales <- subscale_results
  }
  
  return(results)
}