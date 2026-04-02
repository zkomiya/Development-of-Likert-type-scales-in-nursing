# ===================================================
# Cronbach's Alpha Calculator (Model Layer)
# ===================================================

library(psych)

# Main calculation function using psych::alpha
calculate_cronbach_alpha <- function(data, subscale_defs = NULL) {
  
  # Complete cases info - inline
  complete_rows <- complete.cases(data)
  n_complete <- sum(complete_rows)
  n_total <- nrow(data)
  percent_complete <- (n_complete / n_total) * 100
  
  # Calculate using psych::alpha
  alpha_result <- psych::alpha(data, check.keys = FALSE)
  
  # Format results to match existing structure
  results <- list(
    overall = list(
      raw_alpha = alpha_result$total$raw_alpha,
      std_alpha = alpha_result$total$std.alpha,
      n_items = ncol(data),
      n_cases = n_complete,
      n_total_cases = n_total,
      percent_complete = percent_complete
    ),
    if_deleted = data.frame(
      item = rownames(alpha_result$alpha.drop),
      raw_alpha_if_deleted = alpha_result$alpha.drop[, "raw_alpha"],
      std_alpha_if_deleted = alpha_result$alpha.drop[, "std.alpha"],
      stringsAsFactors = FALSE
    ),
    item_data = data
  )
  
  # Handle subscales if definitions provided
  if (!is.null(subscale_defs) && length(subscale_defs) > 0) {
    
    subscale_results <- list()
    
    for (name in names(subscale_defs)) {
      subscale_info <- subscale_defs[[name]]
      available_items <- subscale_info$items[subscale_info$items %in% names(data)]
      
      if (length(available_items) >= 2) {
        subscale_data <- data[, available_items, drop = FALSE]
        subscale_alpha_result <- psych::alpha(subscale_data, check.keys = FALSE)
        
        subscale_results[[name]] <- list(
          raw_alpha = subscale_alpha_result$total$raw_alpha,
          std_alpha = subscale_alpha_result$total$std.alpha,
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