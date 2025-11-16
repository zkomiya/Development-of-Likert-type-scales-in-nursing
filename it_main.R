# ===================================================
# Item-Total Correlation Main (Controller Layer)
# Version: 5.0 - YAML-based configuration only
# Description: Main controller for I-T correlation analysis
# Changes from v4.0:
#   - Removed enable_subscales and subscale_config from arguments
#   - Now reads all configuration from YAML only
#   - Consistent with other analysis functions
# ===================================================

# Main Item-Total correlation analysis function
analyze_item_total <- function(data_obj) {
  
  # Extract data from keyed structure
  source("data_structure.R")
  data <- get_data(data_obj)
  keys <- get_keys(data_obj)
  
  # Load configuration from YAML
  config <- load_config()
  enable_subscales <- config$analysis$item_total_analysis$enable_subscales
  subscale_config <- config$analysis$item_total_analysis
  
  # Display header
  it_display_header()
  
  item_names <- names(data)
  n_items <- length(item_names)
  
  # Display data info
  it_display_data_info(n_items, item_names)
  
  # Display calculation info
  it_display_calculation_info()
  
  # Step 2: Calculate item-total correlations (both Pearson and Polyserial)
  it_results <- calculate_item_total_correlations(data)
  
  # Display results (4-column structure)
  it_display_results(it_results)
  
  # Step 3: Calculate summary statistics
  summary_stats <- calculate_it_summary(it_results)
  
  # Display summary
  it_display_summary(summary_stats)
  
  # Step 4: Subscale analysis (if enabled in YAML)
  subscale_results <- NULL
  
  if (enable_subscales && !is.null(subscale_config)) {
    all_elements <- names(subscale_config)
    subscale_names <- all_elements[!all_elements %in% c("enable_subscales")]
    
    if (length(subscale_names) > 0) {
      cat("\n========================================\n")
      cat("SUBSCALE ITEM-TOTAL CORRELATIONS\n")
      cat("========================================\n")
      
      subscale_results <- list()
      
      for (subscale_id in subscale_names) {
        subscale_info <- subscale_config[[subscale_id]]
        
        if (!is.null(subscale_info$name) && !is.null(subscale_info$items)) {
          subscale_name <- subscale_info$name
          subscale_items <- subscale_info$items
          
          available_items <- subscale_items[subscale_items %in% names(data)]
          
          if (length(available_items) > 0) {
            # Display subscale header
            it_display_subscale_header(subscale_name, length(available_items))
            
            # Calculate subscale I-T correlations (4-column)
            subscale_it <- calculate_subscale_item_total(data, available_items)
            
            # Display results
            it_display_subscale_results(subscale_it)
            
            # Calculate and display summary
            subscale_summary <- calculate_it_summary(subscale_it)
            cat(sprintf("\n%s Summary:\n", subscale_name))
            cat(sprintf("  Pearson Corrected:    Mean = %.3f (SD = %.3f)\n",
                        subscale_summary$mean_corrected_pearson,
                        subscale_summary$sd_corrected_pearson))
            cat(sprintf("  Polyserial Corrected: Mean = %.3f (SD = %.3f)\n",
                        subscale_summary$mean_corrected_polyserial,
                        subscale_summary$sd_corrected_polyserial))
            
            # Store results
            subscale_results[[subscale_id]] <- list(
              name = subscale_name,
              correlations = subscale_it,
              summary = subscale_summary,
              items = available_items
            )
          } else {
            cat(sprintf("\nWarning: No items found for subscale '%s'\n", subscale_name))
          }
        }
      }
    }
  }
  
  # Return results
  results <- list(
    correlations = it_results,
    summary = summary_stats,
    subscale_results = subscale_results,
    item_data = data,
    item_names = item_names,
    config_used = list(
      enable_subscales = enable_subscales,
      subscale_config = subscale_config
    )
  )
  
  invisible(results)
}