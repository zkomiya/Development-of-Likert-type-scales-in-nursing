# ===================================================
# Item-Item Correlation Main (Controller Layer)
# Version: 12.0 - Polychoric + Pearson concurrent calculation
# Description: Main controller for item-item correlation analysis
# Changes from v11.0:
#   - Removed method parameter from YAML (both calculated)
#   - Calculate both Polychoric and Pearson correlations
#   - Calculate both MIICs using calculate_miic_from_correlation_matrix()
#   - Display comparison results
# ===================================================

# Main item-item correlation analysis function
analyze_item_correlations <- function(data_obj) {
  
  # Extract data from keyed structure
  data <- get_data(data_obj)
  keys <- get_keys(data_obj)
  
  # Load configuration for parameters
  config <- load_config()
  use <- config$analysis$correlation_analysis$use
  
  # Display header
  ii_display_header()
  
  # Data is already Q columns only
  item_cols <- names(data)
  n_items <- length(item_cols)
  
  # Complete cases info
  complete_rows <- complete.cases(data)
  n_obs <- sum(complete_rows)
  complete_info <- list(
    n_complete = n_obs,
    n_total = nrow(data),
    percent_complete = n_obs / nrow(data) * 100
  )
  
  # Display data info
  ii_display_data_info(item_cols, n_items, complete_info)
  
  # Display computation info
  cat("\nStep 2: Computing correlations\n")
  cat("  Methods: Polychoric AND Pearson (both calculated)\n")
  cat(sprintf("  Missing data handling: %s\n", use))
  
  # Step 2: Compute both correlation matrices
  cor_poly <- calculate_polychoric_correlation(data)
  cor_pear <- cor(data, method = "pearson", use = use)
  
  # Step 3: Calculate both MIICs and item-level means
  miic_poly <- calculate_miic_from_correlation_matrix(cor_poly, n_obs)
  miic_pear <- calculate_miic_from_correlation_matrix(cor_pear, n_obs)
  
  # Step 4: Display comparison results
  ii_display_miic_comparison(miic_poly, miic_pear)
  ii_display_correlation_comparison(cor_poly, cor_pear)
  
  # Return results as invisible list
  results <- list(
    polychoric = list(
      correlation_matrix = cor_poly,
      miic = miic_poly
    ),
    pearson = list(
      correlation_matrix = cor_pear,
      miic = miic_pear
    ),
    item_data = data,
    complete_info = complete_info,
    use = use,
    item_cols = item_cols
  )
  
  invisible(results)
}