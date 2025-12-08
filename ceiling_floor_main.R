# ===================================================
# Ceiling-Floor Effect Main (Controller Layer)
# Version: 10.0 - YAML-based scale range
# Changes from v9.0:
#   - Removed scale_min and scale_max from function arguments
#   - Now reads scale range from analysis_config.yaml
#   - Consistent with other analysis functions
# ===================================================

# Main Ceiling-Floor effect analysis function
analyze_ceiling_floor <- function(data_obj) {
  
  # Extract data from keyed structure
  data <- get_data(data_obj)
  keys <- get_keys(data_obj)
  
  # Load configuration for scale range
  config <- load_config()
  scale_min <- config$analysis$global$scale$min
  scale_max <- config$analysis$global$scale$max
  
  # Prepare parameters for display
  parameters <- list(
    scale_min = scale_min,
    scale_max = scale_max,
    n_items = ncol(data),
    n_cases = nrow(data)
  )
  
  # Analyze ceiling-floor effects
  cf_results <- analyze_ceiling_floor_effects(
    data,
    scale_min = scale_min,
    scale_max = scale_max
  )
  
  # Display results using unified function
  display_ceiling_floor_results(cf_results, parameters)
  
  # Calculate summary
  summary_stats <- calculate_cf_summary(cf_results)
  
  # Return results invisibly
  invisible(list(
    results = cf_results,
    summary = summary_stats,
    parameters = parameters
  ))
}