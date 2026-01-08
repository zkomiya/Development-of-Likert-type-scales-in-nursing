# ===================================================
# Ceiling-Floor Effect Main (Controller Layer)
# Version: 12.0 - Added CSV output functionality
# ===================================================

# Main Ceiling-Floor effect analysis function (results only, no evaluation)
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
  
  # Display results only (no evaluation)
  display_ceiling_floor_results(cf_results, parameters)
  
  # Output to CSV
  output_dir <- "output"
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }
  
  output_file <- file.path(output_dir, "ceiling_floor_results.csv")
  write.csv(cf_results, output_file, row.names = FALSE)
  cat(sprintf("\nResults saved to: %s\n", output_file))
  
  # Calculate summary
  summary_stats <- calculate_cf_summary(cf_results)
  
  # Return results invisibly
  invisible(list(
    results = cf_results,
    summary = summary_stats,
    parameters = parameters
  ))
}

# Independent evaluation function (takes data_obj directly)
show_cf_evaluation <- function(data_obj) {
  
  # Extract data from keyed structure
  data <- get_data(data_obj)
  keys <- get_keys(data_obj)
  
  # Load configuration for scale range
  config <- load_config()
  scale_min <- config$analysis$global$scale$min
  scale_max <- config$analysis$global$scale$max
  
  # Analyze ceiling-floor effects
  cf_results <- analyze_ceiling_floor_effects(
    data,
    scale_min = scale_min,
    scale_max = scale_max
  )
  
  # Evaluate results
  evaluation <- evaluate_ceiling_floor_results(
    cf_results, 
    scale_min, 
    scale_max
  )
  
  # Display evaluation
  display_ceiling_floor_evaluation(evaluation)
  
  # Output to CSV
  output_dir <- "output"
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }
  
  output_file <- file.path(output_dir, "ceiling_floor_evaluation.csv")
  write.csv(evaluation, output_file, row.names = FALSE)
  cat(sprintf("\nEvaluation saved to: %s\n", output_file))
  
  # Return evaluation invisibly
  invisible(list(
    results = cf_results,
    evaluation = evaluation
  ))
}