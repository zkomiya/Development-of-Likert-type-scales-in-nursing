# ===================================================
# ESEM Main (Controller Layer)
# ===================================================

# Main ESEM analysis function
analyze_esem <- function(data_obj, model_name, display_results = TRUE) {
  
  # Extract data from keyed structure
  source("data_structure.R")
  data <- get_data(data_obj)
  
  # Load configuration
  config <- load_config()
  
  # Get model definition (dataset-aware)
  dataset_name <- config$analysis$data_source$dataset
  dataset_models <- config$analysis$esem_models[[dataset_name]]
  
  if (is.null(dataset_models)) {
    stop(sprintf("No ESEM models defined for dataset '%s'", dataset_name))
  }
  
  if (!model_name %in% names(dataset_models)) {
    stop(sprintf("Model '%s' not found for dataset '%s'. Available: %s",
                 model_name, dataset_name, paste(names(dataset_models), collapse = ", ")))
  }
  
  model_def <- dataset_models[[model_name]]
  display_names <- model_def$display_names
  factor_names <- model_def$factor_names
  items <- model_def$items
  n_factors <- model_def$n_factors
  
  # Get ESEM settings
  esem_settings <- config$analysis$esem_settings
  
  # Build rotation_args list from YAML
  rotation_args <- esem_settings$rotation_args
  
  # Display header
  if (display_results) {
    esem_display_header(model_def$name)
  }
  
  # Step 1: Generate and display model syntax
  cat("Step 1: Generating ESEM model specification\n")
  model_syntax <- generate_esem_syntax(factor_names, items)
  
  if (display_results) {
    cat("\nModel syntax:\n")
    cat(model_syntax, "\n")
    cat(sprintf("\nRotation: %s\n", esem_settings$rotation))
    cat(sprintf("Factors: %d\n", n_factors))
    cat(sprintf("Items: %d\n", length(items)))
  }
  
  # Step 2: Perform ESEM
  cat("Step 2: Estimating ESEM model (", esem_settings$estimator, ")\n", sep = "")
  
  esem_fit <- tryCatch({
    perform_esem(
      data = data,
      factor_names = factor_names,
      items = items,
      n_factors = n_factors,
      estimator = esem_settings$estimator,
      missing = esem_settings$missing,
      se = esem_settings$se,
      rotation = esem_settings$rotation,
      rotation_args = rotation_args
    )
  }, error = function(e) {
    cat("\n[ERROR] Error during ESEM estimation:\n")
    cat(e$message, "\n")
    return(NULL)
  })
  
  if (is.null(esem_fit)) {
    return(NULL)
  }
  
  # Step 3: Calculate all metrics
  cat("Step 3: Calculating comprehensive metrics\n")
  
  # Extended fit indices (lavaan_utils)
  extended_fit <- calculate_extended_fit_indices(esem_fit)
  
  # ESEM loadings (ESEM-specific)
  esem_loadings <- calculate_esem_loadings(esem_fit, factor_names, items)
  
  # Residual diagnostics (lavaan_utils)
  residual_diagnostics <- calculate_residual_diagnostics(esem_fit)
  
  # Modification indices (lavaan_utils)
  modification_indices <- calculate_modification_indices(esem_fit)
  
  # Reliability and validity (ESEM-specific)
  reliability_validity <- calculate_esem_reliability_validity(esem_fit, factor_names, items)
  
  # Estimation problems (lavaan_utils)
  estimation_problems <- check_estimation_problems(esem_fit)
  
  # Detailed parameters (lavaan_utils)
  detailed_parameters <- calculate_detailed_parameters(esem_fit)
  
  # Step 4: Create comprehensive results object
  results <- list(
    fit = esem_fit,
    
    # Fit indices
    fit_indices = extended_fit,
    
    # ESEM-specific
    esem_loadings = esem_loadings,
    
    # Diagnostics
    diagnostics = list(
      residuals = residual_diagnostics,
      modification_indices = modification_indices,
      problems = estimation_problems
    ),
    
    # Reliability and validity
    reliability_validity = reliability_validity,
    
    # Parameters
    parameters = detailed_parameters,
    
    # Basic results
    fit_measures = fitMeasures(esem_fit),
    factor_correlations = lavInspect(esem_fit, "cor.lv"),
    
    # Model information
    model_name = model_name,
    model_def = model_def
  )
  
  # Step 5: Display results if requested
  if (display_results) {
    cat("\n")
    display_extended_fit(extended_fit)
    esem_display_loadings(esem_loadings, display_names)
    display_residuals(residual_diagnostics)
    display_modification_indices(modification_indices, display_names)
    esem_display_reliability_validity(reliability_validity, display_names)
    display_problems(estimation_problems)
    display_parameters(detailed_parameters, display_names)
  }
  
  cat("\n========================================\n")
  cat("ESEM Analysis Complete\n")
  cat("========================================\n")
  
  invisible(results)
}

# Show ESEM summary table wrapper
show_esem_summary <- function(results) {
  display_summary_table(results)
}