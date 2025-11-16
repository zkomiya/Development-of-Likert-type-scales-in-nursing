# ===================================================
# CFA Main (Controller Layer)
# Version: 3.0 - Extended analysis
# Description: Main controller for comprehensive CFA analysis
# ===================================================

# Main CFA analysis function - Extended version
analyze_cfa <- function(data_obj, model_name, display_results = TRUE) {
  
  # Extract data from keyed structure
  source("data_structure.R")
  data <- get_data(data_obj)
  keys <- get_keys(data_obj)
  
  # Load configuration
  config <- load_config()
  
  # Get model definition
  if (!model_name %in% names(config$analysis$cfa_models)) {
    stop(sprintf("Model '%s' not found in configuration", model_name))
  }
  
  model_def <- config$analysis$cfa_models[[model_name]]
  
  # Get CFA settings
  cfa_settings <- config$analysis$cfa_settings
  
  # Display header
  if (display_results) {
    cfa_display_header(model_def$name)
  }
  
  # Step 1: Generate lavaan syntax
  cat("Step 1: Generating model specification\n")
  model_syntax <- generate_lavaan_syntax(model_def)
  
  if (display_results) {
    cat("\nModel structure:\n")
    for (factor in names(model_def$structure)) {
      n_items <- length(model_def$structure[[factor]])
      cat(sprintf("  %s: %d items\n", factor, n_items))
    }
    if (!is.null(model_def$higher_order)) {
      cat("  Higher-order factors defined\n")
    }
    cat("\n")
  }
  
  # Step 2: Perform CFA
  cat("Step 2: Estimating model (", cfa_settings$estimator, ")\n", sep = "")
  
  cfa_fit <- tryCatch({
    perform_cfa(
      data = data,
      model_syntax = model_syntax,
      estimator = cfa_settings$estimator,
      missing = cfa_settings$missing,
      se = cfa_settings$se
    )
  }, error = function(e) {
    cat("\n⚠ Error during model estimation:\n")
    cat(e$message, "\n")
    return(NULL)
  })
  
  if (is.null(cfa_fit)) {
    return(NULL)
  }
  
  # Step 3: Calculate all extended metrics
  cat("Step 3: Calculating comprehensive metrics\n")
  
  # Extended fit indices
  extended_fit <- calculate_extended_fit_indices(cfa_fit)
  
  # Residual diagnostics
  residual_diagnostics <- calculate_residual_diagnostics(cfa_fit)
  
  # Modification indices
  modification_indices <- calculate_modification_indices(cfa_fit)
  
  # Reliability and validity
  reliability_validity <- calculate_reliability_validity(cfa_fit)
  
  # Estimation problems
  estimation_problems <- check_estimation_problems(cfa_fit)
  
  # Detailed parameters
  detailed_parameters <- calculate_detailed_parameters(cfa_fit)
  
  # Step 4: Create comprehensive results object
  results <- list(
    fit = cfa_fit,
    
    # Fit indices
    fit_indices = extended_fit,
    
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
    
    # Basic results for backward compatibility
    fit_measures = fitMeasures(cfa_fit),
    std_solution = standardizedSolution(cfa_fit),
    factor_correlations = lavInspect(cfa_fit, "cor.lv"),
    factor_loadings = lavInspect(cfa_fit, "std")$lambda,
    r_squared = lavInspect(cfa_fit, "r2"),
    
    # Model information
    model_name = model_name,
    model_def = model_def
  )
  
  # Step 5: Display results if requested
  if (display_results) {
    cat("\n")
    cfa_display_extended_fit(extended_fit)
    cfa_display_residuals(residual_diagnostics)
    cfa_display_modification_indices(modification_indices)
    cfa_display_reliability_validity(reliability_validity)
    cfa_display_problems(estimation_problems)
    cfa_display_parameters(detailed_parameters)
  }
  
  cat("\n========================================\n")
  cat("CFA Analysis Complete\n")
  cat("========================================\n")
  
  invisible(results)
}

# Function to extract specific results
extract_cfa_results <- function(results, what = "all") {
  
  valid_options <- c("all", "fit", "residuals", "mi", "reliability", 
                     "parameters", "problems")
  
  if (!what %in% valid_options) {
    stop(sprintf("Invalid option. Choose from: %s", 
                 paste(valid_options, collapse = ", ")))
  }
  
  switch(what,
         all = results,
         fit = results$fit_indices,
         residuals = results$diagnostics$residuals,
         mi = results$diagnostics$modification_indices,
         reliability = results$reliability_validity,
         parameters = results$parameters,
         problems = results$diagnostics$problems
  )
}


# Show summary table wrapper
show_cfa_summary <- function(results) {
  cfa_display_summary_table(results)
}

# Compare multiple CFA models - Extended version
compare_cfa_models <- function(data_obj, model_names, display_individual = FALSE) {
  
  cat("========================================\n")
  cat("CFA MODEL COMPARISON\n")
  cat("========================================\n\n")
  
  # Run all models
  results_list <- list()
  fit_list <- list()
  
  for (model_name in model_names) {
    cat(sprintf("Running model: %s\n", model_name))
    cat("----------------------------------------\n")
    
    result <- analyze_cfa(data_obj, model_name, display_results = display_individual)
    
    if (!is.null(result)) {
      results_list[[model_name]] <- result
      fit_list[[model_name]] <- result$fit
    } else {
      cat(sprintf("⚠ Model %s failed to converge\n", model_name))
    }
    
    cat("\n")
  }
  
  # Create comparison table
  if (length(results_list) > 1) {
    cat("MODEL COMPARISON TABLE\n")
    cat("----------------------\n\n")
    
    comparison_df <- data.frame()
    
    for (model_name in names(results_list)) {
      res <- results_list[[model_name]]
      
      model_summary <- data.frame(
        Model = model_name,
        chi2 = res$fit_indices$chi_square$chi2,
        df = res$fit_indices$chi_square$df,
        p = res$fit_indices$chi_square$pvalue,
        CFI = res$fit_indices$incremental$cfi,
        TLI = res$fit_indices$incremental$tli,
        RMSEA = res$fit_indices$absolute$rmsea,
        SRMR = res$fit_indices$absolute$srmr,
        AIC = res$fit_indices$information$aic,
        BIC = res$fit_indices$information$bic,
        n_params = res$fit_indices$information$n_parameters
      )
      
      comparison_df <- rbind(comparison_df, model_summary)
    }
    
    print(comparison_df, row.names = FALSE, digits = 3)
    
    # Chi-square difference test
    if (length(fit_list) > 1) {
      cat("\nCHI-SQUARE DIFFERENCE TEST\n")
      cat("--------------------------\n")
      comparison <- do.call(lavTestLRT, fit_list)
      print(comparison)
    }
    
    # Use display function for model comparison
    cfa_display_model_comparison(results_list)
  }
  
  invisible(list(
    models = results_list,
    comparison = if(exists("comparison_df")) comparison_df else NULL,
    chi2_test = if(exists("comparison")) comparison else NULL
  ))
}