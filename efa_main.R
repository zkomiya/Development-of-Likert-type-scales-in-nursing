# ===================================================
# EFA Main Controller
# Version: 19.0 - YAML-driven scale range and item pattern
# Changes from v18.0:
#   - Data preprocessing now uses global scale range and item pattern from analysis_config.yaml
#   - Removed 1-4 hardcoding dependency in data_preprocessor.R call
# ===================================================

# Main EFA analysis function
analyze_efa <- function(data_obj,
                        n_factors,
                        verbose = TRUE,
                        show_full_results = TRUE) {
  
  source("data_structure.R")
  data <- get_data(data_obj)
  keys <- get_keys(data_obj)
  
  cat("\n========================================\n")
  cat("EXPLORATORY FACTOR ANALYSIS\n")
  cat("========================================\n")
  
  # Load configuration
  config <- load_config()
  efa_config <- config$analysis$efa_settings
  global_config <- config$analysis$global
  
  # Read parameters from YAML
  missing <- efa_config$missing
  extraction_method <- efa_config$extraction_method
  gamma_values <- efa_config$gamma_values
  kaiser_normalize <- efa_config$kaiser_normalize
  max_iterations <- efa_config$max_iterations
  flip_factors <- efa_config$flip_factors
  promax_kappa_values <- efa_config$promax_kappa_values
  pd_tol <- efa_config$pd_tolerance
  if (is.null(pd_tol)) {
    stop("EFA setting pd_tolerance not found in analysis_config.yaml")
  }
  pd_tol <- as.numeric(pd_tol)
  
  # Read global parameters from YAML (scale range / item pattern)
  if (is.null(global_config$item_pattern)) {
    stop("Global item_pattern not found in analysis_config.yaml")
  }
  if (is.null(global_config$scale$min) || is.null(global_config$scale$max)) {
    stop("Global scale min/max not found in analysis_config.yaml")
  }
  item_pattern <- global_config$item_pattern
  scale_min <- global_config$scale$min
  scale_max <- global_config$scale$max
  
  if (verbose) {
    cat("\nUsing EFA settings from configuration:\n")
    cat("  Missing data handling:", missing, "\n")
    cat("  Extraction method:", extraction_method, "\n")
    cat("  Gamma values:", paste(gamma_values, collapse = ", "), "\n")
    if (!is.null(promax_kappa_values)) {
      cat("  Promax kappa values:", paste(promax_kappa_values, collapse = ", "), "\n")
    }
    cat("  Kaiser normalization:", kaiser_normalize, "\n")
    cat("  Max iterations:", max_iterations, "\n")
    cat("  PD tolerance (cor.smooth eig.tol):", pd_tol, "\n")
    cat("  Factor sign adjustment:", flip_factors, "\n")
    cat("  Item pattern:", item_pattern, "\n")
    cat("  Scale range:", scale_min, "-", scale_max, "\n")
    cat("  Correlation methods: Polychoric AND Pearson\n")
  }
  
  # Validate n_factors
  if (missing(n_factors)) {
    stop("n_factors is required. Run determine_n_factors() first to determine appropriate number.")
  }
  
  if (!is.numeric(n_factors) || n_factors < 1) {
    stop("n_factors must be a positive integer")
  }
  
  # Validate missing parameter
  if (!missing %in% c("listwise", "pairwise")) {
    stop("missing must be 'listwise' or 'pairwise'")
  }
  
  # Step 1: Data Preprocessing
  cat("\nStep 1: Data Preprocessing\n")
  cat("----------------------------\n")
  
  source("data_preprocessor.R")
  data_fa <- preprocess_for_fa(data,
                               method = missing,
                               verbose = verbose,
                               item_pattern = item_pattern,
                               scale_min = scale_min,
                               scale_max = scale_max)
  
  if (n_factors > ncol(data_fa)) {
    stop("n_factors (", n_factors, ") cannot exceed number of variables (", ncol(data_fa), ")")
  }
  
  cat("Number of factors to extract:", n_factors, "\n")
  
  # Step 2: Compute correlation matrices
  cat("\nStep 2: Computing correlation matrices\n")
  cat("------------------------------------------\n")
  cat("  Computing Polychoric correlation...\n")
  
  source("efa_calculator.R")
  cor_poly <- calculate_correlation_for_efa(data_fa, "polychoric", missing, pd_tol = pd_tol)
  
  cat("  Computing Pearson correlation...\n")
  cor_pear <- calculate_correlation_for_efa(data_fa, "pearson", missing, pd_tol = pd_tol)
  
  if (verbose) {
    cat("  Polychoric: ", nrow(cor_poly), "x", ncol(cor_poly), "\n")
    cat("  Pearson: ", nrow(cor_pear), "x", ncol(cor_pear), "\n")
  }
  
  # Step 3: EFA with Polychoric correlation
  cat("\nStep 3: Factor Extraction and Rotation (Polychoric)\n")
  cat("----------------------------------------\n")
  cat("Extracting", n_factors, "factors...\n")
  
  efa_poly <- perform_efa(
    cor_matrix = cor_poly,
    n_factors = n_factors,
    fm = extraction_method,
    gamma_values = gamma_values,
    kaiser_normalize = kaiser_normalize,
    max_iter = max_iterations,
    flip_factors = flip_factors,
    promax_kappa_values = promax_kappa_values,
    verbose = verbose
  )
  
  # Step 4: EFA with Pearson correlation
  cat("\nStep 4: Factor Extraction and Rotation (Pearson)\n")
  cat("----------------------------------------\n")
  cat("Extracting", n_factors, "factors...\n")
  
  efa_pear <- perform_efa(
    cor_matrix = cor_pear,
    n_factors = n_factors,
    fm = extraction_method,
    gamma_values = gamma_values,
    kaiser_normalize = kaiser_normalize,
    max_iter = max_iterations,
    flip_factors = flip_factors,
    promax_kappa_values = promax_kappa_values,
    verbose = verbose
  )
  
  # Step 5: Align Pearson solution to Polychoric
  cat("\nStep 5: Aligning Pearson solution to Polychoric\n")
  cat("------------------------------------------------\n")
  
  efa_pear_aligned <- efa_pear
  
  # Align oblimin rotations
  for (gamma in gamma_values) {
    gamma_key <- paste0("gamma_", gsub("-", "neg", as.character(gamma)))
    
    poly_pattern <- efa_poly$rotations[[gamma_key]]$pattern
    pear_pattern <- efa_pear$rotations[[gamma_key]]$pattern
    
    alignment_result <- align_factors(poly_pattern, pear_pattern)
    factor_mapping <- alignment_result$factor_mapping
    
    efa_pear_aligned$rotations[[gamma_key]] <- 
      align_efa_solution(
        efa_poly$rotations[[gamma_key]],
        efa_pear$rotations[[gamma_key]],
        factor_mapping
      )
  }
  
  cat("  Oblimin alignment completed\n")
  
  # Align promax rotations
  if (!is.null(promax_kappa_values)) {
    for (kappa in promax_kappa_values) {
      kappa_key <- paste0("kappa_", kappa)
      
      poly_pattern <- efa_poly$rotations_promax[[kappa_key]]$pattern
      pear_pattern <- efa_pear$rotations_promax[[kappa_key]]$pattern
      
      alignment_result <- align_factors(poly_pattern, pear_pattern)
      factor_mapping <- alignment_result$factor_mapping
      
      efa_pear_aligned$rotations_promax[[kappa_key]] <- 
        align_efa_solution(
          efa_poly$rotations_promax[[kappa_key]],
          efa_pear$rotations_promax[[kappa_key]],
          factor_mapping
        )
    }
    
    cat("  Promax alignment completed\n")
  }
  
  # Step 6: Results integration
  results <- list(
    polychoric = list(
      correlation_matrix = cor_poly,
      efa = efa_poly
    ),
    pearson = list(
      correlation_matrix = cor_pear,
      efa = efa_pear_aligned
    ),
    data = data_fa,
    n_factors = n_factors,
    config_used = list(
      missing = missing,
      extraction_method = extraction_method,
      gamma_values = gamma_values,
      promax_kappa_values = promax_kappa_values,
      kaiser_normalize = kaiser_normalize,
      max_iterations = max_iterations,
      flip_factors = flip_factors,
      pd_tolerance = pd_tol
    )
  )
  
  # Step 7: Display Results
  cat("\nStep 6: Results\n")
  cat("----------------\n")
  
  if (show_full_results) {
    source("efa_display.R")
    display_efa_comparison(results)
  } else {
    cat("Full results display skipped (show_full_results = FALSE)\n")
  }
  
  # Return results
  cat("\n========================================\n")
  cat("EFA ANALYSIS COMPLETE\n")
  cat("Configuration used:\n")
  cat("  Missing data:", missing, "\n")
  cat("  Extraction method:", extraction_method, "\n")
  cat("  Kaiser normalization:", kaiser_normalize, "\n")
  cat("  Rotation: Oblimin (gamma:", paste(gamma_values, collapse = ", "), ")\n")
  if (!is.null(promax_kappa_values)) {
    cat("           Promax (kappa:", paste(promax_kappa_values, collapse = ", "), ")\n")
  }
  cat("  Correlation methods: Polychoric AND Pearson (aligned)\n")
  cat("========================================\n")
  
  invisible(results)
}

# Function to display specific results
show_efa <- function(results, gamma = 0) {
  source("efa_display.R")
  display_specific_result(results, gamma)
}

# Function to display EFA evaluation (all gamma values)
show_efa_evaluation <- function(data_obj, n_factors) {
  
  if (missing(n_factors)) {
    stop("n_factors is required. Run determine_factors() first to determine appropriate number.")
  }
  
  cat("Running EFA analysis for evaluation...\n\n")
  
  # Load configuration for thresholds
  config <- load_config()
  efa_eval_config <- config$analysis$efa_evaluation
  
  primary_threshold <- efa_eval_config$primary_threshold
  cross_threshold <- efa_eval_config$cross_threshold
  diff_threshold <- efa_eval_config$diff_threshold
  
  # Run EFA analysis quietly
  results <- analyze_efa(data_obj, 
                         n_factors = n_factors, 
                         verbose = FALSE,
                         show_full_results = FALSE)
  
  # Display evaluation with configured thresholds
  source("efa_display.R")
  all_evaluations <- display_efa_evaluation(results,
                                            primary_threshold = primary_threshold,
                                            cross_threshold = cross_threshold,
                                            diff_threshold = diff_threshold)
  
  # Export eligible pattern matrices to CSV
  output_dir <- "efa_output"
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  exported_count <- 0
  for (set_name in names(all_evaluations)) {
    set_info <- all_evaluations[[set_name]]
    
    if (set_info$evaluation$n_failed == 0) {
      # Get pattern matrix
      if (set_info$rotation_type == "oblimin") {
        gamma_key <- paste0("gamma_", gsub("-", "neg", as.character(set_info$param_value)))
        pattern <- set_info$results$efa$rotations[[gamma_key]]$pattern
        filename <- sprintf("pattern_%s_Oblimin_gamma_%.2f.csv",
                            set_info$cor_type, set_info$param_value)
      } else {
        kappa_key <- paste0("kappa_", set_info$param_value)
        pattern <- set_info$results$efa$rotations_promax[[kappa_key]]$pattern
        filename <- sprintf("pattern_%s_Promax_kappa_%d.csv",
                            set_info$cor_type, set_info$param_value)
      }
      
      # Round to 3 decimal places
      pattern_rounded <- round(pattern, 3)
      
      # Export to CSV
      filepath <- file.path(output_dir, filename)
      write.csv(pattern_rounded, filepath)
      cat(sprintf("Exported: %s\n", filepath))
      exported_count <- exported_count + 1
    }
  }
  
  if (exported_count > 0) {
    cat(sprintf("\nTotal %d pattern matrix file(s) exported to %s/\n", 
                exported_count, output_dir))
  } else {
    cat("\nNo eligible pattern matrices to export.\n")
  }
  
  invisible(NULL)
}
