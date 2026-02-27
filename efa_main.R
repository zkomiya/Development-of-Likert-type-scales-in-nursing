# ===================================================
# EFA Main Controller
# Version: 23.0 - n_factors read from yaml as n_factors_list
# Changes from v22.0:
#   - analyze_efa(): removed n_factors argument
#   - analyze_efa(): reads n_factors_list from analysis_config.yaml
#   - analyze_efa(): outer loop over n_factors_list, inner loop over extraction_methods
#   - analyze_efa(): return structure changed to results[[n_key]][[fm]]
#   - show_efa_evaluation(): removed n_factors argument
#   - show_efa_evaluation(): reads n_factors_list from yaml via analyze_efa
#   - Output headers updated per proposal (======== per n_factors, ######## per method)
# ===================================================

# Main EFA analysis function
analyze_efa <- function(data_obj,
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
  extraction_methods <- unlist(efa_config$extraction_methods)
  n_factors_list <- unlist(efa_config$n_factors_list)
  gamma_values <- efa_config$gamma_values
  kaiser_normalize <- efa_config$kaiser_normalize
  max_iterations <- efa_config$max_iterations
  flip_factors <- efa_config$flip_factors
  promax_kappa_values <- efa_config$promax_kappa_values
  display_cutoff <- efa_config$display_cutoff
  pd_tol <- efa_config$pd_tolerance
  if (is.null(pd_tol)) {
    stop("EFA setting pd_tolerance not found in analysis_config.yaml")
  }
  pd_tol <- as.numeric(pd_tol)
  
  if (is.null(extraction_methods) || length(extraction_methods) == 0) {
    stop("extraction_methods not found or empty in analysis_config.yaml")
  }
  
  if (is.null(n_factors_list) || length(n_factors_list) == 0) {
    stop("n_factors_list not found or empty in analysis_config.yaml")
  }
  
  # Read global parameters from YAML
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
    cat("  Extraction methods:", paste(extraction_methods, collapse = ", "), "\n")
    cat("  n_factors_list:", paste(n_factors_list, collapse = ", "), "\n")
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
    if (!is.null(display_cutoff)) {
      cat("  Display cutoff:", display_cutoff, "\n")
    } else {
      cat("  Display cutoff: none (show all values)\n")
    }
    cat("  Correlation methods: Polychoric AND Pearson\n")
  }
  
  # Validate missing parameter
  if (!missing %in% c("listwise", "pairwise")) {
    stop("missing must be 'listwise' or 'pairwise'")
  }
  
  # Step 1: Data Preprocessing (once, shared across all n_factors and methods)
  cat("\nStep 1: Data Preprocessing\n")
  cat("----------------------------\n")
  
  source("data_preprocessor.R")
  data_fa <- preprocess_for_fa(data,
                               method = missing,
                               verbose = verbose,
                               item_pattern = item_pattern,
                               scale_min = scale_min,
                               scale_max = scale_max)
  
  # Step 2: Compute correlation matrices (once, shared across all n_factors and methods)
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
  
  # Outer loop: n_factors
  all_results <- list()
  
  for (n in n_factors_list) {
    
    # Outer header per n_factors
    cat("\n========================================\n")
    cat(sprintf("EFA ANALYSIS: n_factors = %d\n", n))
    cat(sprintf("  Missing: %s\n", missing))
    cat(sprintf("  Extraction methods: %s\n", paste(extraction_methods, collapse = ", ")))
    cat(sprintf("  Gamma values: %s\n", paste(gamma_values, collapse = ", ")))
    if (!is.null(promax_kappa_values)) {
      cat(sprintf("  Promax kappa values: %s\n", paste(promax_kappa_values, collapse = ", ")))
    }
    cat(sprintf("  Kaiser normalization: %s\n", kaiser_normalize))
    cat("  Correlation methods: Polychoric AND Pearson\n")
    cat(sprintf("  PD tolerance: %g\n", pd_tol))
    if (!is.null(display_cutoff)) {
      cat(sprintf("  Display cutoff: %s\n", display_cutoff))
    } else {
      cat("  Display cutoff: none\n")
    }
    cat("========================================\n")
    
    # Validate n_factors against number of variables
    if (n > ncol(data_fa)) {
      stop("n_factors (", n, ") cannot exceed number of variables (", ncol(data_fa), ")")
    }
    
    n_key <- paste0("n", n)
    method_results <- list()
    
    for (fm in extraction_methods) {
      
      # Step 3: EFA with Polychoric correlation
      cat(sprintf("\nStep 3 [n=%d | %s]: Factor Extraction and Rotation (Polychoric)\n", n, fm))
      cat("----------------------------------------\n")
      cat("Extracting", n, "factors...\n")
      
      efa_poly <- perform_efa(
        cor_matrix = cor_poly,
        n_factors = n,
        fm = fm,
        gamma_values = gamma_values,
        kaiser_normalize = kaiser_normalize,
        max_iter = max_iterations,
        flip_factors = flip_factors,
        promax_kappa_values = promax_kappa_values,
        verbose = verbose
      )
      
      # Step 4: EFA with Pearson correlation
      cat(sprintf("\nStep 4 [n=%d | %s]: Factor Extraction and Rotation (Pearson)\n", n, fm))
      cat("----------------------------------------\n")
      cat("Extracting", n, "factors...\n")
      
      efa_pear <- perform_efa(
        cor_matrix = cor_pear,
        n_factors = n,
        fm = fm,
        gamma_values = gamma_values,
        kaiser_normalize = kaiser_normalize,
        max_iter = max_iterations,
        flip_factors = flip_factors,
        promax_kappa_values = promax_kappa_values,
        verbose = verbose
      )
      
      # Step 5: Align Pearson solution to Polychoric
      cat(sprintf("\nStep 5 [n=%d | %s]: Aligning Pearson solution to Polychoric\n", n, fm))
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
      
      # Store per-method results
      method_result <- list(
        polychoric = list(
          correlation_matrix = cor_poly,
          efa = efa_poly
        ),
        pearson = list(
          correlation_matrix = cor_pear,
          efa = efa_pear_aligned
        ),
        n_factors = n,
        config_used = list(
          missing = missing,
          extraction_method = fm,
          gamma_values = gamma_values,
          promax_kappa_values = promax_kappa_values,
          kaiser_normalize = kaiser_normalize,
          max_iterations = max_iterations,
          flip_factors = flip_factors,
          pd_tolerance = pd_tol,
          display_cutoff = display_cutoff
        )
      )
      
      method_results[[fm]] <- method_result
    }
    
    all_results[[n_key]] <- method_results
    
    # Step 6: Display Results for this n_factors
    cat(sprintf("\nStep 6 [n=%d]: Results\n", n))
    cat("----------------\n")
    
    if (show_full_results) {
      source("efa_display.R")
      for (fm in extraction_methods) {
        display_efa_comparison(all_results[[n_key]][[fm]], display_cutoff,
                               method_name = fm, n_factors = n)
      }
    } else {
      cat("Full results display skipped (show_full_results = FALSE)\n")
    }
  }
  
  # Final summary
  cat("\n========================================\n")
  cat("EFA ANALYSIS COMPLETE\n")
  cat(sprintf("  n_factors tested: %s\n", paste(n_factors_list, collapse = ", ")))
  cat(sprintf("  Extraction methods: %s\n", paste(extraction_methods, collapse = ", ")))
  cat(sprintf("  Missing: %s\n", missing))
  cat(sprintf("  Gamma values: %s\n", paste(gamma_values, collapse = ", ")))
  if (!is.null(promax_kappa_values)) {
    cat(sprintf("  Promax kappa values: %s\n", paste(promax_kappa_values, collapse = ", ")))
  }
  cat(sprintf("  Kaiser normalization: %s\n", kaiser_normalize))
  cat(sprintf("  PD tolerance: %g\n", pd_tol))
  if (!is.null(display_cutoff)) {
    cat(sprintf("  Display cutoff: %s\n", display_cutoff))
  } else {
    cat("  Display cutoff: none\n")
  }
  cat("========================================\n")
  
  # Build return value
  results <- all_results
  results$n_factors_list <- n_factors_list
  results$data <- data_fa
  results$config_used <- list(
    missing = missing,
    extraction_methods = extraction_methods,
    n_factors_list = n_factors_list,
    gamma_values = gamma_values,
    promax_kappa_values = promax_kappa_values,
    kaiser_normalize = kaiser_normalize,
    max_iterations = max_iterations,
    flip_factors = flip_factors,
    pd_tolerance = pd_tol,
    display_cutoff = display_cutoff
  )
  
  invisible(results)
}

# Function to display EFA evaluation (all n_factors and extraction methods)
show_efa_evaluation <- function(data_obj) {
  
  cat("Running EFA analysis for evaluation...\n\n")
  
  # Load configuration for thresholds and display_cutoff
  config <- load_config()
  efa_eval_config <- config$analysis$efa_evaluation
  efa_config <- config$analysis$efa_settings
  
  primary_threshold <- efa_eval_config$primary_threshold
  cross_threshold <- efa_eval_config$cross_threshold
  diff_threshold <- efa_eval_config$diff_threshold
  display_cutoff <- efa_config$display_cutoff
  extraction_methods <- unlist(efa_config$extraction_methods)
  
  # Run EFA analysis quietly
  results <- analyze_efa(data_obj,
                         verbose = FALSE,
                         show_full_results = FALSE)
  
  n_factors_list <- results$n_factors_list
  
  # Output directory for CSV export
  output_dir <- "efa_output"
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  exported_count <- 0
  
  source("efa_display.R")
  
  for (n in n_factors_list) {
    n_key <- paste0("n", n)
    
    for (fm in extraction_methods) {
      
      all_evaluations <- display_efa_evaluation(
        results[[n_key]][[fm]],
        primary_threshold = primary_threshold,
        cross_threshold = cross_threshold,
        diff_threshold = diff_threshold,
        display_cutoff = display_cutoff,
        method_name = fm,
        n_factors = n
      )
      
      # Export eligible pattern matrices to CSV
      for (set_name in names(all_evaluations)) {
        set_info <- all_evaluations[[set_name]]
        
        if (set_info$evaluation$n_failed == 0) {
          # Get pattern matrix
          if (set_info$rotation_type == "oblimin") {
            gamma_key <- paste0("gamma_", gsub("-", "neg", as.character(set_info$param_value)))
            pattern <- set_info$results$efa$rotations[[gamma_key]]$pattern
            filename <- sprintf("pattern_n%d_%s_%s_Oblimin_gamma_%.2f.csv",
                                n, fm, set_info$cor_type, set_info$param_value)
          } else {
            kappa_key <- paste0("kappa_", set_info$param_value)
            pattern <- set_info$results$efa$rotations_promax[[kappa_key]]$pattern
            filename <- sprintf("pattern_n%d_%s_%s_Promax_kappa_%d.csv",
                                n, fm, set_info$cor_type, set_info$param_value)
          }
          
          pattern_rounded <- round(pattern, 3)
          filepath <- file.path(output_dir, filename)
          write.csv(pattern_rounded, filepath)
          cat(sprintf("Exported: %s\n", filepath))
          exported_count <- exported_count + 1
        }
      }
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