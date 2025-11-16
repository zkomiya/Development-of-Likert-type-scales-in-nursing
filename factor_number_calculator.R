# ===================================================
# Factor Number Determination for EFA
# Version: 5.0 (Facts only - No recommendations)
# Description: Factor number determination for Exploratory Factor Analysis
# ===================================================

library(psych)

determine_n_factors <- function(data,
                                cor_matrix = NULL,
                                use_revised_map = FALSE,
                                n_iterations,  
                                percentile,
                                seed = NULL,
                                verbose = TRUE) {
  
  if (!is.null(seed)) set.seed(seed)
  
  if (verbose) {
    cat("\n========================================\n")
    cat("FACTOR NUMBER DETERMINATION\n")
    cat("========================================\n\n")
  }
  
  n_vars <- ncol(data)
  n_obs <- nrow(data)
  
  # ========================================
  # Step 1: Parallel Analysis for FACTOR ANALYSIS
  # ========================================
  if (verbose) {
    cat("Step 1: Parallel Analysis (Factor Analysis)\n")
    cat("--------------------------------------------\n")
    cat("  Variables:", n_vars, "\n")
    cat("  Observations:", n_obs, "\n")
    cat("  Iterations:", n_iterations, "\n")
    cat("  Method: Factor Analysis (not PCA)\n")
    cat("  Computing... ")
    start_time <- Sys.time()
  }
  
  pa_result <- fa.parallel(
    x = data,
    n.iter = n_iterations,
    cor = "poly",
    fa = "fa",
    fm = "minres",
    quant = percentile/100,
    plot = FALSE,
    main = "",
    show.legend = FALSE
  )
  
  if (verbose) {
    elapsed <- round(difftime(Sys.time(), start_time, units = "secs"), 1)
    cat("Done (", elapsed, "seconds)\n")
    
    if (!is.null(pa_result$nfact)) {
      cat("  Suggested number of factors:", pa_result$nfact, "\n")
    }
    cat("\n")
  }
  
  # ========================================
  # Step 2: VSS for MAP test
  # ========================================
  if (verbose) {
    cat("Step 2: MAP Test (Velicer's MAP)\n")
    cat("---------------------------------\n")
    cat("  Computing VSS... ")
    start_time <- Sys.time()
  }
  
  max_factors <- min(n_vars - 1, 10)
  vss_result <- VSS(
    x = data,
    n = max_factors,
    cor = "poly",
    fm = "minres",
    plot = FALSE
  )
  
  if (verbose) {
    elapsed <- round(difftime(Sys.time(), start_time, units = "secs"), 1)
    cat("Done (", elapsed, "seconds)\n\n")
  }
  
  # ========================================
  # Step 3: Kaiser's Criterion
  # ========================================
  if (verbose) {
    cat("Step 3: Kaiser's Criterion\n")
    cat("--------------------------\n")
  }
  
  eigenvalues <- pa_result$pc.values
  kaiser_n <- sum(eigenvalues > 1)
  
  if (verbose) {
    cat("  Eigenvalues > 1:", kaiser_n, "\n")
    cat("  All eigenvalues:\n")
    
    eigen_df <- data.frame(
      Factor = 1:length(eigenvalues),
      Eigenvalue = round(eigenvalues, 3),
      Cumulative_Var = round(cumsum(eigenvalues) / sum(eigenvalues) * 100, 1)
    )
    
    for (i in 1:nrow(eigen_df)) {
      cat(sprintf("    F%02d: %6.3f (cum: %5.1f%%)", 
                  eigen_df$Factor[i], 
                  eigen_df$Eigenvalue[i], 
                  eigen_df$Cumulative_Var[i]))
      if (eigen_df$Eigenvalue[i] > 1) cat(" *")
      cat("\n")
    }
    cat("\n")
  }
  
  # ========================================
  # Step 4: Extract and Format Results
  # ========================================
  
  pa_n <- pa_result$nfact
  pa_sim_eigenvalues <- pa_result$fa.sim
  
  map_values <- vss_result$map
  map_n <- which.min(map_values)
  
  kaiser_result <- list(
    n_factors = kaiser_n,
    eigenvalues = eigenvalues,
    eigen_table = data.frame(
      Factor = 1:length(eigenvalues),
      Eigenvalue = round(eigenvalues, 3),
      Retain = eigenvalues > 1
    )
  )
  
  pa_formatted <- list(
    suggested = pa_n,
    n_factors = pa_n,
    eigen_table = data.frame(
      Factor = 1:min(length(eigenvalues), length(pa_sim_eigenvalues)),
      Real = round(eigenvalues[1:min(length(eigenvalues), length(pa_sim_eigenvalues))], 3),
      Simulated_95th = round(pa_sim_eigenvalues[1:min(length(eigenvalues), length(pa_sim_eigenvalues))], 3),
      Retain = 1:min(length(eigenvalues), length(pa_sim_eigenvalues)) <= pa_n
    )
  )
  
  map_result <- list(
    suggested = map_n,
    n_factors = map_n,
    map_table = data.frame(
      Factors = 0:(length(map_values) - 1),
      MAP = round(map_values, 6)
    )
  )
  
  # ========================================
  # Step 5: Display Results (Facts Only)
  # ========================================
  if (verbose) {
    cat("========================================\n")
    cat("RESULTS\n")
    cat("========================================\n")
    cat("Kaiser's criterion:      ", kaiser_n, "factors\n")
    cat("Parallel Analysis (FA):  ", pa_n, "factors\n")
    cat("MAP test:               ", map_n, "factors\n")
    cat("========================================\n\n")
  }
  
  # ========================================
  # Return results
  # ========================================
  return(list(
    kaiser = kaiser_result,
    pa = pa_formatted,
    map = map_result,
    map_original = map_result,
    cor_matrix = pa_result$r
  ))
}