# ===================================================
# Factor Number Determination for EFA
# Version: 7.1 (Added calculation conditions to output)
# Description: Factor number determination for Exploratory Factor Analysis
# ===================================================

library(psych)

determine_n_factors <- function(data,
                                n_iterations,
                                percentile,
                                seed = NULL,
                                verbose = TRUE,
                                missing_method = NULL,
                                scale_min = NULL,
                                scale_max = NULL) {
  
  if (!is.null(seed)) set.seed(seed)
  
  n_vars <- ncol(data)
  n_obs <- nrow(data)
  max_factors <- min(n_vars - 1, 10)
  
  if (verbose) {
    cat("\n========================================\n")
    cat("FACTOR NUMBER DETERMINATION - PROGRESS\n")
    cat("========================================\n")
    cat("Variables:", n_vars, "\n")
    cat("Observations:", n_obs, "\n")
    cat("Iterations:", n_iterations, "\n\n")
  }
  
  # ========================================
  # Step 1: Parallel Analysis for FACTOR ANALYSIS
  # ========================================
  if (verbose) {
    cat("[1/3] Running Parallel Analysis...\n")
    cat("      (This may take a while with", n_iterations, "iterations)\n")
    flush.console()
  }
  
  pa_start <- Sys.time()
  
  pa_result <- fa.parallel(
    x = data,
    n.iter = n_iterations,
    cor = "poly",
    fa = "fa",
    fm = "minres",
    quant = percentile / 100,
    plot = FALSE,
    main = "",
    show.legend = FALSE
  )
  
  pa_end <- Sys.time()
  
  if (verbose) {
    cat("      Completed in", round(difftime(pa_end, pa_start, units = "secs"), 1), "seconds\n\n")
    flush.console()
  }
  
  # ========================================
  # Step 2: VSS for MAP test
  # ========================================
  if (verbose) {
    cat("[2/3] Running VSS (MAP test)...\n")
    cat("      Testing", max_factors, "factor solutions\n")
    flush.console()
  }
  
  vss_start <- Sys.time()
  
  vss_result <- VSS(
    x = data,
    n = max_factors,
    cor = "poly",
    fm = "minres",
    plot = FALSE
  )
  
  vss_end <- Sys.time()
  
  if (verbose) {
    cat("      Completed in", round(difftime(vss_end, vss_start, units = "secs"), 1), "seconds\n\n")
    flush.console()
  }
  
  # ========================================
  # Step 3: Kaiser's Criterion
  # ========================================
  if (verbose) {
    cat("[3/3] Calculating Kaiser's criterion...\n")
    flush.console()
  }
  
  # Kaiser criterion uses PCA eigenvalues (pc.values) with >1 threshold
  eigenvalues    <- pa_result$pc.values   # Kaiser criterion (PCA eigenvalues, >1 threshold)
  # PA comparison table uses FA eigenvalues: fa.values (Real) vs fa.sim (Simulated)
  # because fa.parallel() was run with fa = "fa" (polychoric + minres)
  fa_eigenvalues <- pa_result$fa.values   # PA table: real FA eigenvalues
  kaiser_n <- sum(eigenvalues > 1)
  
  if (verbose) {
    cat("      Completed\n\n")
    cat("========================================\n")
    cat("All calculations finished!\n")
    cat("Total time:", round(difftime(vss_end, pa_start, units = "secs"), 1), "seconds\n")
    cat("========================================\n\n")
    flush.console()
  }
  
  # ========================================
  # Step 4: Format Results
  # ========================================
  pa_n <- pa_result$nfact
  pa_sim_eigenvalues <- pa_result$fa.sim  # PA table: simulated FA eigenvalues (must match fa.values)
  
  map_values <- vss_result$map
  map_n <- which.min(map_values) - 1
  
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
    n_factors = pa_n,
    # Real = fa.values, Simulated = fa.sim: both are FA-side, consistent comparison
    eigen_table = data.frame(
      Factor    = 1:min(length(fa_eigenvalues), length(pa_sim_eigenvalues)),
      Real      = round(fa_eigenvalues[1:min(length(fa_eigenvalues), length(pa_sim_eigenvalues))], 3),
      Simulated = round(pa_sim_eigenvalues[1:min(length(fa_eigenvalues), length(pa_sim_eigenvalues))], 3),
      Retain    = 1:min(length(fa_eigenvalues), length(pa_sim_eigenvalues)) <= pa_n
    )
  )
  
  map_result <- list(
    n_factors = map_n,
    map_table = data.frame(
      Factors = 0:(length(map_values) - 1),
      MAP = round(map_values, 6)
    )
  )
  
  # ========================================
  # Return results
  # ========================================
  conditions <- list(
    correlation_method = "Polychoric",
    extraction_method_pa = "minres",
    pa_iterations = n_iterations,
    pa_percentile = percentile,
    missing_method = missing_method,
    scale_min = scale_min,
    scale_max = scale_max,
    kaiser_eigenvalue_source = "PCA"
  )
  
  return(list(
    kaiser = kaiser_result,
    pa = pa_formatted,
    map = map_result,
    conditions = conditions,
    cor_matrix = pa_result$r,
    n_obs = n_obs,
    n_vars = n_vars
  ))
}