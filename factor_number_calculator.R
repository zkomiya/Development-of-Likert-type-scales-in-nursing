# ===================================================
# Factor Number Determination for EFA
# Description: Factor number determination for Exploratory Factor Analysis
# ===================================================

library(psych)
library(EFA.MRFA)

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
  # Step 1: Parallel Analysis - MRFA
  # ========================================
  if (verbose) {
    cat("[1/4] Running Parallel Analysis (MRFA)...\n")
    cat("      (This may take a while with", n_iterations, "datasets)\n")
    flush.console()
  }
  
  mrfa_start <- Sys.time()
  
  mrfa_result <- parallelMRFA(
    X = data,
    Ndatsets = n_iterations,
    percent = percentile,
    corr = "Polychoric",
    display = FALSE,
    graph = FALSE
  )
  
  mrfa_end <- Sys.time()
  
  if (verbose) {
    cat("      Completed in", round(difftime(mrfa_end, mrfa_start, units = "secs"), 1), "seconds\n\n")
    flush.console()
  }
  
  # ========================================
  # Step 2: Parallel Analysis (FA + PCA)
  # ========================================
  if (verbose) {
    cat("[2/4] Running Parallel Analysis (FA + PCA)...\n")
    cat("      (This may take a while with", n_iterations, "iterations)\n")
    flush.console()
  }
  
  pa_start <- Sys.time()
  
  pa_result <- fa.parallel(
    x = data,
    n.iter = n_iterations,
    cor = "poly",
    fa = "both",
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
  # Step 3: VSS for MAP test
  # ========================================
  if (verbose) {
    cat("[3/4] Running VSS (MAP test)...\n")
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
  # Step 4: Kaiser's Criterion
  # ========================================
  if (verbose) {
    cat("[4/4] Calculating Kaiser's criterion...\n")
    flush.console()
  }
  
  # Kaiser criterion uses PCA eigenvalues (pc.values) with >1 threshold
  pc_eigenvalues <- pa_result$pc.values
  kaiser_n <- sum(pc_eigenvalues > 1)
  
  if (verbose) {
    cat("      Completed\n\n")
    cat("========================================\n")
    cat("All calculations finished!\n")
    cat("Total time:", round(difftime(vss_end, mrfa_start, units = "secs"), 1), "seconds\n")
    cat("========================================\n\n")
    flush.console()
  }
  
  # ========================================
  # Step 5: Format Results
  # ========================================
  
  # --- PA-MRFA ---
  mrfa_n_vars <- length(mrfa_result$Real_Data)
  pa_mrfa_formatted <- list(
    n_factors = mrfa_result$N_factors_percentiles,
    n_factors_mean = mrfa_result$N_factors_mean,
    eigen_table = data.frame(
      Factor = 1:mrfa_n_vars,
      Real_Pct = round(mrfa_result$Real_Data, 3),
      Simulated_Pct = round(mrfa_result$Percentile_random, 3),
      Simulated_Mean_Pct = round(mrfa_result$Mean_random, 3),
      Retain = 1:mrfa_n_vars <= mrfa_result$N_factors_percentiles,
      Retain_mean = 1:mrfa_n_vars <= mrfa_result$N_factors_mean
    )
  )
  
  # --- FA-based PA ---
  fa_eigenvalues <- pa_result$fa.values
  pa_sim_mean <- pa_result$fa.sim
  
  # Quantile threshold for simulated FA eigenvalues
  vals <- pa_result$values
  cn <- colnames(vals)
  sim_cols <- which(grepl("sim", cn, ignore.case = TRUE) & grepl("fa", cn, ignore.case = TRUE))
  qv <- apply(vals[, sim_cols, drop = FALSE], 2, quantile, probs = percentile / 100, na.rm = TRUE)
  pa_sim_quant <- rep(NA_real_, length(fa_eigenvalues))
  pa_sim_quant[1:min(length(pa_sim_quant), length(qv))] <- as.numeric(qv[1:min(length(pa_sim_quant), length(qv))])
  
  # Suggested factor numbers for both rules
  pa_n_mean <- sum(fa_eigenvalues > pa_sim_mean[1:min(length(fa_eigenvalues), length(pa_sim_mean))])
  pa_n_quant <- pa_result$nfact
  
  pa_formatted <- list(
    # n_factors is quantile-based (matches psych::fa.parallel behavior)
    n_factors = pa_n_quant,
    n_factors_mean = pa_n_mean,
    eigen_table = data.frame(
      Factor = 1:min(length(fa_eigenvalues), length(pa_sim_mean)),
      Real = round(fa_eigenvalues[1:min(length(fa_eigenvalues), length(pa_sim_mean))], 3),
      Simulated = round(pa_sim_quant[1:min(length(fa_eigenvalues), length(pa_sim_mean))], 3),
      Simulated_mean = round(pa_sim_mean[1:min(length(fa_eigenvalues), length(pa_sim_mean))], 3),
      Retain = 1:min(length(fa_eigenvalues), length(pa_sim_mean)) <= pa_n_quant,
      Retain_mean = 1:min(length(fa_eigenvalues), length(pa_sim_mean)) <= pa_n_mean
    )
  )
  
  # --- PCA-based PA ---
  pc_sim_mean <- pa_result$pc.sim
  
  # Quantile threshold for simulated PCA eigenvalues
  sim_pc_cols <- which(grepl("sim", cn, ignore.case = TRUE) & grepl("pc", cn, ignore.case = TRUE))
  qv_pc <- apply(vals[, sim_pc_cols, drop = FALSE], 2, quantile, probs = percentile / 100, na.rm = TRUE)
  pc_sim_quant <- rep(NA_real_, length(pc_eigenvalues))
  pc_sim_quant[1:min(length(pc_sim_quant), length(qv_pc))] <- as.numeric(qv_pc[1:min(length(pc_sim_quant), length(qv_pc))])
  
  # Suggested component numbers for both rules
  pa_pca_n_quant <- pa_result$ncomp
  pa_pca_n_mean <- sum(pc_eigenvalues > pc_sim_mean[1:min(length(pc_eigenvalues), length(pc_sim_mean))])
  
  pa_pca_len <- min(length(pc_eigenvalues), length(pc_sim_mean))
  pa_pca_formatted <- list(
    n_factors = pa_pca_n_quant,
    n_factors_mean = pa_pca_n_mean,
    eigen_table = data.frame(
      Component = 1:pa_pca_len,
      Real = round(pc_eigenvalues[1:pa_pca_len], 3),
      Simulated = round(pc_sim_quant[1:pa_pca_len], 3),
      Simulated_mean = round(pc_sim_mean[1:pa_pca_len], 3),
      Retain = 1:pa_pca_len <= pa_pca_n_quant,
      Retain_mean = 1:pa_pca_len <= pa_pca_n_mean
    )
  )
  
  # --- MAP ---
  map_values <- vss_result$map
  map_n <- which.min(map_values) - 1
  
  map_result <- list(
    n_factors = map_n,
    map_table = data.frame(
      Factors = 0:(length(map_values) - 1),
      MAP = round(map_values, 6)
    )
  )
  
  # --- Kaiser ---
  kaiser_result <- list(
    n_factors = kaiser_n,
    eigenvalues = pc_eigenvalues,
    eigen_table = data.frame(
      Factor = 1:length(pc_eigenvalues),
      Eigenvalue = round(pc_eigenvalues, 3),
      Retain = pc_eigenvalues > 1
    )
  )
  
  # ========================================
  # Return results
  # ========================================
  conditions <- list(
    correlation_method = "Polychoric",
    extraction_method_pa_mrfa = "MRFA",
    extraction_method_pa = "minres",
    pa_mrfa_datasets = n_iterations,
    pa_mrfa_random_method = "Permutation of the raw data",
    pa_iterations = n_iterations,
    pa_percentile = percentile,
    pa_fa_reference = "Mean and quantile (FA simulated)",
    pa_pca_reference = "Mean and quantile (PCA simulated)",
    missing_method = missing_method,
    scale_min = scale_min,
    scale_max = scale_max,
    kaiser_eigenvalue_source = "PCA"
  )
  
  return(list(
    pa_mrfa = pa_mrfa_formatted,
    kaiser = kaiser_result,
    pa = pa_formatted,
    pa_pca = pa_pca_formatted,
    map = map_result,
    conditions = conditions,
    cor_matrix = pa_result$r,
    n_obs = n_obs,
    n_vars = n_vars
  ))
}