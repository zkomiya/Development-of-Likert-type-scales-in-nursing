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
  # Step 1: Parallel Analysis - MRFA (Polychoric)
  # ========================================
  if (verbose) {
    cat("[1/8] Running Parallel Analysis (MRFA) - Polychoric...\n")
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
  # Step 2: Parallel Analysis (FA + PCA) (Polychoric)
  # ========================================
  if (verbose) {
    cat("[2/8] Running Parallel Analysis (FA + PCA) - Polychoric...\n")
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
  # Step 3: VSS for MAP test (Polychoric)
  # ========================================
  if (verbose) {
    cat("[3/8] Running VSS (MAP test) - Polychoric...\n")
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
  # Step 4: Kaiser's Criterion (Polychoric)
  # ========================================
  if (verbose) {
    cat("[4/8] Calculating Kaiser's criterion - Polychoric...\n")
    flush.console()
  }
  
  # Kaiser criterion uses PCA eigenvalues (pc.values) with >1 threshold
  pc_eigenvalues <- pa_result$pc.values
  kaiser_n <- sum(pc_eigenvalues > 1)
  
  if (verbose) {
    cat("      Completed\n\n")
    flush.console()
  }
  
  # ========================================
  # Step 5: Parallel Analysis - MRFA (Pearson)
  # ========================================
  if (verbose) {
    cat("[5/8] Running Parallel Analysis (MRFA) - Pearson...\n")
    cat("      (This may take a while with", n_iterations, "datasets)\n")
    flush.console()
  }
  
  mrfa_pear_start <- Sys.time()
  
  mrfa_result_pearson <- parallelMRFA(
    X = data,
    Ndatsets = n_iterations,
    percent = percentile,
    corr = "Pearson",
    display = FALSE,
    graph = FALSE
  )
  
  mrfa_pear_end <- Sys.time()
  
  if (verbose) {
    cat("      Completed in", round(difftime(mrfa_pear_end, mrfa_pear_start, units = "secs"), 1), "seconds\n\n")
    flush.console()
  }
  
  # ========================================
  # Step 6: Parallel Analysis (FA + PCA) (Pearson)
  # ========================================
  if (verbose) {
    cat("[6/8] Running Parallel Analysis (FA + PCA) - Pearson...\n")
    cat("      (This may take a while with", n_iterations, "iterations)\n")
    flush.console()
  }
  
  pa_pear_start <- Sys.time()
  
  pa_result_pearson <- fa.parallel(
    x = data,
    n.iter = n_iterations,
    cor = "cor",
    fa = "both",
    fm = "minres",
    quant = percentile / 100,
    plot = FALSE,
    main = "",
    show.legend = FALSE
  )
  
  pa_pear_end <- Sys.time()
  
  if (verbose) {
    cat("      Completed in", round(difftime(pa_pear_end, pa_pear_start, units = "secs"), 1), "seconds\n\n")
    flush.console()
  }
  
  # ========================================
  # Step 7: VSS for MAP test (Pearson)
  # ========================================
  if (verbose) {
    cat("[7/8] Running VSS (MAP test) - Pearson...\n")
    cat("      Testing", max_factors, "factor solutions\n")
    flush.console()
  }
  
  vss_pear_start <- Sys.time()
  
  vss_result_pearson <- VSS(
    x = data,
    n = max_factors,
    cor = "cor",
    fm = "minres",
    plot = FALSE
  )
  
  vss_pear_end <- Sys.time()
  
  if (verbose) {
    cat("      Completed in", round(difftime(vss_pear_end, vss_pear_start, units = "secs"), 1), "seconds\n\n")
    flush.console()
  }
  
  # ========================================
  # Step 8: Kaiser's Criterion (Pearson)
  # ========================================
  if (verbose) {
    cat("[8/8] Calculating Kaiser's criterion - Pearson...\n")
    flush.console()
  }
  
  pc_eigenvalues_pearson <- pa_result_pearson$pc.values
  kaiser_n_pearson <- sum(pc_eigenvalues_pearson > 1)
  
  if (verbose) {
    cat("      Completed\n\n")
    cat("========================================\n")
    cat("All calculations finished!\n")
    cat("Total time:", round(difftime(vss_pear_end, mrfa_start, units = "secs"), 1), "seconds\n")
    cat("========================================\n\n")
    flush.console()
  }
  
  # ========================================
  # Format Results - Polychoric
  # ========================================
  
  # --- PA-MRFA (Polychoric) ---
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
  
  # --- FA-based PA (Polychoric) ---
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
  
  # --- PCA-based PA (Polychoric) ---
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
  
  # --- MAP (Polychoric) ---
  map_values <- vss_result$map
  map_n <- which.min(map_values) - 1
  
  map_result <- list(
    n_factors = map_n,
    map_table = data.frame(
      Factors = 0:(length(map_values) - 1),
      MAP = round(map_values, 6)
    )
  )
  
  # --- Kaiser (Polychoric) ---
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
  # Format Results - Pearson
  # ========================================
  
  # --- PA-MRFA (Pearson) ---
  mrfa_n_vars_pear <- length(mrfa_result_pearson$Real_Data)
  pa_mrfa_pearson_formatted <- list(
    n_factors = mrfa_result_pearson$N_factors_percentiles,
    n_factors_mean = mrfa_result_pearson$N_factors_mean,
    eigen_table = data.frame(
      Factor = 1:mrfa_n_vars_pear,
      Real_Pct = round(mrfa_result_pearson$Real_Data, 3),
      Simulated_Pct = round(mrfa_result_pearson$Percentile_random, 3),
      Simulated_Mean_Pct = round(mrfa_result_pearson$Mean_random, 3),
      Retain = 1:mrfa_n_vars_pear <= mrfa_result_pearson$N_factors_percentiles,
      Retain_mean = 1:mrfa_n_vars_pear <= mrfa_result_pearson$N_factors_mean
    )
  )
  
  # --- FA-based PA (Pearson) ---
  fa_eigenvalues_pear <- pa_result_pearson$fa.values
  pa_sim_mean_pear <- pa_result_pearson$fa.sim
  
  vals_pear <- pa_result_pearson$values
  cn_pear <- colnames(vals_pear)
  sim_cols_pear <- which(grepl("sim", cn_pear, ignore.case = TRUE) & grepl("fa", cn_pear, ignore.case = TRUE))
  qv_pear <- apply(vals_pear[, sim_cols_pear, drop = FALSE], 2, quantile, probs = percentile / 100, na.rm = TRUE)
  pa_sim_quant_pear <- rep(NA_real_, length(fa_eigenvalues_pear))
  pa_sim_quant_pear[1:min(length(pa_sim_quant_pear), length(qv_pear))] <- as.numeric(qv_pear[1:min(length(pa_sim_quant_pear), length(qv_pear))])
  
  pa_n_mean_pear <- sum(fa_eigenvalues_pear > pa_sim_mean_pear[1:min(length(fa_eigenvalues_pear), length(pa_sim_mean_pear))])
  pa_n_quant_pear <- pa_result_pearson$nfact
  
  pa_pearson_formatted <- list(
    n_factors = pa_n_quant_pear,
    n_factors_mean = pa_n_mean_pear,
    eigen_table = data.frame(
      Factor = 1:min(length(fa_eigenvalues_pear), length(pa_sim_mean_pear)),
      Real = round(fa_eigenvalues_pear[1:min(length(fa_eigenvalues_pear), length(pa_sim_mean_pear))], 3),
      Simulated = round(pa_sim_quant_pear[1:min(length(fa_eigenvalues_pear), length(pa_sim_mean_pear))], 3),
      Simulated_mean = round(pa_sim_mean_pear[1:min(length(fa_eigenvalues_pear), length(pa_sim_mean_pear))], 3),
      Retain = 1:min(length(fa_eigenvalues_pear), length(pa_sim_mean_pear)) <= pa_n_quant_pear,
      Retain_mean = 1:min(length(fa_eigenvalues_pear), length(pa_sim_mean_pear)) <= pa_n_mean_pear
    )
  )
  
  # --- PCA-based PA (Pearson) ---
  pc_sim_mean_pear <- pa_result_pearson$pc.sim
  
  sim_pc_cols_pear <- which(grepl("sim", cn_pear, ignore.case = TRUE) & grepl("pc", cn_pear, ignore.case = TRUE))
  qv_pc_pear <- apply(vals_pear[, sim_pc_cols_pear, drop = FALSE], 2, quantile, probs = percentile / 100, na.rm = TRUE)
  pc_sim_quant_pear <- rep(NA_real_, length(pc_eigenvalues_pearson))
  pc_sim_quant_pear[1:min(length(pc_sim_quant_pear), length(qv_pc_pear))] <- as.numeric(qv_pc_pear[1:min(length(pc_sim_quant_pear), length(qv_pc_pear))])
  
  pa_pca_n_quant_pear <- pa_result_pearson$ncomp
  pa_pca_n_mean_pear <- sum(pc_eigenvalues_pearson > pc_sim_mean_pear[1:min(length(pc_eigenvalues_pearson), length(pc_sim_mean_pear))])
  
  pa_pca_len_pear <- min(length(pc_eigenvalues_pearson), length(pc_sim_mean_pear))
  pa_pca_pearson_formatted <- list(
    n_factors = pa_pca_n_quant_pear,
    n_factors_mean = pa_pca_n_mean_pear,
    eigen_table = data.frame(
      Component = 1:pa_pca_len_pear,
      Real = round(pc_eigenvalues_pearson[1:pa_pca_len_pear], 3),
      Simulated = round(pc_sim_quant_pear[1:pa_pca_len_pear], 3),
      Simulated_mean = round(pc_sim_mean_pear[1:pa_pca_len_pear], 3),
      Retain = 1:pa_pca_len_pear <= pa_pca_n_quant_pear,
      Retain_mean = 1:pa_pca_len_pear <= pa_pca_n_mean_pear
    )
  )
  
  # --- MAP (Pearson) ---
  map_values_pear <- vss_result_pearson$map
  map_n_pear <- which.min(map_values_pear) - 1
  
  map_result_pearson <- list(
    n_factors = map_n_pear,
    map_table = data.frame(
      Factors = 0:(length(map_values_pear) - 1),
      MAP = round(map_values_pear, 6)
    )
  )
  
  # --- Kaiser (Pearson) ---
  kaiser_result_pearson <- list(
    n_factors = kaiser_n_pearson,
    eigenvalues = pc_eigenvalues_pearson,
    eigen_table = data.frame(
      Factor = 1:length(pc_eigenvalues_pearson),
      Eigenvalue = round(pc_eigenvalues_pearson, 3),
      Retain = pc_eigenvalues_pearson > 1
    )
  )
  
  # ========================================
  # Return results
  # ========================================
  conditions <- list(
    correlation_method = "Polychoric and Pearson",
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
    pa_mrfa_pearson = pa_mrfa_pearson_formatted,
    kaiser_pearson = kaiser_result_pearson,
    pa_pearson = pa_pearson_formatted,
    pa_pca_pearson = pa_pca_pearson_formatted,
    map_pearson = map_result_pearson,
    conditions = conditions,
    cor_matrix = pa_result$r,
    n_obs = n_obs,
    n_vars = n_vars
  ))
}