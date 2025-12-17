# ===================================================
# Factor Number Determination for EFA
# Version: 6.1 (Calculator only - MAP fix)
# Description: Factor number determination for Exploratory Factor Analysis
# ===================================================

library(psych)

determine_n_factors <- function(data,
                                n_iterations,
                                percentile,
                                seed = NULL) {
  
  if (!is.null(seed)) set.seed(seed)
  
  n_vars <- ncol(data)
  n_obs <- nrow(data)
  max_factors <- min(n_vars - 1, 10)
  
  # ========================================
  # Step 1: Parallel Analysis for FACTOR ANALYSIS
  # ========================================
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
  
  # ========================================
  # Step 2: VSS for MAP test
  # ========================================
  vss_result <- VSS(
    x = data,
    n = max_factors,
    cor = "poly",
    fm = "minres",
    plot = FALSE
  )
  
  # ========================================
  # Step 3: Kaiser's Criterion
  # ========================================
  eigenvalues <- pa_result$pc.values
  kaiser_n <- sum(eigenvalues > 1)
  
  # ========================================
  # Step 4: Format Results
  # ========================================
  pa_n <- pa_result$nfact
  pa_sim_eigenvalues <- pa_result$fa.sim
  
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
    eigen_table = data.frame(
      Factor = 1:min(length(eigenvalues), length(pa_sim_eigenvalues)),
      Real = round(eigenvalues[1:min(length(eigenvalues), length(pa_sim_eigenvalues))], 3),
      Simulated = round(pa_sim_eigenvalues[1:min(length(eigenvalues), length(pa_sim_eigenvalues))], 3),
      Retain = 1:min(length(eigenvalues), length(pa_sim_eigenvalues)) <= pa_n
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
  return(list(
    kaiser = kaiser_result,
    pa = pa_formatted,
    map = map_result,
    cor_matrix = pa_result$r,
    n_obs = n_obs,
    n_vars = n_vars
  ))
}