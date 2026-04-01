# ===================================================
# Lavaan Utilities Calculator (Model Layer)
# Description: Generic calculation functions for any lavaan fit object.
#              Used by CFA, ESEM, and any future lavaan-based analysis.
# ===================================================

library(lavaan)

# Calculate extended fit indices from a lavaan fit object
calculate_extended_fit_indices <- function(fit) {
  
  # Get all fit measures
  all_measures <- fitMeasures(fit)
  
  # Organize by category
  extended_fit <- list(
    
    # Chi-square based
    chi_square = list(
      chi2 = all_measures["chisq"],
      df = all_measures["df"],
      pvalue = all_measures["pvalue"],
      chi2_df_ratio = all_measures["chisq"] / all_measures["df"],
      chi2_scaled = all_measures["chisq.scaled"],
      df_scaled = all_measures["df.scaled"],
      pvalue_scaled = all_measures["pvalue.scaled"],
      chi2_df_ratio_scaled = if(!is.na(all_measures["chisq.scaled"]))
        all_measures["chisq.scaled"] / all_measures["df.scaled"] else NA
    ),
    
    # Absolute fit
    absolute = list(
      rmsea = all_measures["rmsea"],
      rmsea_ci_lower = all_measures["rmsea.ci.lower"],
      rmsea_ci_upper = all_measures["rmsea.ci.upper"],
      rmsea_pclose = all_measures["rmsea.pvalue"],
      rmsea_scaled = all_measures["rmsea.scaled"],
      rmsea_ci_lower_scaled = all_measures["rmsea.ci.lower.scaled"],
      rmsea_ci_upper_scaled = all_measures["rmsea.ci.upper.scaled"],
      rmsea_pclose_scaled = all_measures["rmsea.pvalue.scaled"],
      srmr = all_measures["srmr"],
      srmr_bentler = all_measures["srmr_bentler"],
      srmr_mplus = all_measures["srmr_mplus"],
      rmr = all_measures["rmr"],
      rmr_nomean = all_measures["rmr_nomean"],
      gfi = all_measures["gfi"],
      agfi = all_measures["agfi"],
      pgfi = all_measures["pgfi"]
    ),
    
    # Incremental fit
    incremental = list(
      cfi = all_measures["cfi"],
      tli = all_measures["tli"],
      nnfi = all_measures["nnfi"],
      cfi_scaled = all_measures["cfi.scaled"],
      tli_scaled = all_measures["tli.scaled"],
      nnfi_scaled = all_measures["nnfi.scaled"],
      cfi_robust = all_measures["cfi.robust"],
      tli_robust = all_measures["tli.robust"],
      nfi = all_measures["nfi"],
      pnfi = all_measures["pnfi"],
      rfi = all_measures["rfi"],
      ifi = all_measures["ifi"],
      rni = all_measures["rni"]
    ),
    
    # Information criteria
    information = list(
      aic = all_measures["aic"],
      bic = all_measures["bic"],
      bic_adj = all_measures["bic2"],
      hqic = all_measures["hqic"],
      loglik = all_measures["logl"],
      unrestricted_loglik = all_measures["unrestricted.logl"],
      n_parameters = all_measures["npar"],
      scaling_factor = all_measures["scaling.factor.h1"],
      shift_parameter = all_measures["shift.parameter.h0"]
    ),
    
    # Sample size
    sample = list(
      n_total = all_measures["ntotal"],
      n_groups = all_measures["ngroups"],
      n_observations = all_measures["nobs"],
      n_patterns = all_measures["npatterns"],
      n_clusters = all_measures["nclusters"]
    )
  )
  
  return(extended_fit)
}

# Calculate residual diagnostics from a lavaan fit object
calculate_residual_diagnostics <- function(fit) {
  
  # Get residual matrices
  res_cov <- lavResiduals(fit)
  
  # Extract components
  raw_residuals <- res_cov$cov
  std_residuals <- res_cov$cov.z
  
  # Summary statistics
  residual_summary <- list(
    raw = list(
      matrix = raw_residuals,
      mean = mean(abs(raw_residuals[lower.tri(raw_residuals)])),
      max = max(abs(raw_residuals[lower.tri(raw_residuals)])),
      rmr = sqrt(mean(raw_residuals[lower.tri(raw_residuals)]^2))
    ),
    standardized = list(
      matrix = std_residuals,
      mean = mean(abs(std_residuals[lower.tri(std_residuals)]), na.rm = TRUE),
      max = max(abs(std_residuals[lower.tri(std_residuals)]), na.rm = TRUE),
      n_above_2 = sum(abs(std_residuals[lower.tri(std_residuals)]) > 2, na.rm = TRUE),
      n_above_3 = sum(abs(std_residuals[lower.tri(std_residuals)]) > 3, na.rm = TRUE),
      n_above_4 = sum(abs(std_residuals[lower.tri(std_residuals)]) > 4, na.rm = TRUE)
    )
  )
  
  # Identify largest residuals
  std_res_lower <- std_residuals[lower.tri(std_residuals)]
  res_indices <- which(lower.tri(std_residuals), arr.ind = TRUE)
  res_df <- data.frame(
    var1 = rownames(std_residuals)[res_indices[,1]],
    var2 = colnames(std_residuals)[res_indices[,2]],
    std_residual = std_res_lower
  )
  res_df <- res_df[order(abs(res_df$std_residual), decreasing = TRUE), ]
  
  residual_summary$top_residuals <- head(res_df, 10)
  
  return(residual_summary)
}

# Calculate modification indices from a lavaan fit object
calculate_modification_indices <- function(fit) {
  
  # Get modification indices
  mi_full <- modificationIndices(fit, sort = TRUE)
  
  # Separate by type
  mi_loadings <- mi_full[mi_full$op == "=~", ]
  mi_covariances <- mi_full[mi_full$op == "~~", ]
  mi_regressions <- mi_full[mi_full$op == "~", ]
  
  # Summary
  mi_summary <- list(
    all = mi_full,
    loadings = mi_loadings,
    covariances = mi_covariances,
    regressions = mi_regressions,
    top_10_overall = head(mi_full, 10),
    top_5_loadings = head(mi_loadings, 5),
    top_5_covariances = head(mi_covariances, 5),
    n_mi_above_10 = sum(mi_full$mi > 10, na.rm = TRUE),
    n_mi_above_20 = sum(mi_full$mi > 20, na.rm = TRUE)
  )
  
  return(mi_summary)
}

# Check estimation problems in a lavaan fit object
check_estimation_problems <- function(fit) {
  
  problems <- list()
  
  # Check convergence
  problems$converged <- lavInspect(fit, "converged")
  
  # Check for Heywood cases (negative variances)
  theta <- lavInspect(fit, "theta")
  problems$heywood_cases <- any(diag(theta) < 0)
  
  if (problems$heywood_cases) {
    problems$negative_variances <- names(which(diag(theta) < 0))
  }
  
  # Check for identification
  problems$df <- fitMeasures(fit, "df")
  problems$identified <- problems$df >= 0
  
  # Check standard errors
  se_values <- parameterEstimates(fit)$se
  problems$large_se <- any(se_values > 10, na.rm = TRUE)
  
  if (problems$large_se) {
    problems$parameters_with_large_se <- sum(se_values > 10, na.rm = TRUE)
  }
  
  # Check for warning messages
  problems$warnings <- fit@optim$warnings
  
  # Check condition number (multicollinearity)
  cov_matrix <- lavInspect(fit, "sampstat")$cov
  eigenvalues <- eigen(cov_matrix)$values
  problems$condition_number <- max(eigenvalues) / min(eigenvalues[eigenvalues > 0])
  
  return(problems)
}

# Calculate parameter estimates with detailed information from a lavaan fit object
calculate_detailed_parameters <- function(fit) {
  
  # Get parameter estimates
  params <- parameterEstimates(fit, standardized = TRUE, ci = TRUE)
  
  # Separate by type
  detailed_params <- list(
    all = params,
    
    loadings = params[params$op == "=~", ],
    
    covariances = params[params$op == "~~" & params$lhs != params$rhs, ],
    
    variances = params[params$op == "~~" & params$lhs == params$rhs, ],
    
    regressions = params[params$op == "~", ],
    
    intercepts = params[params$op == "~1", ],
    
    thresholds = params[params$op == "|", ],
    
    # Summary statistics
    summary = list(
      n_parameters = nrow(params),
      n_free = sum(params$free > 0),
      n_fixed = sum(params$free == 0),
      n_constrained = sum(params$free > 0 & duplicated(params$free))
    )
  )
  
  return(detailed_params)
}