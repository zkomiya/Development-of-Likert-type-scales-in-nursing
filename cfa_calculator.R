# ===================================================
# CFA Calculator (Model Layer)
# Version: 3.0 - Extended calculations
# Description: Comprehensive CFA calculations without judgments
# ===================================================

library(lavaan)

# Generate lavaan syntax from YAML model definition
generate_lavaan_syntax <- function(model_def) {
  syntax_parts <- c()
  
  # First-order factors
  for (factor_name in names(model_def$structure)) {
    items <- model_def$structure[[factor_name]]
    items_str <- paste(items, collapse = " + ")
    factor_line <- paste0(factor_name, " =~ ", items_str)
    syntax_parts <- c(syntax_parts, factor_line)
  }
  
  # Higher-order factors (if exists)
  if (!is.null(model_def$higher_order)) {
    for (ho_factor in names(model_def$higher_order)) {
      first_order_factors <- model_def$higher_order[[ho_factor]]
      factors_str <- paste(first_order_factors, collapse = " + ")
      ho_line <- paste0(ho_factor, " =~ ", factors_str)
      syntax_parts <- c(syntax_parts, "", ho_line)
    }
  }
  
  # Combine all parts
  syntax <- paste(syntax_parts, collapse = "\n")
  return(syntax)
}

# Perform CFA
perform_cfa <- function(data, model_syntax, estimator = "WLSMV", 
                        missing = "pairwise", se = "robust") {
  
  # Ensure data is ordered for WLSMV
  if (estimator %in% c("WLSMV", "ULSMV")) {
    for (col in names(data)) {
      if (is.numeric(data[[col]])) {
        data[[col]] <- ordered(data[[col]])
      }
    }
  }
  
  # Fit CFA model
  cfa_fit <- cfa(
    model = model_syntax,
    data = data,
    estimator = estimator,
    missing = missing,
    se = se,
    std.lv = FALSE,
    ordered = names(data)
  )
  
  return(cfa_fit)
}

# Calculate residual diagnostics
calculate_residual_diagnostics <- function(cfa_fit) {
  
  # Get residual matrices
  res_cov <- lavResiduals(cfa_fit)
  
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

# Calculate modification indices
calculate_modification_indices <- function(cfa_fit) {
  
  # Get modification indices
  mi_full <- modificationIndices(cfa_fit, sort = TRUE)
  
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

# Calculate reliability and validity indices
calculate_reliability_validity <- function(cfa_fit) {
  
  # Get standardized solution
  std_sol <- standardizedSolution(cfa_fit)
  loadings <- std_sol[std_sol$op == "=~", ]
  
  # Get factor names
  factors <- unique(loadings$lhs)
  
  # Initialize results
  reliability_validity <- list()
  
  for (factor in factors) {
    # Get loadings for this factor
    factor_loadings <- loadings[loadings$lhs == factor, "est.std"]
    
    # Composite Reliability (CR)
    sum_loadings <- sum(factor_loadings)
    sum_loadings_sq <- sum_loadings^2
    sum_error_var <- sum(1 - factor_loadings^2)
    cr <- sum_loadings_sq / (sum_loadings_sq + sum_error_var)
    
    # Average Variance Extracted (AVE)
    ave <- mean(factor_loadings^2)
    
    reliability_validity[[factor]] <- list(
      cr = cr,
      ave = ave,
      n_items = length(factor_loadings),
      loadings = factor_loadings,
      mean_loading = mean(factor_loadings),
      min_loading = min(factor_loadings),
      max_loading = max(factor_loadings)
    )
  }
  
  # Calculate shared variances (if multiple factors)
  if (length(factors) > 1) {
    factor_cors <- lavInspect(cfa_fit, "cor.lv")
    
    # Maximum Shared Variance (MSV) and Average Shared Variance (ASV)
    for (factor in factors) {
      factor_index <- which(rownames(factor_cors) == factor)
      cors_with_others <- factor_cors[factor_index, -factor_index]
      
      reliability_validity[[factor]]$msv <- max(cors_with_others^2)
      reliability_validity[[factor]]$asv <- mean(cors_with_others^2)
      
      # Store correlations for discriminant validity check
      reliability_validity[[factor]]$correlations_with_others <- cors_with_others
      reliability_validity[[factor]]$sqrt_ave <- sqrt(reliability_validity[[factor]]$ave)
    }
  }
  
  return(reliability_validity)
}

# Check estimation problems
check_estimation_problems <- function(cfa_fit) {
  
  problems <- list()
  
  # Check convergence
  problems$converged <- lavInspect(cfa_fit, "converged")
  
  # Check for Heywood cases (negative variances)
  theta <- lavInspect(cfa_fit, "theta")
  problems$heywood_cases <- any(diag(theta) < 0)
  
  if (problems$heywood_cases) {
    problems$negative_variances <- names(which(diag(theta) < 0))
  }
  
  # Check for identification
  problems$df <- fitMeasures(cfa_fit, "df")
  problems$identified <- problems$df >= 0
  
  # Check standard errors
  se_values <- parameterEstimates(cfa_fit)$se
  problems$large_se <- any(se_values > 10, na.rm = TRUE)
  
  if (problems$large_se) {
    problems$parameters_with_large_se <- sum(se_values > 10, na.rm = TRUE)
  }
  
  # Check for warning messages
  problems$warnings <- cfa_fit@optim$warnings
  
  # Check condition number (multicollinearity)
  cov_matrix <- lavInspect(cfa_fit, "sampstat")$cov
  eigenvalues <- eigen(cov_matrix)$values
  problems$condition_number <- max(eigenvalues) / min(eigenvalues[eigenvalues > 0])
  
  return(problems)
}

# Calculate extended fit indices
calculate_extended_fit_indices <- function(cfa_fit) {
  
  # Get all fit measures
  all_measures <- fitMeasures(cfa_fit)
  
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

# Calculate parameter estimates with detailed information
calculate_detailed_parameters <- function(cfa_fit) {
  
  # Get parameter estimates
  params <- parameterEstimates(cfa_fit, standardized = TRUE, ci = TRUE)
  
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