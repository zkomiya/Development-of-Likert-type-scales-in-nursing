# ===================================================
# CFA Calculator (Model Layer)
# Description: CFA-specific calculations.
#              Generic lavaan calculations are in lavaan_utils_calculator.R
# ===================================================

library(lavaan)

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

# Calculate reliability and validity indices (CFA-specific)
# CFA assumes each item loads on exactly one factor.
# Loadings used are the standardized loadings from the CFA model.
calculate_cfa_reliability_validity <- function(fit) {
  
  # Get standardized solution
  std_sol <- standardizedSolution(fit)
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
    factor_cors <- lavInspect(fit, "cor.lv")
    
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