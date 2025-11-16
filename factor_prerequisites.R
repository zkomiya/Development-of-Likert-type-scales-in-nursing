# ===================================================
# Factor Analysis Prerequisites Checker
# Version: 6.1
# Description: Check prerequisites using psych package
# ===================================================

library(psych)

# Main prerequisites check function
check_fa_prerequisites <- function(data, cor_matrix, verbose = TRUE) {
  
  if (verbose) {
    cat("\nFACTOR ANALYSIS PREREQUISITES CHECK\n")
    cat("------------------------------------\n")
  }
  
  # Check that cor_matrix is provided
  if (missing(cor_matrix) || is.null(cor_matrix)) {
    stop("Correlation matrix must be provided")
  }
  
  n_obs <- nrow(data)
  n_vars <- ncol(cor_matrix)
  
  # ========================================
  # 1. Sample size
  # ========================================
  ratio <- n_obs / n_vars
  if (verbose) {
    cat("\nSample size:\n")
    cat("  N:", n_obs, "\n")
    cat("  p:", n_vars, "\n")
    cat("  N:p ratio:", round(ratio, 1), ":1\n")
  }
  
  # ========================================
  # 2. KMO test (Package version)
  # ========================================
  kmo_result_raw <- psych::KMO(cor_matrix)
  
  # Format output to maintain compatibility
  kmo_result <- list(
    overall = kmo_result_raw$MSA,
    MSA = kmo_result_raw$MSAi
  )
  
  if (verbose) {
    cat("\nKMO (Kaiser-Meyer-Olkin) Test\n")
    cat("Overall KMO:", round(kmo_result$overall, 4), "\n")
  }
  
  # ========================================
  # 3. Bartlett test (Package version)
  # ========================================
  bart_result_raw <- psych::cortest.bartlett(
    R = cor_matrix,
    n = n_obs
  )
  
  # Format output to maintain compatibility
  bart_result <- list(
    statistic = bart_result_raw$chisq,
    df = bart_result_raw$df,
    p.value = bart_result_raw$p.value
  )
  
  if (verbose) {
    cat("\nBartlett's Test of Sphericity\n")
    cat("Chi-square:", round(bart_result$statistic, 2), "\n")
    cat("df:", bart_result$df, "\n")
    cat("p-value:", 
        ifelse(bart_result$p.value < 0.001, "< 0.001", 
               round(bart_result$p.value, 4)), "\n")
  }
  
  # ========================================
  # Return results (maintaining compatibility)
  # ========================================
  return(list(
    sample_size = list(
      n = n_obs, 
      p = n_vars, 
      ratio = ratio
    ),
    kmo = kmo_result,
    bartlett = bart_result
  ))
}