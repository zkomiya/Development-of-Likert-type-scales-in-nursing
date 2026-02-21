# ===================================================
# Factor Suitability Calculator (Model Layer)
# Version: 2.0
# Description: Calculate correlation matrices and FA suitability
# Changes from v1.0:
#   - Bartlett uses Pearson correlation only
# ===================================================

library(psych)

# Calculate both correlation matrices
calculate_both_correlations <- function(data) {
  
  # Polychoric correlation
  poly_result <- psych::polychoric(data, global = FALSE)
  cor_poly <- poly_result$rho
  
  # Pearson correlation
  cor_pearson <- cor(data, method = "pearson", use = "pairwise.complete.obs")
  
  return(list(
    polychoric = cor_poly,
    pearson = cor_pearson
  ))
}

# Calculate FA suitability
# Bartlett: Pearson only
# KMO: Both Polychoric and Pearson
calculate_fa_suitability_both <- function(data, cor_poly, cor_pearson) {
  
  source("factor_prerequisites.R")
  
  # Polychoric correlation check (KMO only)
  prereq_poly <- check_fa_prerequisites(data, cor_poly, verbose = FALSE)
  
  # Pearson correlation check (Bartlett and KMO)
  prereq_pearson <- check_fa_prerequisites(data, cor_pearson, verbose = FALSE)
  
  # Format and return results
  return(list(
    sample_size = prereq_poly$sample_size,
    bartlett = prereq_pearson$bartlett,
    kmo = list(
      polychoric = prereq_poly$kmo,
      pearson = prereq_pearson$kmo
    ),
    cor_matrix = list(
      polychoric = cor_poly,
      pearson = cor_pearson
    ),
    data = data
  ))
}

# Check correlation matrix singularity
# Uses base R (eigen, kappa, chol) and Matrix package (rankMatrix, rcond)
check_matrix_singularity <- function(cor_matrix) {
  
  p <- ncol(cor_matrix)
  
  ev <- eigen(cor_matrix, symmetric = TRUE, only.values = TRUE)$values
  min_eigen   <- min(ev)
  n_nonpos    <- sum(ev <= 0)
  
  rank <- if (requireNamespace("Matrix", quietly = TRUE)) {
    as.integer(Matrix::rankMatrix(cor_matrix))
  } else {
    qr(cor_matrix)$rank
  }
  
  kappa_2 <- kappa(cor_matrix)
  
  rcond_val <- if (requireNamespace("Matrix", quietly = TRUE)) {
    as.numeric(Matrix::rcond(cor_matrix))
  } else {
    NA_real_
  }
  
  chol_ok <- !inherits(try(chol(cor_matrix), silent = TRUE), "try-error")
  
  return(list(
    p            = p,
    min_eigen    = min_eigen,
    n_eigen_le_0 = n_nonpos,
    rank         = rank,
    is_singular  = (rank < p),
    kappa_2      = kappa_2,
    rcond        = rcond_val,
    chol_ok      = chol_ok
  ))
}