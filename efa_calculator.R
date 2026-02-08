# ===================================================
# EFA Calculator
# Version: 7.0 - Remove default gamma_values
# Description: Perform EFA with psych package
# Changes from v6.1:
#   - Removed default gamma_values from perform_efa()
#   - All parameters must be passed explicitly
# ===================================================

library(psych)
library(GPArotation)
library(clue)

# ===================================================
# Factor Extraction (unified function)
# ===================================================

extract_factors <- function(R, n_factors, fm, max_iter = 1000) {
  
  fa_result <- psych::fa(
    r = R,
    nfactors = n_factors,
    rotate = "none",
    fm = fm,
    max.iter = max_iter,
    SMC = TRUE
  )
  
  communalities_values <- if (!is.null(fa_result$communality)) {
    fa_result$communality
  } else if (!is.null(fa_result$communalities)) {
    fa_result$communalities
  } else {
    rowSums(as.matrix(fa_result$loadings)^2)
  }
  
  return(list(
    loadings = as.matrix(fa_result$loadings),
    communalities = communalities_values,
    eigenvalues = fa_result$e.values,
    method = fm,
    fa_object = fa_result
  ))
}

# ===================================================
# Oblimin Rotation
# ===================================================

oblimin_rotation <- function(loadings, gamma = 0, normalize = TRUE, 
                             max_iter = 1000, tol = 1e-5) {
  
  p <- nrow(loadings)
  m <- ncol(loadings)
  
  # Kaiser normalization
  if (normalize) {
    h <- sqrt(rowSums(loadings^2))
    loadings_norm <- loadings / h
  } else {
    loadings_norm <- loadings
    h <- rep(1, p)
  }
  
  # Oblimin rotation
  rotation_result <- GPArotation::oblimin(
    loadings_norm,
    Tmat = diag(m),
    gam = gamma,
    normalize = FALSE,
    eps = tol,
    maxit = max_iter
  )
  
  # De-normalize
  if (normalize) {
    pattern_matrix <- rotation_result$loadings * h
  } else {
    pattern_matrix <- rotation_result$loadings
  }
  
  # Calculate factor correlations
  T_mat <- rotation_result$Th
  factor_correlation <- t(T_mat) %*% T_mat
  
  # Calculate structure matrix
  structure_matrix <- pattern_matrix %*% factor_correlation
  
  return(list(
    pattern = pattern_matrix,
    structure = structure_matrix,
    factor_correlation = factor_correlation,
    rotation_matrix = rotation_result$Th,
    converged = rotation_result$convergence,
    iterations = nrow(rotation_result$Table),
    gamma = gamma,
    kaiser_normalized = normalize
  ))
}

# ===================================================
# Main EFA Function (single extraction method)
# ===================================================

perform_efa <- function(cor_matrix, n_factors, fm, gamma_values,
                        kaiser_normalize, max_iter, verbose = TRUE) {
  
  if (verbose) {
    cat("Extraction Method:", fm, "\n")
  }
  
  # Extract factors
  extraction_result <- extract_factors(cor_matrix, n_factors, fm, max_iter)
  
  if (verbose) {
    cat("Extraction completed\n")
  }
  
  # Store results
  results <- list(
    extraction = extraction_result,
    rotations = list()
  )
  
  # Perform rotation for each gamma value
  for (gamma in gamma_values) {
    
    if (verbose) {
      cat("  Oblimin rotation - gamma:", gamma, "\n")
    }
    
    rotation_result <- oblimin_rotation(
      extraction_result$loadings,
      gamma = gamma,
      normalize = kaiser_normalize,
      max_iter = max_iter
    )
    
    if (verbose && rotation_result$converged) {
      cat("    Rotation completed (iterations:", rotation_result$iterations, ")\n")
    } else if (verbose) {
      cat("    Rotation did not converge\n")
    }
    
    # Store rotation results
    gamma_key <- paste0("gamma_", gsub("-", "neg", as.character(gamma)))
    results$rotations[[gamma_key]] <- rotation_result
  }
  
  # Add metadata
  results$metadata <- list(
    n_factors = n_factors,
    extraction_method = fm,
    gamma_values = gamma_values,
    kaiser_normalize = kaiser_normalize,
    cor_matrix_dim = dim(cor_matrix)
  )
  
  return(results)
}

# ===================================================
# Calculate correlation matrix for EFA
# ===================================================

calculate_correlation_for_efa <- function(data, method = "polychoric", missing_mode = "listwise") {
  
  if (method == "polychoric") {
    poly_result <- psych::polychoric(data, global = FALSE)
    cor_matrix <- poly_result$rho
    
  } else if (method == "pearson") {
    use_arg <- if (missing_mode == "pairwise") "pairwise.complete.obs" else "complete.obs"
    cor_matrix <- cor(data, use = use_arg, method = "pearson")
    
  } else {
    stop("method must be 'polychoric' or 'pearson'")
  }
  
  # Check positive definiteness
  eigenvalues <- eigen(cor_matrix, symmetric = TRUE, only.values = TRUE)$values
  
  if (any(!is.finite(eigenvalues)) || min(eigenvalues) <= 0) {
    cor_matrix <- psych::cor.smooth(cor_matrix)
  }
  
  return(cor_matrix)
}

# ===================================================
# Factor Alignment Functions
# ===================================================

align_factors <- function(pattern1, pattern2) {
  
  congruence_matrix <- psych::factor.congruence(pattern1, pattern2)
  abs_congruence <- abs(congruence_matrix)
  optimal_match <- clue::solve_LSAP(abs_congruence, maximum = TRUE)
  
  pattern2_aligned <- pattern2[, optimal_match]
  
  for (i in 1:ncol(pattern1)) {
    if (sum(pattern1[, i] * pattern2_aligned[, i]) < 0) {
      pattern2_aligned[, i] <- -pattern2_aligned[, i]
    }
  }
  
  return(list(
    pattern_aligned = pattern2_aligned,
    factor_mapping = as.vector(optimal_match)
  ))
}

align_efa_solution <- function(solution1, solution2, factor_mapping) {
  
  aligned <- solution2
  
  # Align pattern matrix
  aligned$pattern <- solution2$pattern[, factor_mapping]
  
  for (i in 1:ncol(solution1$pattern)) {
    if (sum(solution1$pattern[, i] * aligned$pattern[, i]) < 0) {
      aligned$pattern[, i] <- -aligned$pattern[, i]
    }
  }
  
  # Align structure matrix
  aligned$structure <- solution2$structure[, factor_mapping]
  
  for (i in 1:ncol(solution1$structure)) {
    if (sum(solution1$structure[, i] * aligned$structure[, i]) < 0) {
      aligned$structure[, i] <- -aligned$structure[, i]
    }
  }
  
  # Align factor correlation matrix
  aligned$factor_correlation <- solution2$factor_correlation[factor_mapping, factor_mapping]
  
  return(aligned)
}