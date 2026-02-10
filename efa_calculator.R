# ===================================================
# EFA Calculator
# Version: 9.0 - Promax rotation added
# Description: Perform EFA with psych package
# Changes from v8.0:
#   - Added promax_rotation() function using stats::promax()
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
# Factor Sign Adjustment
# ===================================================

flip_factors_by_absolute_sum <- function(pattern, 
                                         structure = NULL, 
                                         factor_correlation = NULL) {
  
  n_factors <- ncol(pattern)
  
  # Determine which factors to flip
  for (f in 1:n_factors) {
    sum_positive <- sum(pattern[pattern[, f] > 0, f])
    sum_negative_abs <- abs(sum(pattern[pattern[, f] < 0, f]))
    
    # Flip if negative sum is larger
    if (sum_negative_abs > sum_positive) {
      # Flip pattern
      pattern[, f] <- -pattern[, f]
      
      # Flip structure if provided
      if (!is.null(structure)) {
        structure[, f] <- -structure[, f]
      }
      
      # Flip factor correlation if provided
      if (!is.null(factor_correlation)) {
        factor_correlation[f, ] <- -factor_correlation[f, ]
        factor_correlation[, f] <- -factor_correlation[, f]
        # Restore diagonal to 1
        diag(factor_correlation) <- 1
      }
    }
  }
  
  return(list(
    pattern = pattern,
    structure = structure,
    factor_correlation = factor_correlation
  ))
}

# ===================================================
# Oblimin Rotation
# ===================================================

oblimin_rotation <- function(loadings, gamma = 0, normalize = TRUE, 
                             max_iter = 1000, tol = 1e-5, flip_factors = FALSE) {
  
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
  
  # Factor sign adjustment
  if (flip_factors) {
    flip_result <- flip_factors_by_absolute_sum(
      pattern = pattern_matrix,
      structure = structure_matrix,
      factor_correlation = factor_correlation
    )
    
    pattern_matrix <- flip_result$pattern
    structure_matrix <- flip_result$structure
    factor_correlation <- flip_result$factor_correlation
  }
  
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
# Promax Rotation
# ===================================================

promax_rotation <- function(loadings, kappa = 4, flip_factors = FALSE) {
  
  # stats::promax performs varimax first, then power transformation
  promax_result <- stats::promax(loadings, m = kappa)
  
  # Extract pattern matrix (promax$loadings)
  pattern_matrix <- as.matrix(promax_result$loadings)
  
  # Extract rotation matrix
  rotation_matrix <- promax_result$rotmat
  
  # Calculate factor correlation matrix
  # For promax: Phi = (T'T)^{-1} where T is the rotation matrix
  factor_correlation <- solve(t(rotation_matrix) %*% rotation_matrix)
  
  # Standardize to correlation matrix (ensure diagonal = 1)
  D_inv <- diag(1 / sqrt(diag(factor_correlation)))
  factor_correlation <- D_inv %*% factor_correlation %*% D_inv
  
  # Preserve row/column names
  rownames(factor_correlation) <- colnames(pattern_matrix)
  colnames(factor_correlation) <- colnames(pattern_matrix)
  
  # Calculate structure matrix: S = P %*% Phi
  structure_matrix <- pattern_matrix %*% factor_correlation
  
  # Factor sign adjustment
  if (flip_factors) {
    flip_result <- flip_factors_by_absolute_sum(
      pattern = pattern_matrix,
      structure = structure_matrix,
      factor_correlation = factor_correlation
    )
    
    pattern_matrix <- flip_result$pattern
    structure_matrix <- flip_result$structure
    factor_correlation <- flip_result$factor_correlation
  }
  
  return(list(
    pattern = pattern_matrix,
    structure = structure_matrix,
    factor_correlation = factor_correlation,
    rotation_matrix = rotation_matrix,
    converged = TRUE,
    iterations = NA,
    kappa = kappa
  ))
}

# ===================================================
# Main EFA Function (single extraction method)
# ===================================================

perform_efa <- function(cor_matrix, n_factors, fm, gamma_values,
                        kaiser_normalize, max_iter, flip_factors = TRUE,
                        promax_kappa_values = NULL,
                        verbose = TRUE) {
  
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
    rotations = list(),
    rotations_promax = list()
  )
  
  # Perform oblimin rotation for each gamma value
  for (gamma in gamma_values) {
    
    if (verbose) {
      cat("  Oblimin rotation - gamma:", gamma, "\n")
    }
    
    rotation_result <- oblimin_rotation(
      extraction_result$loadings,
      gamma = gamma,
      normalize = kaiser_normalize,
      max_iter = max_iter,
      flip_factors = flip_factors
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
  
  # Perform promax rotation for each kappa value
  if (!is.null(promax_kappa_values)) {
    for (kappa in promax_kappa_values) {
      
      if (verbose) {
        cat("  Promax rotation - kappa:", kappa, "\n")
      }
      
      promax_result <- promax_rotation(
        extraction_result$loadings,
        kappa = kappa,
        flip_factors = flip_factors
      )
      
      if (verbose) {
        cat("    Rotation completed\n")
      }
      
      # Store promax results
      kappa_key <- paste0("kappa_", kappa)
      results$rotations_promax[[kappa_key]] <- promax_result
    }
  }
  
  # Add metadata
  results$metadata <- list(
    n_factors = n_factors,
    extraction_method = fm,
    gamma_values = gamma_values,
    promax_kappa_values = promax_kappa_values,
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