# ===================================================
# EFA Calculator
# Version: 5.0 - Added Polychoric + Pearson support with alignment
# Description: Perform EFA with psych package
# Changes from v4.0:
#   - Added calculate_correlation_for_efa()
#   - Added align_factors()
#   - Added align_efa_solution()
# ===================================================

library(psych)
library(GPArotation)
library(clue)

# Perform factor extraction with ULS (Package version)
extract_factors_uls <- function(R, n_factors, max_iter = 1000, tol = 1e-6) {
  
  # psych::fa with ULS method
  fa_result <- psych::fa(
    r = R,
    nfactors = n_factors,
    rotate = "none",
    fm = "uls",
    max.iter = max_iter,
    SMC = TRUE
  )
  
  # Extract values with consistent naming
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
    method = "ULS",
    fa_object = fa_result
  ))
}

# Perform factor extraction with MINRES (Simplified)
extract_factors_minres <- function(R, n_factors, max_iter = 1000, tol = 1e-6) {
  
  # psych::fa with MINRES method
  fa_result <- psych::fa(
    r = R,
    nfactors = n_factors,
    rotate = "none",
    fm = "minres",
    max.iter = max_iter,
    SMC = TRUE
  )
  
  # Extract values with consistent naming
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
    method = "MINRES",
    fa_object = fa_result
  ))
}

# Perform Oblimin rotation with Kaiser normalization
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
    iterations = rotation_result$iter,
    gamma = gamma,
    kaiser_normalized = normalize
  ))
}

# Main EFA function (Simplified)
perform_efa <- function(cor_matrix, n_factors, 
                        extraction = c("ULS", "MINRES"),
                        gamma_values = c(0, -0.2, -0.5, -0.8),
                        kaiser_normalize = TRUE,
                        max_iter = 1000,
                        verbose = TRUE) {
  
  results <- list()
  
  # Perform extraction for each method
  for (method in extraction) {
    
    if (verbose) {
      cat("\nExtraction Method:", method, "\n")
    }
    
    # Extract factors
    if (method == "ULS") {
      extraction_result <- extract_factors_uls(cor_matrix, n_factors, max_iter)
    } else if (method == "MINRES") {
      extraction_result <- extract_factors_minres(cor_matrix, n_factors, max_iter)
    }
    
    if (verbose) {
      cat("Extraction completed\n")
    }
    
    # Store extraction results
    results[[method]] <- list(
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
      results[[method]]$rotations[[gamma_key]] <- rotation_result
    }
  }
  
  # Add metadata
  results$metadata <- list(
    n_factors = n_factors,
    extraction_methods = extraction,
    gamma_values = gamma_values,
    kaiser_normalize = kaiser_normalize,
    cor_matrix_dim = dim(cor_matrix)
  )
  
  return(results)
}

# ===================================================
# Calculate correlation matrix for EFA
# Version: 1.0
# Description: Calculate polychoric or Pearson correlation with positive definiteness check
# ===================================================

calculate_correlation_for_efa <- function(data, method = "polychoric", missing_mode = "listwise") {
  
  if (method == "polychoric") {
    # Polychoric correlation
    poly_result <- psych::polychoric(data, global = FALSE)
    cor_matrix <- poly_result$rho
    
  } else if (method == "pearson") {
    # Pearson correlation
    use_arg <- if (missing_mode == "pairwise") "pairwise.complete.obs" else "complete.obs"
    cor_matrix <- cor(data, use = use_arg, method = "pearson")
    
  } else {
    stop("method must be 'polychoric' or 'pearson'")
  }
  
  # Check positive definiteness
  eigenvalues <- eigen(cor_matrix, symmetric = TRUE, only.values = TRUE)$values
  
  if (any(!is.finite(eigenvalues)) || min(eigenvalues) <= 0) {
    # Smooth to nearest positive definite matrix
    cor_matrix <- psych::cor.smooth(cor_matrix)
  }
  
  return(cor_matrix)
}

# ===================================================
# Align factors between two solutions
# Version: 1.0
# Description: Align factor order and signs using Tucker's congruence
# ===================================================

# Determine optimal factor matching
align_factors <- function(pattern1, pattern2) {
  
  # Calculate Tucker's congruence coefficient
  congruence_matrix <- psych::factor.congruence(pattern1, pattern2)
  
  # Find optimal matching using Hungarian algorithm
  abs_congruence <- abs(congruence_matrix)
  optimal_match <- clue::solve_LSAP(abs_congruence, maximum = TRUE)
  
  # Reorder pattern2
  pattern2_aligned <- pattern2[, optimal_match]
  
  # Align signs
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

# Align entire EFA solution
align_efa_solution <- function(solution1, solution2, factor_mapping) {
  
  aligned <- solution2
  
  # Align pattern matrix
  aligned$pattern <- solution2$pattern[, factor_mapping]
  
  # Align signs for pattern
  for (i in 1:ncol(solution1$pattern)) {
    if (sum(solution1$pattern[, i] * aligned$pattern[, i]) < 0) {
      aligned$pattern[, i] <- -aligned$pattern[, i]
    }
  }
  
  # Align structure matrix
  aligned$structure <- solution2$structure[, factor_mapping]
  
  # Align signs for structure
  for (i in 1:ncol(solution1$structure)) {
    if (sum(solution1$structure[, i] * aligned$structure[, i]) < 0) {
      aligned$structure[, i] <- -aligned$structure[, i]
    }
  }
  
  # Align factor correlation matrix
  aligned$factor_correlation <- solution2$factor_correlation[factor_mapping, factor_mapping]
  
  return(aligned)
}