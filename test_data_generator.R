# ===================================================
# Test Dataset Generator for Factor Number Validation
# Generates test data with known theoretical factor structures
# ===================================================

library(MASS)  # For mvrnorm

# ===================================================
# Pattern 1: Clear 3-factor structure (eigenvalues: ~9, ~6, ~4, <1...)
# ===================================================
generate_pattern1 <- function(n = 300, seed = 123) {
  set.seed(seed)
  
  # Design loading matrix (28 items x 3 factors)
  lambda <- matrix(0, nrow = 28, ncol = 3)
  
  # Factor 1 (Q01-Q10): strong loadings
  lambda[1:10, 1] <- runif(10, 0.7, 0.85)
  
  # Factor 2 (Q11-Q20): moderate loadings
  lambda[11:20, 2] <- runif(10, 0.65, 0.75)
  
  # Factor 3 (Q21-Q28): somewhat weak loadings
  lambda[21:28, 3] <- runif(8, 0.55, 0.70)
  
  # Factor correlations
  phi <- matrix(c(1.0, 0.3, 0.2,
                  0.3, 1.0, 0.25,
                  0.2, 0.25, 1.0), 3, 3)
  
  # Communalities and uniqueness
  communalities <- rowSums(lambda^2)
  uniqueness <- 1 - communalities
  
  # Generate correlation matrix
  R <- lambda %*% phi %*% t(lambda) + diag(uniqueness)
  
  # Generate continuous data
  data_continuous <- mvrnorm(n, mu = rep(0, 28), Sigma = R)
  
  # Convert to ordinal data (1-4 scale)
  data_ordinal <- matrix(NA, n, 28)
  thresholds <- c(-1.5, -0.5, 0.5)  # thresholds
  
  for (j in 1:28) {
    data_ordinal[, j] <- as.numeric(cut(data_continuous[, j], 
                                        breaks = c(-Inf, thresholds, Inf),
                                        labels = FALSE))
  }
  
  df <- as.data.frame(data_ordinal)
  colnames(df) <- paste0("Q", sprintf("%02d", 1:28))
  
  # Ensure all columns are numeric
  for (col in names(df)) {
    df[[col]] <- as.numeric(df[[col]])
  }
  
  # Calculate theoretical eigenvalues for validation
  theoretical_eigenvalues <- eigen(R)$values
  
  return(list(
    data = df,
    true_n_factors = 3,
    theoretical_eigenvalues = theoretical_eigenvalues,
    description = "Pattern 1: Clear 3-factor structure"
  ))
}

# ===================================================
# Pattern 2: Clear 4-factor structure (eigenvalues: ~7, ~5, ~4, ~3, <1...)
# ===================================================
generate_pattern2 <- function(n = 300, seed = 456) {
  set.seed(seed)
  
  lambda <- matrix(0, nrow = 28, ncol = 4)
  
  # Factor 1 (Q01-Q07)
  lambda[1:7, 1] <- runif(7, 0.75, 0.85)
  
  # Factor 2 (Q08-Q14)
  lambda[8:14, 2] <- runif(7, 0.70, 0.80)
  
  # Factor 3 (Q15-Q21)
  lambda[15:21, 3] <- runif(7, 0.65, 0.75)
  
  # Factor 4 (Q22-Q28)
  lambda[22:28, 4] <- runif(7, 0.60, 0.70)
  
  # Factor correlations (somewhat low)
  phi <- matrix(0.2, 4, 4)
  diag(phi) <- 1
  
  communalities <- rowSums(lambda^2)
  uniqueness <- 1 - communalities
  
  R <- lambda %*% phi %*% t(lambda) + diag(uniqueness)
  
  data_continuous <- mvrnorm(n, mu = rep(0, 28), Sigma = R)
  
  data_ordinal <- matrix(NA, n, 28)
  thresholds <- c(-1.2, -0.3, 0.6)
  
  for (j in 1:28) {
    data_ordinal[, j] <- as.numeric(cut(data_continuous[, j], 
                                        breaks = c(-Inf, thresholds, Inf),
                                        labels = FALSE))
  }
  
  df <- as.data.frame(data_ordinal)
  colnames(df) <- paste0("Q", sprintf("%02d", 1:28))
  
  # Ensure all columns are numeric
  for (col in names(df)) {
    df[[col]] <- as.numeric(df[[col]])
  }
  
  theoretical_eigenvalues <- eigen(R)$values
  
  return(list(
    data = df,
    true_n_factors = 4,
    theoretical_eigenvalues = theoretical_eigenvalues,
    description = "Pattern 2: Clear 4-factor structure"
  ))
}

# ===================================================
# Pattern 3: Boundary case (eigenvalues near 1)
# ===================================================
generate_pattern3 <- function(n = 300, seed = 789) {
  set.seed(seed)
  
  lambda <- matrix(0, nrow = 28, ncol = 5)
  
  # Factor 1 (strong)
  lambda[1:8, 1] <- runif(8, 0.70, 0.80)
  
  # Factor 2 (moderate)
  lambda[9:14, 2] <- runif(6, 0.60, 0.70)
  
  # Factor 3 (weak)
  lambda[15:20, 3] <- runif(6, 0.45, 0.55)
  
  # Factor 4 (borderline)
  lambda[21:24, 4] <- runif(4, 0.40, 0.45)
  
  # Factor 5 (very weak)
  lambda[25:28, 5] <- runif(4, 0.30, 0.35)
  
  phi <- matrix(0.15, 5, 5)
  diag(phi) <- 1
  
  communalities <- rowSums(lambda^2)
  uniqueness <- 1 - communalities
  
  R <- lambda %*% phi %*% t(lambda) + diag(uniqueness)
  
  data_continuous <- mvrnorm(n, mu = rep(0, 28), Sigma = R)
  
  data_ordinal <- matrix(NA, n, 28)
  thresholds <- c(-1.0, 0, 1.0)
  
  for (j in 1:28) {
    data_ordinal[, j] <- as.numeric(cut(data_continuous[, j], 
                                        breaks = c(-Inf, thresholds, Inf),
                                        labels = FALSE))
  }
  
  df <- as.data.frame(data_ordinal)
  colnames(df) <- paste0("Q", sprintf("%02d", 1:28))
  
  # Ensure all columns are numeric
  for (col in names(df)) {
    df[[col]] <- as.numeric(df[[col]])
  }
  
  theoretical_eigenvalues <- eigen(R)$values
  
  return(list(
    data = df,
    true_n_factors = 3,  # Theoretically 3 is appropriate (4,5 are too weak)
    theoretical_eigenvalues = theoretical_eigenvalues,
    description = "Pattern 3: Boundary case (eigenvalues near 1)"
  ))
}

# ===================================================
# Pattern 4: Single factor structure
# ===================================================
generate_pattern4 <- function(n = 300, seed = 321) {
  set.seed(seed)
  
  lambda <- matrix(0, nrow = 28, ncol = 1)
  
  # All items load on single factor (varying strengths)
  lambda[1:10, 1] <- runif(10, 0.70, 0.85)
  lambda[11:20, 1] <- runif(10, 0.60, 0.75)
  lambda[21:28, 1] <- runif(8, 0.50, 0.65)
  
  phi <- matrix(1, 1, 1)
  
  communalities <- rowSums(lambda^2)
  uniqueness <- 1 - communalities
  
  R <- lambda %*% phi %*% t(lambda) + diag(uniqueness)
  
  data_continuous <- mvrnorm(n, mu = rep(0, 28), Sigma = R)
  
  data_ordinal <- matrix(NA, n, 28)
  thresholds <- c(-1.5, 0, 1.5)
  
  for (j in 1:28) {
    data_ordinal[, j] <- as.numeric(cut(data_continuous[, j], 
                                        breaks = c(-Inf, thresholds, Inf),
                                        labels = FALSE))
  }
  
  df <- as.data.frame(data_ordinal)
  colnames(df) <- paste0("Q", sprintf("%02d", 1:28))
  
  # Ensure all columns are numeric
  for (col in names(df)) {
    df[[col]] <- as.numeric(df[[col]])
  }
  
  theoretical_eigenvalues <- eigen(R)$values
  
  return(list(
    data = df,
    true_n_factors = 1,
    theoretical_eigenvalues = theoretical_eigenvalues,
    description = "Pattern 4: Single factor structure"
  ))
}

# ===================================================
# Main execution function
# ===================================================
generate_test_datasets <- function() {
  cat("Generating test datasets for factor number validation\n")
  cat("=====================================================\n\n")
  
  datasets <- list()
  
  # Pattern 1: 3 factors
  cat("Generating Pattern 1 (3 factors)...\n")
  datasets$pattern1 <- generate_pattern1(n = 400)
  cat("- Eigenvalues > 1:", sum(datasets$pattern1$theoretical_eigenvalues > 1), "\n")
  cat("- Top eigenvalues:", paste(round(datasets$pattern1$theoretical_eigenvalues[1:6], 2), collapse=", "), "\n\n")
  
  # Pattern 2: 4 factors
  cat("Generating Pattern 2 (4 factors)...\n")
  datasets$pattern2 <- generate_pattern2(n = 400)
  cat("- Eigenvalues > 1:", sum(datasets$pattern2$theoretical_eigenvalues > 1), "\n")
  cat("- Top eigenvalues:", paste(round(datasets$pattern2$theoretical_eigenvalues[1:6], 2), collapse=", "), "\n\n")
  
  # Pattern 3: boundary case
  cat("Generating Pattern 3 (boundary case)...\n")
  datasets$pattern3 <- generate_pattern3(n = 400)
  cat("- Eigenvalues > 1:", sum(datasets$pattern3$theoretical_eigenvalues > 1), "\n")
  cat("- Top eigenvalues:", paste(round(datasets$pattern3$theoretical_eigenvalues[1:6], 2), collapse=", "), "\n\n")
  
  # Pattern 4: single factor
  cat("Generating Pattern 4 (single factor)...\n")
  datasets$pattern4 <- generate_pattern4(n = 400)
  cat("- Eigenvalues > 1:", sum(datasets$pattern4$theoretical_eigenvalues > 1), "\n")
  cat("- Top eigenvalues:", paste(round(datasets$pattern4$theoretical_eigenvalues[1:6], 2), collapse=", "), "\n\n")
  
  return(datasets)
}

# ===================================================
# Validation execution function
# ===================================================
validate_factor_determination <- function(test_data) {
  cat("\n========================================\n")
  cat("VALIDATING:", test_data$description, "\n")
  cat("True number of factors:", test_data$true_n_factors, "\n")
  cat("========================================\n")
  
  # Data cleaning
  df_clean <- clean_data(test_data$data)
  
  # Execute factor number determination
  results <- determine_n_factors(
    df_clean,
    n_iterations = 1000,
    percentile = 95,
    seed = 123,
    verbose = TRUE
  )
  
  cat("\n--- VALIDATION RESULTS ---\n")
  cat("True factors:", test_data$true_n_factors, "\n")
  cat("Kaiser suggestion:", results$kaiser$n_factors, "\n")
  cat("PA suggestion:", results$pa$suggested, "\n")
  cat("MAP suggestion:", results$map$suggested, "\n")
  
  return(results)
}

# ===================================================
# Usage examples
# ===================================================
# test_datasets <- generate_test_datasets()
# 
# # Validate each pattern
# results1 <- validate_factor_determination(test_datasets$pattern1)
# results2 <- validate_factor_determination(test_datasets$pattern2)
# results3 <- validate_factor_determination(test_datasets$pattern3)
# results4 <- validate_factor_determination(test_datasets$pattern4)