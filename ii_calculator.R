# ===================================================
# Item-Item Correlation Calculator (Model Layer)
# Version: 11.0 - Polychoric + Pearson concurrent calculation
# Description: Calculate correlations and MIIC for both methods
# Changes from v10.0:
#   - Added calculate_miic_from_correlation_matrix()
#   - Uses psych::alpha() for MIIC calculation from correlation matrix
#   - Uses psych::cor.smooth() for non-positive definite matrices
# ===================================================

library(psych)

# Calculate polychoric correlation matrix
calculate_polychoric_correlation <- function(data) {
  poly_result <- psych::polychoric(data, global = FALSE)
  return(poly_result$rho)
}

# Calculate MIIC from correlation matrix (Polychoric or Pearson)
calculate_miic_from_correlation_matrix <- function(cor_matrix, n_obs) {
  
  # Smooth correlation matrix for numerical stability
  cor_smoothed <- psych::cor.smooth(cor_matrix)
  
  # Use psych::alpha with correlation matrix input
  alpha_result <- psych::alpha(
    x = cor_smoothed,
    n.obs = n_obs,
    check.keys = FALSE
  )
  
  k <- nrow(cor_matrix)
  
  # Calculate item-level mean correlations from original matrix
  item_means <- (rowSums(cor_matrix) - 1) / (k - 1)
  names(item_means) <- rownames(cor_matrix)
  
  list(
    miic = alpha_result$total$average_r,
    n_items = k,
    n_pairs = k * (k - 1) / 2,
    item_mean_correlations = item_means
  )
}

# Calculate MIIC using psych::alpha (Pearson - for backward compatibility)
calculate_miic <- function(data) {
  # Use psych::alpha for accurate MIIC calculation
  alpha_result <- psych::alpha(data, check.keys = FALSE)
  
  k <- ncol(data)
  
  # Calculate item-level mean correlations
  cor_matrix <- cor(data, use = "pairwise.complete.obs")
  item_means <- (rowSums(cor_matrix, na.rm = TRUE) - 1) / (k - 1)
  names(item_means) <- colnames(data)
  
  list(
    miic = alpha_result$total$average_r,
    n_items = k,
    n_pairs = k * (k - 1) / 2,
    item_mean_correlations = item_means
  )
}