# ===================================================
# Factor Suitability Calculator (Model Layer)
# Version: 1.0
# Description: Calculate correlation matrices and FA suitability
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

# Calculate FA suitability for both correlation methods
calculate_fa_suitability_both <- function(data, cor_poly, cor_pearson) {
  
  # Polychoric correlation check
  source("factor_prerequisites.R")
  prereq_poly <- check_fa_prerequisites(data, cor_poly, verbose = FALSE)
  
  # Pearson correlation check
  prereq_pearson <- check_fa_prerequisites(data, cor_pearson, verbose = FALSE)
  
  # Format and return results
  return(list(
    sample_size = prereq_poly$sample_size,
    polychoric = list(
      kmo = prereq_poly$kmo,
      bartlett = prereq_poly$bartlett,
      cor_matrix = cor_poly
    ),
    pearson = list(
      kmo = prereq_pearson$kmo,
      bartlett = prereq_pearson$bartlett,
      cor_matrix = cor_pearson
    ),
    data = data
  ))
}