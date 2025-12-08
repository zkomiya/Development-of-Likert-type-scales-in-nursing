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