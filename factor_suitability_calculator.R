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

# Diagnose zero-frequency cells in polychoric estimation
# Step 1: marginal frequencies per item
# Step 2: zero cells per pair (combn + table)
# Step 3: aggregate zero cell counts back to item level
calculate_zero_cell_diagnosis <- function(data, scale_min, scale_max) {
  
  levels    <- scale_min:scale_max
  items     <- colnames(data)
  n_items   <- length(items)
  N         <- nrow(data)
  
  # Step 1: marginal frequencies
  marg_list <- lapply(items, function(it) {
    tbl <- table(factor(data[[it]], levels = levels))
    data.frame(
      item                = it,
      min_category_count  = min(as.integer(tbl)),
      n_unused_categories = sum(tbl == 0),
      stringsAsFactors    = FALSE
    )
  })
  item_df <- do.call(rbind, marg_list)
  
  # Step 2: pair-level zero cells
  pairs    <- combn(items, 2, simplify = FALSE)
  pair_list <- lapply(pairs, function(p) {
    xi  <- factor(data[[p[1]]], levels = levels)
    xj  <- factor(data[[p[2]]], levels = levels)
    tab <- table(xi, xj)
    rs  <- rowSums(tab)
    cs  <- colSums(tab)
    exp <- outer(rs, cs) / sum(tab)
    data.frame(
      item1        = p[1],
      item2        = p[2],
      zero_cells   = sum(tab == 0),
      min_cell     = min(as.integer(tab)),
      min_expected = min(exp),
      stringsAsFactors = FALSE
    )
  })
  pair_df <- do.call(rbind, pair_list)
  
  # Step 3: aggregate to item level
  pairs_with_zero  <- setNames(rep(0L, n_items), items)
  total_zero_cells <- setNames(rep(0L, n_items), items)
  
  for (k in seq_len(nrow(pair_df))) {
    i1 <- pair_df$item1[k]
    i2 <- pair_df$item2[k]
    z  <- pair_df$zero_cells[k]
    total_zero_cells[i1] <- total_zero_cells[i1] + z
    total_zero_cells[i2] <- total_zero_cells[i2] + z
    if (z > 0) {
      pairs_with_zero[i1] <- pairs_with_zero[i1] + 1L
      pairs_with_zero[i2] <- pairs_with_zero[i2] + 1L
    }
  }
  
  item_df$pairs_with_zero  <- as.integer(pairs_with_zero[item_df$item])
  item_df$total_zero_cells <- as.integer(total_zero_cells[item_df$item])
  item_df <- item_df[order(-item_df$pairs_with_zero,
                           -item_df$total_zero_cells,
                           item_df$min_category_count), ]
  
  pair_df <- pair_df[order(-pair_df$zero_cells, pair_df$min_expected), ]
  
  return(list(
    N        = N,
    item_df  = item_df,
    pair_df  = pair_df,
    n_items  = n_items
  ))
}