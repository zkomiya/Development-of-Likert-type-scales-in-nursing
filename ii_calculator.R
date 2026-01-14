# ===================================================
# Item-Item Correlation Calculator (Model Layer)
# Version: 13.0 - Removed Within-Between difference calculation
# Description: Calculate correlations and MIIC for both methods
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

# Calculate correlation distribution statistics
calculate_correlation_distribution <- function(cor_matrix) {
  
  # Extract upper triangle (unique pairs)
  upper_tri_indices <- upper.tri(cor_matrix)
  correlations <- cor_matrix[upper_tri_indices]
  
  # Calculate statistics
  n_pairs <- length(correlations)
  mean_r <- mean(correlations, na.rm = TRUE)
  median_r <- median(correlations, na.rm = TRUE)
  min_r <- min(correlations, na.rm = TRUE)
  max_r <- max(correlations, na.rm = TRUE)
  
  # Percentiles
  q25 <- quantile(correlations, 0.25, na.rm = TRUE)
  q75 <- quantile(correlations, 0.75, na.rm = TRUE)
  iqr <- q75 - q25
  
  # Count by strength
  very_high <- sum(correlations > 0.70, na.rm = TRUE)
  high <- sum(correlations > 0.50 & correlations <= 0.70, na.rm = TRUE)
  moderate <- sum(correlations >= 0.30 & correlations <= 0.50, na.rm = TRUE)
  low <- sum(correlations < 0.30, na.rm = TRUE)
  
  list(
    n_pairs = n_pairs,
    mean = mean_r,
    median = median_r,
    min = min_r,
    max = max_r,
    q25 = q25,
    q75 = q75,
    iqr = iqr,
    strength_counts = list(
      very_high = very_high,
      high = high,
      moderate = moderate,
      low = low
    ),
    all_correlations = correlations
  )
}

# Calculate within and between subscale correlations
calculate_subscale_correlations <- function(cor_matrix, subscale_def) {
  
  # Initialize storage
  within_stats <- list()
  between_correlations <- c()
  
  subscale_names <- names(subscale_def)
  
  # Calculate within-subscale correlations
  for (sub_name in subscale_names) {
    items <- subscale_def[[sub_name]]$items
    
    if (length(items) < 2) {
      within_stats[[sub_name]] <- list(
        mean = NA,
        min = NA,
        max = NA,
        n_pairs = 0
      )
      next
    }
    
    # Extract submatrix
    sub_matrix <- cor_matrix[items, items]
    upper_tri <- upper.tri(sub_matrix)
    sub_cors <- sub_matrix[upper_tri]
    
    within_stats[[sub_name]] <- list(
      mean = mean(sub_cors, na.rm = TRUE),
      min = min(sub_cors, na.rm = TRUE),
      max = max(sub_cors, na.rm = TRUE),
      n_pairs = length(sub_cors)
    )
  }
  
  # Calculate between-subscale correlations
  for (i in 1:(length(subscale_names) - 1)) {
    for (j in (i + 1):length(subscale_names)) {
      items_i <- subscale_def[[subscale_names[i]]]$items
      items_j <- subscale_def[[subscale_names[j]]]$items
      
      # Extract cross-subscale correlations
      cross_matrix <- cor_matrix[items_i, items_j]
      between_correlations <- c(between_correlations, as.vector(cross_matrix))
    }
  }
  
  # Calculate between statistics
  between_stats <- list(
    mean = mean(between_correlations, na.rm = TRUE),
    min = min(between_correlations, na.rm = TRUE),
    max = max(between_correlations, na.rm = TRUE),
    n_pairs = length(between_correlations)
  )
  
  # Calculate overall within mean
  all_within_means <- sapply(within_stats, function(x) x$mean)
  overall_within_mean <- mean(all_within_means, na.rm = TRUE)
  
  list(
    within_stats = within_stats,
    between_stats = between_stats,
    overall_within_mean = overall_within_mean
  )
}

# Identify high correlation clusters
identify_high_correlation_clusters <- function(cor_matrix, threshold = 0.70) {
  
  # Find all pairs above threshold
  high_pairs <- which(cor_matrix > threshold & upper.tri(cor_matrix), arr.ind = TRUE)
  
  if (nrow(high_pairs) == 0) {
    return(list(clusters = list(), n_clusters = 0))
  }
  
  # Create edge list
  edges <- data.frame(
    item1 = rownames(cor_matrix)[high_pairs[, 1]],
    item2 = colnames(cor_matrix)[high_pairs[, 2]],
    correlation = cor_matrix[high_pairs],
    stringsAsFactors = FALSE
  )
  
  # Find connected components (clusters)
  all_items <- unique(c(edges$item1, edges$item2))
  clusters <- list()
  visited <- c()
  
  for (item in all_items) {
    if (item %in% visited) next
    
    # BFS to find connected component
    cluster <- c(item)
    queue <- c(item)
    visited <- c(visited, item)
    
    while (length(queue) > 0) {
      current <- queue[1]
      queue <- queue[-1]
      
      # Find neighbors
      neighbors <- unique(c(
        edges$item2[edges$item1 == current],
        edges$item1[edges$item2 == current]
      ))
      
      for (neighbor in neighbors) {
        if (!(neighbor %in% visited)) {
          cluster <- c(cluster, neighbor)
          queue <- c(queue, neighbor)
          visited <- c(visited, neighbor)
        }
      }
    }
    
    if (length(cluster) >= 2) {
      clusters[[length(clusters) + 1]] <- list(
        items = sort(cluster),
        edges = edges[
          (edges$item1 %in% cluster & edges$item2 %in% cluster), 
        ]
      )
    }
  }
  
  # Sort clusters by size (descending)
  cluster_sizes <- sapply(clusters, function(x) length(x$items))
  clusters <- clusters[order(cluster_sizes, decreasing = TRUE)]
  
  list(
    clusters = clusters,
    n_clusters = length(clusters),
    threshold = threshold
  )
}