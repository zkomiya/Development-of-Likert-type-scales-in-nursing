# ===================================================
# Item-Item Correlation Display (View Layer)
# Version: 9.0 - Removed Within-Between difference display
# Description: Display functions for item-item correlation analysis
# ===================================================

# Display header
ii_display_header <- function() {
  cat("========================================\n")
  cat("Item-Item Correlation Analysis\n")
  cat("========================================\n\n")
}

# Display data information
ii_display_data_info <- function(item_cols, n_items, complete_info = NULL) {
  cat("Step 1: Data detection\n")
  cat(sprintf("  Found %d item columns: %s ... %s\n", 
              n_items, item_cols[1], item_cols[n_items]))
  
  if (!is.null(complete_info)) {
    cat(sprintf("  Complete cases: %d out of %d (%.1f%%)\n",
                complete_info$n_complete,
                complete_info$n_total,
                complete_info$percent_complete))
  }
}

# Display MIIC comparison (without Difference column)
ii_display_miic_comparison <- function(miic_poly, miic_pear) {
  cat("\n========================================\n")
  cat("MIIC COMPARISON\n")
  cat("========================================\n\n")
  
  # Header
  cat(sprintf("%-25s %12s %12s\n", "", "Polychoric", "Pearson"))
  cat(paste(rep("-", 50), collapse = ""), "\n")
  
  # MIIC values
  cat(sprintf("%-25s %12.3f %12.3f\n", 
              "MIIC:", miic_poly$miic, miic_pear$miic))
  
  cat(sprintf("%-25s %12d %12d\n", 
              "Item pairs:", miic_poly$n_pairs, miic_pear$n_pairs))
  
  # Item-level mean correlations
  cat("\nItem-level mean correlations:\n")
  cat(sprintf("%-8s %12s %12s\n", "Item", "Polychoric", "Pearson"))
  cat(paste(rep("-", 35), collapse = ""), "\n")
  
  items <- names(miic_poly$item_mean_correlations)
  for (item in items) {
    poly_val <- miic_poly$item_mean_correlations[[item]]
    pear_val <- miic_pear$item_mean_correlations[[item]]
    
    cat(sprintf("%-8s %12.3f %12.3f\n", item, poly_val, pear_val))
  }
}

# Display correlation matrix comparison
ii_display_correlation_comparison <- function(cor_poly, cor_pear) {
  cat("\n========================================\n")
  cat("CORRELATION MATRIX COMPARISON\n")
  cat("========================================\n\n")
  
  cat("Polychoric Correlation Matrix:\n")
  print(round(cor_poly, 3))
  
  cat("\n----------------------------------------\n")
  cat("Pearson Correlation Matrix:\n")
  print(round(cor_pear, 3))
  
  cat("\n----------------------------------------\n")
  cat("Difference (Polychoric - Pearson):\n")
  cor_diff <- cor_poly - cor_pear
  print(round(cor_diff, 3))
}

# Display correlation distribution
ii_display_correlation_distribution <- function(dist_stats) {
  cat("\n[1] Correlation Distribution\n")
  cat(sprintf("  Polychoric correlations (%d pairs):\n", dist_stats$n_pairs))
  cat(sprintf("    Mean:          %.2f\n", dist_stats$mean))
  cat(sprintf("    Median:        %.2f\n", dist_stats$median))
  cat(sprintf("    Range:         %.2f - %.2f\n", dist_stats$min, dist_stats$max))
  cat("    Percentiles:\n")
  cat(sprintf("      25th:        %.2f\n", dist_stats$q25))
  cat(sprintf("      75th:        %.2f\n", dist_stats$q75))
  cat(sprintf("      IQR:         %.2f\n", dist_stats$iqr))
  cat("\n")
  cat("  Pairs by strength:\n")
  cat(sprintf("    r > 0.70:      %d pairs (%.1f%%)\n", 
              dist_stats$strength_counts$very_high,
              dist_stats$strength_counts$very_high / dist_stats$n_pairs * 100))
  cat(sprintf("    0.50-0.70:     %d pairs (%.1f%%)\n",
              dist_stats$strength_counts$high,
              dist_stats$strength_counts$high / dist_stats$n_pairs * 100))
  cat(sprintf("    0.30-0.50:     %d pairs (%.1f%%)\n",
              dist_stats$strength_counts$moderate,
              dist_stats$strength_counts$moderate / dist_stats$n_pairs * 100))
  cat(sprintf("    < 0.30:        %d pairs (%.1f%%)\n",
              dist_stats$strength_counts$low,
              dist_stats$strength_counts$low / dist_stats$n_pairs * 100))
}

# Display subscale analysis
ii_display_subscale_analysis <- function(subscale_stats) {
  cat("\n[2] Subscale Correlations\n")
  cat("  Within subscale (same factor):\n")
  
  for (sub_name in names(subscale_stats$within_stats)) {
    stats <- subscale_stats$within_stats[[sub_name]]
    
    if (is.na(stats$mean)) {
      cat(sprintf("    %-35s (insufficient items)\n", sub_name))
    } else {
      cat(sprintf("    %-35s Mean r = %.2f (range: %.2f-%.2f, n=%d pairs)\n",
                  paste0(sub_name, ":"),
                  stats$mean,
                  stats$min,
                  stats$max,
                  stats$n_pairs))
    }
  }
  
  cat("\n")
  cat(sprintf("  Overall within-subscale mean: %.2f\n",
              subscale_stats$overall_within_mean))
  cat("\n")
  cat(sprintf("  Between subscales (different factors):\n"))
  cat(sprintf("    Mean r = %.2f (range: %.2f-%.2f, n=%d pairs)\n",
              subscale_stats$between_stats$mean,
              subscale_stats$between_stats$min,
              subscale_stats$between_stats$max,
              subscale_stats$between_stats$n_pairs))
}

# Display cluster analysis
ii_display_cluster_analysis <- function(clusters) {
  cat(sprintf("\n[3] High Correlation Clusters (r > %.2f)\n", clusters$threshold))
  
  if (clusters$n_clusters == 0) {
    cat("  No clusters found\n")
    return()
  }
  
  for (i in seq_along(clusters$clusters)) {
    cluster <- clusters$clusters[[i]]
    cat(sprintf("  Cluster %d: %s\n", i, paste(cluster$items, collapse = ", ")))
    
    # Display edges within cluster
    for (j in 1:nrow(cluster$edges)) {
      edge <- cluster$edges[j, ]
      cat(sprintf("    %s-%s: %.2f\n", edge$item1, edge$item2, edge$correlation))
    }
    
    # Calculate average within-cluster correlation
    avg_r <- mean(cluster$edges$correlation)
    cat(sprintf("    → Average within-cluster r = %.2f\n", avg_r))
    cat("\n")
  }
}

# Display overall assessment
ii_display_overall_assessment <- function(miic_poly, miic_pear, dist_stats, 
                                          subscale_stats, clusters) {
  cat("\n[4] Summary Statistics\n")
  
  # MIIC Summary
  cat("  MIIC:\n")
  cat(sprintf("    Polychoric: %.3f\n", miic_poly$miic))
  cat(sprintf("    Pearson:    %.3f\n", miic_pear$miic))
  cat("\n")
  
  # Item Redundancy Summary
  cat("  High correlations (r > 0.70):\n")
  very_high_count <- dist_stats$strength_counts$very_high
  very_high_pct <- very_high_count / dist_stats$n_pairs * 100
  
  cat(sprintf("    Count: %d pairs (%.1f%% of all pairs)\n",
              very_high_count, very_high_pct))
  
  if (clusters$n_clusters > 0) {
    cluster_names <- sapply(clusters$clusters, function(x) {
      paste(x$items, collapse = "-")
    })
    cat(sprintf("    Clusters: %s\n", 
                paste(cluster_names, collapse = ", ")))
  }
  cat("\n")
  
  # Subscale Summary
  cat("  Subscale structure:\n")
  cat(sprintf("    Within-subscale mean:  %.2f\n", subscale_stats$overall_within_mean))
  cat(sprintf("    Between-subscale mean: %.2f\n", subscale_stats$between_stats$mean))
}

# Display evaluation results (extended version)
ii_display_evaluation <- function(miic_poly, miic_pear, cor_poly, cor_pear,
                                  dist_stats, subscale_stats, clusters) {
  cat("\n========================================\n")
  cat("II CORRELATION EVALUATION\n")
  cat("========================================\n")
  
  # Display all sections
  ii_display_correlation_distribution(dist_stats)
  ii_display_subscale_analysis(subscale_stats)
  ii_display_cluster_analysis(clusters)
  ii_display_overall_assessment(miic_poly, miic_pear, dist_stats, 
                                subscale_stats, clusters)
}