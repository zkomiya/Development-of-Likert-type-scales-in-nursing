# ===================================================
# Validity Display (View Layer)
# ===================================================

# Resolve display name from key
resolve_display_name <- function(key, target_subscales, rehab_gb_subscales,
                                 rehab_db_definition) {
  # Check target subscales
  if (key %in% names(target_subscales)) {
    return(target_subscales[[key]]$name)
  }
  # Check REHAB GB subscales
  if (key %in% names(rehab_gb_subscales)) {
    return(rehab_gb_subscales[[key]]$name)
  }
  # Check deviant behavior
  if (key == "deviant_behavior" && !is.null(rehab_db_definition)) {
    return(rehab_db_definition$name)
  }
  # Fallback
  return(key)
}

# Display header
validity_display_header <- function() {
  cat("========================================\n")
  cat("REHAB External Validity Analysis\n")
  cat("========================================\n\n")
}

# Total score level display
validity_display_total <- function(results) {
  cat("=== Total Score Correlations ===\n")
  cat(sprintf("Sample size: n = %d\n", results$total_level$gb$n))
  cat(sprintf("Method: %s\n\n", results$total_level$gb$method))
  
  cat("              r        95% CI           Power\n")
  cat("-------------------------------------------------------\n")
  
  cat(sprintf("REHAB-GB   %6.3f   [%6.3f, %6.3f]   %5.3f\n",
              results$total_level$gb$r,
              results$total_level$gb$ci_lower,
              results$total_level$gb$ci_upper,
              results$total_level$gb$power))
  
  cat(sprintf("REHAB-DB   %6.3f   [%6.3f, %6.3f]   %5.3f\n",
              results$total_level$db$r,
              results$total_level$db$ci_lower,
              results$total_level$db$ci_upper,
              results$total_level$db$power))
}

# Subscale level display
validity_display_subscales <- function(results, target_subscales) {
  cat("\n=== Subscale Correlations ===\n\n")
  
  subscale_names <- names(results$subscale_level$gb)
  
  cat("Correlations with REHAB-GB:\n")
  cat("                                           r        95% CI           Power\n")
  cat("--------------------------------------------------------------------------------\n")
  
  for (subscale in subscale_names) {
    display_name <- target_subscales[[subscale]]$name
    if (nchar(display_name) > 40) {
      display_name <- paste0(substr(display_name, 1, 37), "...")
    }
    
    result <- results$subscale_level$gb[[subscale]]
    cat(sprintf("%-40s  %6.3f   [%6.3f, %6.3f]   %5.3f\n",
                display_name,
                result$r,
                result$ci_lower,
                result$ci_upper,
                result$power))
  }
  
  cat("\nCorrelations with REHAB-DB:\n")
  cat("                                           r        95% CI           Power\n")
  cat("--------------------------------------------------------------------------------\n")
  
  for (subscale in subscale_names) {
    display_name <- target_subscales[[subscale]]$name
    if (nchar(display_name) > 40) {
      display_name <- paste0(substr(display_name, 1, 37), "...")
    }
    
    result <- results$subscale_level$db[[subscale]]
    cat(sprintf("%-40s  %6.3f   [%6.3f, %6.3f]   %5.3f\n",
                display_name,
                result$r,
                result$ci_lower,
                result$ci_upper,
                result$power))
  }
}

# Subscale x subscale matrix display
validity_display_subscale_matrix <- function(results, target_subscales,
                                             rehab_gb_subscales) {
  cat("\n=== Target Subscales x REHAB GB Subscales Correlation Matrix ===\n\n")
  
  target_names <- names(results$subscale_x_subscale)
  gb_names <- names(results$subscale_x_subscale[[target_names[1]]])
  
  cat(sprintf("Method: %s\n\n",
              results$subscale_x_subscale[[target_names[1]]][[gb_names[1]]]$method))
  
  cat(sprintf("%-45s", ""))
  for (gb_name in gb_names) {
    display_name <- rehab_gb_subscales[[gb_name]]$name
    if (nchar(display_name) > 14) {
      display_name <- substr(display_name, 1, 14)
    }
    cat(sprintf("%16s", display_name))
  }
  cat("\n")
  cat(paste(rep("-", 45 + 16 * length(gb_names)), collapse = ""), "\n")
  
  for (target_name in target_names) {
    display_name <- target_subscales[[target_name]]$name
    if (nchar(display_name) > 43) {
      display_name <- paste0(substr(display_name, 1, 40), "...")
    }
    cat(sprintf("%-45s", display_name))
    
    for (gb_name in gb_names) {
      r <- results$subscale_x_subscale[[target_name]][[gb_name]]$r
      cat(sprintf("%16.3f", r))
    }
    cat("\n")
  }
}

# Hypothesis testing display
validity_display_hypotheses <- function(hyp_results,
                                        target_subscales,
                                        rehab_gb_subscales,
                                        rehab_db_definition) {
  method <- hyp_results[[1]]$method
  cat(sprintf("\n=== Hypothesis Testing (%s) ===\n\n",
              ifelse(method == "pearson", "Primary: Pearson",
                     "Sensitivity: Spearman")))
  
  n <- hyp_results[[1]]$n
  cat(sprintf("Sample size: n = %d\n\n", n))
  
  # Header
  cat(sprintf("%-4s %-24s %-18s %-18s %8s %8s %8s %8s %10s %s\n",
              "No.", "Target Factor", "Proximal", "Distal",
              "|r_prox|", "|r_dist|", "diff", "t", "p(1-side)", "Support"))
  cat(paste(rep("-", 140), collapse = ""), "\n")
  
  supported_count <- 0
  total_count <- 0
  
  for (h_name in names(hyp_results)) {
    h <- hyp_results[[h_name]]
    total_count <- total_count + 1
    
    target_name <- resolve_display_name(
      h$target, target_subscales, rehab_gb_subscales, rehab_db_definition
    )
    proximal_name <- resolve_display_name(
      h$proximal, target_subscales, rehab_gb_subscales, rehab_db_definition
    )
    distal_name <- resolve_display_name(
      h$distal, target_subscales, rehab_gb_subscales, rehab_db_definition
    )
    
    # Truncate names
    if (nchar(target_name) > 22) {
      target_name <- paste0(substr(target_name, 1, 19), "...")
    }
    if (nchar(proximal_name) > 16) {
      proximal_name <- paste0(substr(proximal_name, 1, 13), "...")
    }
    if (nchar(distal_name) > 16) {
      distal_name <- paste0(substr(distal_name, 1, 13), "...")
    }
    
    if (is.na(h$supported)) {
      cat(sprintf("%-4s %-24s %-18s %-18s %8s %8s %8s %8s %10s %s\n",
                  h_name, target_name, proximal_name, distal_name,
                  "NA", "NA", "NA", "NA", "NA", "NA"))
    } else {
      support_label <- ifelse(h$supported, "Yes", "No")
      if (h$supported) supported_count <- supported_count + 1
      
      cat(sprintf("%-4s %-24s %-18s %-18s %8.3f %8.3f %8.3f %8.3f %10.4f %s\n",
                  h_name, target_name, proximal_name, distal_name,
                  h$abs_r_proximal, h$abs_r_distal, h$abs_diff,
                  h$t_stat, h$p_value, support_label))
    }
  }
  
  cat(paste(rep("-", 140), collapse = ""), "\n")
  cat(sprintf("\nSummary: %d / %d hypotheses supported (%.1f%%)\n",
              supported_count, total_count,
              100 * supported_count / total_count))
  
  # Signed correlations detail
  cat(sprintf("\nSigned correlations (r):\n"))
  cat(sprintf("%-4s %10s %10s %10s\n",
              "No.", "r(X,Y)", "r(X,Z)", "r(Y,Z)"))
  cat(paste(rep("-", 40), collapse = ""), "\n")
  
  for (h_name in names(hyp_results)) {
    h <- hyp_results[[h_name]]
    if (!is.na(h$r_proximal)) {
      cat(sprintf("%-4s %10.3f %10.3f %10.3f\n",
                  h_name, h$r_proximal, h$r_distal, h$r_yz))
    }
  }
}