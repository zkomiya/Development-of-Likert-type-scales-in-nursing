# ===================================================
# GP Display (View Layer)
# Version: 9.1
# Changes from v9.0:
#   - Added Cohen's d evaluation section
# Description: Display functions for GP analysis results
# Note: All function names have gp_ prefix
# ===================================================

# Function to display header
gp_display_header <- function() {
  cat("========================================\n")
  cat("GP Analysis (Good-Poor Analysis)\n")
  cat("========================================\n\n")
}

# Function to display data detection results
gp_display_data_info <- function(item_cols, n_items) {
  cat("Step 1: Data detection\n")
  cat(sprintf("  Found %d item columns: %s ... %s\n", 
              n_items, item_cols[1], item_cols[n_items]))
}

# Function to display total score info
gp_display_total_score_info <- function(total_score_min, total_score_max) {
  cat("\nStep 2: Total score calculation\n")
  cat(sprintf("  Total score range: %.1f - %.1f\n", 
              total_score_min, total_score_max))
}

# Function to display group splitting info
gp_display_group_info <- function(cutoff_percentile, n_good, n_poor, cutoff_upper, cutoff_lower) {
  cat("\nStep 3: Group splitting\n")
  cat(sprintf("  Using cutoff percentile: %.1f%%\n", cutoff_percentile * 100))
  cat(sprintf("  Good group (upper %.1f%%): n = %d (cutoff >= %.1f)\n", 
              cutoff_percentile * 100, n_good, cutoff_upper))
  cat(sprintf("  Poor group (lower %.1f%%): n = %d (cutoff <= %.1f)\n", 
              cutoff_percentile * 100, n_poor, cutoff_lower))
}

# Function to format results for display
gp_format_results <- function(discrimination_results) {
  formatted <- discrimination_results
  formatted$M_good <- sprintf("%.2f", discrimination_results$M_good)
  formatted$M_poor <- sprintf("%.2f", discrimination_results$M_poor)
  formatted$SD_good <- sprintf("%.2f", discrimination_results$SD_good)
  formatted$SD_poor <- sprintf("%.2f", discrimination_results$SD_poor)
  formatted$Cohens_d <- sprintf("%.6f", discrimination_results$Cohens_d)
  formatted$Hedges_g <- sprintf("%.6f", discrimination_results$Hedges_g) 
  formatted$D_star <- sprintf("%.3f", discrimination_results$D_star)  
  
  return(formatted[, c("item", "n_poor", "n_good", "M_poor", "SD_poor", "M_good", "SD_good", "D_star", "Cohens_d", "Hedges_g")])
}

# Function to display main results
gp_display_results <- function(discrimination_results) {
  cat("\n========================================\n")
  cat("RESULTS\n")
  cat("========================================\n\n")
  
  formatted <- gp_format_results(discrimination_results)
  print(formatted, row.names = FALSE)
}

# Function to display summary statistics
gp_display_summary <- function(summary_stats) {
  cat("\n----------------------------------------\n")
  cat("SUMMARY\n")
  cat("----------------------------------------\n")
  
  cat(sprintf("\nD* (Discrimination Index):\n"))
  cat(sprintf("  Range: %.3f to %.3f (SD = %.3f)\n", 
              summary_stats$d_star_min, 
              summary_stats$d_star_max,
              summary_stats$d_star_sd))
  
  cat(sprintf("\nCohen's d:\n"))
  cat(sprintf("  Range: %.3f to %.3f (SD = %.3f)\n", 
              summary_stats$cohens_d_min, 
              summary_stats$cohens_d_max,
              summary_stats$cohens_d_sd))
  
  cat(sprintf("\nHedge's g:\n"))
  cat(sprintf("  Range: %.3f to %.3f (SD = %.3f)\n", 
              summary_stats$hedges_g_min, 
              summary_stats$hedges_g_max,
              summary_stats$hedges_g_sd))
  
}

# ===================================================
# GP Analysis Evaluation
# ===================================================

gp_display_evaluation <- function(discrimination_results) {
  
  # Thresholds based on established criteria
  # D*: Ebel & Frisbie (1991)
  # Effect size: Cohen (1988)
  THRESH <- list(
    d_star_excellent = 0.40,
    d_star_good = 0.30,
    d_star_acceptable = 0.20,
    effect_large = 0.80,
    effect_medium = 0.50,
    effect_small = 0.20
  )
  
  n_items <- nrow(discrimination_results)
  
  cat("\n========================================\n")
  cat("GP ANALYSIS EVALUATION\n")
  cat("========================================\n")
  
  # --- D* Evaluation ---
  cat("\nDISCRIMINATION INDEX (D*) EVALUATION\n")
  cat("------------------------------------\n")
  cat(sprintf("Threshold: Acceptable >= %.2f\n\n", THRESH$d_star_acceptable))
  
  d_star_categories <- character(n_items)
  d_star_poor_items <- character(0)
  
  for (i in 1:n_items) {
    item <- discrimination_results$item[i]
    d_star <- discrimination_results$D_star[i]
    
    if (d_star >= THRESH$d_star_excellent) {
      category <- "Excellent"
    } else if (d_star >= THRESH$d_star_good) {
      category <- "Good"
    } else if (d_star >= THRESH$d_star_acceptable) {
      category <- "Acceptable"
    } else {
      category <- "Poor"
      d_star_poor_items <- c(d_star_poor_items, item)
    }
    
    d_star_categories[i] <- category
  }
  
  # Display only items below threshold
  if (length(d_star_poor_items) > 0) {
    cat(sprintf("%-8s %8s   %-12s\n", "Item", "D*", "Category"))
    cat(paste(rep("-", 35), collapse = ""), "\n")
    
    for (item in d_star_poor_items) {
      idx <- which(discrimination_results$item == item)
      d_star <- discrimination_results$D_star[idx]
      cat(sprintf("%-8s %8.3f   %-12s\n", item, d_star, "Poor"))
    }
  } else {
    cat("No items below threshold.\n")
  }
  
  # D* Summary
  n_excellent <- sum(d_star_categories == "Excellent")
  n_good <- sum(d_star_categories == "Good")
  n_acceptable <- sum(d_star_categories == "Acceptable")
  n_poor <- sum(d_star_categories == "Poor")
  n_ok <- n_items - n_poor
  pct_ok <- round(100 * n_ok / n_items, 1)
  
  cat(sprintf("\nSummary: %d/%d items acceptable (%.1f%%)\n", n_ok, n_items, pct_ok))
  cat(sprintf("  Excellent: %d, Good: %d, Acceptable: %d, Poor: %d\n",
              n_excellent, n_good, n_acceptable, n_poor))
  
  # --- Cohen's d Evaluation ---
  cat("\nEFFECT SIZE (Cohen's d) EVALUATION\n")
  cat("----------------------------------\n")
  cat(sprintf("Threshold: Small >= %.2f\n\n", THRESH$effect_small))
  
  cohens_d_categories <- character(n_items)
  cohens_d_negligible_items <- character(0)
  
  for (i in 1:n_items) {
    item <- discrimination_results$item[i]
    cohens_d <- discrimination_results$Cohens_d[i]
    
    if (cohens_d >= THRESH$effect_large) {
      category <- "Large"
    } else if (cohens_d >= THRESH$effect_medium) {
      category <- "Medium"
    } else if (cohens_d >= THRESH$effect_small) {
      category <- "Small"
    } else {
      category <- "Negligible"
      cohens_d_negligible_items <- c(cohens_d_negligible_items, item)
    }
    
    cohens_d_categories[i] <- category
  }
  
  # Display only items below threshold
  if (length(cohens_d_negligible_items) > 0) {
    cat(sprintf("%-8s %10s   %-12s\n", "Item", "Cohen's d", "Category"))
    cat(paste(rep("-", 35), collapse = ""), "\n")
    
    for (item in cohens_d_negligible_items) {
      idx <- which(discrimination_results$item == item)
      cohens_d <- discrimination_results$Cohens_d[idx]
      cat(sprintf("%-8s %10.3f   %-12s\n", item, cohens_d, "Negligible"))
    }
  } else {
    cat("No items below threshold.\n")
  }
  
  # Cohen's d Summary
  n_large_d <- sum(cohens_d_categories == "Large")
  n_medium_d <- sum(cohens_d_categories == "Medium")
  n_small_d <- sum(cohens_d_categories == "Small")
  n_negligible_d <- sum(cohens_d_categories == "Negligible")
  n_effect_ok_d <- n_items - n_negligible_d
  pct_effect_ok_d <- round(100 * n_effect_ok_d / n_items, 1)
  
  cat(sprintf("\nSummary: %d/%d items with small+ effect (%.1f%%)\n", 
              n_effect_ok_d, n_items, pct_effect_ok_d))
  cat(sprintf("  Large: %d, Medium: %d, Small: %d, Negligible: %d\n",
              n_large_d, n_medium_d, n_small_d, n_negligible_d))
  
  # --- Hedges' g Evaluation ---
  cat("\nEFFECT SIZE (Hedges' g) EVALUATION\n")
  cat("----------------------------------\n")
  cat(sprintf("Threshold: Small >= %.2f\n\n", THRESH$effect_small))
  
  hedges_g_categories <- character(n_items)
  hedges_g_negligible_items <- character(0)
  
  for (i in 1:n_items) {
    item <- discrimination_results$item[i]
    hedges_g <- discrimination_results$Hedges_g[i]
    
    if (hedges_g >= THRESH$effect_large) {
      category <- "Large"
    } else if (hedges_g >= THRESH$effect_medium) {
      category <- "Medium"
    } else if (hedges_g >= THRESH$effect_small) {
      category <- "Small"
    } else {
      category <- "Negligible"
      hedges_g_negligible_items <- c(hedges_g_negligible_items, item)
    }
    
    hedges_g_categories[i] <- category
  }
  
  # Display only items below threshold
  if (length(hedges_g_negligible_items) > 0) {
    cat(sprintf("%-8s %10s   %-12s\n", "Item", "Hedges' g", "Category"))
    cat(paste(rep("-", 35), collapse = ""), "\n")
    
    for (item in hedges_g_negligible_items) {
      idx <- which(discrimination_results$item == item)
      hedges_g <- discrimination_results$Hedges_g[idx]
      cat(sprintf("%-8s %10.3f   %-12s\n", item, hedges_g, "Negligible"))
    }
  } else {
    cat("No items below threshold.\n")
  }
  
  # Hedges' g Summary
  n_large_g <- sum(hedges_g_categories == "Large")
  n_medium_g <- sum(hedges_g_categories == "Medium")
  n_small_g <- sum(hedges_g_categories == "Small")
  n_negligible_g <- sum(hedges_g_categories == "Negligible")
  n_effect_ok_g <- n_items - n_negligible_g
  pct_effect_ok_g <- round(100 * n_effect_ok_g / n_items, 1)
  
  cat(sprintf("\nSummary: %d/%d items with small+ effect (%.1f%%)\n", 
              n_effect_ok_g, n_items, pct_effect_ok_g))
  cat(sprintf("  Large: %d, Medium: %d, Small: %d, Negligible: %d\n",
              n_large_g, n_medium_g, n_small_g, n_negligible_g))
  
  # --- Overall Assessment ---
  problem_items <- union(d_star_poor_items, 
                         union(cohens_d_negligible_items, hedges_g_negligible_items))
  
  cat("\nOVERALL ASSESSMENT\n")
  cat("------------------\n")
  
  if (length(problem_items) == 0) {
    cat("[OK] All items show adequate discrimination.\n")
  } else if (length(problem_items) <= 3) {
    cat(sprintf("[WARN] %d item(s) may need review: %s\n", 
                length(problem_items), paste(problem_items, collapse = ", ")))
  } else {
    cat(sprintf("[WARN] %d items show inadequate discrimination: %s\n", 
                length(problem_items), paste(problem_items, collapse = ", ")))
  }
  
  cat("\n========================================\n")
  
  # Return evaluation summary (invisible)
  invisible(list(
    d_star_categories = d_star_categories,
    cohens_d_categories = cohens_d_categories,
    hedges_g_categories = hedges_g_categories,
    problem_items = problem_items,
    summary = list(
      d_star = list(excellent = n_excellent, good = n_good, 
                    acceptable = n_acceptable, poor = n_poor),
      cohens_d = list(large = n_large_d, medium = n_medium_d, 
                      small = n_small_d, negligible = n_negligible_d),
      hedges_g = list(large = n_large_g, medium = n_medium_g, 
                      small = n_small_g, negligible = n_negligible_g)
    )
  ))
}