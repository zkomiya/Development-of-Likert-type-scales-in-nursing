# ===================================================
# GP Display (View Layer)
# Version: 7.0
# Changes from v6.0:
#   - Added gp_display_evaluation() function
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
  
  return(formatted[, c("item", "M_poor", "M_good", "SD_poor", "SD_good", "Cohens_d", "Hedges_g", "D_star")])
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
  cat(sprintf("Threshold: Excellent >= %.2f, Good >= %.2f, Acceptable >= %.2f\n\n",
              THRESH$d_star_excellent, THRESH$d_star_good, THRESH$d_star_acceptable))
  
  cat(sprintf("%-8s %8s   %-12s %s\n", "Item", "D*", "Category", "Status"))
  cat(paste(rep("-", 45), collapse = ""), "\n")
  
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
    status <- if (category == "Poor") "[!]" else ""
    
    cat(sprintf("%-8s %8.3f   %-12s %s\n", item, d_star, category, status))
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
  
  # --- Effect Size Evaluation (Hedges' g) ---
  cat("\nEFFECT SIZE (Hedges' g) EVALUATION\n")
  cat("----------------------------------\n")
  cat(sprintf("Threshold: Large >= %.2f, Medium >= %.2f, Small >= %.2f\n\n",
              THRESH$effect_large, THRESH$effect_medium, THRESH$effect_small))
  
  cat(sprintf("%-8s %10s   %-12s %s\n", "Item", "Hedges_g", "Category", "Status"))
  cat(paste(rep("-", 45), collapse = ""), "\n")
  
  effect_categories <- character(n_items)
  effect_negligible_items <- character(0)
  
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
      effect_negligible_items <- c(effect_negligible_items, item)
    }
    
    effect_categories[i] <- category
    status <- if (category == "Negligible") "[!]" else ""
    
    cat(sprintf("%-8s %10.3f   %-12s %s\n", item, hedges_g, category, status))
  }
  
  # Effect Size Summary
  n_large <- sum(effect_categories == "Large")
  n_medium <- sum(effect_categories == "Medium")
  n_small <- sum(effect_categories == "Small")
  n_negligible <- sum(effect_categories == "Negligible")
  n_effect_ok <- n_items - n_negligible
  pct_effect_ok <- round(100 * n_effect_ok / n_items, 1)
  
  cat(sprintf("\nSummary: %d/%d items with small+ effect (%.1f%%)\n", 
              n_effect_ok, n_items, pct_effect_ok))
  cat(sprintf("  Large: %d, Medium: %d, Small: %d, Negligible: %d\n",
              n_large, n_medium, n_small, n_negligible))
  
  # --- Items Requiring Attention ---
  problem_items <- union(d_star_poor_items, effect_negligible_items)
  
  if (length(problem_items) > 0) {
    cat("\nITEMS REQUIRING ATTENTION\n")
    cat("-------------------------\n")
    
    for (item in problem_items) {
      idx <- which(discrimination_results$item == item)
      d_star <- discrimination_results$D_star[idx]
      hedges_g <- discrimination_results$Hedges_g[idx]
      d_cat <- d_star_categories[idx]
      e_cat <- effect_categories[idx]
      
      cat(sprintf("[!] %s: D* = %.3f (%s), Hedges' g = %.3f (%s)\n",
                  item, d_star, d_cat, hedges_g, e_cat))
    }
  }
  
  # --- Overall Assessment ---
  cat("\nOVERALL ASSESSMENT\n")
  cat("------------------\n")
  
  if (length(problem_items) == 0) {
    cat("[OK] All items show adequate discrimination.\n")
  } else if (length(problem_items) <= 3) {
    cat(sprintf("[OK] Majority of items show adequate discrimination.\n"))
    cat(sprintf("[!] %d item(s) may need review: %s\n", 
                length(problem_items), paste(problem_items, collapse = ", ")))
  } else {
    cat(sprintf("[!] %d items show inadequate discrimination.\n", length(problem_items)))
    cat(sprintf("    Items: %s\n", paste(problem_items, collapse = ", ")))
  }
  
  cat("\n========================================\n")
  
  # Return evaluation summary (invisible)
  invisible(list(
    d_star_categories = d_star_categories,
    effect_categories = effect_categories,
    problem_items = problem_items,
    summary = list(
      d_star = list(excellent = n_excellent, good = n_good, 
                    acceptable = n_acceptable, poor = n_poor),
      effect = list(large = n_large, medium = n_medium, 
                    small = n_small, negligible = n_negligible)
    )
  ))
}