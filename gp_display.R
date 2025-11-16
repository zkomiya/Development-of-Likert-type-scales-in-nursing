# ===================================================
# GP Display (View Layer)
# Version: 6.0
# Changes from v5.0:
#   - Added Hedges_g_psych display
#   - Now displays all 4 effect sizes (2x Cohen's d, 2x Hedges' g)
#   - Extended summary statistics display for 4 effect sizes
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
  
  return(formatted[, c("item", "M_good", "M_poor", "SD_good", "SD_poor","Cohens_d", "Hedges_g","D_star")])
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
  
  # 新規追加: psych版 Hedge's g
  cat(sprintf("\nHedge's g:\n"))
  cat(sprintf("  Range: %.3f to %.3f (SD = %.3f)\n", 
              summary_stats$hedges_g_min, 
              summary_stats$hedges_g_max,
              summary_stats$hedges_g_sd))
  
}