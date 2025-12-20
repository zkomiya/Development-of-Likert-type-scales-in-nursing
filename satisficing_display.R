# ===================================================
# Satisficing Display (View Layer)
# Version: 1.1
# Description: Display functions for satisficing analysis
# Changes from v1.0:
#   - Display all flagged cases (removed max_display limit)
#   - Display ID instead of row index
# ===================================================

# Display header
satisficing_display_header <- function() {
  cat("========================================\n")
  cat("SATISFICING ANALYSIS\n")
  cat("(Careless/Insufficient Effort Responding)\n")
  cat("========================================\n\n")
}

# Display descriptive statistics
satisficing_display_descriptives <- function(stats) {
  cat("--- Descriptive Statistics ---\n\n")
  
  cat("Index              n      Mean      SD      Min      Q25   Median      Q75      Max\n")
  cat("------------------------------------------------------------------------------------\n")
  
  # Longstring
  cat(sprintf("Longstring     %5.0f   %7.2f  %6.2f   %6.2f   %6.2f   %6.2f   %6.2f   %6.2f\n",
              stats$longstring["n"],
              stats$longstring["mean"],
              stats$longstring["sd"],
              stats$longstring["min"],
              stats$longstring["q25"],
              stats$longstring["median"],
              stats$longstring["q75"],
              stats$longstring["max"]))
  
  # IRV
  cat(sprintf("IRV            %5.0f   %7.3f  %6.3f   %6.3f   %6.3f   %6.3f   %6.3f   %6.3f\n",
              stats$irv["n"],
              stats$irv["mean"],
              stats$irv["sd"],
              stats$irv["min"],
              stats$irv["q25"],
              stats$irv["median"],
              stats$irv["q75"],
              stats$irv["max"]))
  
  # Mahalanobis D^2
  cat(sprintf("Mahalanobis    %5.0f   %7.2f  %6.2f   %6.2f   %6.2f   %6.2f   %6.2f   %6.2f\n",
              stats$mahalanobis_d2["n"],
              stats$mahalanobis_d2["mean"],
              stats$mahalanobis_d2["sd"],
              stats$mahalanobis_d2["min"],
              stats$mahalanobis_d2["q25"],
              stats$mahalanobis_d2["median"],
              stats$mahalanobis_d2["q75"],
              stats$mahalanobis_d2["max"]))
  
  cat("\n")
}

# Display thresholds used
satisficing_display_thresholds <- function(thresholds) {
  cat("--- Thresholds Applied ---\n\n")
  
  cat(sprintf("Longstring:    > %d consecutive identical responses\n", 
              thresholds$ls_threshold))
  cat(sprintf("IRV:           < %.3f (%.0fth percentile)\n", 
              thresholds$irv_threshold_value,
              thresholds$irv_percentile))
  cat(sprintf("Mahalanobis:   p < %.4f\n", 
              thresholds$mahad_p_threshold))
  
  cat("\n")
}

# Display flagging summary
satisficing_display_summary <- function(summary) {
  cat("--- Flagging Summary ---\n\n")
  
  cat(sprintf("Total cases:                    %d\n", summary$n_total))
  cat(sprintf("Longstring flagged:             %d (%.1f%%)\n", 
              summary$n_ls_flagged,
              100 * summary$n_ls_flagged / summary$n_total))
  cat(sprintf("IRV flagged:                    %d (%.1f%%)\n", 
              summary$n_irv_flagged,
              100 * summary$n_irv_flagged / summary$n_total))
  cat(sprintf("Mahalanobis flagged:            %d (%.1f%%)\n", 
              summary$n_mahad_flagged,
              100 * summary$n_mahad_flagged / summary$n_total))
  cat(sprintf("Any indicator flagged:          %d (%.1f%%)\n", 
              summary$n_any_flagged,
              100 * summary$n_any_flagged / summary$n_total))
  
  cat("\n")
}

# Display flagged cases detail (all cases)
satisficing_display_flagged_cases <- function(indices, flags) {
  flagged_rows <- which(flags$any_flag)
  n_flagged <- length(flagged_rows)
  
  if (n_flagged == 0) {
    cat("--- No cases flagged ---\n\n")
    return(invisible(NULL))
  }
  
  cat("--- Flagged Cases Detail ---\n\n")
  
  cat("ID                LS    IRV     D^2        p      Flags\n")
  cat("--------------------------------------------------------\n")
  
  for (i in seq_along(flagged_rows)) {
    idx <- flagged_rows[i]
    
    # Build flag string
    flag_str <- ""
    if (flags$ls_flag[idx]) flag_str <- paste0(flag_str, "LS ")
    if (flags$irv_flag[idx]) flag_str <- paste0(flag_str, "IRV ")
    if (flags$mahad_flag[idx]) flag_str <- paste0(flag_str, "MAH ")
    
    # Get ID (truncate if too long)
    id_str <- as.character(indices$id[idx])
    if (nchar(id_str) > 14) {
      id_str <- paste0(substr(id_str, 1, 11), "...")
    }
    
    cat(sprintf("%-14s  %4.0f  %5.3f  %6.2f   %6.4f   %s\n",
                id_str,
                indices$longstring[idx],
                indices$irv[idx],
                indices$mahalanobis_d2[idx],
                indices$mahalanobis_p[idx],
                flag_str))
  }
  
  cat("\n")
}

# Display mathematical definitions
satisficing_display_definitions <- function() {
  cat("--- Mathematical Definitions ---\n\n")
  
  cat("Longstring (LS):\n")
  cat("  LS_i = max(consecutive identical responses)\n\n")
  
  cat("Intra-individual Response Variability (IRV):\n")
  cat("  IRV_i = sqrt(1/(J-1) * sum((x_ij - x_bar_i)^2))\n\n")
  
  cat("Mahalanobis Distance:\n")
  cat("  D^2_i = (x_i - mu)' * S^(-1) * (x_i - mu)\n")
  cat("  where mu = sample mean vector, S = sample covariance matrix\n\n")
}