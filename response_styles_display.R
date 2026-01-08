# ===================================================
# Response Styles Display (View Layer)
# Version: 1.4
# Description: Display functions for response styles analysis
# Changes from v1.3:
#   - Handle threshold at maximum (>= 1.0) cases
# ===================================================

# Display header
response_styles_display_header <- function(dataset_name) {
  cat("========================================\n")
  cat(sprintf("RESPONSE STYLES ANALYSIS: %s\n", dataset_name))
  cat("(ERS and MRS Estimation)\n")
  cat("========================================\n\n")
}

# Display mathematical definitions
response_styles_display_definitions <- function(min_cat, max_cat) {
  middle_cats <- (min_cat + 1):(max_cat - 1)
  
  cat("--- Mathematical Definitions ---\n\n")
  
  cat("Extreme Response Style (ERS):\n")
  cat(sprintf("  ERS_i = (n_{i,%d} + n_{i,%d}) / J\n", min_cat, max_cat))
  cat(sprintf("  where n_{i,k} = count of category k for individual i\n"))
  cat(sprintf("  Extreme categories: %d and %d\n\n", min_cat, max_cat))
  
  cat("Midpoint Response Style (MRS):\n")
  cat(sprintf("  MRS_i = (n_{i,%s}) / J\n", 
              paste(middle_cats, collapse = "} + n_{i,")))
  cat(sprintf("  Middle categories: %s\n\n", 
              paste(middle_cats, collapse = ", ")))
}

# Display descriptive statistics
response_styles_display_descriptives <- function(stats) {
  cat("--- Descriptive Statistics ---\n\n")
  
  cat("Index       n      Mean      SD      Min      Q25   Median      Q75      Max\n")
  cat("-----------------------------------------------------------------------------\n")
  
  # ERS
  cat(sprintf("ERS     %5.0f    %5.3f   %5.3f    %5.3f    %5.3f    %5.3f    %5.3f    %5.3f\n",
              stats$ers["n"],
              stats$ers["mean"],
              stats$ers["sd"],
              stats$ers["min"],
              stats$ers["q25"],
              stats$ers["median"],
              stats$ers["q75"],
              stats$ers["max"]))
  
  # MRS
  cat(sprintf("MRS     %5.0f    %5.3f   %5.3f    %5.3f    %5.3f    %5.3f    %5.3f    %5.3f\n",
              stats$mrs["n"],
              stats$mrs["mean"],
              stats$mrs["sd"],
              stats$mrs["min"],
              stats$mrs["q25"],
              stats$mrs["median"],
              stats$mrs["q75"],
              stats$mrs["max"]))
  
  cat("\n")
}

# Display thresholds
response_styles_display_thresholds <- function(thresholds) {
  cat("--- Flagging Thresholds ---\n\n")
  
  cat(sprintf("Percentile used: %.0fth\n", thresholds$percentile))
  
  if (thresholds$ers_threshold >= 1.0) {
    cat("ERS threshold:   (No high ERS cases possible)\n")
  } else {
    cat(sprintf("ERS threshold:   > %.3f\n", thresholds$ers_threshold))
  }
  
  if (thresholds$mrs_threshold >= 1.0) {
    cat("MRS threshold:   (No high MRS cases possible)\n")
  } else {
    cat(sprintf("MRS threshold:   > %.3f\n", thresholds$mrs_threshold))
  }
  
  cat("\n")
}

# Display flagging summary
response_styles_display_summary <- function(summary, thresholds) {
  cat("--- Flagging Summary ---\n\n")
  
  cat(sprintf("Total cases:            %d\n", summary$n_total))
  
  if (thresholds$ers_threshold >= 1.0) {
    cat("High ERS flagged:       N/A (threshold at maximum)\n")
  } else {
    cat(sprintf("High ERS flagged:       %d (%.1f%%)\n",
                summary$n_ers_flagged,
                100 * summary$n_ers_flagged / summary$n_total))
  }
  
  if (thresholds$mrs_threshold >= 1.0) {
    cat("High MRS flagged:       N/A (threshold at maximum)\n")
  } else {
    cat(sprintf("High MRS flagged:       %d (%.1f%%)\n",
                summary$n_mrs_flagged,
                100 * summary$n_mrs_flagged / summary$n_total))
  }
  
  cat("\n")
}

# Display correlations with scale scores
response_styles_display_correlations <- function(correlations) {
  cat("--- Correlations with Total Score ---\n\n")
  
  cat("Index       r        95% CI              p\n")
  cat("--------------------------------------------\n")
  
  # ERS
  cat(sprintf("ERS     %6.3f   [%6.3f, %6.3f]   %s\n",
              correlations$ers_total$r,
              correlations$ers_total$ci_lower,
              correlations$ers_total$ci_upper,
              format_p_value(correlations$ers_total$p)))
  
  # MRS
  cat(sprintf("MRS     %6.3f   [%6.3f, %6.3f]   %s\n",
              correlations$mrs_total$r,
              correlations$mrs_total$ci_lower,
              correlations$mrs_total$ci_upper,
              format_p_value(correlations$mrs_total$p)))
  
  cat("\n")
}

# Format p-value for display
format_p_value <- function(p) {
  if (is.na(p)) {
    return("NA")
  } else if (p < 0.001) {
    return("< .001")
  } else {
    return(sprintf("%.3f", p))
  }
}

# Display flagged cases detail (all cases)
response_styles_display_flagged_cases <- function(indices, flags, key_name, thresholds) {
  # Get flagged rows for each type
  ers_flagged <- which(flags$ers_flag)
  mrs_flagged <- which(flags$mrs_flag)
  
  # ERS cases
  if (thresholds$ers_threshold >= 1.0) {
    cat("--- High ERS Cases ---\n\n")
    cat("No high ERS cases possible (threshold at maximum)\n\n")
  } else if (length(ers_flagged) > 0) {
    cat("--- High ERS Cases ---\n\n")
    
    cat(sprintf("%-14s    ERS      MRS\n", key_name))
    cat("-------------------------------\n")
    
    for (i in seq_along(ers_flagged)) {
      idx <- ers_flagged[i]
      
      # Get ID (truncate if too long)
      id_str <- as.character(indices$id[idx])
      if (nchar(id_str) > 14) {
        id_str <- paste0(substr(id_str, 1, 11), "...")
      }
      
      cat(sprintf("%-14s  %5.3f    %5.3f\n",
                  id_str,
                  indices$ers[idx],
                  indices$mrs[idx]))
    }
    
    cat("\n")
  }
  
  # MRS cases
  if (thresholds$mrs_threshold >= 1.0) {
    cat("--- High MRS Cases ---\n\n")
    cat("No high MRS cases possible (threshold at maximum)\n\n")
  } else if (length(mrs_flagged) > 0) {
    cat("--- High MRS Cases ---\n\n")
    
    cat(sprintf("%-14s    ERS      MRS\n", key_name))
    cat("-------------------------------\n")
    
    for (i in seq_along(mrs_flagged)) {
      idx <- mrs_flagged[i]
      
      # Get ID (truncate if too long)
      id_str <- as.character(indices$id[idx])
      if (nchar(id_str) > 14) {
        id_str <- paste0(substr(id_str, 1, 11), "...")
      }
      
      cat(sprintf("%-14s  %5.3f    %5.3f\n",
                  id_str,
                  indices$ers[idx],
                  indices$mrs[idx]))
    }
    
    cat("\n")
  }
}