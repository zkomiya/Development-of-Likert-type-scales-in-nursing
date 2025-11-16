# ===================================================
# Data Preprocessor for Factor Analysis
# Version: 2.0 (Revised - No auto-imputation)
# Description: EFA/CFA specific data preprocessing
# ===================================================

# Preprocess data for factor analysis
preprocess_for_fa <- function(data, method = "listwise", verbose = TRUE) {
  
  if (verbose) {
    cat("\nFactor Analysis Data Preprocessing\n")
  }
  
  initial_n <- nrow(data)
  initial_p <- ncol(data)
  
  # Get Q columns
  q_cols <- grep("^Q\\d{1,2}$", names(data), value = TRUE)
  
  if (length(q_cols) == 0) {
    stop("No Q columns found in data")
  }
  
  # Extract Q columns only
  fa_data <- data[q_cols]
  
  if (verbose) {
    cat("Selected", length(q_cols), "variables\n")
  }
  
  # Handle missing data
  if (method == "listwise") {
    complete_rows <- complete.cases(fa_data)
    fa_data <- fa_data[complete_rows, ]
    
    if (verbose) {
      n_removed <- sum(!complete_rows)
      if (n_removed > 0) {
        cat("Removed", n_removed, "cases with missing values\n")
        cat("Remaining cases:", nrow(fa_data), "\n")
      }
    }
    
  } else if (method == "pairwise") {
    if (verbose) {
      missing_per_var <- colSums(is.na(fa_data))
      if (any(missing_per_var > 0)) {
        cat("Missing values per variable:\n")
        print(missing_per_var[missing_per_var > 0])
      }
    }
  } else {
    stop("Method must be 'listwise' or 'pairwise'")
  }
  
  # Verify ordinal range
  check_range <- function(x) {
    vals <- x[!is.na(x)]
    out_of_range <- sum(vals < 1 | vals > 4 | vals != round(vals))
    return(out_of_range)
  }
  
  range_issues <- sapply(fa_data, check_range)
  if (any(range_issues > 0)) {
    stop("Values outside 1-4 range in: ", 
         paste(names(range_issues)[range_issues > 0], collapse = ", "))
  }
  
  # Final summary
  if (verbose) {
    cat("Final dimensions:", nrow(fa_data), "cases x", ncol(fa_data), "variables\n")
    ratio <- nrow(fa_data) / ncol(fa_data)
    cat("Case-to-variable ratio:", round(ratio, 1), ":1\n")
  }
  
  return(fa_data)
}