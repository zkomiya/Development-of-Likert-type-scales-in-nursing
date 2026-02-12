# ===================================================
# Data Preprocessor for Factor Analysis
# Version: 2.2 (YAML-driven scale range and item pattern)
# Description: EFA/CFA specific data preprocessing
# ===================================================

# Preprocess data for factor analysis
preprocess_for_fa <- function(data,
                              method = "listwise",
                              verbose = TRUE,
                              item_pattern,
                              scale_min,
                              scale_max) {
  
  # Validate required parameters
  if (missing(item_pattern) || is.null(item_pattern) || !is.character(item_pattern)) {
    stop("item_pattern must be provided (from analysis_config.yaml: global$item_pattern)")
  }
  
  if (missing(scale_min) || missing(scale_max) || is.null(scale_min) || is.null(scale_max)) {
    stop("scale_min and scale_max must be provided (from analysis_config.yaml: global$scale$min/max)")
  }
  
  if (!is.numeric(scale_min) || !is.numeric(scale_max) || scale_min >= scale_max) {
    stop("Invalid scale range: scale_min must be numeric and < scale_max")
  }
  
  if (verbose) {
    cat("\nFactor Analysis Data Preprocessing\n")
  }
  
  initial_n <- nrow(data)
  initial_p <- ncol(data)
  
  # Get Q columns based on configured pattern
  q_cols <- grep(item_pattern, names(data), value = TRUE)
  
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
  
  # Verify ordinal range (configured)
  check_range <- function(x) {
    vals <- x[!is.na(x)]
    out_of_range <- sum(vals < scale_min | vals > scale_max | vals != round(vals))
    return(out_of_range)
  }
  
  range_issues <- sapply(fa_data, check_range)
  if (any(range_issues > 0)) {
    stop(sprintf("Values outside %s-%s range in: ",
                 as.character(scale_min), as.character(scale_max)),
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
