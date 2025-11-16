# ===================================================
# Validity Calculator (Model Layer)
# Version: 7.0 - Pearson correlation with progress messages
# Changes from v6.0:
#   - Added progress messages for better user feedback
#   - Removed unnecessary Polychoric correlation for continuous data
# ===================================================

library(pwr)
library(psych)

# Calculate REHAB scores (raw scores only)
calculate_rehab_scores <- function(rehab_data) {
  # GB items (R_Q08 to R_Q23)
  gb_items <- c("R_Q08", "R_Q09", "R_Q10", "R_Q11", "R_Q12", "R_Q13",
                "R_Q14", "R_Q15", "R_Q16", "R_Q17", "R_Q18", "R_Q19",
                "R_Q20", "R_Q21", "R_Q22", "R_Q23")
  
  # DB items (R_Q01 to R_Q07)
  db_items <- paste0("R_Q", sprintf("%02d", 1:7))
  
  # Calculate raw scores
  gb_score <- rowSums(rehab_data[, gb_items], na.rm = TRUE)
  db_score <- rowSums(rehab_data[, db_items], na.rm = TRUE)
  
  return(list(
    gb = gb_score,
    db = db_score
  ))
}

# Calculate REHAB GB subscale scores
calculate_rehab_gb_subscale_scores <- function(rehab_data) {
  CONFIG <- load_config()
  gb_subscales <- CONFIG$analysis$rehab_subscales$gb_subscales
  
  scores <- list()
  for (name in names(gb_subscales)) {
    subscale_info <- gb_subscales[[name]]
    items <- subscale_info$items
    available_items <- items[items %in% names(rehab_data)]
    if (length(available_items) > 0) {
      scores[[name]] <- rowSums(rehab_data[available_items], na.rm = TRUE)
    }
  }
  
  return(scores)
}

# Calculate 30items subscale scores
calculate_30items_scores <- function(data) {
  # Load subscale definitions from config
  CONFIG <- load_config()
  subscales <- CONFIG$analysis$item_total_analysis
  
  scores <- list()
  scores$total <- rowSums(data, na.rm = TRUE)
  
  # Calculate each subscale
  for (name in names(subscales)) {
    if (name != "enable_subscales" && !is.null(subscales[[name]]$items)) {
      items <- subscales[[name]]$items
      available_items <- items[items %in% names(data)]
      if (length(available_items) > 0) {
        scores[[name]] <- rowSums(data[available_items], na.rm = TRUE)
      }
    }
  }
  
  return(scores)
}

# Calculate correlation with confidence interval and power
calculate_correlation_with_ci_power <- function(x, y, alpha = 0.05) {
  # Remove missing pairs
  complete_cases <- complete.cases(x, y)
  x_clean <- x[complete_cases]
  y_clean <- y[complete_cases]
  n <- length(x_clean)
  
  if (n < 3) {
    return(list(r = NA, ci_lower = NA, ci_upper = NA, power = NA, n = n))
  }
  
  # Correlation and CI
  cor_result <- cor.test(x_clean, y_clean, method = "pearson")
  r <- cor_result$estimate
  ci <- cor_result$conf.int
  
  # Post-hoc power
  if (abs(r) > 0 && n > 3) {
    power <- pwr.r.test(n = n, r = abs(r), sig.level = alpha)$power
  } else {
    power <- NA
  }
  
  return(list(
    r = r,
    ci_lower = ci[1],
    ci_upper = ci[2],
    power = power,
    n = n
  ))
}

# Main validity analysis function with ID matching
analyze_validity_correlations <- function(target_obj, rehab_obj) {
  
  # Load data structure functions
  source("data_structure.R")
  
  # Extract data and keys
  target_data <- get_data(target_obj)
  target_keys <- get_keys(target_obj)
  
  rehab_data <- get_data(rehab_obj)
  rehab_keys <- get_keys(rehab_obj)
  
  # Convert to data.frame if tibble
  target_data <- as.data.frame(target_data)
  rehab_data <- as.data.frame(rehab_data)
  
  # Get patient IDs from row names
  target_ids <- rownames(target_data)
  rehab_ids <- rownames(rehab_data)
  
  # Find common patient IDs
  common_ids <- intersect(target_ids, rehab_ids)
  
  # Report matching status
  cat("\n--- Patient ID Matching ---\n")
  cat(sprintf("Target patients: %d\n", length(target_ids)))
  cat(sprintf("REHAB patients: %d\n", length(rehab_ids)))
  cat(sprintf("Matched patients: %d\n", length(common_ids)))
  
  if (length(common_ids) == 0) {
    stop("No matching patient IDs found between datasets!")
  }
  
  # Extract matched data only
  target_matched <- target_data[target_ids %in% common_ids, , drop = FALSE]
  rehab_matched <- rehab_data[rehab_ids %in% common_ids, , drop = FALSE]
  
  # Ensure same order
  target_matched <- target_matched[order(rownames(target_matched)), , drop = FALSE]
  rehab_matched <- rehab_matched[order(rownames(rehab_matched)), , drop = FALSE]
  
  # Verify matching
  if (!all(rownames(target_matched) == rownames(rehab_matched))) {
    stop("Patient ID ordering mismatch after sorting!")
  }
  
  cat("Patient matching successful!\n\n")
  
  # Calculate scores on matched data
  cat("Calculating scores...\n")
  rehab_scores <- calculate_rehab_scores(rehab_matched)
  rehab_gb_subscale_scores <- calculate_rehab_gb_subscale_scores(rehab_matched)
  items30_scores <- calculate_30items_scores(target_matched)
  cat("Score calculation complete.\n\n")
  
  # Initialize results
  results <- list()
  
  # Total score correlations with progress messages
  cat("Calculating total-level correlations...\n")
  cat("  REHAB-GB correlation...")
  results$total_level <- list(
    gb = calculate_correlation_with_ci_power(items30_scores$total, rehab_scores$gb),
    db = NULL
  )
  cat(" done\n")
  
  cat("  REHAB-DB correlation...")
  results$total_level$db <- calculate_correlation_with_ci_power(items30_scores$total, rehab_scores$db)
  cat(" done\n")
  cat("Total-level correlations complete.\n\n")
  
  # Subscale correlations with progress messages
  cat("Calculating subscale-level correlations...\n")
  subscale_names <- names(items30_scores)[names(items30_scores) != "total"]
  results$subscale_level <- list(gb = list(), db = list())
  
  for (i in seq_along(subscale_names)) {
    subscale <- subscale_names[i]
    cat(sprintf("  Subscale %d/%d: %s...", i, length(subscale_names), subscale))
    
    results$subscale_level$gb[[subscale]] <- 
      calculate_correlation_with_ci_power(items30_scores[[subscale]], rehab_scores$gb)
    results$subscale_level$db[[subscale]] <- 
      calculate_correlation_with_ci_power(items30_scores[[subscale]], rehab_scores$db)
    
    cat(" done\n")
  }
  cat("Subscale-level correlations complete.\n\n")
  
  # Subscale x subscale correlations with progress messages
  cat("Calculating subscale x subscale correlations...\n")
  results$subscale_x_subscale <- list()
  
  total_combinations <- length(subscale_names) * length(rehab_gb_subscale_scores)
  current <- 0
  
  for (items30_subscale in subscale_names) {
    results$subscale_x_subscale[[items30_subscale]] <- list()
    
    for (gb_subscale in names(rehab_gb_subscale_scores)) {
      current <- current + 1
      
      # Progress every 10 items or at the end
      if (current %% 10 == 0 || current == total_combinations) {
        cat(sprintf("  Progress: %d/%d (%d%%)\n", 
                    current, total_combinations, 
                    round(100 * current / total_combinations)))
      }
      
      results$subscale_x_subscale[[items30_subscale]][[gb_subscale]] <- 
        calculate_correlation_with_ci_power(
          items30_scores[[items30_subscale]], 
          rehab_gb_subscale_scores[[gb_subscale]]
        )
    }
  }
  cat("Subscale x subscale correlations complete.\n\n")
  
  return(results)
}