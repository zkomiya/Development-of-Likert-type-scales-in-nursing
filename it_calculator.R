# ===================================================
# Item-Total Correlation Calculator (Model Layer)
# Version: 4.0 - Pearson + Polyserial concurrent calculation
# Description: Calculate both Pearson and Polyserial I-T correlations
# Note: Always calculates both methods for numerical comparison
# Changes from v3.0:
#   - Added calculate_item_total_polyserial() function
#   - Modified calculate_item_total_correlations() to return 4-column structure
#   - Updated calculate_it_summary() to handle 4 columns + difference stats
#   - Updated subscale functions for 4-column support
#   - Uses polycor::polyserial() instead of psych::polyserial()
# ===================================================

library(psych)
library(polycor)

# Calculate Polyserial I-T correlations
calculate_item_total_polyserial <- function(item_data) {
  n_items <- ncol(item_data)
  item_names <- colnames(item_data)
  
  cor_corrected <- numeric(n_items)
  cor_uncorrected <- numeric(n_items)
  
  total_score <- rowSums(item_data, na.rm = TRUE)
  
  for (i in 1:n_items) {
    item_values <- item_data[[i]]
    
    # Corrected (item-rest)
    rest_score <- total_score - item_values
    complete_cases_rest <- complete.cases(item_values, rest_score)
    
    # polycor::polyserial expects numeric vectors (not ordered factors)
    cor_corrected[i] <- polycor::polyserial(
      item_values[complete_cases_rest],
      rest_score[complete_cases_rest]
    )
    
    # Uncorrected (item-total)
    complete_cases_total <- complete.cases(item_values, total_score)
    
    # polycor::polyserial expects numeric vectors (not ordered factors)
    cor_uncorrected[i] <- polycor::polyserial(
      item_values[complete_cases_total],
      total_score[complete_cases_total]
    )
  }
  
  data.frame(
    item = item_names,
    cor_corrected = cor_corrected,
    cor_uncorrected = cor_uncorrected,
    stringsAsFactors = FALSE
  )
}

# Main function: Calculate both Pearson and Polyserial, return 4-column structure
calculate_item_total_correlations <- function(item_data) {
  
  # Pearson (existing implementation via psych::alpha)
  alpha_result <- psych::alpha(item_data, check.keys = FALSE)
  
  pearson_corrected <- alpha_result$item.stats[, "r.drop"]
  pearson_uncorrected <- alpha_result$item.stats[, "r.cor"]
  
  # Polyserial (new implementation)
  polyserial_results <- calculate_item_total_polyserial(item_data)
  
  # Integrate into 4-column structure
  results <- data.frame(
    item = rownames(alpha_result$item.stats),
    cor_corrected_pearson = pearson_corrected,
    cor_uncorrected_pearson = pearson_uncorrected,
    cor_corrected_polyserial = polyserial_results$cor_corrected,
    cor_uncorrected_polyserial = polyserial_results$cor_uncorrected,
    stringsAsFactors = FALSE
  )
  
  return(results)
}

# Calculate summary statistics (4-column version)
calculate_it_summary <- function(it_results) {
  list(
    n_items = nrow(it_results),
    
    # Pearson corrected
    mean_corrected_pearson = mean(it_results$cor_corrected_pearson, na.rm = TRUE),
    sd_corrected_pearson = sd(it_results$cor_corrected_pearson, na.rm = TRUE),
    min_corrected_pearson = min(it_results$cor_corrected_pearson, na.rm = TRUE),
    max_corrected_pearson = max(it_results$cor_corrected_pearson, na.rm = TRUE),
    
    # Pearson uncorrected
    mean_uncorrected_pearson = mean(it_results$cor_uncorrected_pearson, na.rm = TRUE),
    sd_uncorrected_pearson = sd(it_results$cor_uncorrected_pearson, na.rm = TRUE),
    
    # Polyserial corrected
    mean_corrected_polyserial = mean(it_results$cor_corrected_polyserial, na.rm = TRUE),
    sd_corrected_polyserial = sd(it_results$cor_corrected_polyserial, na.rm = TRUE),
    min_corrected_polyserial = min(it_results$cor_corrected_polyserial, na.rm = TRUE),
    max_corrected_polyserial = max(it_results$cor_corrected_polyserial, na.rm = TRUE),
    
    # Polyserial uncorrected
    mean_uncorrected_polyserial = mean(it_results$cor_uncorrected_polyserial, na.rm = TRUE),
    sd_uncorrected_polyserial = sd(it_results$cor_uncorrected_polyserial, na.rm = TRUE),
    
    # Differences (Polyserial - Pearson)
    mean_diff_corrected = mean(it_results$cor_corrected_polyserial - it_results$cor_corrected_pearson, na.rm = TRUE),
    sd_diff_corrected = sd(it_results$cor_corrected_polyserial - it_results$cor_corrected_pearson, na.rm = TRUE),
    mean_diff_uncorrected = mean(it_results$cor_uncorrected_polyserial - it_results$cor_uncorrected_pearson, na.rm = TRUE),
    sd_diff_uncorrected = sd(it_results$cor_uncorrected_polyserial - it_results$cor_uncorrected_pearson, na.rm = TRUE),
    
    # NA count (quality check)
    n_na_corrected_polyserial = sum(is.na(it_results$cor_corrected_polyserial)),
    n_na_uncorrected_polyserial = sum(is.na(it_results$cor_uncorrected_polyserial))
  )
}

# Calculate subscale item-total correlations (4-column version)
calculate_subscale_item_total <- function(data, subscale_items) {
  subscale_data <- data[, subscale_items, drop = FALSE]
  calculate_item_total_correlations(subscale_data)
}

# Calculate subscale summary statistics
calculate_subscale_summary <- function(subscale_results, subscale_name) {
  summary_stats <- calculate_it_summary(subscale_results)
  summary_stats$subscale_name <- subscale_name
  summary_stats
}