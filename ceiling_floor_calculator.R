# ===================================================
# Ceiling-Floor Effect Analyzer (Model Layer)
# Version: 9.0 - Added kurtosis calculation
# ===================================================

library(psych)

# Calculate basic statistics for each item
calculate_item_statistics <- function(item_data) {
  stats <- data.frame(
    item = character(),
    mean = numeric(),
    sd = numeric(),
    skewness = numeric(),
    kurtosis = numeric(),
    mean_plus_sd = numeric(),
    mean_minus_sd = numeric(),
    n_valid = integer(),
    stringsAsFactors = FALSE
  )
  
  for (col_name in names(item_data)) {
    col_values <- item_data[[col_name]]
    valid_values <- col_values[!is.na(col_values)]
    
    mean_val <- mean(valid_values)
    sd_val <- sd(valid_values)
    skew_val <- psych::skew(valid_values)
    kurt_val <- psych::kurtosi(valid_values)
    
    stats <- rbind(stats, data.frame(
      item = col_name,
      mean = mean_val,
      sd = sd_val,
      skewness = skew_val,
      kurtosis = kurt_val,
      mean_plus_sd = mean_val + sd_val,
      mean_minus_sd = mean_val - sd_val,
      n_valid = length(valid_values)
    ))
  }
  
  stats
}

# Calculate ceiling and floor percentages
calculate_ceiling_floor_percentages <- function(item_data, min_val, max_val) {
  percentages <- data.frame(
    item = character(),
    ceiling_pct = numeric(),
    floor_pct = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (col_name in names(item_data)) {
    col_values <- item_data[[col_name]]
    valid_values <- col_values[!is.na(col_values)]
    n_valid <- length(valid_values)
    
    ceiling_pct <- (sum(valid_values == max_val) / n_valid) * 100
    floor_pct <- (sum(valid_values == min_val) / n_valid) * 100
    
    percentages <- rbind(percentages, data.frame(
      item = col_name,
      ceiling_pct = ceiling_pct,
      floor_pct = floor_pct
    ))
  }
  
  percentages
}

# Main analysis function
analyze_ceiling_floor_effects <- function(item_data, scale_min, scale_max) {
  # Calculate basic statistics
  stats <- calculate_item_statistics(item_data)
  
  # Calculate ceiling and floor percentages
  percentages <- calculate_ceiling_floor_percentages(item_data, scale_min, scale_max)
  
  # Merge all results
  results <- merge(stats, percentages, by = "item")
  
  # Add metadata
  attr(results, "scale_range") <- c(scale_min, scale_max)
  
  results
}

# Calculate summary statistics
calculate_cf_summary <- function(cf_results) {
  list(
    n_items = nrow(cf_results),
    mean_ceiling_pct = mean(cf_results$ceiling_pct),
    mean_floor_pct = mean(cf_results$floor_pct),
    min_ceiling_pct = min(cf_results$ceiling_pct),
    max_ceiling_pct = max(cf_results$ceiling_pct),
    min_floor_pct = min(cf_results$floor_pct),
    max_floor_pct = max(cf_results$floor_pct),
    mean_skewness = mean(cf_results$skewness),
    sd_skewness = sd(cf_results$skewness),
    mean_kurtosis = mean(cf_results$kurtosis),
    sd_kurtosis = sd(cf_results$kurtosis)
  )
}