# ===================================================
# Response Styles Calculator (Model Layer)
# Version: 1.1
# Description: ERS and MRS calculation
# Mathematical definitions:
#   ERS_i = (n_{i,min} + n_{i,max}) / J
#   MRS_i = (n_{i,mid1} + n_{i,mid2} + ...) / J
# Where n_{i,k} = count of category k for individual i, J = number of items
# Changes from v1.0:
#   - Added row_ids parameter
#   - Fixed quantile extraction in descriptives
# ===================================================

# Calculate category counts for each individual
calculate_category_counts <- function(data, min_cat, max_cat) {
  n <- nrow(data)
  categories <- min_cat:max_cat
  
  counts <- matrix(0, nrow = n, ncol = length(categories))
  colnames(counts) <- paste0("cat_", categories)
  
  for (i in 1:n) {
    responses <- as.numeric(data[i, ])
    for (k in seq_along(categories)) {
      counts[i, k] <- sum(responses == categories[k], na.rm = TRUE)
    }
  }
  
  return(as.data.frame(counts))
}

# Calculate ERS (Extreme Response Style)
# ERS_i = (n_{i,min} + n_{i,max}) / J
calculate_ers <- function(data, min_cat, max_cat) {
  n <- nrow(data)
  j <- ncol(data)
  
  ers_values <- numeric(n)
  
  for (i in 1:n) {
    responses <- as.numeric(data[i, ])
    n_extreme <- sum(responses == min_cat, na.rm = TRUE) + 
      sum(responses == max_cat, na.rm = TRUE)
    n_valid <- sum(!is.na(responses))
    
    if (n_valid > 0) {
      ers_values[i] <- n_extreme / n_valid
    } else {
      ers_values[i] <- NA
    }
  }
  
  return(ers_values)
}

# Calculate MRS (Midpoint/Medium Response Style)
# MRS_i = (n_{i,mid1} + n_{i,mid2} + ...) / J
calculate_mrs <- function(data, min_cat, max_cat) {
  n <- nrow(data)
  
  # Middle categories = all except min and max
  middle_cats <- (min_cat + 1):(max_cat - 1)
  
  mrs_values <- numeric(n)
  
  for (i in 1:n) {
    responses <- as.numeric(data[i, ])
    n_middle <- sum(responses %in% middle_cats, na.rm = TRUE)
    n_valid <- sum(!is.na(responses))
    
    if (n_valid > 0) {
      mrs_values[i] <- n_middle / n_valid
    } else {
      mrs_values[i] <- NA
    }
  }
  
  return(mrs_values)
}

# Calculate all response style indices
calculate_response_styles <- function(data, min_cat, max_cat, row_ids = NULL) {
  n <- nrow(data)
  
  # Use row names if row_ids not provided
  if (is.null(row_ids)) {
    row_ids <- rownames(data)
    if (is.null(row_ids)) {
      row_ids <- as.character(1:n)
    }
  }
  
  ers <- calculate_ers(data, min_cat, max_cat)
  mrs <- calculate_mrs(data, min_cat, max_cat)
  
  return(data.frame(
    id = row_ids,
    ers = ers,
    mrs = mrs,
    stringsAsFactors = FALSE
  ))
}

# Flag extreme response style users
flag_response_styles <- function(indices, extreme_percentile) {
  # ERS high = extreme responder
  ers_threshold <- unname(quantile(indices$ers, probs = extreme_percentile / 100, na.rm = TRUE))
  ers_flag <- indices$ers > ers_threshold
  
  # MRS high = middle responder
  mrs_threshold <- unname(quantile(indices$mrs, probs = extreme_percentile / 100, na.rm = TRUE))
  mrs_flag <- indices$mrs > mrs_threshold
  
  return(list(
    flags = data.frame(
      id = indices$id,
      ers_flag = ers_flag,
      mrs_flag = mrs_flag,
      stringsAsFactors = FALSE
    ),
    thresholds = list(
      ers_threshold = ers_threshold,
      mrs_threshold = mrs_threshold,
      percentile = extreme_percentile
    ),
    summary = list(
      n_total = nrow(indices),
      n_ers_flagged = sum(ers_flag, na.rm = TRUE),
      n_mrs_flagged = sum(mrs_flag, na.rm = TRUE)
    )
  ))
}

# Calculate descriptive statistics for response style indices
calculate_response_styles_descriptives <- function(indices) {
  calc_stats <- function(x) {
    x_clean <- x[!is.na(x)]
    q <- quantile(x_clean, probs = c(0.25, 0.5, 0.75))
    c(
      n = length(x_clean),
      mean = mean(x_clean),
      sd = sd(x_clean),
      min = min(x_clean),
      q25 = unname(q[1]),
      median = unname(q[2]),
      q75 = unname(q[3]),
      max = max(x_clean)
    )
  }
  
  stats <- list(
    ers = calc_stats(indices$ers),
    mrs = calc_stats(indices$mrs)
  )
  
  return(stats)
}

# Calculate correlation with scale scores
calculate_rs_correlations <- function(indices, data) {
  # Calculate total score
  total_score <- rowSums(data, na.rm = TRUE)
  
  # Correlations
  ers_cor <- cor.test(indices$ers, total_score, method = "pearson")
  mrs_cor <- cor.test(indices$mrs, total_score, method = "pearson")
  
  return(list(
    ers_total = list(
      r = ers_cor$estimate,
      p = ers_cor$p.value,
      ci_lower = ers_cor$conf.int[1],
      ci_upper = ers_cor$conf.int[2]
    ),
    mrs_total = list(
      r = mrs_cor$estimate,
      p = mrs_cor$p.value,
      ci_lower = mrs_cor$conf.int[1],
      ci_upper = mrs_cor$conf.int[2]
    )
  ))
}