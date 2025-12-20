# ===================================================
# Satisficing Calculator (Model Layer)
# Version: 1.2
# Description: Careless/insufficient effort responding detection
# Mathematical definitions:
#   LS_i = max(consecutive identical responses)
#   IRV_i = sqrt(1/(J-1) * sum((x_ij - mean_i)^2))
#   D^2_i = (x_i - mu)' * S^(-1) * (x_i - mu)
# Changes from v1.1:
#   - Added row_ids parameter to flag_careless_responders
# ===================================================

library(careless)

# Calculate Longstring index
# LS_i = maximum length of consecutive identical responses
calculate_longstring <- function(data) {
  ls_values <- careless::longstring(data)
  return(ls_values)
}

# Calculate IRV (Intra-individual Response Variability)
# IRV_i = SD of responses for each individual
calculate_irv <- function(data) {
  irv_values <- careless::irv(data)
  return(irv_values)
}

# Calculate Mahalanobis distance
# D^2_i = (x_i - mu)' * S^(-1) * (x_i - mu)
calculate_mahalanobis <- function(data) {
  mahad_values <- careless::mahad(data, plot = FALSE, flag = FALSE)
  return(mahad_values)
}

# Flag careless responders based on thresholds
flag_careless_responders <- function(data, ls_threshold, irv_percentile, mahad_p_threshold, row_ids = NULL) {
  n <- nrow(data)
  p <- ncol(data)
  
  # Use row names if row_ids not provided
  if (is.null(row_ids)) {
    row_ids <- rownames(data)
    if (is.null(row_ids)) {
      row_ids <- as.character(1:n)
    }
  }
  
  # Calculate indices
  ls_values <- calculate_longstring(data)
  irv_values <- calculate_irv(data)
  mahad_values <- calculate_mahalanobis(data)
  
  # Longstring flag: exceeds threshold
  ls_flag <- ls_values > ls_threshold
  
  # IRV flag: below percentile threshold (low variability = suspicious)
  irv_threshold_value <- unname(quantile(irv_values, probs = irv_percentile / 100, na.rm = TRUE))
  irv_flag <- irv_values < irv_threshold_value
  
  # Mahalanobis flag: p-value below threshold
  mahad_p <- pchisq(mahad_values, df = p, lower.tail = FALSE)
  mahad_flag <- mahad_p < mahad_p_threshold
  
  # Combined flag: any indicator triggered
  any_flag <- ls_flag | irv_flag | mahad_flag
  
  return(list(
    indices = data.frame(
      id = row_ids,
      longstring = ls_values,
      irv = irv_values,
      mahalanobis_d2 = mahad_values,
      mahalanobis_p = mahad_p,
      stringsAsFactors = FALSE
    ),
    flags = data.frame(
      id = row_ids,
      ls_flag = ls_flag,
      irv_flag = irv_flag,
      mahad_flag = mahad_flag,
      any_flag = any_flag,
      stringsAsFactors = FALSE
    ),
    thresholds = list(
      ls_threshold = ls_threshold,
      irv_threshold_value = irv_threshold_value,
      irv_percentile = irv_percentile,
      mahad_p_threshold = mahad_p_threshold
    ),
    summary = list(
      n_total = n,
      n_ls_flagged = sum(ls_flag, na.rm = TRUE),
      n_irv_flagged = sum(irv_flag, na.rm = TRUE),
      n_mahad_flagged = sum(mahad_flag, na.rm = TRUE),
      n_any_flagged = sum(any_flag, na.rm = TRUE)
    )
  ))
}

# Calculate descriptive statistics for satisficing indices
calculate_satisficing_descriptives <- function(indices) {
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
    longstring = calc_stats(indices$longstring),
    irv = calc_stats(indices$irv),
    mahalanobis_d2 = calc_stats(indices$mahalanobis_d2)
  )
  
  return(stats)
}