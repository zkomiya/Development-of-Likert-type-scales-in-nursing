# ===================================================
# GP Calculator - Version 6.2
# Changes from v6.1:
#   - Added CI columns for Cohen's d and Hedges' g
#   - cohens_d_result$cohen.d[,"lower"] / [,"upper"]
#   - cohens_d_result$hedges.g[,"lower"] / [,"upper"]
# Changes from v6.1:
#   - FIXED: Changed [2] to [,"effect"] for correct indexing
#   - cohens_d_result$cohen.d[,"effect"]
#   - cohens_d_result$hedges.g[,"effect"]
# Changes from v5.1:
#   - Added Hedges_g_psych from psych::cohen.d()$hedges.g
#   - Now all 4 effect sizes: Cohen's d x2, Hedges' g x2
# ===================================================

library(psych)

# Calculate cutoffs for Good-Poor groups
calculate_cutoffs <- function(scores, pct) {
  list(
    lower = quantile(scores, pct, na.rm = TRUE),
    upper = quantile(scores, 1 - pct, na.rm = TRUE)
  )
}

# Assign GP groups based on cutoffs
assign_gp_groups <- function(scores, cutoffs) {
  grp <- rep(NA, length(scores))
  grp[scores <= cutoffs$lower] <- "Poor"
  grp[scores >= cutoffs$upper] <- "Good"
  grp
}

# Calculate item discrimination indices
calculate_item_indices <- function(good, poor, range = c(1, 4)) {
  good <- good[!is.na(good)]
  poor <- poor[!is.na(poor)]
  
  mg <- mean(good)
  mp <- mean(poor)
  sg <- sd(good)
  sp <- sd(poor)
  ng <- length(good)
  np <- length(poor)
  
  # D* (Discrimination Index)
  d_star <- (mg - mp) / (range[2] - range[1])
  
  # Cohen's d and Hedges' g using psych::cohen.d()
  combined <- c(good, poor)
  groups <- factor(c(rep("good", ng), rep("poor", np)), levels = c("poor", "good"))
  
  cohens_d_result <- psych::cohen.d(combined, groups)
  cohens_d       <- cohens_d_result$cohen.d[,"effect"]
  cohens_d_lower <- cohens_d_result$cohen.d[,"lower"]
  cohens_d_upper <- cohens_d_result$cohen.d[,"upper"]
  hedges_g       <- cohens_d_result$hedges.g[,"effect"]
  hedges_g_lower <- cohens_d_result$hedges.g[,"lower"]
  hedges_g_upper <- cohens_d_result$hedges.g[,"upper"]
  
  data.frame(
    M_good         = mg,
    M_poor         = mp,
    SD_good        = sg,
    SD_poor        = sp,
    Cohens_d       = cohens_d,
    Cohens_d_lower = cohens_d_lower,
    Cohens_d_upper = cohens_d_upper,
    Hedges_g       = hedges_g,
    Hedges_g_lower = hedges_g_lower,
    Hedges_g_upper = hedges_g_upper,
    D_star         = d_star,
    n_good         = ng,
    n_poor         = np
  )
}

# Calculate GP indices for all items
calculate_gp_indices <- function(data, gp_group, scale_range = c(1, 4)) {
  good_idx <- which(gp_group == "Good")
  poor_idx <- which(gp_group == "Poor")
  
  results <- lapply(names(data), function(item) {
    indices <- calculate_item_indices(
      data[[item]][good_idx],
      data[[item]][poor_idx],
      scale_range
    )
    indices$item <- item
    indices
  })
  
  do.call(rbind, results)
}

# Calculate summary statistics
calculate_summary_stats <- function(res, cutoffs, scores) {
  list(
    n_total = length(scores),
    n_good = res$n_good[1],
    n_poor = res$n_poor[1],
    cutoff_lower = cutoffs$lower,
    cutoff_upper = cutoffs$upper,
    total_score_min = min(scores, na.rm = TRUE),
    total_score_max = max(scores, na.rm = TRUE),
    d_star_sd = sd(res$D_star, na.rm = TRUE),
    d_star_min = min(res$D_star, na.rm = TRUE),
    d_star_max = max(res$D_star, na.rm = TRUE),
    cohens_d_sd = sd(res$Cohens_d, na.rm = TRUE),
    cohens_d_min = min(res$Cohens_d, na.rm = TRUE),
    cohens_d_max = max(res$Cohens_d, na.rm = TRUE),
    hedges_g_sd = sd(res$Hedges_g, na.rm = TRUE),
    hedges_g_min = min(res$Hedges_g, na.rm = TRUE),
    hedges_g_max = max(res$Hedges_g, na.rm = TRUE)
  )
}