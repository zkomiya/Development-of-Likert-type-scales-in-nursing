# ===================================================
# Interrater Reliability Calculator (Model Layer)
# Version: 6.0 - Added Gwet's AC2 functionality
# ===================================================

library(psych)
library(irr)
library(boot)
library(BlandAltmanLeh)
library(irrCAC)

# Process item-level analysis with AC2
process_item_level <- function(data_rater1, data_rater2, ac2_weights = "quadratic", scale_range = c(1, 4), n_boot = 1000) {
  items <- names(data_rater1)
  results <- list()
  
  for (item in items) {
    r1 <- data_rater1[[item]]
    r2 <- data_rater2[[item]]
    
    # Remove NA pairs
    complete <- complete.cases(r1, r2)
    r1_clean <- r1[complete]
    r2_clean <- r2[complete]
    n_pairs <- length(r1_clean)
    
    if (n_pairs < 3) {
      results[[item]] <- list(
        item = item,
        p_o = NA,
        kappa_w = NA,
        ci_lower = NA,
        ci_upper = NA,
        ac2_value = NA,
        ac2_ci_lower = NA,
        ac2_ci_upper = NA,
        n_pairs = n_pairs
      )
      next
    }
    
    # Calculate percent agreement using irr::agree
    agree_data <- cbind(r1_clean, r2_clean)
    p_o <- irr::agree(agree_data)$value
    
    # Calculate weighted kappa using irr::kappa2
    kappa_data <- data.frame(rater1 = r1_clean, rater2 = r2_clean)
    kappa_result <- irr::kappa2(kappa_data, weight = "squared")
    kappa_w <- kappa_result$value
    
    # Bootstrap CI for kappa
    kappa_boot_func <- function(data, indices) {
      d <- data[indices, ]
      result <- tryCatch({
        irr::kappa2(d, weight = "squared")$value
      }, error = function(e) {
        return(NA)
      })
      return(result)
    }
    
    set.seed(123)
    boot_result <- boot::boot(kappa_data, kappa_boot_func, R = n_boot)
    
    # Calculate CI
    ci_result <- tryCatch({
      boot::boot.ci(boot_result, type = "perc", conf = 0.95)$percent[4:5]
    }, error = function(e) {
      c(NA, NA)
    })
    
    # Calculate AC2 using irrCAC
    ac2_result <- tryCatch({
      if (ac2_weights == "unweighted") {
        gwet.ac1.raw(kappa_data)$est
      } else {
        gwet.ac1.raw(kappa_data, weights = ac2_weights)$est
      }
    }, error = function(e) {
      list(coeff.val = NA, conf.int = "NA,NA")
    })
    
    # Extract AC2 confidence interval
    if (!is.na(ac2_result$coeff.val) && ac2_result$conf.int != "NA,NA") {
      ac2_ci_parts <- as.numeric(strsplit(gsub("[()]", "", ac2_result$conf.int), ",")[[1]])
      ac2_ci_lower <- ac2_ci_parts[1]
      ac2_ci_upper <- ac2_ci_parts[2]
    } else {
      ac2_ci_lower <- NA
      ac2_ci_upper <- NA
    }
    
    results[[item]] <- list(
      item = item,
      p_o = p_o,
      kappa_w = kappa_w,
      ci_lower = ci_result[1],
      ci_upper = ci_result[2],
      ac2_value = ac2_result$coeff.val,
      ac2_ci_lower = ac2_ci_lower,
      ac2_ci_upper = ac2_ci_upper,
      n_pairs = n_pairs
    )
  }
  
  # Convert to data frame
  df_results <- do.call(rbind, lapply(results, function(x) {
    data.frame(
      item = x$item,
      p_o = x$p_o,
      kappa_w = x$kappa_w,
      ci_lower = x$ci_lower,
      ci_upper = x$ci_upper,
      ac2_value = x$ac2_value,
      ac2_ci_lower = x$ac2_ci_lower,
      ac2_ci_upper = x$ac2_ci_upper,
      n_pairs = x$n_pairs,
      stringsAsFactors = FALSE
    )
  }))
  
  return(df_results)
}

# Calculate ICC using psych package - FIXED VERSION
calculate_icc <- function(data_wide) {
  # Calculate ICC using psych package
  icc_result <- tryCatch({
    psych::ICC(data_wide, alpha = 0.05)
  }, error = function(e) {
    return(NULL)
  })
  
  if (is.null(icc_result)) {
    return(list(
      icc_2_1 = list(value = NA, ci = c(NA, NA)),
      icc_2_2 = list(value = NA, ci = c(NA, NA))
    ))
  }
  
  # Extract ICC(2,1) and ICC(2,2) using correct row names
  # ICC(2,1): Single random raters
  icc_2_1 <- icc_result$results["Single_random_raters", "ICC"]
  icc_2_1_lower <- icc_result$results["Single_random_raters", "lower bound"]
  icc_2_1_upper <- icc_result$results["Single_random_raters", "upper bound"]
  
  # ICC(2,2): Average random raters (ICC2k)
  icc_2_2 <- icc_result$results["Average_random_raters", "ICC"]
  icc_2_2_lower <- icc_result$results["Average_random_raters", "lower bound"]
  icc_2_2_upper <- icc_result$results["Average_random_raters", "upper bound"]
  
  return(list(
    icc_2_1 = list(
      value = icc_2_1,
      ci = c(icc_2_1_lower, icc_2_1_upper)
    ),
    icc_2_2 = list(
      value = icc_2_2,
      ci = c(icc_2_2_lower, icc_2_2_upper)
    )
  ))
}

# Calculate scale-level AC2
calculate_ac2_scale_level <- function(total_rater1, total_rater2, ac2_weights = "quadratic") {
  # Create data frame for irrCAC
  total_data <- data.frame(
    rater1 = total_rater1,
    rater2 = total_rater2
  )
  
  # Remove NA pairs
  complete_pairs <- complete.cases(total_data)
  total_data_clean <- total_data[complete_pairs, ]
  
  if (nrow(total_data_clean) < 3) {
    return(list(
      value = NA,
      ci_lower = NA,
      ci_upper = NA,
      type = ifelse(ac2_weights == "unweighted", "AC1", "AC2")
    ))
  }
  
  # Calculate AC2 using irrCAC
  ac2_result <- tryCatch({
    if (ac2_weights == "unweighted") {
      gwet.ac1.raw(total_data_clean)$est
    } else {
      gwet.ac1.raw(total_data_clean, weights = ac2_weights)$est
    }
  }, error = function(e) {
    list(coeff.val = NA, conf.int = "NA,NA")
  })
  
  # Extract confidence interval
  if (!is.na(ac2_result$coeff.val) && ac2_result$conf.int != "NA,NA") {
    ci_parts <- as.numeric(strsplit(gsub("[()]", "", ac2_result$conf.int), ",")[[1]])
    ci_lower <- ci_parts[1]
    ci_upper <- ci_parts[2]
  } else {
    ci_lower <- NA
    ci_upper <- NA
  }
  
  return(list(
    value = ac2_result$coeff.val,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    type = ifelse(ac2_weights == "unweighted", "AC1", "AC2")
  ))
}

# Calculate simple totals - FIXED VERSION
calculate_pairwise_totals <- function(data_rater1, data_rater2) {
  # Simple row sums for each rater
  total_r1 <- rowSums(data_rater1, na.rm = TRUE)
  total_r2 <- rowSums(data_rater2, na.rm = TRUE)
  
  # Count valid items per patient (both raters have non-NA)
  n_valid_items <- rowSums(!is.na(data_rater1) & !is.na(data_rater2))
  
  return(list(
    total_r1 = total_r1,
    total_r2 = total_r2,
    n_valid_items = n_valid_items
  ))
}

# Calculate Bland-Altman statistics using BlandAltmanLeh package
calculate_bland_altman <- function(rater1_total, rater2_total) {
  ba_result <- BlandAltmanLeh::bland.altman.stats(rater1_total, rater2_total, conf.int = 0.95)
  
  return(list(
    mean_diff = ba_result$mean.diffs,
    sd_diff = ba_result$diffs.sd,
    limits = c(ba_result$lower.limit, ba_result$upper.limit),
    differences = ba_result$diffs,
    means = ba_result$means
  ))
}