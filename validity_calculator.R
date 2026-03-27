# ===================================================
# Validity Calculator (Model Layer)
# ===================================================

library(pwr)
library(psych)

# Calculate REHAB scores (raw scores only)
calculate_rehab_scores <- function(rehab_data) {
  gb_items <- c("R_Q08", "R_Q09", "R_Q10", "R_Q11", "R_Q12", "R_Q13",
                "R_Q14", "R_Q15", "R_Q16", "R_Q17", "R_Q18", "R_Q19",
                "R_Q20", "R_Q21", "R_Q22", "R_Q23")
  db_items <- paste0("R_Q", sprintf("%02d", 1:7))
  
  gb_score <- rowSums(rehab_data[, gb_items], na.rm = TRUE)
  db_score <- rowSums(rehab_data[, db_items], na.rm = TRUE)
  
  return(list(
    gb = gb_score,
    db = db_score
  ))
}

# Calculate REHAB GB subscale scores
calculate_rehab_gb_subscale_scores <- function(rehab_data, gb_subscales) {
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

# Calculate target scale subscale scores
calculate_target_subscale_scores <- function(data, subscale_definitions) {
  scores <- list()
  scores$total <- rowSums(data, na.rm = TRUE)
  
  for (name in names(subscale_definitions)) {
    if (!is.null(subscale_definitions[[name]]$items)) {
      items <- subscale_definitions[[name]]$items
      available_items <- items[items %in% names(data)]
      if (length(available_items) > 0) {
        scores[[name]] <- rowSums(data[available_items], na.rm = TRUE)
      }
    }
  }
  return(scores)
}

# Calculate REHAB DB score from config definition
calculate_rehab_db_score <- function(rehab_data, db_definition) {
  items <- db_definition$items
  available_items <- items[items %in% names(rehab_data)]
  if (length(available_items) > 0) {
    return(rowSums(rehab_data[available_items], na.rm = TRUE))
  }
  return(NULL)
}

# Prepare all scores with ID matching
# Returns target_scores and rehab_scores as flat named lists
prepare_validity_scores <- function(target_obj, rehab_obj,
                                    target_subscales, rehab_gb_subscales,
                                    rehab_db_definition) {
  source("data_structure.R")
  
  target_data <- as.data.frame(get_data(target_obj))
  rehab_data <- as.data.frame(get_data(rehab_obj))
  
  target_ids <- rownames(target_data)
  rehab_ids <- rownames(rehab_data)
  common_ids <- intersect(target_ids, rehab_ids)
  
  cat("\n--- Patient ID Matching ---\n")
  cat(sprintf("Target patients: %d\n", length(target_ids)))
  cat(sprintf("REHAB patients: %d\n", length(rehab_ids)))
  cat(sprintf("Matched patients: %d\n", length(common_ids)))
  
  if (length(common_ids) == 0) {
    stop("No matching patient IDs found between datasets!")
  }
  
  target_matched <- target_data[target_ids %in% common_ids, , drop = FALSE]
  rehab_matched <- rehab_data[rehab_ids %in% common_ids, , drop = FALSE]
  target_matched <- target_matched[order(rownames(target_matched)), , drop = FALSE]
  rehab_matched <- rehab_matched[order(rownames(rehab_matched)), , drop = FALSE]
  
  if (!all(rownames(target_matched) == rownames(rehab_matched))) {
    stop("Patient ID ordering mismatch after sorting!")
  }
  cat("Patient matching successful!\n\n")
  
  # Target scores
  target_scores <- calculate_target_subscale_scores(target_matched, target_subscales)
  
  # REHAB scores: flat named list
  rehab_scores <- list()
  
  # GB total and DB total (from hardcoded items)
  raw_rehab <- calculate_rehab_scores(rehab_matched)
  rehab_scores$gb_total <- raw_rehab$gb
  
  # GB subscale scores
  gb_sub <- calculate_rehab_gb_subscale_scores(rehab_matched, rehab_gb_subscales)
  for (name in names(gb_sub)) {
    rehab_scores[[name]] <- gb_sub[[name]]
  }
  
  # DB score from config definition (used by hypothesis keys)
  if (!is.null(rehab_db_definition)) {
    rehab_scores$deviant_behavior <- calculate_rehab_db_score(
      rehab_matched, rehab_db_definition
    )
  }
  
  return(list(
    target_scores = target_scores,
    rehab_scores = rehab_scores,
    n = length(common_ids)
  ))
}

# Calculate correlation with CI and power
# method: "pearson" or "spearman"
calculate_correlation_with_ci_power <- function(x, y, alpha = 0.05,
                                                method = "pearson") {
  complete_cases <- complete.cases(x, y)
  x_clean <- x[complete_cases]
  y_clean <- y[complete_cases]
  n <- length(x_clean)
  
  if (n < 3) {
    return(list(r = NA, ci_lower = NA, ci_upper = NA,
                power = NA, n = n, method = method))
  }
  
  cor_result <- cor.test(x_clean, y_clean, method = method)
  r <- unname(cor_result$estimate)
  
  # CI
  if (method == "pearson") {
    ci <- cor_result$conf.int
    ci_lower <- ci[1]
    ci_upper <- ci[2]
  } else {
    # Fisher z transformation for Spearman CI
    z <- atanh(r)
    se <- 1 / sqrt(n - 3)
    z_crit <- qnorm(1 - alpha / 2)
    ci_lower <- tanh(z - z_crit * se)
    ci_upper <- tanh(z + z_crit * se)
  }
  
  # Post-hoc power
  if (abs(r) > 0 && n > 3) {
    power <- pwr.r.test(n = n, r = abs(r), sig.level = alpha)$power
  } else {
    power <- NA
  }
  
  return(list(
    r = r,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    power = power,
    n = n,
    method = method
  ))
}

# Calculate full correlation matrix (supplementary analysis)
calculate_validity_correlations <- function(target_scores, rehab_scores,
                                            method = "pearson") {
  # Total level: target total vs GB total, DB (deviant_behavior)
  total_level <- list(
    gb = calculate_correlation_with_ci_power(
      target_scores$total, rehab_scores$gb_total, method = method
    ),
    db = calculate_correlation_with_ci_power(
      target_scores$total, rehab_scores$deviant_behavior, method = method
    )
  )
  
  # Subscale level: each target factor vs GB total, DB
  subscale_names <- names(target_scores)[names(target_scores) != "total"]
  subscale_level <- list(gb = list(), db = list())
  
  for (subscale in subscale_names) {
    subscale_level$gb[[subscale]] <-
      calculate_correlation_with_ci_power(
        target_scores[[subscale]], rehab_scores$gb_total, method = method
      )
    subscale_level$db[[subscale]] <-
      calculate_correlation_with_ci_power(
        target_scores[[subscale]], rehab_scores$deviant_behavior, method = method
      )
  }
  
  # Subscale x subscale: target factors vs REHAB GB subscales
  rehab_gb_names <- names(rehab_scores)[
    !names(rehab_scores) %in% c("gb_total", "deviant_behavior")
  ]
  
  subscale_x_subscale <- list()
  for (target_subscale in subscale_names) {
    subscale_x_subscale[[target_subscale]] <- list()
    for (gb_subscale in rehab_gb_names) {
      subscale_x_subscale[[target_subscale]][[gb_subscale]] <-
        calculate_correlation_with_ci_power(
          target_scores[[target_subscale]],
          rehab_scores[[gb_subscale]],
          method = method
        )
    }
  }
  
  return(list(
    total_level = total_level,
    subscale_level = subscale_level,
    subscale_x_subscale = subscale_x_subscale
  ))
}

# Test a set of hypotheses using dependent correlation difference test
# Each hypothesis: |r(target, proximal)| > |r(target, distal)|
# Uses psych::r.test() for dependent correlations sharing one variable
test_hypothesis_set <- function(target_scores, rehab_scores,
                                hypotheses, method = "pearson") {
  results <- list()
  
  for (h_name in names(hypotheses)) {
    h <- hypotheses[[h_name]]
    
    target_key <- h$target
    proximal_key <- h$proximal
    distal_key <- h$distal
    
    x <- target_scores[[target_key]]
    y <- rehab_scores[[proximal_key]]
    z <- rehab_scores[[distal_key]]
    
    if (is.null(x) || is.null(y) || is.null(z)) {
      cat(sprintf("WARNING: Missing scores for %s (target=%s, proximal=%s, distal=%s)\n",
                  h_name, target_key, proximal_key, distal_key))
      results[[h_name]] <- list(
        target = target_key, proximal = proximal_key, distal = distal_key,
        r_proximal = NA, r_distal = NA, r_yz = NA,
        abs_r_proximal = NA, abs_r_distal = NA, abs_diff = NA,
        t_stat = NA, p_value = NA, supported = NA,
        method = method, n = 0
      )
      next
    }
    
    # Listwise deletion across all three variables
    complete <- complete.cases(x, y, z)
    x_c <- x[complete]
    y_c <- y[complete]
    z_c <- z[complete]
    n_complete <- length(x_c)
    
    if (n_complete < 4) {
      results[[h_name]] <- list(
        target = target_key, proximal = proximal_key, distal = distal_key,
        r_proximal = NA, r_distal = NA, r_yz = NA,
        abs_r_proximal = NA, abs_r_distal = NA, abs_diff = NA,
        t_stat = NA, p_value = NA, supported = NA,
        method = method, n = n_complete
      )
      next
    }
    
    # Three correlations
    r_xy <- cor(x_c, y_c, method = method)  # target x proximal
    r_xz <- cor(x_c, z_c, method = method)  # target x distal
    r_yz <- cor(y_c, z_c, method = method)  # proximal x distal
    
    abs_diff <- abs(r_xy) - abs(r_xz)
    
    # Dependent correlation difference test (Steiger 1980)
    # psych::r.test: r12 and r13 share variable 1 (target)
    rtest_result <- psych::r.test(
      n = n_complete,
      r12 = r_xy,
      r13 = r_xz,
      r23 = r_yz
    )
    
    # One-sided p-value for |r_proximal| > |r_distal|
    if (abs_diff > 0) {
      p_one_sided <- rtest_result$p / 2
    } else {
      p_one_sided <- 1 - rtest_result$p / 2
    }
    
    supported <- (abs_diff > 0) && (p_one_sided < 0.05)
    
    results[[h_name]] <- list(
      target = target_key,
      proximal = proximal_key,
      distal = distal_key,
      r_proximal = r_xy,
      r_distal = r_xz,
      r_yz = r_yz,
      abs_r_proximal = abs(r_xy),
      abs_r_distal = abs(r_xz),
      abs_diff = abs_diff,
      t_stat = rtest_result$t,
      p_value = p_one_sided,
      supported = supported,
      method = method,
      n = n_complete
    )
  }
  
  return(results)
}