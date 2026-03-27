# ===================================================
# Interrater Reliability Main (Controller Layer)
# ===================================================

source("sensitivity_utils.R")

analyze_interrater <- function(data_obj) {
  
  # Extract data from keyed structure
  source("data_structure.R")
  data <- get_data(data_obj)
  keys <- get_keys(data_obj)
  
  # Load configuration
  config <- load_config()
  
  # Get parameters from YAML
  patient_id_col <- config$analysis$data_keys$interrater[1]
  item_pattern <- config$analysis$global$item_pattern
  scale_min <- config$analysis$global$scale$min
  scale_max <- config$analysis$global$scale$max
  n_boot <- config$analysis$interrater_analysis$bootstrap_iterations
  ac2_weights <- config$analysis$interrater_analysis$ac2_weights
  
  # Read sensitivity_exclude_items
  sensitivity_exclude_items <- unlist(config$analysis$interrater_analysis$sensitivity_exclude_items)
  exclude_subsets <- generate_exclude_subsets(sensitivity_exclude_items)
  
  # Display header
  ir_display_header()
  
  # Step 1: Data preparation (common to all subsets)
  cat("Data Overview\n")
  cat("-------------\n")
  
  # Detect item columns using pattern
  item_cols_all <- grep(item_pattern, names(data), value = TRUE)
  
  if (length(item_cols_all) == 0) {
    stop("No item columns found matching pattern: ", item_pattern)
  }
  
  # Check required columns
  required_cols <- c(patient_id_col, item_cols_all)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Extract relevant columns
  data_subset_all <- data[, c(patient_id_col, item_cols_all)]
  
  # Identify paired data (2 rows per patient)
  patient_counts <- table(data_subset_all[[patient_id_col]])
  paired_patients <- names(patient_counts)[patient_counts == 2]
  
  if (length(paired_patients) == 0) {
    stop("No paired data found. Each patient should have exactly 2 ratings.")
  }
  
  # Filter to paired data only
  data_paired_all <- data_subset_all[data_subset_all[[patient_id_col]] %in% paired_patients, ]
  
  # Sort by patient ID to ensure consistent ordering
  data_paired_all <- data_paired_all[order(data_paired_all[[patient_id_col]]), ]
  
  # Split into list by patient
  data_list_all <- split(data_paired_all, data_paired_all[[patient_id_col]])
  n_patients <- length(data_list_all)
  patient_names <- names(data_list_all)
  
  # Build full rater matrices (all items)
  data_rater1_all <- do.call(rbind, lapply(data_list_all, function(x) x[1, item_cols_all]))
  data_rater2_all <- do.call(rbind, lapply(data_list_all, function(x) x[2, item_cols_all]))
  data_rater1_all <- as.data.frame(data_rater1_all)
  data_rater2_all <- as.data.frame(data_rater2_all)
  rownames(data_rater1_all) <- patient_names
  rownames(data_rater2_all) <- patient_names
  
  scale_range <- c(scale_min, scale_max)
  
  cat(sprintf("Matched patients: %d\n", n_patients))
  cat(sprintf("Total items detected: %d (%s-%s)\n", length(item_cols_all), item_cols_all[1], item_cols_all[length(item_cols_all)]))
  cat(sprintf("Scale: %d-%d (%d-point Likert)\n",
              scale_min, scale_max, scale_max - scale_min + 1))
  cat(sprintf("Bootstrap iterations: %d\n", n_boot))
  cat(sprintf("AC2 weights: %s\n", ac2_weights))
  
  if (!is.null(sensitivity_exclude_items) && length(sensitivity_exclude_items) > 0) {
    cat(sprintf("Sensitivity exclude items: %s\n", paste(sensitivity_exclude_items, collapse = ", ")))
    cat(sprintf("Number of subsets: %d\n", length(exclude_subsets)))
  } else {
    cat("Sensitivity analysis: disabled\n")
  }
  cat("\n")
  
  # Outer loop: subsets (sensitivity analysis)
  all_subset_results <- list()
  
  for (subset_info in exclude_subsets) {
    
    subset_key <- subset_info$key
    exclude_items <- subset_info$items
    
    cat("################################################################\n")
    cat(sprintf("SENSITIVITY SUBSET: %s\n", subset_key))
    if (length(exclude_items) > 0) {
      cat(sprintf("  Excluded items: %s\n", paste(exclude_items, collapse = ", ")))
    } else {
      cat("  Excluded items: none (full dataset)\n")
    }
    cat("################################################################\n\n")
    
    # Determine item columns for this subset
    if (length(exclude_items) > 0) {
      cols_to_remove <- intersect(exclude_items, item_cols_all)
      if (length(cols_to_remove) != length(exclude_items)) {
        missing_excl <- setdiff(exclude_items, item_cols_all)
        stop(sprintf("Exclude items not found in data: %s",
                     paste(missing_excl, collapse = ", ")))
      }
      item_cols <- item_cols_all[!item_cols_all %in% exclude_items]
    } else {
      item_cols <- item_cols_all
    }
    
    n_items <- length(item_cols)
    data_rater1 <- data_rater1_all[, item_cols, drop = FALSE]
    data_rater2 <- data_rater2_all[, item_cols, drop = FALSE]
    
    cat(sprintf("Items analyzed: %d (%s-%s)\n\n", n_items, item_cols[1], item_cols[n_items]))
    
    # Step 2: Item-level analysis with AC2
    item_results <- process_item_level(data_rater1, data_rater2,
                                       ac2_weights = ac2_weights,
                                       scale_range = scale_range,
                                       n_boot = n_boot)
    
    # Display item results
    ir_display_item_results(item_results, scale_range)
    
    # Step 3: Scale-level analysis
    pairwise_results <- calculate_pairwise_totals(data_rater1, data_rater2)
    total_rater1 <- pairwise_results$total_r1
    total_rater2 <- pairwise_results$total_r2
    
    valid_pairs <- !is.na(total_rater1) & !is.na(total_rater2)
    total_rater1_clean <- total_rater1[valid_pairs]
    total_rater2_clean <- total_rater2[valid_pairs]
    
    total_wide <- data.frame(
      rater1 = total_rater1_clean,
      rater2 = total_rater2_clean
    )
    
    icc_results <- calculate_icc(total_wide)
    ac2_scale_result <- calculate_ac2_scale_level(total_rater1_clean, total_rater2_clean,
                                                  ac2_weights = ac2_weights)
    ba_results <- calculate_bland_altman(total_rater1_clean, total_rater2_clean)
    
    scale_results <- list(
      icc_2_1 = icc_results$icc_2_1,
      icc_2_2 = icc_results$icc_2_2,
      ac2 = ac2_scale_result,
      bland_altman = ba_results
    )
    
    ir_display_scale_results(scale_results)
    
    # Step 4: Summary
    summary_stats <- list(
      mean_p_o = mean(item_results$p_o, na.rm = TRUE),
      mean_kappa = mean(item_results$kappa_w, na.rm = TRUE),
      mean_ac2 = mean(item_results$ac2_value, na.rm = TRUE),
      n_low_kappa = sum(item_results$kappa_w < 0.5, na.rm = TRUE),
      n_low_ac2 = sum(item_results$ac2_value < 0.5, na.rm = TRUE),
      n_patients = n_patients,
      n_items = n_items
    )
    
    all_subset_results[[subset_key]] <- list(
      item_level = item_results,
      scale_level = scale_results,
      summary = summary_stats,
      n_patients = n_patients,
      data_rater1 = data_rater1,
      data_rater2 = data_rater2,
      config_used = list(
        patient_id_col = patient_id_col,
        item_pattern = item_pattern,
        item_cols = item_cols,
        exclude_items = exclude_items,
        scale_min = scale_min,
        scale_max = scale_max,
        n_boot = n_boot,
        ac2_weights = ac2_weights
      )
    )
    
    cat("\n========================================\n")
    cat(sprintf("Subset Complete: %s\n", subset_key))
    cat("========================================\n\n")
  }
  
  invisible(all_subset_results)
}