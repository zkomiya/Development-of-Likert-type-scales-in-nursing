# ===================================================
# Interrater Reliability Main (Controller Layer)
# Version: 6.0 - YAML-based configuration only
# Description: Main controller for interrater reliability analysis
# Changes from v5.0:
#   - Removed patient_id_col, item_pattern, scale_min, scale_max, n_boot from arguments
#   - Now reads all configuration from YAML only
#   - Consistent with other analysis functions (analyze_gp, analyze_ceiling_floor, etc.)
# ===================================================

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
  
  # Display header
  ir_display_header()
  
  # Step 1: Data preparation
  cat("Data Overview\n")
  cat("-------------\n")
  
  # Detect item columns using pattern
  item_cols <- grep(item_pattern, names(data), value = TRUE)
  
  if (length(item_cols) == 0) {
    stop("No item columns found matching pattern: ", item_pattern)
  }
  
  # Check required columns
  required_cols <- c(patient_id_col, item_cols)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Extract relevant columns - data is already numeric from prepare_data
  data_subset <- data[, c(patient_id_col, item_cols)]
  
  # Identify paired data (2 rows per patient)
  patient_counts <- table(data_subset[[patient_id_col]])
  paired_patients <- names(patient_counts)[patient_counts == 2]
  
  if (length(paired_patients) == 0) {
    stop("No paired data found. Each patient should have exactly 2 ratings.")
  }
  
  # Filter to paired data only
  data_paired <- data_subset[data_subset[[patient_id_col]] %in% paired_patients, ]
  
  # Sort by patient ID to ensure consistent ordering
  data_paired <- data_paired[order(data_paired[[patient_id_col]]), ]
  
  # Split into list by patient
  data_list <- split(data_paired, data_paired[[patient_id_col]])
  
  # Create rater data matrices
  n_patients <- length(data_list)
  patient_names <- names(data_list)
  
  # Extract rater 1 and rater 2 data
  data_rater1 <- do.call(rbind, lapply(data_list, function(x) x[1, item_cols]))
  data_rater2 <- do.call(rbind, lapply(data_list, function(x) x[2, item_cols]))
  
  # Convert to data.frame and ensure proper structure
  data_rater1 <- as.data.frame(data_rater1)
  data_rater2 <- as.data.frame(data_rater2)
  
  rownames(data_rater1) <- patient_names
  rownames(data_rater2) <- patient_names
  
  n_items <- length(item_cols)
  scale_range <- c(scale_min, scale_max)
  
  cat(sprintf("Matched patients: %d\n", n_patients))
  cat(sprintf("Items analyzed: %d (%s-%s)\n", n_items, item_cols[1], item_cols[n_items]))
  cat(sprintf("Scale: %d-%d (%d-point Likert)\n", 
              scale_min, scale_max, scale_max - scale_min + 1))
  cat(sprintf("Bootstrap iterations: %d\n", n_boot))
  cat(sprintf("AC2 weights: %s\n\n", ac2_weights))
  
  # Step 2: Item-level analysis with AC2
  item_results <- process_item_level(data_rater1, data_rater2, 
                                     ac2_weights = ac2_weights, 
                                     scale_range = scale_range, 
                                     n_boot = n_boot)
  
  # Display item results
  ir_display_item_results(item_results, scale_range)
  
  # Step 3: Scale-level analysis
  # Calculate total scores
  pairwise_results <- calculate_pairwise_totals(data_rater1, data_rater2)
  total_rater1 <- pairwise_results$total_r1
  total_rater2 <- pairwise_results$total_r2
  
  # Remove NA pairs for ICC calculation
  valid_pairs <- !is.na(total_rater1) & !is.na(total_rater2)
  total_rater1_clean <- total_rater1[valid_pairs]
  total_rater2_clean <- total_rater2[valid_pairs]
  
  # Create wide format for ICC
  total_wide <- data.frame(
    rater1 = total_rater1_clean,
    rater2 = total_rater2_clean
  )
  
  # Calculate ICC
  icc_results <- calculate_icc(total_wide)
  
  # Calculate scale-level AC2
  ac2_scale_result <- calculate_ac2_scale_level(total_rater1_clean, total_rater2_clean, 
                                                ac2_weights = ac2_weights)
  
  # Calculate Bland-Altman
  ba_results <- calculate_bland_altman(total_rater1_clean, total_rater2_clean)
  
  # Combine scale-level results
  scale_results <- list(
    icc_2_1 = icc_results$icc_2_1,
    icc_2_2 = icc_results$icc_2_2,
    ac2 = ac2_scale_result,
    bland_altman = ba_results
  )
  
  # Display scale results
  ir_display_scale_results(scale_results)
  
  # Step 4: Create summary
  summary_stats <- list(
    mean_p_o = mean(item_results$p_o, na.rm = TRUE),
    mean_kappa = mean(item_results$kappa_w, na.rm = TRUE),
    mean_ac2 = mean(item_results$ac2_value, na.rm = TRUE),
    n_low_kappa = sum(item_results$kappa_w < 0.5, na.rm = TRUE),
    n_low_ac2 = sum(item_results$ac2_value < 0.5, na.rm = TRUE),
    n_patients = n_patients,
    n_items = n_items
  )
  
  # Return results invisibly
  results <- list(
    item_level = item_results,
    scale_level = scale_results,
    summary = summary_stats,
    n_patients = n_patients,
    data_rater1 = data_rater1,
    data_rater2 = data_rater2,
    config_used = list(
      patient_id_col = patient_id_col,
      item_pattern = item_pattern,
      scale_min = scale_min,
      scale_max = scale_max,
      n_boot = n_boot,
      ac2_weights = ac2_weights
    )
  )
  
  cat("\n========================================\n")
  cat("Analysis Complete\n")
  cat("========================================\n")
  
  invisible(results)
}