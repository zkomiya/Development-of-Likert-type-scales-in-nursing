# ===================================================
# Data Preparation Function - With Type Conversion
# Version: 10.0 - YAML-based connector
# ===================================================

prepare_data <- function() {
  cat("========================================\n")
  cat("Data Preparation\n")
  cat("========================================\n\n")
  
  # Load configuration
  config <- load_config()
  
  # Connect to Google Sheets
  connect_sheets()
  
  # Get target dataset name from config
  target_dataset_name <- config$analysis$data_source$dataset
  
  # Load target dataset
  cat("Loading target dataset...\n")
  raw_target <- get_sheets_data(target_dataset_name)
  
  # Load REHAB dataset
  cat("Loading REHAB dataset...\n")
  raw_rehab <- get_sheets_data("rehab")
  
  # Load Interrater dataset
  cat("Loading Interrater dataset...\n")
  raw_interrater <- get_sheets_data("interrater")
  
  # Get key column names from config
  target_key_column <- config$analysis$data_keys[[target_dataset_name]][1]
  rehab_key_column <- config$analysis$data_keys$rehab[1]
  interrater_key_column <- config$analysis$data_keys$interrater[1]
  
  cat(sprintf("\nProcessing datasets with type conversion...\n"))
  
  # Process target dataset with type conversion
  target_q_cols <- grep("^Q\\d{2}$", names(raw_target), value = TRUE)
  target_subset <- raw_target[, target_q_cols, drop = FALSE]
  
  # Use clean_data for type conversion
  source("data_cleaner.R")
  target_clean <- clean_data(target_subset, 
                             item_pattern = "^Q\\d{2}$",
                             remove_na_rows = FALSE)
  
  # Convert to data.frame and set row names
  target_clean <- as.data.frame(target_clean)
  rownames(target_clean) <- raw_target[[target_key_column]]
  
  # Process REHAB dataset with type conversion
  rehab_q_cols <- grep("^R_Q\\d{2}$", names(raw_rehab), value = TRUE)
  rehab_subset <- raw_rehab[, rehab_q_cols, drop = FALSE]
  
  # Use clean_data for type conversion
  rehab_clean <- clean_data(rehab_subset,
                            item_pattern = "^R_Q\\d{2}$",
                            remove_na_rows = FALSE)
  
  # Convert to data.frame and set row names
  rehab_clean <- as.data.frame(rehab_clean)
  rownames(rehab_clean) <- raw_rehab[[rehab_key_column]]
  
  # Process Interrater dataset - keep patient ID but convert Q columns
  interrater_clean <- raw_interrater
  
  # Convert Q columns to numeric while keeping patient ID column
  interrater_clean <- convert_q_columns(interrater_clean, "^Q\\d{2}$")
  interrater_clean <- as.data.frame(interrater_clean)
  
  # Verify data types
  cat("\nData type verification:\n")
  
  # Check target
  target_numeric <- all(sapply(target_clean, is.numeric))
  cat(sprintf("  Target: %d items, all numeric: %s\n", 
              ncol(target_clean), target_numeric))
  
  # Check REHAB
  rehab_numeric <- all(sapply(rehab_clean, is.numeric))
  cat(sprintf("  REHAB: %d items, all numeric: %s\n",
              ncol(rehab_clean), rehab_numeric))
  
  # Check interrater Q columns
  interrater_q_cols <- grep("^Q\\d{2}$", names(interrater_clean), value = TRUE)
  interrater_q_numeric <- all(sapply(interrater_clean[interrater_q_cols], is.numeric))
  cat(sprintf("  Interrater: %d Q columns, all numeric: %s\n",
              length(interrater_q_cols), interrater_q_numeric))
  
  # Data dimensions and matching
  cat("\nData summary:\n")
  cat(sprintf("  Target: %d patients, %d items\n", 
              nrow(target_clean), ncol(target_clean)))
  cat(sprintf("  REHAB: %d patients, %d items\n",
              nrow(rehab_clean), ncol(rehab_clean)))
  cat(sprintf("  Interrater: %d rows, %d columns\n",
              nrow(interrater_clean), ncol(interrater_clean)))
  
  # Check matching between target and REHAB
  common_ids <- intersect(rownames(target_clean), rownames(rehab_clean))
  cat(sprintf("  Common patients (target-REHAB): %d\n", length(common_ids)))
  
  if (length(common_ids) < nrow(target_clean)) {
    cat("  WARNING: Not all patients match between target and REHAB!\n")
  }
  
  # Check interrater data structure
  if (interrater_key_column %in% names(interrater_clean)) {
    patient_counts <- table(interrater_clean[[interrater_key_column]])
    n_pairs <- sum(patient_counts == 2)
    cat(sprintf("  Interrater: %d patient pairs (2 ratings each)\n", n_pairs))
  }
  
  # Create result with data_structure format
  source("data_structure.R")
  
  result <- list(
    target = create_data_with_keys(
      keys = target_key_column,
      data = target_clean
    ),
    rehab = create_data_with_keys(
      keys = rehab_key_column, 
      data = rehab_clean
    ),
    interrater = create_data_with_keys(
      keys = interrater_key_column,
      data = interrater_clean
    ),
    config = config
  )
  
  cat("\nData preparation complete!\n")
  cat("========================================\n")
  
  return(result)
}