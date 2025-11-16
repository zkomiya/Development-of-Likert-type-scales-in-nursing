# ===================================================
# Data Cleaner
# Version: 8.0 - 明示的引数版
# ===================================================

# Flatten list columns to vectors
flatten_list_columns <- function(data) {
  list_cols <- sapply(data, is.list)
  for (col in names(data)[list_cols]) {
    data[[col]] <- unlist(data[[col]])
  }
  data
}

# Convert columns matching pattern to numeric
convert_q_columns <- function(data, item_pattern) {
  cols <- grep(item_pattern, names(data), value = TRUE)
  
  for (col in cols) {
    if (is.factor(data[[col]])) {
      data[[col]] <- as.numeric(as.character(data[[col]]))
    } else if (is.character(data[[col]])) {
      data[[col]] <- as.numeric(data[[col]])
    } else if (is.logical(data[[col]])) {
      data[[col]] <- as.numeric(data[[col]])
    } else if (!is.numeric(data[[col]])) {
      data[[col]] <- as.numeric(as.character(data[[col]]))
    }
  }
  
  data
}

# Main cleaning function
clean_data <- function(raw_data, item_pattern, remove_na_rows) {
  # Step 1: Flatten list columns
  data <- flatten_list_columns(raw_data)
  
  # Step 2: Convert columns to numeric
  data <- convert_q_columns(data, item_pattern)
  
  # Step 3: Extract columns matching pattern only
  matched_cols <- grep(item_pattern, names(data), value = TRUE)
  data <- data[matched_cols]
  
  # Step 4: Handle NA rows
  if (remove_na_rows) {
    complete_rows <- complete.cases(data)
    data <- data[complete_rows, ]
  }
  
  data
}