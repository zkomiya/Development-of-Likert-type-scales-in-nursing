# ===================================================
# Google Sheets Connector
# Version: 8.0 - YAML-based authentication and data loading
# ===================================================

connect_sheets <- function() {
  config <- load_config()
  email_auth <- config$system$authentication$email_auth
  cache_folder <- config$system$authentication$cache_folder
  
  options(gargle_oauth_cache = cache_folder)
  gs4_auth(email = email_auth, cache = cache_folder)
  cat("Connected to Google Sheets\n")
}

get_sheets_data <- function(dataset_name) {
  config <- load_config()
  dataset_config <- config$system$google_sheets[[dataset_name]]
  spreadsheet_id <- dataset_config$spreadsheet_id
  sheet_name <- dataset_config$sheet_name
  
  data <- read_sheet(spreadsheet_id, sheet = sheet_name)
  cat(sprintf("Data loaded: %d rows x %d columns\n", nrow(data), ncol(data)))
  data
}