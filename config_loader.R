# ===================================================
# Configuration Loader
# Version: 6.0 - Multiple spreadsheets structure
# ===================================================

load_config <- function() {
  library(yaml)
  
  CONFIG <- list()
  
  # Load system configuration
  if (file.exists("system_config.yaml")) {
    CONFIG$system <- yaml::read_yaml("system_config.yaml")
  } else {
    stop("System configuration file not found: system_config.yaml")
  }
  
  # Load analysis configuration
  if (file.exists("analysis_config.yaml")) {
    CONFIG$analysis <- yaml::read_yaml("analysis_config.yaml")
  } else {
    stop("Analysis configuration file not found: analysis_config.yaml")
  }
  
  # 新しい構造の検証（複数スプレッドシート対応）
  if (is.null(CONFIG$system$google_sheets)) {
    stop("Google Sheets configuration not found")
  }
  
  if (is.null(CONFIG$system$authentication$email_auth)) {
    stop("Email authentication not specified")
  }
  
  if (is.null(CONFIG$analysis$global$scale)) {
    stop("Global scale settings not found")
  }
  
  return(CONFIG)
}# ===================================================
# Configuration Loader
# Version: 6.0 - Multiple spreadsheets structure
# ===================================================

load_config <- function() {
  library(yaml)
  
  CONFIG <- list()
  
  # Load system configuration
  if (file.exists("system_config.yaml")) {
    CONFIG$system <- yaml::read_yaml("system_config.yaml")
  } else {
    stop("System configuration file not found: system_config.yaml")
  }
  
  # Load analysis configuration
  if (file.exists("analysis_config.yaml")) {
    CONFIG$analysis <- yaml::read_yaml("analysis_config.yaml")
  } else {
    stop("Analysis configuration file not found: analysis_config.yaml")
  }
  
  # 新しい構造の検証（複数スプレッドシート対応）
  if (is.null(CONFIG$system$google_sheets)) {
    stop("Google Sheets configuration not found")
  }
  
  if (is.null(CONFIG$system$authentication$email_auth)) {
    stop("Email authentication not specified")
  }
  
  if (is.null(CONFIG$analysis$global$scale)) {
    stop("Global scale settings not found")
  }
  
  return(CONFIG)
}