# ===================================================
# Data Structure Functions
# Version: 1.1 - Strict error handling
# Description: Functions to create and manipulate data with keys
# ===================================================

# Create data object with keys
create_data_with_keys <- function(keys, data) {
  if (is.null(keys)) {
    stop("keys cannot be NULL")
  }
  if (!is.character(keys)) {
    stop("keys must be a character vector")
  }
  if (!is.data.frame(data)) {
    stop("data must be a data.frame")
  }
  
  structure(
    list(
      keys = keys,
      data = data
    ),
    class = c("keyed_data", "list")
  )
}

# Extract data part
get_data <- function(data_obj) {
  if (inherits(data_obj, "keyed_data")) {
    return(data_obj$data)
  } else if (is.list(data_obj) && "data" %in% names(data_obj)) {
    return(data_obj$data)
  } else {
    stop("Input must be a keyed_data object or a list with 'data' field")
  }
}

# Extract keys
get_keys <- function(data_obj) {
  if (inherits(data_obj, "keyed_data")) {
    return(data_obj$keys)
  } else if (is.list(data_obj) && "keys" %in% names(data_obj)) {
    return(data_obj$keys)
  } else {
    stop("Input must be a keyed_data object or a list with 'keys' field")
  }
}

# Check if object has keys
has_keys <- function(data_obj) {
  if (inherits(data_obj, "keyed_data")) {
    return(TRUE)
  } else if (is.list(data_obj) && "keys" %in% names(data_obj)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}