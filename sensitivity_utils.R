# ===================================================
# Sensitivity Analysis Utilities
# Description: Shared utility functions for sensitivity analysis
#              (exclude-item subset generation)
# ===================================================

# Generate power-set of exclude items (all combinations including empty set)
generate_exclude_subsets <- function(exclude_items) {
  
  if (is.null(exclude_items) || length(exclude_items) == 0) {
    return(list(list(key = "full", items = character(0))))
  }
  
  exclude_items <- sort(exclude_items)
  n <- length(exclude_items)
  
  # Start with empty set (full dataset)
  subsets <- list(list(key = "full", items = character(0)))
  
  for (k in seq_len(n)) {
    combos <- utils::combn(exclude_items, k, simplify = FALSE)
    for (combo in combos) {
      key <- paste0("excl_", paste(combo, collapse = "_"))
      subsets <- c(subsets, list(list(key = key, items = combo)))
    }
  }
  
  return(subsets)
}