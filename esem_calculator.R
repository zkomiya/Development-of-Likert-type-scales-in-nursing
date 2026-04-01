# ===================================================
# ESEM Calculator (Model Layer)
# Description: ESEM-specific calculations.
#              Generic lavaan calculations are in lavaan_utils_calculator.R
# ===================================================

library(lavaan)

# Generate lavaan ESEM syntax (efa block)
# ESEM syntax is mechanically generated from factor names and items.
generate_esem_syntax <- function(factor_names, items) {
  
  efa_label <- "efa1"
  
  # Build efa block: efa("efa1")*f1 + efa("efa1")*f2 + ... =~ item1 + item2 + ...
  factor_terms <- paste0('efa("', efa_label, '")*', factor_names, collapse = " + ")
  item_terms <- paste(items, collapse = " + ")
  
  model_syntax <- paste0(factor_terms, " =~ ", item_terms)
  
  return(model_syntax)
}

# Perform ESEM
perform_esem <- function(data, factor_names, items, n_factors,
                         estimator, missing, se,
                         rotation, rotation_args) {
  
  # Subset data to specified items only
  data <- data[, items, drop = FALSE]
  
  # Ensure data is ordered for WLSMV
  if (estimator %in% c("WLSMV", "ULSMV")) {
    for (col in names(data)) {
      if (is.numeric(data[[col]])) {
        data[[col]] <- ordered(data[[col]])
      }
    }
  }
  
  # Generate ESEM model syntax
  model_syntax <- generate_esem_syntax(factor_names, items)
  
  # Fit ESEM model
  esem_fit <- sem(
    model = model_syntax,
    data = data,
    estimator = estimator,
    missing = missing,
    se = se,
    ordered = names(data),
    rotation = rotation,
    rotation.args = rotation_args
  )
  
  return(esem_fit)
}

# Extract ESEM factor loading matrix (all loadings including cross-loadings)
calculate_esem_loadings <- function(fit, factor_names, items) {
  
  # Get standardized solution
  std_sol <- standardizedSolution(fit)
  loadings_df <- std_sol[std_sol$op == "=~", ]
  
  # Build loading matrix: rows = items, columns = factors
  loading_matrix <- matrix(NA, nrow = length(items), ncol = length(factor_names))
  rownames(loading_matrix) <- items
  colnames(loading_matrix) <- factor_names
  
  for (i in seq_len(nrow(loadings_df))) {
    row_item <- loadings_df$rhs[i]
    col_factor <- loadings_df$lhs[i]
    if (row_item %in% items && col_factor %in% factor_names) {
      loading_matrix[row_item, col_factor] <- loadings_df$est.std[i]
    }
  }
  
  # Identify primary factor for each item (highest absolute loading)
  primary_factor <- apply(abs(loading_matrix), 1, which.max)
  primary_factor_name <- factor_names[primary_factor]
  
  # Extract primary loading value for each item
  primary_loading <- sapply(seq_along(items), function(i) {
    loading_matrix[i, primary_factor[i]]
  })
  
  results <- list(
    matrix = loading_matrix,
    primary_factor = primary_factor_name,
    primary_loading = primary_loading,
    loadings_df = loadings_df
  )
  
  return(results)
}

# Calculate reliability and validity indices (ESEM-specific)
# ESEM allows cross-loadings; primary factor is determined by highest absolute loading.
# CR and AVE are computed using only the primary loadings for each factor.
calculate_esem_reliability_validity <- function(fit, factor_names, items) {
  
  # Get loading matrix
  esem_loadings <- calculate_esem_loadings(fit, factor_names, items)
  loading_matrix <- esem_loadings$matrix
  primary_factor <- esem_loadings$primary_factor
  
  # Initialize results
  reliability_validity <- list()
  
  for (factor in factor_names) {
    # Items assigned to this factor (by primary loading)
    assigned_items <- items[primary_factor == factor]
    
    if (length(assigned_items) == 0) {
      reliability_validity[[factor]] <- list(
        cr = NA, ave = NA, n_items = 0,
        primary_loadings = numeric(0),
        assigned_items = character(0),
        mean_loading = NA, min_loading = NA, max_loading = NA
      )
      next
    }
    
    # Primary loadings for assigned items
    primary_loadings <- loading_matrix[assigned_items, factor]
    
    # Composite Reliability (CR)
    sum_loadings <- sum(primary_loadings)
    sum_loadings_sq <- sum_loadings^2
    sum_error_var <- sum(1 - primary_loadings^2)
    cr <- sum_loadings_sq / (sum_loadings_sq + sum_error_var)
    
    # Average Variance Extracted (AVE)
    ave <- mean(primary_loadings^2)
    
    reliability_validity[[factor]] <- list(
      cr = cr,
      ave = ave,
      n_items = length(assigned_items),
      primary_loadings = primary_loadings,
      assigned_items = assigned_items,
      mean_loading = mean(primary_loadings),
      min_loading = min(primary_loadings),
      max_loading = max(primary_loadings)
    )
  }
  
  # Calculate shared variances (if multiple factors)
  if (length(factor_names) > 1) {
    factor_cors <- lavInspect(fit, "cor.lv")
    
    for (factor in factor_names) {
      if (reliability_validity[[factor]]$n_items == 0) next
      
      factor_index <- which(rownames(factor_cors) == factor)
      cors_with_others <- factor_cors[factor_index, -factor_index]
      
      reliability_validity[[factor]]$msv <- max(cors_with_others^2)
      reliability_validity[[factor]]$asv <- mean(cors_with_others^2)
      
      # Store correlations for discriminant validity check
      reliability_validity[[factor]]$correlations_with_others <- cors_with_others
      reliability_validity[[factor]]$sqrt_ave <- sqrt(reliability_validity[[factor]]$ave)
    }
  }
  
  return(reliability_validity)
}