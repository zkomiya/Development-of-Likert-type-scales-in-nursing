# ===================================================
# Visualization Calculator (Model Layer)
# Version: 1.0 - Data visualization calculations
# ===================================================

# Calculate response distribution for each item
# data: data.frame with Q columns only
# scale_min, scale_max: scale range (e.g., 1 to 4)
# Returns: list of data.frames per item
calc_response_distribution <- function(data, scale_min, scale_max) {
  
  items <- names(data)
  categories <- seq(scale_min, scale_max)
  
  result <- list()
  
  for (item in items) {
    counts <- table(factor(data[[item]], levels = categories))
    result[[item]] <- data.frame(
      category = as.integer(names(counts)),
      count = as.integer(counts)
    )
  }
  
  return(result)
}

# Calculate total score distribution
# data: data.frame with Q columns only
# Returns: list(scores, mean, sd, n, n_complete)
calc_total_score_distribution <- function(data) {
  
  # Calculate total scores (row sums)
  scores <- rowSums(data, na.rm = FALSE)
  
  # Remove NA for statistics
  scores_complete <- scores[!is.na(scores)]
  
  result <- list(
    scores = scores_complete,
    mean = mean(scores_complete),
    sd = sd(scores_complete),
    n = length(scores),
    n_complete = length(scores_complete)
  )
  
  return(result)
}

# Calculate correlation matrix
# data: data.frame with Q columns only
# use: how to handle missing values
# Returns: correlation matrix
calc_correlation_matrix <- function(data, use) {
  
  cor_matrix <- cor(data, use = use)
  
  return(cor_matrix)
}