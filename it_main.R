# ===================================================
# Item-Total Correlation Main (Controller Layer)
# Version: 7.2 - Fixed subscale config loading from scale_structure
# Description: Main controller for I-T correlation analysis
# Changes from v7.1:
#   - Fixed subscale config loading: item_total_analysis -> scale_structure
# ===================================================

# Evaluate I-T correlation results
evaluate_it_results <- function(it_results) {
  
  # Thresholds (hardcoded)
  threshold_poor <- 0.20
  threshold_marginal <- 0.30
  threshold_good <- 0.40
  
  # Evaluation function
  categorize <- function(r) {
    dplyr::case_when(
      is.na(r) ~ "NA",
      r < threshold_poor ~ "Poor",
      r < threshold_marginal ~ "Marginal",
      r < threshold_good ~ "Good",
      TRUE ~ "Excellent"
    )
  }
  
  # Evaluate both methods (corrected only)
  evaluation <- data.frame(
    item = it_results$item,
    pearson_r = it_results$cor_corrected_pearson,
    pearson_eval = categorize(it_results$cor_corrected_pearson),
    polyserial_r = it_results$cor_corrected_polyserial,
    polyserial_eval = categorize(it_results$cor_corrected_polyserial),
    stringsAsFactors = FALSE
  )
  
  # Summary counts
  pearson_counts <- table(factor(evaluation$pearson_eval, 
                                 levels = c("Poor", "Marginal", "Good", "Excellent", "NA")))
  polyserial_counts <- table(factor(evaluation$polyserial_eval,
                                    levels = c("Poor", "Marginal", "Good", "Excellent", "NA")))
  
  # Problem items
  problem_items <- evaluation$item[evaluation$pearson_eval %in% c("Poor", "Marginal") |
                                     evaluation$polyserial_eval %in% c("Poor", "Marginal")]
  
  list(
    evaluation = evaluation,
    pearson_counts = pearson_counts,
    polyserial_counts = polyserial_counts,
    problem_items = problem_items,
    thresholds = list(
      poor = threshold_poor,
      marginal = threshold_marginal,
      good = threshold_good
    )
  )
}

# Main Item-Total correlation analysis function
analyze_item_total <- function(data_obj) {
  
  # Extract data from keyed structure
  data <- get_data(data_obj)
  keys <- get_keys(data_obj)
  
  # Load configuration from YAML
  config <- load_config()
  dataset_name <- config$analysis$data_source$dataset
  subscale_config <- config$analysis$scale_structure[[dataset_name]]
  enable_subscales <- !is.null(subscale_config)
  
  # Display header
  it_display_header()
  
  item_names <- names(data)
  n_items <- length(item_names)
  
  # Display data info
  it_display_data_info(n_items, item_names)
  
  # Display calculation info
  it_display_calculation_info()
  
  # Step 2: Calculate item-total correlations (both Pearson and Polyserial)
  it_results <- calculate_item_total_correlations(data)
  
  # Display results (4-column structure)
  it_display_results(it_results)
  
  # Step 3: Calculate summary statistics
  summary_stats <- calculate_it_summary(it_results)
  
  # Display summary
  it_display_summary(summary_stats)
  
  # Step 4: Subscale analysis (if enabled in YAML)
  subscale_results <- NULL
  
  if (enable_subscales && !is.null(subscale_config)) {
    all_elements <- names(subscale_config)
    subscale_names <- all_elements[!all_elements %in% c("enable_subscales")]
    
    if (length(subscale_names) > 0) {
      cat("\n========================================\n")
      cat("SUBSCALE ITEM-TOTAL CORRELATIONS\n")
      cat("========================================\n")
      
      subscale_results <- list()
      
      for (subscale_id in subscale_names) {
        subscale_info <- subscale_config[[subscale_id]]
        
        if (!is.null(subscale_info$name) && !is.null(subscale_info$items)) {
          subscale_name <- subscale_info$name
          subscale_items <- subscale_info$items
          
          available_items <- subscale_items[subscale_items %in% names(data)]
          
          if (length(available_items) > 0) {
            # Display subscale header
            it_display_subscale_header(subscale_name, length(available_items))
            
            # Calculate subscale I-T correlations (4-column)
            subscale_it <- calculate_subscale_item_total(data, available_items)
            
            # Display results
            it_display_subscale_results(subscale_it)
            
            # Calculate and display summary
            subscale_summary <- calculate_it_summary(subscale_it)
            cat(sprintf("\n%s Summary:\n", subscale_name))
            cat(sprintf("  Pearson Corrected:    Mean = %.3f (SD = %.3f)\n",
                        subscale_summary$mean_corrected_pearson,
                        subscale_summary$sd_corrected_pearson))
            cat(sprintf("  Polyserial Corrected: Mean = %.3f (SD = %.3f)\n",
                        subscale_summary$mean_corrected_polyserial,
                        subscale_summary$sd_corrected_polyserial))
            
            # Store results
            subscale_results[[subscale_id]] <- list(
              name = subscale_name,
              correlations = subscale_it,
              summary = subscale_summary,
              items = available_items
            )
          } else {
            cat(sprintf("\nWarning: No items found for subscale '%s'\n", subscale_name))
          }
        }
      }
    }
  }
  
  # Step 5: Display comparison (if subscales exist)
  comparison <- NULL
  if (!is.null(subscale_results) && length(subscale_results) > 0) {
    comparison <- create_it_comparison(it_results, subscale_results, subscale_config)
    it_display_comparison(comparison)
  }
  
  # ===== CSV Output =====
  # Create output directory if not exists
  if (!dir.exists("output")) {
    dir.create("output")
  }
  
  # Output 1: Main correlations
  write.csv(it_results, "output/it_correlations.csv", row.names = FALSE)
  cat("\nCSV exported: output/it_correlations.csv\n")
  
  # Output 2: Subscale results (if exists)
  if (!is.null(subscale_results) && length(subscale_results) > 0) {
    subscale_all <- do.call(rbind, lapply(names(subscale_results), function(id) {
      df <- subscale_results[[id]]$correlations
      df$subscale <- subscale_results[[id]]$name
      df
    }))
    write.csv(subscale_all, "output/it_subscale_correlations.csv", row.names = FALSE)
    cat("CSV exported: output/it_subscale_correlations.csv\n")
  }
  
  # Output 3: Comparison (if exists)
  if (!is.null(comparison)) {
    write.csv(comparison, "output/it_comparison.csv", row.names = FALSE)
    cat("CSV exported: output/it_comparison.csv\n")
  }
  
  # Return results
  results <- list(
    correlations = it_results,
    summary = summary_stats,
    subscale_results = subscale_results,
    comparison = comparison,
    item_data = data,
    item_names = item_names,
    config_used = list(
      enable_subscales = enable_subscales,
      subscale_config = subscale_config
    )
  )
  
  invisible(results)
}

# Show I-T evaluation (standalone function)
show_it_evaluation <- function(data_obj) {
  
  # Extract data from keyed structure
  data <- get_data(data_obj)
  
  # Calculate I-T correlations
  it_results <- calculate_item_total_correlations(data)
  
  # Evaluate results
  eval_results <- evaluate_it_results(it_results)
  
  # Display evaluation
  display_it_evaluation(eval_results)
  
  invisible(eval_results)
}