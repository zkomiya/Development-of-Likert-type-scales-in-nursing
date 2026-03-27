# ===================================================
# Validity Main (Controller Layer)
# ===================================================

analyze_validity <- function(target_obj, rehab_obj) {
  
  # Load config (controller layer only)
  config <- load_config()
  dataset_name <- config$analysis$data_source$dataset
  target_subscales <- config$analysis$scale_structure[[dataset_name]]
  rehab_gb_subscales <- config$analysis$rehab_subscales$general_behavior
  rehab_db_definition <- config$analysis$rehab_subscales$deviant_behavior
  hypotheses <- config$analysis$validity_hypotheses
  
  # Display header
  validity_display_header()
  
  # Step 1: Data overview
  cat("Step 1: Data Overview\n")
  cat("----------------------\n")
  
  source("data_structure.R")
  target_data <- get_data(target_obj)
  target_keys <- get_keys(target_obj)
  rehab_data <- get_data(rehab_obj)
  rehab_keys <- get_keys(rehab_obj)
  
  cat(sprintf("Target dataset:\n"))
  cat(sprintf("  Data: %d rows x %d columns\n",
              nrow(target_data), ncol(target_data)))
  cat(sprintf("  Keys: %s\n", paste(target_keys, collapse = ", ")))
  
  cat(sprintf("REHAB dataset:\n"))
  cat(sprintf("  Data: %d rows x %d columns\n",
              nrow(rehab_data), ncol(rehab_data)))
  cat(sprintf("  Keys: %s\n\n", paste(rehab_keys, collapse = ", ")))
  
  # Step 2: Prepare scores (once)
  cat("Step 2: Score Preparation\n")
  cat("--------------------------\n")
  
  score_data <- prepare_validity_scores(
    target_obj, rehab_obj,
    target_subscales, rehab_gb_subscales, rehab_db_definition
  )
  
  target_scores <- score_data$target_scores
  rehab_scores <- score_data$rehab_scores
  n <- score_data$n
  
  # Step 3: Primary analysis - Hypothesis testing (Pearson)
  cat("Step 3: Primary Analysis - Hypothesis Testing (Pearson)\n")
  cat("--------------------------------------------------------\n")
  
  hyp_pearson <- test_hypothesis_set(
    target_scores, rehab_scores, hypotheses, method = "pearson"
  )
  validity_display_hypotheses(
    hyp_pearson, target_subscales, rehab_gb_subscales, rehab_db_definition
  )
  
  # Step 4: Sensitivity analysis - Hypothesis testing (Spearman)
  cat("\nStep 4: Sensitivity Analysis - Hypothesis Testing (Spearman)\n")
  cat("--------------------------------------------------------------\n")
  
  hyp_spearman <- test_hypothesis_set(
    target_scores, rehab_scores, hypotheses, method = "spearman"
  )
  validity_display_hypotheses(
    hyp_spearman, target_subscales, rehab_gb_subscales, rehab_db_definition
  )
  
  # Step 5: Supplementary analysis - Full correlation matrix (Pearson)
  cat("\nStep 5: Supplementary Analysis - Full Correlation Matrix\n")
  cat("----------------------------------------------------------\n\n")
  
  corr_results <- calculate_validity_correlations(
    target_scores, rehab_scores, method = "pearson"
  )
  
  validity_display_total(corr_results)
  validity_display_subscales(corr_results, target_subscales)
  validity_display_subscale_matrix(corr_results, target_subscales, rehab_gb_subscales)
  
  # Summary
  cat("\n========================================\n")
  cat("Analysis Complete\n")
  cat("========================================\n")
  
  cat("\nInterpretation Guide:\n")
  cat("- High REHAB scores indicate worse functioning (both GB and DB)\n")
  cat("- Negative correlations indicate convergent validity\n")
  cat("  (higher readiness on target = lower dysfunction on REHAB)\n")
  cat("- Hypothesis testing: proximal REHAB subscale should show\n")
  cat("  stronger |r| than distal REHAB subscale\n")
  cat("- Steiger (1980) test for dependent correlations sharing one variable\n")
  cat("- One-sided p-value: H1 |r(proximal)| > |r(distal)|\n")
  
  invisible(list(
    hypotheses_pearson = hyp_pearson,
    hypotheses_spearman = hyp_spearman,
    correlations = corr_results
  ))
}