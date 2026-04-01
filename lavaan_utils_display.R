# ===================================================
# Lavaan Utilities Display (View Layer)
# Description: Generic display functions for any lavaan fit results.
#              Used by CFA, ESEM, and any future lavaan-based analysis.
# ===================================================

# Translate factor names using display_names mapping
# Returns original name if not found in mapping (e.g., item names)
translate_factor_name <- function(name, display_names) {
  if (name %in% names(display_names)) {
    return(display_names[[name]])
  }
  name
}

# Translate a vector of names
translate_factor_names <- function(names_vec, display_names) {
  sapply(names_vec, translate_factor_name, display_names = display_names,
         USE.NAMES = FALSE)
}

# Display extended fit indices
display_extended_fit <- function(extended_fit) {
  
  cat("CHI-SQUARE TEST\n")
  cat("---------------\n")
  cat(sprintf("chi2 = %.2f, df = %d, p = %.4f\n",
              extended_fit$chi_square$chi2,
              extended_fit$chi_square$df,
              extended_fit$chi_square$pvalue))
  cat(sprintf("chi2/df = %.2f\n", extended_fit$chi_square$chi2_df_ratio))
  
  if (!is.na(extended_fit$chi_square$chi2_scaled)) {
    cat(sprintf("chi2 scaled = %.2f, df = %d, p = %.4f\n",
                extended_fit$chi_square$chi2_scaled,
                extended_fit$chi_square$df_scaled,
                extended_fit$chi_square$pvalue_scaled))
    cat(sprintf("chi2/df scaled = %.2f\n", extended_fit$chi_square$chi2_df_ratio_scaled))
  }
  
  cat("\nABSOLUTE FIT INDICES\n")
  cat("--------------------\n")
  cat(sprintf("RMSEA = %.3f [90%%CI: %.3f - %.3f], pclose = %.3f\n",
              extended_fit$absolute$rmsea,
              extended_fit$absolute$rmsea_ci_lower,
              extended_fit$absolute$rmsea_ci_upper,
              extended_fit$absolute$rmsea_pclose))
  
  if (!is.na(extended_fit$absolute$rmsea_scaled)) {
    cat(sprintf("RMSEA scaled = %.3f [90%%CI: %.3f - %.3f]\n",
                extended_fit$absolute$rmsea_scaled,
                extended_fit$absolute$rmsea_ci_lower_scaled,
                extended_fit$absolute$rmsea_ci_upper_scaled))
  }
  
  cat(sprintf("SRMR = %.3f\n", extended_fit$absolute$srmr))
  
  if (!is.na(extended_fit$absolute$rmr)) {
    cat(sprintf("RMR = %.3f\n", extended_fit$absolute$rmr))
  }
  
  if (!is.na(extended_fit$absolute$gfi)) {
    cat(sprintf("GFI = %.3f, AGFI = %.3f\n",
                extended_fit$absolute$gfi,
                extended_fit$absolute$agfi))
  }
  
  cat("\nINCREMENTAL FIT INDICES\n")
  cat("-----------------------\n")
  cat(sprintf("CFI = %.3f\n", extended_fit$incremental$cfi))
  cat(sprintf("TLI = %.3f\n", extended_fit$incremental$tli))
  
  if (!is.na(extended_fit$incremental$cfi_scaled)) {
    cat(sprintf("CFI scaled = %.3f\n", extended_fit$incremental$cfi_scaled))
    cat(sprintf("TLI scaled = %.3f\n", extended_fit$incremental$tli_scaled))
  }
  
  if (!is.na(extended_fit$incremental$nfi)) {
    cat(sprintf("NFI = %.3f\n", extended_fit$incremental$nfi))
  }
  
  cat("\nINFORMATION CRITERIA\n")
  cat("--------------------\n")
  cat(sprintf("AIC = %.1f\n", extended_fit$information$aic))
  cat(sprintf("BIC = %.1f\n", extended_fit$information$bic))
  
  if (!is.na(extended_fit$information$bic_adj)) {
    cat(sprintf("BIC adjusted = %.1f\n", extended_fit$information$bic_adj))
  }
  
  cat(sprintf("LogLik = %.1f\n", extended_fit$information$loglik))
  cat(sprintf("Number of parameters = %d\n", extended_fit$information$n_parameters))
  
  cat("\nSAMPLE INFORMATION\n")
  cat("------------------\n")
  cat(sprintf("N = %d\n", extended_fit$sample$n_total))
}

# Display residual diagnostics
display_residuals <- function(residual_summary) {
  
  cat("\nRESIDUAL ANALYSIS\n")
  cat("-----------------\n")
  
  cat("Raw Residuals:\n")
  cat(sprintf("  Mean absolute = %.4f\n", residual_summary$raw$mean))
  cat(sprintf("  Max absolute = %.4f\n", residual_summary$raw$max))
  cat(sprintf("  RMR = %.4f\n", residual_summary$raw$rmr))
  
  cat("\nStandardized Residuals:\n")
  cat(sprintf("  Mean absolute = %.3f\n", residual_summary$standardized$mean))
  cat(sprintf("  Max absolute = %.3f\n", residual_summary$standardized$max))
  cat(sprintf("  N |z| > 2: %d\n", residual_summary$standardized$n_above_2))
  cat(sprintf("  N |z| > 3: %d\n", residual_summary$standardized$n_above_3))
  cat(sprintf("  N |z| > 4: %d\n", residual_summary$standardized$n_above_4))
  
  cat("\nTop 10 Largest Standardized Residuals:\n")
  print(residual_summary$top_residuals[, c("var1", "var2", "std_residual")],
        row.names = FALSE)
}

# Display modification indices
display_modification_indices <- function(mi_summary, display_names) {
  
  cat("\nMODIFICATION INDICES\n")
  cat("--------------------\n")
  
  cat(sprintf("Total MI > 10: %d\n", mi_summary$n_mi_above_10))
  cat(sprintf("Total MI > 20: %d\n", mi_summary$n_mi_above_20))
  
  cat("\nTop 10 Overall Modification Indices:\n")
  if (nrow(mi_summary$top_10_overall) > 0) {
    display_df <- mi_summary$top_10_overall[, c("lhs", "op", "rhs", "mi", "epc", "sepc.all")]
    colnames(display_df) <- c("LHS", "OP", "RHS", "MI", "EPC", "Std.EPC")
    display_df$LHS <- translate_factor_names(display_df$LHS, display_names)
    display_df$RHS <- translate_factor_names(display_df$RHS, display_names)
    print(display_df, row.names = FALSE)
  }
  
  cat("\nTop 5 Cross-Loadings:\n")
  if (nrow(mi_summary$top_5_loadings) > 0) {
    display_df <- mi_summary$top_5_loadings[, c("lhs", "op", "rhs", "mi", "sepc.all")]
    colnames(display_df) <- c("Factor", "OP", "Item", "MI", "Std.EPC")
    display_df$Factor <- translate_factor_names(display_df$Factor, display_names)
    print(display_df, row.names = FALSE)
  }
  
  cat("\nTop 5 Error Covariances:\n")
  if (nrow(mi_summary$top_5_covariances) > 0) {
    display_df <- mi_summary$top_5_covariances[, c("lhs", "op", "rhs", "mi", "sepc.all")]
    colnames(display_df) <- c("Var1", "OP", "Var2", "MI", "Std.EPC")
    display_df$Var1 <- translate_factor_names(display_df$Var1, display_names)
    display_df$Var2 <- translate_factor_names(display_df$Var2, display_names)
    print(display_df, row.names = FALSE)
  }
}

# Display estimation problems
display_problems <- function(problems) {
  
  cat("\nESTIMATION DIAGNOSTICS\n")
  cat("----------------------\n")
  
  cat(sprintf("Converged: %s\n", ifelse(problems$converged, "Yes", "No")))
  cat(sprintf("Degrees of freedom: %d\n", problems$df))
  cat(sprintf("Model identified: %s\n", ifelse(problems$identified, "Yes", "No")))
  cat(sprintf("Heywood cases: %s\n", ifelse(problems$heywood_cases, "Yes", "No")))
  
  if (problems$heywood_cases) {
    cat("  Variables with negative variances:",
        paste(problems$negative_variances, collapse = ", "), "\n")
  }
  
  cat(sprintf("Large standard errors: %s\n", ifelse(problems$large_se, "Yes", "No")))
  
  if (problems$large_se) {
    cat(sprintf("  Number of parameters with SE > 10: %d\n",
                problems$parameters_with_large_se))
  }
  
  cat(sprintf("Condition number: %.1f\n", problems$condition_number))
  
  if (!is.null(problems$warnings) && length(problems$warnings) > 0) {
    cat("\nWarnings:\n")
    for (warning in problems$warnings) {
      cat("  -", warning, "\n")
    }
  }
}

# Display detailed parameters
display_parameters <- function(detailed_params, display_names) {
  
  cat("\nPARAMETER ESTIMATES\n")
  cat("-------------------\n")
  
  cat(sprintf("Total parameters: %d (Free: %d, Fixed: %d)\n",
              detailed_params$summary$n_parameters,
              detailed_params$summary$n_free,
              detailed_params$summary$n_fixed))
  
  cat("\nFactor Loadings:\n")
  loadings_display <- detailed_params$loadings[,
                                               c("lhs", "rhs", "est", "se", "z", "pvalue", "ci.lower", "ci.upper", "std.all")]
  colnames(loadings_display) <- c("Factor", "Item", "Est", "SE", "Z", "p",
                                  "CI.Low", "CI.High", "Std")
  loadings_display$Factor <- translate_factor_names(loadings_display$Factor, display_names)
  print(loadings_display, row.names = FALSE, digits = 3)
  
  if (nrow(detailed_params$covariances) > 0) {
    cat("\nFactor Covariances:\n")
    cov_display <- detailed_params$covariances[,
                                               c("lhs", "rhs", "est", "se", "z", "pvalue", "std.all")]
    colnames(cov_display) <- c("Factor1", "Factor2", "Est", "SE", "Z", "p", "Std")
    cov_display$Factor1 <- translate_factor_names(cov_display$Factor1, display_names)
    cov_display$Factor2 <- translate_factor_names(cov_display$Factor2, display_names)
    print(cov_display, row.names = FALSE, digits = 3)
  }
  
  cat("\nItem R-squared:\n")
  r2_values <- detailed_params$variances[detailed_params$variances$lhs == detailed_params$variances$rhs, ]
  r2_values <- r2_values[!grepl("^F", r2_values$lhs), ]  # Exclude factors
  if (nrow(r2_values) > 0) {
    r2_display <- data.frame(
      Item = r2_values$lhs,
      R2 = 1 - r2_values$std.all
    )
    print(r2_display[order(r2_display$R2, decreasing = TRUE), ],
          row.names = FALSE, digits = 3)
  }
}

# Display summary table
display_summary_table <- function(results) {
  cat("\nMODEL SUMMARY TABLE\n")
  cat("-------------------\n")
  
  summary_df <- data.frame(
    Metric = c("N", "chi2", "df", "p-value", "chi2/df",
               "CFI", "TLI", "RMSEA", "RMSEA 90%CI", "SRMR",
               "AIC", "BIC", "Parameters", "Converged"),
    Value = c(
      results$fit_indices$sample$n_total,
      sprintf("%.2f", results$fit_indices$chi_square$chi2),
      results$fit_indices$chi_square$df,
      sprintf("%.4f", results$fit_indices$chi_square$pvalue),
      sprintf("%.2f", results$fit_indices$chi_square$chi2_df_ratio),
      sprintf("%.3f", results$fit_indices$incremental$cfi),
      sprintf("%.3f", results$fit_indices$incremental$tli),
      sprintf("%.3f", results$fit_indices$absolute$rmsea),
      sprintf("[%.3f, %.3f]",
              results$fit_indices$absolute$rmsea_ci_lower,
              results$fit_indices$absolute$rmsea_ci_upper),
      sprintf("%.3f", results$fit_indices$absolute$srmr),
      sprintf("%.1f", results$fit_indices$information$aic),
      sprintf("%.1f", results$fit_indices$information$bic),
      results$fit_indices$information$n_parameters,
      ifelse(results$diagnostics$problems$converged, "Yes", "No")
    )
  )
  
  print(summary_df, row.names = FALSE)
}

# Display comparison of multiple models
display_model_comparison <- function(results_list) {
  cat("\nMODEL COMPARISON\n")
  cat("----------------\n\n")
  
  model_names <- names(results_list)
  
  # Create comparison matrix
  metrics <- c("chi2", "df", "p", "CFI", "TLI", "RMSEA", "SRMR", "AIC", "BIC")
  comparison_matrix <- matrix(NA, nrow = length(metrics), ncol = length(model_names))
  rownames(comparison_matrix) <- metrics
  colnames(comparison_matrix) <- model_names
  
  for (i in 1:length(model_names)) {
    res <- results_list[[model_names[i]]]
    comparison_matrix["chi2", i] <- sprintf("%.2f", res$fit_indices$chi_square$chi2)
    comparison_matrix["df", i] <- res$fit_indices$chi_square$df
    comparison_matrix["p", i] <- sprintf("%.4f", res$fit_indices$chi_square$pvalue)
    comparison_matrix["CFI", i] <- sprintf("%.3f", res$fit_indices$incremental$cfi)
    comparison_matrix["TLI", i] <- sprintf("%.3f", res$fit_indices$incremental$tli)
    comparison_matrix["RMSEA", i] <- sprintf("%.3f", res$fit_indices$absolute$rmsea)
    comparison_matrix["SRMR", i] <- sprintf("%.3f", res$fit_indices$absolute$srmr)
    comparison_matrix["AIC", i] <- sprintf("%.1f", res$fit_indices$information$aic)
    comparison_matrix["BIC", i] <- sprintf("%.1f", res$fit_indices$information$bic)
  }
  
  print(comparison_matrix, quote = FALSE)
  
  # Identify best model for each criterion
  cat("\nBest model by criterion:\n")
  cat(sprintf("  Lowest AIC: %s\n", model_names[which.min(sapply(results_list, function(x)
    x$fit_indices$information$aic))]))
  cat(sprintf("  Lowest BIC: %s\n", model_names[which.min(sapply(results_list, function(x)
    x$fit_indices$information$bic))]))
  cat(sprintf("  Highest CFI: %s\n", model_names[which.max(sapply(results_list, function(x)
    x$fit_indices$incremental$cfi))]))
  cat(sprintf("  Lowest RMSEA: %s\n", model_names[which.min(sapply(results_list, function(x)
    x$fit_indices$absolute$rmsea))]))
}

# ===================================================
# CFA vs ESEM Comparison Display (neutral)
# ===================================================

# Display CFA vs ESEM comparison
display_cfa_esem_comparison <- function(cfa_results, esem_results) {
  
  # --- 1. Fit indices comparison ---
  cat("\nFIT INDICES COMPARISON\n")
  cat("----------------------\n")
  
  cfa_fit <- cfa_results$fit_indices
  esem_fit <- esem_results$fit_indices
  
  comparison_df <- data.frame(
    Metric = c("chi2", "df", "CFI", "TLI", "RMSEA", "RMSEA CI lower",
               "RMSEA CI upper", "SRMR", "AIC", "BIC", "Parameters"),
    CFA = c(
      sprintf("%.2f", cfa_fit$chi_square$chi2),
      sprintf("%d", cfa_fit$chi_square$df),
      sprintf("%.3f", cfa_fit$incremental$cfi),
      sprintf("%.3f", cfa_fit$incremental$tli),
      sprintf("%.3f", cfa_fit$absolute$rmsea),
      sprintf("%.3f", cfa_fit$absolute$rmsea_ci_lower),
      sprintf("%.3f", cfa_fit$absolute$rmsea_ci_upper),
      sprintf("%.3f", cfa_fit$absolute$srmr),
      sprintf("%.1f", cfa_fit$information$aic),
      sprintf("%.1f", cfa_fit$information$bic),
      sprintf("%d", cfa_fit$information$n_parameters)
    ),
    ESEM = c(
      sprintf("%.2f", esem_fit$chi_square$chi2),
      sprintf("%d", esem_fit$chi_square$df),
      sprintf("%.3f", esem_fit$incremental$cfi),
      sprintf("%.3f", esem_fit$incremental$tli),
      sprintf("%.3f", esem_fit$absolute$rmsea),
      sprintf("%.3f", esem_fit$absolute$rmsea_ci_lower),
      sprintf("%.3f", esem_fit$absolute$rmsea_ci_upper),
      sprintf("%.3f", esem_fit$absolute$srmr),
      sprintf("%.1f", esem_fit$information$aic),
      sprintf("%.1f", esem_fit$information$bic),
      sprintf("%d", esem_fit$information$n_parameters)
    ),
    stringsAsFactors = FALSE
  )
  
  print(comparison_df, row.names = FALSE)
  
  # --- 2. ESEM factor loading matrix ---
  display_names <- esem_results$model_def$display_names
  esem_display_loadings(esem_results$esem_loadings, display_names)
  
  # --- 3. Factor correlations comparison ---
  cat("\nFACTOR CORRELATIONS COMPARISON\n")
  cat("-------------------------------\n")
  
  cfa_cors <- cfa_results$factor_correlations
  esem_cors <- lavInspect(esem_results$fit, "cor.lv")
  
  cat("\nCFA factor correlations:\n")
  print(round(cfa_cors, 3))
  
  cat("\nESEM factor correlations:\n")
  print(round(esem_cors, 3))
  
  # --- 4. Reliability comparison ---
  cat("\nRELIABILITY COMPARISON (CR / AVE)\n")
  cat("----------------------------------\n")
  
  cfa_rv <- cfa_results$reliability_validity
  esem_rv <- esem_results$reliability_validity
  
  cfa_factors <- names(cfa_rv)
  esem_factors <- names(esem_rv)
  
  common_factors <- intersect(cfa_factors, esem_factors)
  
  if (length(common_factors) > 0) {
    rv_comparison <- data.frame(
      Factor = character(),
      CFA_CR = numeric(),
      ESEM_CR = numeric(),
      CFA_AVE = numeric(),
      ESEM_AVE = numeric(),
      stringsAsFactors = FALSE
    )
    
    display_names <- esem_results$model_def$display_names
    
    for (factor in common_factors) {
      rv_comparison <- rbind(rv_comparison, data.frame(
        Factor = translate_factor_name(factor, display_names),
        CFA_CR = cfa_rv[[factor]]$cr,
        ESEM_CR = esem_rv[[factor]]$cr,
        CFA_AVE = cfa_rv[[factor]]$ave,
        ESEM_AVE = esem_rv[[factor]]$ave,
        stringsAsFactors = FALSE
      ))
    }
    
    print(rv_comparison, row.names = FALSE, digits = 3)
  } else {
    cat("Factor names do not match between CFA and ESEM.\n")
    cat("CFA factors: ", paste(cfa_factors, collapse = ", "), "\n")
    cat("ESEM factors: ", paste(esem_factors, collapse = ", "), "\n")
  }
}