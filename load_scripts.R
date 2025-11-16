# ===================================================
# Master Script Loader
# Version: 24.0 - Added polycor for Polychoric correlations
# Description: Load all required scripts and libraries
# Usage: source("load_scripts.R")
# Changes from v23.0:
#   - Added polycor package for Polychoric correlations in validity analysis
# ===================================================

cat("========================================\n")
cat("Loading all scripts...\n")
cat("========================================\n\n")

# Step 1: Load required libraries
cat("Loading required libraries...\n")
suppressPackageStartupMessages({
  library(googlesheets4)
  library(tidyverse)
  library(yaml)
  library(pwr)
  library(psych)
  library(polycor)        # 追加: Polychoric相関用 (Version 24.0)
  library(GPArotation)
  library(clue)
  library(irr)
  library(boot)
  library(BlandAltmanLeh)
  library(irrCAC)
  library(lavaan)
  library(effectsize)
})

# Step 2: Load configuration loader
cat("Loading configuration loader...\n")
source("config_loader.R")

# Step 2.5: Load data structure functions
cat("Loading data structure functions...\n")
source("data_structure.R")

# Step 3: Load data management modules
cat("Loading data management modules...\n")
source("google_sheets_connector.R")
source("data_cleaner.R")
source("prepare_data.R") 

# Step 4: Load GP analysis modules
cat("Loading GP analysis modules...\n")
source("gp_calculator.R")
source("gp_display.R")
source("gp_main.R")

# Step 5: Load Item-Item analysis modules
cat("Loading Item-Item analysis modules...\n")
source("ii_calculator.R")
source("ii_display.R")
source("ii_main.R")

# Step 6: Load Item-Total analysis modules
cat("Loading Item-Total analysis modules...\n")
source("it_calculator.R")
source("it_display.R")
source("it_main.R")

# Step 7: Load Ceiling-Floor effect analysis modules
cat("Loading Ceiling-Floor effect analysis modules...\n")
source("ceiling_floor_calculator.R")
source("ceiling_floor_display.R")
source("ceiling_floor_main.R")

# Step 8: Load Reliability analysis modules
cat("Loading Reliability analysis modules...\n")
source("alpha_calculator.R")
source("alpha_display.R")
source("omega_calculator.R")
source("omega_display.R")
source("reliability_main.R")

# Step 9: Load Factor Analysis common modules
cat("Loading Factor Analysis common modules...\n")
source("factor_prerequisites.R")
source("factor_suitability_calculator.R")
source("factor_suitability_display.R")
source("factor_suitability_main.R")
source("factor_number_calculator.R")
source("data_preprocessor.R")

# Step 10: Load EFA modules
cat("Loading EFA modules...\n")
source("efa_calculator.R")
source("efa_display.R")
source("efa_main.R")

# Step 11: Load Validity analysis modules
cat("Loading Validity analysis modules...\n")
source("validity_calculator.R")
source("validity_display.R")
source("validity_main.R")

# Step 12: Load Interrater Reliability modules
cat("Loading Interrater Reliability modules...\n")
source("interrater_calculator.R")
source("interrater_display.R")
source("interrater_main.R")

# Step 13: Load CFA modules
cat("Loading CFA modules...\n")
source("cfa_calculator.R")
source("cfa_display.R")
source("cfa_main.R")

cat("\n========================================\n")
cat("All scripts loaded successfully!\n")
cat("========================================\n\n")

# ===================================================
# USAGE EXAMPLES
# ===================================================

cat("========================================\n")
cat("USAGE EXAMPLES\n")
cat("========================================\n\n")

cat("--- 1. DATA PREPARATION ---\n")
cat("# Load all datasets (target, rehab, interrater)\n")
cat("data <- prepare_data()\n\n")

cat("--- 2. ITEM ANALYSIS ---\n")
cat("# Good-Poor Analysis\n")
cat("gp_results <- analyze_gp(data$target)\n\n")

cat("# Item-Item Correlation\n")
cat("ii_results <- analyze_item_correlations(data$target)\n\n")

cat("# Item-Total Correlation\n")
cat("it_results <- analyze_item_total(data$target)\n\n")

cat("--- 3. DISTRIBUTION ANALYSIS ---\n")
cat("# Ceiling-Floor Effects\n")
cat("cf_results <- analyze_ceiling_floor(data$target)\n\n")

cat("--- 4. RELIABILITY ANALYSIS ---\n")
cat("# Cronbach's Alpha + McDonald's Omega\n")
cat("rel_results <- analyze_reliability(data$target)\n\n")

cat("--- 5. FACTOR ANALYSIS ---\n")
cat("# Check FA suitability\n")
cat("fa_suit <- check_fa_suitability(data$target)\n\n")

cat("# Determine number of factors\n")
cat("factor_det <- determine_n_factors(data$target,\n")
cat("                                  n_iterations = 1000,\n")
cat("                                  percentile = 99)\n\n")

cat("# Exploratory Factor Analysis (Polychoric + Pearson)\n")
cat("efa_results <- analyze_efa(data$target, n_factors = 5)\n")
cat("# Results include both correlation methods, aligned for comparison\n\n")
cat("# View individual results:\n")
cat("show_efa(efa_results$polychoric$efa, method = \"MINRES\", gamma = 0)\n")
cat("show_efa(efa_results$pearson$efa, method = \"MINRES\", gamma = 0)\n\n")

cat("# Confirmatory Factor Analysis (Extended)\n")
cat("cfa_results <- analyze_cfa(data$target, model_name = \"five_factor_model\")\n")
cat("# Extended results include:\n")
cat("#   - Comprehensive fit indices (χ²/df, CFI, TLI, RMSEA, SRMR, etc.)\n")
cat("#   - Residual diagnostics\n")
cat("#   - Modification indices\n")
cat("#   - Reliability & validity metrics (CR, AVE, MSV, ASV)\n")
cat("#   - Estimation problems check\n")
cat("#   - Detailed parameter estimates\n\n")

cat("# Extract specific CFA results\n")
cat("fit_indices <- extract_cfa_results(cfa_results, \"fit\")\n")
cat("mi <- extract_cfa_results(cfa_results, \"mi\")\n")
cat("reliability <- extract_cfa_results(cfa_results, \"reliability\")\n\n")

cat("# Export CFA results to Excel\n")
cat("export_cfa_results(cfa_results, \"cfa_results.xlsx\")\n\n")

cat("# Show summary table\n")
cat("show_cfa_summary(cfa_results)\n\n")

cat("# Compare CFA models\n")
cat("comparison <- compare_cfa_models(data$target,\n")
cat("                                 model_names = c(\"five_factor_model\",\n")
cat("                                               \"four_factor_model\"))\n\n")

cat("--- 6. VALIDITY ANALYSIS ---\n")
cat("# External validity with REHAB (Polychoric + Pearson)\n")
cat("validity_results <- analyze_validity(data$target, data$rehab)\n")
cat("# Results now include both correlation methods for comparison\n\n")

cat("--- 7. INTERRATER RELIABILITY ---\n")
cat("# Interrater reliability analysis\n")
cat("ir_results <- analyze_interrater(data$interrater)\n\n")

cat("========================================\n")
cat("CONFIGURATION MANAGEMENT\n")
cat("========================================\n\n")

cat("All analysis functions use YAML configuration.\n")
cat("To modify analysis parameters:\n\n")

cat("1. Edit analysis_config.yaml:\n")
cat("   - Scale ranges (min/max)\n")
cat("   - GP cutoff percentiles\n")
cat("   - Correlation methods\n")
cat("   - EFA settings\n")
cat("   - CFA settings (estimator, missing, se)\n")
cat("   - Interrater settings\n")
cat("   - Subscale definitions\n")
cat("   - CFA model specifications\n\n")

cat("2. Edit system_config.yaml:\n")
cat("   - Google Sheets authentication\n")
cat("   - Spreadsheet IDs\n")
cat("   - Data source selection\n\n")

cat("Advantages of YAML configuration:\n")
cat("  ✓ Centralized parameter management\n")
cat("  ✓ Prevents inconsistencies\n")
cat("  ✓ Easy to reproduce analyses\n")
cat("  ✓ Version control friendly\n")
cat("  ✓ No need to remember arguments\n\n")

cat("========================================\n")
cat("FUNCTION SUMMARY\n")
cat("========================================\n\n")

cat("Data Preparation:\n")
cat("  prepare_data()                    - Load all datasets from Google Sheets\n\n")

cat("Item Analysis:\n")
cat("  analyze_gp()                      - Good-Poor discrimination analysis\n")
cat("  analyze_item_correlations()       - Item-Item correlation matrix\n")
cat("  analyze_item_total()              - Item-Total correlations\n\n")

cat("Distribution Analysis:\n")
cat("  analyze_ceiling_floor()           - Ceiling-Floor effects\n\n")

cat("Reliability Analysis:\n")
cat("  analyze_reliability()             - Cronbach's α & McDonald's ω\n\n")

cat("Factor Analysis:\n")
cat("  check_fa_suitability()            - KMO & Bartlett's test\n")
cat("  determine_n_factors()             - Kaiser, PA, MAP methods\n")
cat("  analyze_efa()                     - Exploratory Factor Analysis (Poly + Pearson)\n")
cat("  analyze_cfa()                     - Confirmatory Factor Analysis (Extended)\n")
cat("  extract_cfa_results()             - Extract specific CFA results\n")
cat("  export_cfa_results()              - Export CFA results to Excel\n")
cat("  show_cfa_summary()                - Display summary table\n")
cat("  compare_cfa_models()              - Compare multiple CFA models\n\n")

cat("Validity Analysis:\n")
cat("  analyze_validity()                - External validity (Polychoric + Pearson)\n\n")

cat("Interrater Reliability:\n")
cat("  analyze_interrater()              - ICC, Kappa, AC2, Bland-Altman\n\n")

cat("========================================\n")
cat("Ready to analyze!\n")
cat("========================================\n\n")