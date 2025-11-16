# ===================================================
# EFA Workflow Example
# Version: 2.0 - Updated for target + REHAB structure
# Description: Complete workflow for EFA analysis
# ===================================================

# ---------------------------
# 1. Initialize and Load Data
# ---------------------------
source("load_scripts.R")

# Load target dataset (as specified in YAML) and REHAB
data <- prepare_data()

# Extract target dataset for analysis
target_data <- data$target
rehab_data <- data$rehab  # Available for external validity

# Check dimensions
cat("Target data:", nrow(target_data), "rows x", ncol(target_data), "columns\n")
cat("REHAB data:", nrow(rehab_data), "rows x", ncol(rehab_data), "columns\n")

# ---------------------------
# 2. Determine Number of Factors
# ---------------------------

# Run factor number determination on target dataset
factor_det <- determine_factors(target_data, n_iterations = 1000, percentile = 99)

# Review recommendations
# Kaiser: X factors
# PA: X factors  
# MAP: X factors

# ---------------------------
# 3. Execute EFA on Target
# ---------------------------

# Based on recommendations, choose number of factors
# For example, if all methods suggest 5:
efa_results <- analyze_efa(target_data, n_factors = 5)

# ---------------------------
# 4. View Specific Results
# ---------------------------

# View MINRES with orthogonal rotation (gamma = 0)
show_efa(efa_results, method = "MINRES", gamma = 0)

# View MINRES with oblique rotation (gamma = -0.5)
show_efa(efa_results, method = "MINRES", gamma = -0.5)

# View ULS results
show_efa(efa_results, method = "ULS", gamma = 0)

# ---------------------------
# 5. Try Different Factor Solutions
# ---------------------------

# If needed, try different number of factors
efa_4factors <- analyze_efa(target_data, n_factors = 4)
efa_6factors <- analyze_efa(target_data, n_factors = 6)

# Compare results
show_efa(efa_4factors, method = "MINRES", gamma = 0)
show_efa(efa_6factors, method = "MINRES", gamma = 0)

# ---------------------------
# 6. External Validity (Optional)
# ---------------------------

# If external validity analysis is available
# analyze_external_validity(target_data, rehab_data)