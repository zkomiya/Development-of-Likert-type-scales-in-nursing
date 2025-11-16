# EFA (Exploratory Factor Analysis) Usage Guide

## Overview
EFA analysis is now separated into two independent steps:
1. **Factor Number Determination** - Determine appropriate number of factors
2. **EFA Execution** - Run EFA with specified number of factors

## Step-by-Step Usage

### 1. Data Preparation
```r
# Connect and load data
connect_sheets()
raw_data <- get_sheets_data()
df_clean <- clean_data(raw_data)
```

### 2. Determine Number of Factors
```r
# Run factor number determination
factor_det <- determine_factors(df_clean, 
                                n_iterations = 1000,
                                percentile = 99)

# Output example:
# Kaiser's criterion:    5 factors
# Parallel Analysis:     5 factors  
# MAP test:             5 factors
```

### 3. Execute EFA with Chosen Number
```r
# Based on recommendations, run EFA with 5 factors
efa_results <- analyze_efa(df_clean, n_factors = 5)
```

### 4. Explore Different Solutions (Optional)
```r
# Try different number of factors if needed
efa_4factors <- analyze_efa(df_clean, n_factors = 4)
efa_6factors <- analyze_efa(df_clean, n_factors = 6)
```

### 5. View Specific Results
```r
# View specific rotation results
show_efa(efa_results, method = "MINRES", gamma = 0)
show_efa(efa_results, method = "ULS", gamma = -0.5)
```

## Key Changes from Previous Version

| Before | After |
|--------|-------|
| `analyze_efa(df_clean)` with interactive input | `analyze_efa(df_clean, n_factors = 5)` |
| Combined determination and execution | Separated functions |
| User prompted during execution | User decides between steps |

## Function Reference

### determine_factors()
```r
determine_factors(data, 
                 n_iterations = 1000,  # PA iterations
                 percentile = 99,      # PA percentile
                 seed = NULL,          # Random seed
                 verbose = TRUE)       # Show details
```

### analyze_efa()
```r
analyze_efa(data,
           n_factors,                    # REQUIRED
           extraction = c("ULS", "MINRES"),
           gamma_values = c(0, -0.2, -0.5, -0.8),
           kaiser_normalize = TRUE,
           missing = "listwise",
           verbose = TRUE)
```

## Important Notes

- **n_factors is now required** for analyze_efa()
- No interactive prompts during execution
- Factor determination and EFA execution are independent
- Can run multiple EFA analyses with different factor numbers