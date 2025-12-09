# ===================================================
# Visualization Display (View Layer)
# Version: 1.2 - Heatmap with Q01 at top-left
# ===================================================

# Display header
visualization_display_header <- function() {
  cat("========================================\n")
  cat("Data Visualization\n")
  cat("========================================\n\n")
}

# Display response distribution histograms (grid)
# dist_data: list of data.frames from calc_response_distribution
# scale_min, scale_max: scale range
display_response_histograms <- function(dist_data, scale_min, scale_max) {
  
  items <- names(dist_data)
  n_items <- length(items)
  
  cat(sprintf("Displaying response distributions for %d items\n", n_items))
  
  # Define colors for each category
  category_colors <- c("1" = "#ef4444", "2" = "#f97316", 
                       "3" = "#22c55e", "4" = "#3b82f6")
  
  # Create individual plots
  plot_list <- list()
  
  for (i in seq_along(items)) {
    item <- items[i]
    df <- dist_data[[item]]
    df$category <- factor(df$category)
    
    p <- ggplot(df, aes(x = category, y = count, fill = category)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = category_colors, guide = "none") +
      labs(title = item, x = NULL, y = NULL) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 10, hjust = 0.5),
        axis.text = element_text(size = 8),
        plot.margin = margin(5, 5, 5, 5)
      )
    
    plot_list[[i]] <- p
  }
  
  # Arrange in grid
  n_cols <- min(6, n_items)
  n_rows <- ceiling(n_items / n_cols)
  
  grid_plot <- gridExtra::grid.arrange(
    grobs = plot_list,
    ncol = n_cols,
    top = "Response Distribution by Item"
  )
  
  # Print legend info
  cat("\nCategory colors: 1=Red, 2=Orange, 3=Green, 4=Blue\n")
  
  invisible(grid_plot)
}

# Display total score histogram with normal curve
# total_data: list from calc_total_score_distribution
display_total_score_histogram <- function(total_data) {
  
  scores <- total_data$scores
  mean_val <- total_data$mean
  sd_val <- total_data$sd
  n <- total_data$n_complete
  
  cat(sprintf("N = %d, Mean = %.2f, SD = %.2f\n", n, mean_val, sd_val))
  
  # Create data frame for plotting
  df <- data.frame(score = scores)
  
  # Determine bin width
  score_range <- max(scores) - min(scores)
  bin_width <- max(1, round(score_range / 15))
  
  # Create histogram with normal curve overlay
  p <- ggplot(df, aes(x = score)) +
    geom_histogram(aes(y = after_stat(count)), 
                   binwidth = bin_width,
                   fill = "#93c5fd", 
                   color = "#3b82f6",
                   alpha = 0.7) +
    stat_function(
      fun = function(x) {
        dnorm(x, mean = mean_val, sd = sd_val) * n * bin_width
      },
      color = "#ef4444",
      linewidth = 1.2
    ) +
    labs(
      title = "Total Score Distribution with Fitted Normal Curve",
      x = "Total Score",
      y = "Frequency"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 12, hjust = 0.5),
      axis.title = element_text(size = 10)
    )
  
  print(p)
  
  cat("\nBlue bars: Observed distribution\n")
  cat("Red line: Fitted normal distribution\n")
  
  invisible(p)
}

# Display correlation heatmap
# cor_matrix: correlation matrix from calc_correlation_matrix
display_correlation_heatmap <- function(cor_matrix) {
  
  n_items <- nrow(cor_matrix)
  cat(sprintf("Displaying %d x %d correlation matrix\n", n_items, n_items))
  
  # Convert matrix to long format
  cor_df <- as.data.frame(as.table(cor_matrix))
  names(cor_df) <- c("Var1", "Var2", "value")
  
  # Get item order (Q01, Q02, ...)
  item_order <- rownames(cor_matrix)
  
  # Create heatmap with high contrast colors
  # Y-axis reversed so Q01 is at top-left
  p <- ggplot(cor_df, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile(color = "white", linewidth = 0.5) +
    scale_fill_gradientn(
      colors = c("#08306b", "#2171b5", "#6baed6", "#c6dbef", 
                 "#ffffff",
                 "#fcbba1", "#fb6a4a", "#cb181d", "#67000d"),
      values = scales::rescale(c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)),
      limits = c(-1, 1),
      name = "r"
    ) +
    scale_x_discrete(limits = item_order) +
    scale_y_discrete(limits = rev(item_order)) +
    geom_text(aes(label = sprintf("%.2f", value)), size = 2, color = "black") +
    labs(
      title = "Item-Item Correlation Matrix",
      x = NULL,
      y = NULL
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 12, hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
      axis.text.y = element_text(size = 7),
      legend.position = "right",
      panel.grid = element_blank()
    ) +
    coord_fixed()
  
  print(p)
  
  cat("\nColor scale: Dark blue (r=-1) -> White (r=0) -> Dark red (r=1)\n")
  
  invisible(p)
}