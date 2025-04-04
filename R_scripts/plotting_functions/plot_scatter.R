library(ggplot2)
library(data.table)

source(here("R_scripts", "plotting_functions", "knmi_plot_metadata.R"))

plot_scatter_xy <- function(dt, bsn, color_col, value_col_x, value_col_y, group) {
  
  x_name <- plot_info$column_info$names[[value_col_x]]
  x_unit <- plot_info$column_info$units[[value_col_x]]
  y_name <- plot_info$column_info$names[[value_col_y]]
  y_unit <- plot_info$column_info$units[[value_col_y]]
  
  p <- ggplot(dt, aes(x = .data[[value_col_x]], y = .data[[value_col_y]], color = .data[[color_col]])) +
    geom_point(alpha = 0.7, size = 2) +
    labs(
      title = paste(plot_info[[color_col]]$labels[group]),
      x = paste(x_name, x_unit),
      y = paste(y_name, y_unit),
      color = "Dataset"
    ) +
    custom_theme() +
    scale_color_manual(
      values = plot_info[[color_col]]$colors,
      labels = plot_info[[color_col]]$labels
    )
  
  # Save the plot
  save_dir <- file.path(here::here(), "Plots", "Reference_Period_Analysis", "bias_correction", "Scatter")
  filename <- paste0("scatter_xy_", bsn, "_", value_col_x, "_vs_", value_col_y, "_", group, ".pdf")
  save_plot(p, save_dir, filename, width = 8, height = 6)
}

plot_scatter_group <- function(dt, bsn, color_col, value_col, group_col, groups) {
  stopifnot(length(groups) == 2)
  
  # Extract values for each group
  x_vals <- dt[get(group_col) == groups[1], get(value_col)]
  y_vals <- dt[get(group_col) == groups[2], get(value_col)]
  
  # Ensure equal length by sampling the shorter one
  n <- min(length(x_vals), length(y_vals))
  x_vals <- sample(x_vals, n)
  y_vals <- sample(y_vals, n)
  
  # Create data.table for plotting
  scatter_dt <- data.table(x = x_vals, y = y_vals)
  
  # Axis labels
  value_name <- plot_info$column_info$names[[value_col]]
  value_unit <- plot_info$column_info$units[[value_col]]
  
  # Plot
  p <- ggplot(scatter_dt, aes(x = x, y = y)) +
    geom_point(alpha = 0.7, size = 2, color = "steelblue") +
    labs(
      title = paste(value_name, value_unit),
      x = paste(plot_info[[color_col]]$labels[groups[1]]),
      y = paste(plot_info[[color_col]]$labels[groups[2]])
    ) +
    custom_theme()
  
  # Save the plot
  save_dir <- file.path(here::here(), "Plots", "Reference_Period_Analysis", "bias_correction", "Scatter")
  filename <- paste0("scatter_groups_", bsn, "_", groups[1], "_vs_", groups[2], "_", value_col, ".pdf")
  save_plot(p, save_dir, filename, width = 8, height = 6)
}