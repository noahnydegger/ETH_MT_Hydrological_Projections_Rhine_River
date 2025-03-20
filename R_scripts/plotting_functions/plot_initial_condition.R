
source(here("R_scripts", "plotting_functions", "knmi_plot_metadata.R"))

plot_one_day_values <- function(dt, bsn, color_col, value_col, group_cols, date_col, target_day) {
  
  value_name <- plot_info$column_info$names[[value_col]]
  value_unit <- plot_info$column_info$units[[value_col]]
  
  # Filter data for the target day
  dt_filtered <- dt[get(date_col) == target_day]
  
  # Check if there's any data to plot
  if (nrow(dt_filtered) == 0) {
    warning("No data available for the specified day.")
    return(NULL)
  }
  
  # Create a row number column to stack multiple values per scenario
  dt_filtered[, row_num := seq_len(.N), by = color_col]
  
  # Plotting
  p <- ggplot(dt_filtered, aes(x = .data[[color_col]], 
                               y = .data[[value_col]],
                               group = interaction(.data[[color_col]], row_num))) +
    #geom_hline(aes(yintercept = .data[[value_col]], color = .data[[color_col]]), linewidth = 1) +
    geom_point(aes(y = .data[[value_col]], color = .data[[color_col]]), size = 2) +
    labs(
      title = paste(value_name, "on", target_day, bsn),
      x = "Dataset",
      y = paste(value_name, value_unit),
      color = "Dataset"
    ) +
    custom_theme() +
    theme(
      axis.title.x = element_blank()  # Remove the x-axis title
    ) +
    scale_color_manual(
      values = plot_info[[color_col]]$colors,
      labels = plot_info[[color_col]]$labels
    ) +
    scale_x_discrete(
      labels = plot_info[[color_col]]$labels  # Map color_col values to labels on x-axis
    )
  
  # Save the plot
  save_dir <- file.path(here::here(), "Plots", "Reference_Period_Analysis", "Initial_Condition", value_col)
  filename <- paste0("ic_", bsn, "_", value_col, ".pdf")
  save_plot(p, save_dir, filename, width = 12, height = 6)
}

