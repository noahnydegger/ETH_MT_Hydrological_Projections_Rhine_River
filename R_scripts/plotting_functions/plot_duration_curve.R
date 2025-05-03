library(ggplot2)
library(data.table)

source(here("R_scripts", "plotting_functions", "knmi_plot_metadata.R"))

plot_fdc <- function(dt, plot_dir, bsn, color_col, value_col, group_cols) {
  
  value_name <- plot_info$column_info$names[[value_col]]
  value_unit <- plot_info$column_info$units[[value_col]]
  
  # Calculate exceedance probabilities
  dt[, exceedance := rank(-get(value_col)) / .N, by = group_cols]
  
  # Plotting
  p <- ggplot(dt, aes(x = exceedance, y = .data[[value_col]], color = .data[[color_col]])) +
    geom_line() +
    labs(title = paste("Duration Curve", value_name, bsn),
         x = "Exceedance Probability",
         y = paste(value_name, value_unit),
         color = "Dataset"
    ) +
    custom_theme() +
    scale_color_manual(
      values = plot_info[[color_col]]$colors,
      labels = plot_info[[color_col]]$labels
    ) +
    scale_x_continuous(
      breaks = seq(0, 1, 0.1)
    )
  
  # Save the plot
  save_dir <- file.path(plot_dir, "Duration_Curve", value_col)
  filename <- paste0("fdc_", bsn, "_", value_col, ".pdf")
  save_plot(p, save_dir, filename, width = 12, height = 6)
}