library(ggplot2)
library(data.table)

source(here("R_scripts", "plotting_functions", "knmi_plot_metadata.R"))

# PDF Plot Function
plot_pdf <- function(dt, bsn, color_col, value_col, group_cols) {
  
  value_name <- plot_info$column_info$names[[value_col]]
  value_unit <- plot_info$column_info$units[[value_col]]
  
  p <- ggplot(dt, aes(x = .data[[value_col]], color = .data[[color_col]])) +
    stat_density(geom = "line") +
    labs(title = paste("PDF", value_name, bsn),
         x = paste(value_name, value_unit),
         y = "Density",
         color = "Dataset"
         ) +
    custom_theme() +
    scale_color_manual(
      values = plot_info[[color_col]]$colors,
      labels = plot_info[[color_col]]$labels,
      guide = guide_legend(override.aes = list(linetype = 1, shape = NA))  # Only a line in the legend
    ) +
    scale_x_continuous(
      limits = c(min(dt[[value_col]]), max(dt[[value_col]]))  # Add small space on both ends
    )
  
  # Save the plot
  save_dir <- file.path(here::here(), "Plots", "Reference_Period_Analysis", "bias_correction", "Distribution", value_col)
  filename <- paste0("pdf_", bsn, "_", value_col, ".pdf")
  save_plot(p, save_dir, filename, width = 8, height = 6)
}

# CDF Plot Function
plot_cdf <- function(dt, bsn, info_col, color_col, value_col, group_cols) {
  
  value_name <- plot_info$column_info$names[[info_col]]
  value_unit <- plot_info$column_info$units[[info_col]]
  
  p <- ggplot(dt, aes(x = .data[[value_col]], color = .data[[color_col]])) +
    stat_ecdf(geom = "step") +  # Step plot for CDF
    #stat_ecdf(aes(x = .data[[value_col_month]]), geom = "step", linetype = "dashed") +  # CDF for value_col_month (dashed)
    #stat_ecdf(aes(x = .data[[value_col_overall]]), geom = "step", linetype = "dotted") +  # CDF for value_col_overall (dotted)
    labs(title = paste("CDF", value_name, bsn),
         x = paste(value_name, value_unit),
         y = "Cumulative Probability",
         color = "Dataset"
    ) +
    custom_theme() +
    scale_color_manual(
      values = plot_info[[color_col]]$colors,
      labels = plot_info[[color_col]]$labels
    ) +
    scale_x_continuous(
      limits = c(min(dt[[value_col]]), max(dt[[value_col]]))  # Add small space on both ends
    )
  
  # Save the plot
  save_dir <- file.path(here::here(), "Plots", "Reference_Period_Analysis", "bias_correction", "Distribution", value_col)
  filename <- paste0("cdf_", bsn, "_", value_col, ".pdf")
  save_plot(p, save_dir, filename, width = 8, height = 6)
}
