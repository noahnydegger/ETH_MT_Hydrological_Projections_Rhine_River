library(ggplot2)

source(here("R_scripts", "plotting_functions", "knmi_plot_metadata.R"))

plot_annual_boxplots <- function(dt, plot_dir, bsn, color_col, value_col, group_cols, info_text = "") {
  
  value_name <- plot_info$column_info$names[[value_col]]
  value_unit <- plot_info$column_info$units[[value_col]]
  
  # Ensure grouping column is a factor with proper order
  #dt[[group_col]] <- factor(dt[[group_col]], levels = unique(dt[[group_col]]))
  
  # Extract the year from the date column
  dt[, YYYY := year(date)]
  
  # Compute annual means by year, member and group_cols
  dt_annual <<- dt[, .(annual_mean = mean(get(value_col), na.rm = TRUE)), 
                  by = c("YYYY", "member", group_cols)]
  
  # Convert group_cols to a single grouping variable for x-axis if needed
  dt_annual[, group_combined := do.call(paste, c(.SD, sep = "_")), .SDcols = group_cols]
  
  dt_annual[, group_combined := factor(group_combined, levels = c("observation", "hindcast", "sund_bc", "no_sund_bc"))]
  
  # Plot annual boxplots
  p <- ggplot(dt_annual, aes(x = .data[[color_col]], y = annual_mean, fill = .data[[color_col]])) +
    geom_boxplot(position = position_dodge(width = 0.8), fatten = 2, size = 0.8) +  
    labs(
      title = paste("Annual mean", value_name, bsn),
      x = "Dataset",
      y = paste(value_name, value_unit),
      fill = "Dataset"
    )  +
    custom_theme() +
    theme(
      axis.title.x = element_blank()  # Remove the x-axis title
    ) +
    scale_fill_manual(
      values = plot_info[[color_col]]$colors,
      labels = plot_info[[color_col]]$labels
    ) +
    #ylim(600, 1600) +
    scale_x_discrete(labels = NULL)  
  
  # Save the plot
  save_dir <- file.path(plot_dir, "annual_means", value_col)
  filename <- paste0("annual_mean_", bsn, "_", value_col, info_text, ".pdf")
  save_plot(p, save_dir, filename, width = 8, height = 6)
  
}