library(ggplot2)
library(data.table)

source(here("R_scripts", "plotting_functions", "knmi_plot_metadata.R"))

plot_pdf_with_percentiles <- function(dt, plot_dir, bsn, color_col, color_col_levels, line_col, line_col_levels, group_col, value_col, group_cols, info_text) {
  
  value_name <- plot_info$column_info$names[[value_col]]
  value_unit <- plot_info$column_info$units[[value_col]]
  
  # Ensure color column has defined factor levels
  dt[, (color_col) := factor(get(color_col), levels = color_col_levels)]
  dt[, (line_col) := factor(get(line_col), levels = line_col_levels)]
  
  summary_dt <- dt[, .(
    q10 = quantile(get(value_col), 0.10, na.rm = TRUE),
    median = quantile(get(value_col), 0.50, na.rm = TRUE),
    mean = mean(get(value_col), na.rm = TRUE),
    q90 = quantile(get(value_col), 0.90, na.rm = TRUE)
  ), by = group_cols]
  
  # 1. Rename color_col to 'group' for convenience
  summary_dt[, group := get(group_col)]
  summary_dt[, line := get(line_col)]
  summary_dt[, color := get(color_col)]
  
  # 2. Melt the percentiles + median
  summary_melted <- melt(
    summary_dt[, .(group, line, color, q10, median, q90)],
    id.vars = c("group", "line", "color"),
    variable.name = "stat",
    value.name = "value"
  )
  
  summary_quants <- summary_melted[stat %in% c("q10", "q90")]
  summary_median <- summary_melted[stat == "median"]
  
  p <- ggplot(dt, aes(x = .data[[value_col]], 
                      color = .data[[color_col]], 
                      linetype = .data[[line_col]], 
                      group = .data[[group_col]])) +
    
    stat_density(geom = "line", position = "identity", linewidth = 0.3) +
    
    # Add ticks
    geom_rug(
      data = summary_quants,
      aes(x = value, y = 0, color = color, linetype = line),
      inherit.aes = FALSE,
      sides = "b", size = 0.6, length = unit(20, "pt")
    ) +
    
    geom_rug(
      data = summary_median,
      aes(x = value, y = 0, color = color, linetype = line),
      inherit.aes = FALSE,
      sides = "b", size = 1.0, length = unit(20, "pt")
    ) +
    
    # Add mean
    geom_point(
      data = summary_dt,
      aes(
        x = mean,
        y = 0.0001,
        color = color,
        shape = line
      ),
      inherit.aes = FALSE,
      size = 2
    ) +
  
    labs(title = NULL, # paste("PDF", value_name, bsn, info_text),
         x = paste(value_name, value_unit),
         y = "empirical density",
         color = "Scenario",
         linetype = "Variant",
         shape = NULL
    ) +
    
    custom_theme() +
    theme(
      legend.position = "top",
      legend.direction = "horizontal",
      legend.box = "vertical",
      legend.spacing.y = unit(0.2, "lines"),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    ) +
    scale_color_manual(
      values = plot_info[[color_col]]$colors,
      labels = plot_info[[color_col]]$labels,
      guide = guide_legend(override.aes = list(linetype = 1, shape = NA))  # Only a line in the legend
    ) +
    scale_linetype_manual(
      values = plot_info[[line_col]]$linetypes,
      labels = plot_info[[line_col]]$labels
    ) +
    scale_shape_manual(
      values = plot_info[[line_col]]$shapes,
      labels = plot_info[[line_col]]$labels
    ) +
    guides(
      color = guide_legend(order = 1, title.position = "left", nrow = 1),
      linetype = guide_legend(order = 2, title.position = "left", nrow = 1),
      shape = guide_legend(order = 3, title.position = "left", nrow = 1)
    ) +
    scale_x_continuous(
      #limits = c(min(dt[[value_col]]), max(dt[[value_col]]))  # Add small space on both ends
      limits = c(0, 3500)
    )
  
  # Save the plot
  save_dir <- file.path(plot_dir, "Distribution", value_col)
  filename <- paste0("pdf_", bsn, "_", value_col, info_text, ".pdf")
  save_plot(p, save_dir, filename, width = 8, height = 6)
}

# PDF Plot Function
plot_pdf <- function(dt, plot_dir, bsn, color_col, color_col_levels, value_col, group_cols, info_text) {
  
  value_name <- plot_info$column_info$names[[value_col]]
  value_unit <- plot_info$column_info$units[[value_col]]
  
  # Ensure color column has defined factor levels
  dt[, (color_col) := factor(get(color_col), levels = color_col_levels)]
  
  p <- ggplot(dt, aes(x = .data[[value_col]], color = .data[[color_col]])) +
    stat_density(geom = "line", linewidth = 0.2) +
    labs(title = NULL, # paste("PDF", value_name, bsn, info_text),
         x = paste(value_name, value_unit),
         y = "Density",
         color = "Dataset"
         ) +
    custom_theme() +
    theme(
      legend.position = "top"
    ) +
    scale_color_manual(
      values = plot_info[[color_col]]$colors,
      labels = plot_info[[color_col]]$labels,
      guide = guide_legend(override.aes = list(linetype = 1, shape = NA))  # Only a line in the legend
    ) +
    scale_x_continuous(
      #limits = c(min(dt[[value_col]]), max(dt[[value_col]]))  # Add small space on both ends
      limits = c(5000, 8000)
    )
  
  # Save the plot
  save_dir <- file.path(plot_dir, "Distribution", value_col)
  filename <- paste0("pdf_", bsn, "_", value_col, info_text, ".pdf")
  save_plot(p, save_dir, filename, width = 8, height = 6)
}

# CDF Plot Function
plot_cdf <- function(dt, plot_dir, bsn, color_col, color_col_levels, value_col, group_cols, info_text) {
  
  value_name <- plot_info$column_info$names[[value_col]]
  value_unit <- plot_info$column_info$units[[value_col]]
  
  # Ensure color column has defined factor levels
  dt[, (color_col) := factor(get(color_col), levels = color_col_levels)]
  
  p <- ggplot(dt, aes(x = .data[[value_col]], color = .data[[color_col]])) +
    stat_ecdf(geom = "step", linewidth = 0.2) +  # Step plot for CDF
    #stat_ecdf(aes(x = .data[[value_col_month]]), geom = "step", linetype = "dashed") +  # CDF for value_col_month (dashed)
    #stat_ecdf(aes(x = .data[[value_col_overall]]), geom = "step", linetype = "dotted") +  # CDF for value_col_overall (dotted)
    labs(title = NULL, # paste("CDF", value_name, bsn, info_text),
         x = paste(value_name, value_unit),
         y = "Cumulative Probability",
         color = "Dataset"
    ) +
    custom_theme() +
    theme(
      legend.position = "top"
    ) +
    scale_color_manual(
      values = plot_info[[color_col]]$colors,
      labels = plot_info[[color_col]]$labels
    ) +
    scale_x_continuous(
      limits = c(min(dt[[value_col]]), max(dt[[value_col]]))  # Add small space on both ends
    )
  
  # Save the plot
  save_dir <- file.path(plot_dir, "Distribution", value_col)
  filename <- paste0("cdf_", bsn, "_", value_col, info_text, ".pdf")
  save_plot(p, save_dir, filename, width = 8, height = 6)
}
