library(data.table)
library(zoo)
library(ggplot2)

source(here("R_scripts", "plotting_functions", "knmi_plot_metadata.R"))

# Compute annual or seasonal statistics
compute_annual_or_seasonal <- function(dt, value_col, group_cols, statistic = "mean", months = 1:12, seasonal = FALSE) {
  dt <- dt[month(date) %in% months]
  dt[, YYYY := year(date)]
  dt[, MM := month(date)]
  
  if (seasonal) {
    # Define seasons manually
    dt[, season := fifelse(MM %in% c(11,12,1), "NDJ",
                           fifelse(MM %in% c(2,3,4), "FMA",
                                   fifelse(MM %in% c(5,6,7), "MJJ", "ASO")))]
    group_vars <- c("season", "YYYY", "member", group_cols)
  } else {
    group_vars <- c("YYYY", "member", group_cols)
  }
  
  # Statistic computation
  value_name <- paste0(value_col, "_", statistic)
  
  if (statistic == "mean") {
    dt_out <<- dt[, .(value = mean(get(value_col), na.rm = TRUE)), by = group_vars]
  } else if (statistic == "min") {
    dt_out <- dt[, .(value = min(get(value_col), na.rm = TRUE)), by = group_vars]
  } else if (statistic == "max") {
    dt_out <- dt[, .(value = max(get(value_col), na.rm = TRUE)), by = group_vars]
  } else if (statistic == "7day_low") {
    dt[, rollmean7 := rollapply(get(value_col), width = 7, FUN = mean, fill = NA, align = "right"), by = group_vars]
    dt_out <- dt[, .(value = min(rollmean7, na.rm = TRUE)), by = group_vars]
  } else {
    stop("Invalid statistic: ", statistic)
  }
  
  # Rename the computed column
  setnames(dt_out, "value", value_name)
  
  return(dt_out)
}

plot_annual_boxplots <- function(dt_annual, plot_dir, bsn, color_col, color_col_levels, value_col, group_cols, stat, info_text = "") {
  
  value_name <- plot_info$column_info$names[[value_col]]
  value_unit <- plot_info$column_info$units[[value_col]]
  
  stat_col <- paste0(value_col, "_", stat)
  
  dt_annual[, (color_col) := factor(get(color_col), levels = color_col_levels)]
  
  # Plot annual boxplots
  p <- ggplot(dt_annual, aes(x = .data[[color_col]], y = .data[[stat_col]], fill = .data[[color_col]])) +
    geom_boxplot(position = position_dodge(width = 0.8), fatten = 2, size = 0.8) +  
    labs(
      title = paste("Annual", stat, value_name, bsn),
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
  save_dir <- file.path(plot_dir, "annual_boxplots", value_col)
  filename <- paste0("annual_", bsn, "_", stat_col, info_text, ".pdf")
  save_plot(p, save_dir, filename, width = 8, height = 6)
  
}

plot_seasonal_boxplots <- function(dt_seasonal, plot_dir, bsn, color_col, color_col_levels, value_col, group_cols, stat, info_text = "") {
  
  value_name <- plot_info$column_info$names[[value_col]]
  value_unit <- plot_info$column_info$units[[value_col]]
  
  stat_col <- paste0(value_col, "_", stat)
  
  # Ensure color column has defined factor levels
  dt_seasonal[, (color_col) := factor(get(color_col), levels = color_col_levels)]
  
  # Convert group_cols to combined label for facets if needed
  dt_seasonal[, group_combined := do.call(paste, c(.SD, sep = "_")), .SDcols = group_cols]
  
  # Define season as factor with desired order
  dt_seasonal[, season := factor(season, levels = c("NDJ", "FMA", "MJJ", "ASO"))]
  
  # Create a combined season-dataset label for x-axis
  dt_seasonal[, season_dataset := interaction(season, get(color_col), lex.order = TRUE)]
  
  num_groups <- length(color_col_levels)
  
  # Plot grouped seasonal boxplots
  p <- ggplot(dt_seasonal, aes(x = season, y = .data[[stat_col]], fill = .data[[color_col]])) +
    geom_boxplot(
      position = position_dodge(width = 0.8),
      width = 0.6,  # narrower boxes
      fatten = 2,
      size = 0.8
    ) +
    labs(
      title = paste("Seasonal", stat, value_name, bsn),
      x = "Season",
      y = paste(value_name, value_unit),
      fill = "Dataset"
    ) +
    custom_theme() +
    scale_fill_manual(
      values = plot_info[[color_col]]$colors,
      labels = plot_info[[color_col]]$labels
    )
  
  # Save the plot
  save_dir <- file.path(plot_dir, "seasonal_boxplots", value_col)
  filename <- paste0("seasonal_", bsn, "_", stat_col, info_text, ".pdf")
  save_plot(p, save_dir, filename, width = 10, height = 6)
}