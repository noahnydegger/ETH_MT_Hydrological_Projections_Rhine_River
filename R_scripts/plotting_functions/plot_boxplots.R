library(data.table)
library(zoo)
library(ggplot2)

source(here("R_scripts", "plotting_functions", "knmi_plot_metadata.R"))

# Compute annual or seasonal statistics
compute_annual_or_seasonal <- function(dt, value_col, group_cols, 
                                       statistic = "mean", months = 1:12, 
                                       seasonal = FALSE, monthly = FALSE) {
  dt <- dt[month(date) %in% months]
  dt[, YYYY := year(date)]
  dt[, MM := month(date)]
  
  # Determine grouping
  if (monthly) {
    group_vars <- c("MM", "member", group_cols)
  } else if (seasonal) {
    dt[, season := fifelse(MM %in% c(12,1,2), "DJF",
                           fifelse(MM %in% c(3,4,5), "MAM",
                                   fifelse(MM %in% c(6,7,8), "JJA", "SON")))]
    group_vars <- c("season", "YYYY", "member", group_cols)
  } else {
    group_vars <- c("member", group_cols)
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

compute_relative_mean_old <- function(dt_mean, value_col, comparison_col, comparison_ref, seasonal = FALSE, monthly = FALSE) {
  value_col_ref <- paste0(value_col, "_rel")
  
  if (seasonal) {
    if (!"season" %in% names(dt_mean)) {
      stop("Column 'season' not found in dt_mean. Did you set seasonal = TRUE in preprocessing?")
    }
    
    # Compute relative deviation per season using reference within group
    dt_mean[, (value_col_ref) := {
      ref_mean <- dt_mean[get(comparison_col) == comparison_ref & season == .BY[[1]], 
                          median(get(value_col), na.rm = TRUE)]
      100 * (get(value_col) - ref_mean) / ref_mean
    }, by = "season"]
    
  } else {
    # Global reference mean
    ref_mean <- dt_mean[get(comparison_col) == comparison_ref, median(get(value_col), na.rm = TRUE)]
    dt_mean[, (value_col_ref) := 100 * (get(value_col) - ref_mean) / ref_mean]
  }
  
  return(dt_mean)
}

compute_relative_mean <- function(dt_mean, value_col, comparison_col, comparison_ref, 
                                  seasonal = FALSE, monthly = FALSE) {
  value_col_rel <- paste0(value_col, "_rel")
  
  # Determine grouping column
  group_col <- if (seasonal) {
    if (!"season" %in% names(dt_mean)) stop("Missing 'season' column.")
    "season"
  } else if (monthly) {
    if (!"MM" %in% names(dt_mean)) stop("Missing 'MM' column.")
    "MM"
  } else {
    NULL
  }
  
  # Compute relative deviation
  if (is.null(group_col)) {
    ref_mean <- dt_mean[get(comparison_col) == comparison_ref, median(get(value_col), na.rm = TRUE)]
    dt_mean[, (value_col_rel) := 100 * (get(value_col) - ref_mean) / ref_mean]
  } else {
    dt_mean[, (value_col_rel) := {
      ref <- dt_mean[get(comparison_col) == comparison_ref & get(group_col) == .BY[[1]],
                     median(get(value_col), na.rm = TRUE)]
      100 * (get(value_col) - ref) / ref
    }, by = group_col]
  }
  
  return(dt_mean)
}

plot_annual_boxplots <- function(dt_annual, plot_dir, bsn, color_col, color_col_levels, comparison_ref, value_col, group_cols, stat, info_text = "", rel = FALSE) {
  
  value_name <- plot_info$column_info$names[[value_col]]
  value_unit <- plot_info$column_info$units[[value_col]]
  
  stat_col <- paste0(value_col, "_", stat)
  
  if (rel) {
    stat_col <- paste0(stat_col, "_rel")
    info_text <- paste0(info_text, "_rel")
    value_unit <- "[%]"
  }
  
  dt_annual[, (color_col) := factor(get(color_col), levels = color_col_levels)]
  
  # Plot annual boxplots
  p <- ggplot(dt_annual, aes(x = .data[[color_col]], y = .data[[stat_col]], fill = .data[[color_col]])) +
    geom_boxplot(position = position_dodge(width = 0.8), fatten = 2, size = 0.8) +  
    labs(
      title = paste("Annual", stat, value_name, bsn, info_text),
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
    #ylim(0.0, 0.7) +
    scale_x_discrete(labels = NULL)  
  
  # Save the plot
  save_dir <- file.path(plot_dir, "annual_boxplots", value_col)
  filename <- paste0("annual_", bsn, "_", stat_col, info_text, ".pdf")
  save_plot(p, save_dir, filename, width = 8, height = 6)
  
}

plot_seasonal_boxplots <- function(dt_seasonal, plot_dir, bsn, color_col, color_col_levels, value_col, group_cols, stat, info_text = "", rel = FALSE) {
  
  value_name <- plot_info$column_info$names[[value_col]]
  value_unit <- plot_info$column_info$units[[value_col]]
  
  stat_col <- paste0(value_col, "_", stat)
  
  if (rel) {
    stat_col <- paste0(stat_col, "_rel")
    info_text <- paste0(info_text, "_rel")
    value_unit <- "[%]"
  }
  
  # Ensure color column has defined factor levels
  dt_seasonal[, (color_col) := factor(get(color_col), levels = color_col_levels)]
  
  # Convert group_cols to combined label for facets if needed
  dt_seasonal[, group_combined := do.call(paste, c(.SD, sep = "_")), .SDcols = group_cols]
  
  # Define season as factor with desired order
  dt_seasonal[, season := factor(season, levels = c("DJF", "MAM", "JJA", "SON"))]
  
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
      title = paste("Seasonal", stat, value_name, bsn, info_text),
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

# Function to plot monthly and yearly boxplots from daily data.table
plot_month_year_boxplots <- function(dt_month, dt_year, plot_dir, bsn, color_col, color_col_levels, comparison_ref, value_col, group_cols, stat, info_text = "", rel = FALSE) {
  
  value_name <- plot_info$column_info$names[[value_col]]
  value_unit <- plot_info$column_info$units[[value_col]]
  
  stat_col <- paste0(value_col, "_", stat)
  
  if (rel) {
    stat_col <- paste0(stat_col, "_rel")
    info_text <- paste0(info_text, "_rel")
    value_unit <- "[%]"
    
    # Exclude comparison_ref from plotting
    dt_month <- dt_month[get(color_col) != comparison_ref]
    dt_year <- dt_year[get(color_col) != comparison_ref]
  }
  
  # Ensure color_col is a factor with the right order
  dt_month[, (color_col) := factor(get(color_col), levels = color_col_levels)]
  dt_year[, (color_col) := factor(get(color_col), levels = color_col_levels)]
  
  dt_month[, MM := factor(month.abb[MM], levels = month.abb)]  # e.g., Jan, Feb
  
  # Plotting
  p <- ggplot(dt_month, aes(x = MM, y = .data[[stat_col]], fill = .data[[color_col]])) +
    geom_boxplot(
      position = position_dodge(width = 0.8),
      width = 0.6,  # narrower boxes
      fatten = 2,
      size = 0.8
    ) +
    labs(
      title = paste(stat, value_name, bsn, info_text),
      x = "Month",
      y = paste(value_name, value_unit),
      fill = "Dataset"
    ) +
    custom_theme() +
    theme(
      axis.title.x = element_blank()  # Remove the x-axis title
    ) +
    scale_x_discrete() +
    scale_fill_manual(
      values = plot_info[[color_col]]$colors,
      labels = plot_info[[color_col]]$labels
    )
  # Add yearly boxplot
  p <- p +
    geom_boxplot(
      data = dt_year,
      aes(x = "Year", y = .data[[stat_col]], fill = .data[[color_col]]),
      position = position_dodge(width = 0.8),
      width = 0.6,  # narrower boxes
      fatten = 2,
      size = 0.8
    )
  
  # Save the plot
  save_dir <- file.path(plot_dir, "monthly_boxplots", value_col)
  filename <- paste0("monthly_", bsn, "_", stat_col, info_text, ".pdf")
  save_plot(p, save_dir, filename, width = 18, height = 6)
}
