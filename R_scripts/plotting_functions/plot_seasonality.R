library(data.table)
library(zoo)
library(hydroGOF)


source(here("R_scripts", "data_processing_functions", "gof_metrics.R"))
source(here("R_scripts", "plotting_functions", "knmi_plot_metadata.R"))

compute_rolling_stats <- function(dt, group_cols, value_cols, stat = "mean", width = 30) {
  # Create a copy of the data to avoid modifying the original
  dt <- copy(dt)
  
  group_cols <- c(group_cols, "member")
  
  # Compute rolling statistic for each column in value_cols
  dt[, paste0("rm_", value_cols) := lapply(.SD, function(x) zoo::rollapply(
    x, 
    width = width, 
    FUN = match.fun(stat),  # Flexible statistics (mean, min, max)
    fill = NA, 
    align = "center", 
    partial = TRUE,
    na.rm = TRUE
  )), by = group_cols, .SDcols = value_cols]
  
  # Add DayOfYear column and filter out Day 366
  dt <- dt[as.numeric(format(date, "%j")) != 366]
  dt[, DayOfYear := as.numeric(format(date, "%j"))]
  
  return(dt)
}

# Function to compute mean of selected columns grouped by specified columns
compute_seasonality <- function(dt, group_cols, value_cols, stat = "mean", q_bot = 0.25, q_top = 0.75) {
  # Ensure required columns are present
  if (!all(c(group_cols, value_cols) %in% names(dt))) {
    stop("Some specified columns are not in the data.table")
  }
  
  # Compute specified statistics
  dt[, c(
    setNames(lapply(.SD, match.fun(stat), na.rm = TRUE), paste0(stat, "_", value_cols)),
    setNames(lapply(.SD, quantile, probs = q_bot, na.rm = TRUE), paste0("q_bot_", value_cols)),
    setNames(lapply(.SD, quantile, probs = q_top, na.rm = TRUE), paste0("q_top_", value_cols))
  ), 
  by = group_cols, 
  .SDcols = value_cols]
}

# Function to plot the statistics
plot_seasonality_ts <- function(dt, plot_dir, bsn, info_col, color_col, value_col, stat, info_text, q_bot = 0.10, q_top = 0.90, show_ensemble = FALSE, show_range = FALSE, gof_pairs = NULL) {
  
  value_name <- plot_info$column_info$names[[info_col]]
  value_unit <- plot_info$column_info$units[[info_col]]
  
  stat_col <- paste0(stat, "_", value_col)
  q_bot_col <- paste0("q_bot_", value_col)
  q_top_col <- paste0("q_top_", value_col)
  
  if (!is.null(gof_pairs)) {

    obs_data <- dt[scen_var_hor == gof_pairs[1] & basin == bsn, .(get(stat_col))]
    sim_data <- dt[scen_var_hor == gof_pairs[2] & basin == bsn, .(get(stat_col))]
    obs_name <- plot_info[[color_col]]$labels[gof_pairs[1]]
    sim_name <- plot_info[[color_col]]$labels[gof_pairs[2]]
    # Ensure both datasets have equal length
    if (nrow(obs_data) == nrow(sim_data) && nrow(obs_data) > 0) {
      subtitle_text <- paste0(
        sim_name, " vs ", obs_name, ", NSE = ", round(NSE(sim_data, obs_data), 2), " | ",
        "KGE", " = ", round(KGE(sim_data, obs_data), 2), " | ",
        "ME", " = ", round(me(sim_data, obs_data), 2)
      )
    } else {
      subtitle_text <- NULL
    }
  } else {
    subtitle_text <- NULL
  }

  # Add month lines and labels
  month_lines <- seq(as.Date("2023-01-01"), as.Date("2023-12-01"), by = "1 month")
  month_labels <- seq(as.Date("2023-01-15"), as.Date("2023-12-15"), by = "1 month")

  # Plot
  p <- ggplot(dt, aes(x = as.Date(DayOfYear - 1, origin = "2023-01-01"),
                                     group = .data[[color_col]], color = .data[[color_col]])) +
    geom_vline(xintercept = as.numeric(month_lines), color = "gray90")

  if (show_range) {
    p <- p + geom_ribbon(aes(ymin = .data[[q_bot_col]], ymax = .data[[q_top_col]], fill = .data[[color_col]]), alpha = 0.4)
  }

  if (show_ensemble) {
    p <- p + geom_line(aes(y = .data[[stat_col]], color = .data[[color_col]]), linewidth = 0.7)
  }

  p <- p + geom_line(aes(y = .data[[stat_col]]), linewidth = 2) +
    scale_x_date(date_labels = "%b", breaks = month_labels, expand = c(0, 0)) +
    labs(
      title = paste("30-day Moving Average", value_name, bsn, info_text),
      subtitle = subtitle_text,
      x = "Month",
      y = paste(value_name, value_unit),
      color = "Dataset",
      fill = "Dataset"
    ) +
    custom_theme() +
    theme(
      axis.title.x = element_blank()  # Remove the x-axis title
    ) +
    scale_color_manual(
      values = plot_info[[color_col]]$colors,
      labels = plot_info[[color_col]]$labels
    ) +
    ylim(0.2, 0.7) +
    (if (show_range) scale_fill_manual(
      values = plot_info[[color_col]]$colors,
      labels = plot_info[[color_col]]$labels
    ) else NULL)
    

  # Save the plot
  save_dir <- file.path(plot_dir, "seasonality", info_col)
  filename <- paste0("seasonality_ts_", bsn, "_", stat, "_", value_col, ifelse(show_ensemble,"ens", ""), ifelse(show_range, paste0("_Q", q_bot*100, "_Q", q_top*100), ""), info_text, ".pdf")
  save_plot(p, save_dir, filename, width = 18, height = 6)
  
}

plot_seasonality_bars <- function(dt, plot_dir, bsn, info_col, color_col, value_col, stat, info_text) {
  
  value_name <- plot_info$column_info$names[[info_col]]
  value_unit <- plot_info$column_info$units[[info_col]]
  
  stat_col <- paste0(stat, "_", value_col)
  
  # Extract month from DayOfYear (assumes 2023 calendar for mapping)
  dt[, month := format(as.Date(DayOfYear - 1, origin = "2023-01-01"), "%b")]
  dt[, month := factor(month, levels = format(seq(as.Date("2023-01-01"), by = "1 month", length.out = 12), "%b"))]
  
  # Plot
  p <- ggplot(dt, aes(
    x = month,
    y = .data[[stat_col]],
    fill = .data[[color_col]],
    group = .data[[color_col]]
  )) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    labs(
      title = paste("Monthly", stat, value_name, "in", bsn, info_text),
      x = "Month",
      y = paste(value_name, value_unit),
      fill = "Dataset"
    ) +
    custom_theme() +
    scale_fill_manual(
      values = plot_info[[color_col]]$colors,
      labels = plot_info[[color_col]]$labels
    ) +
    theme(
      axis.title.x = element_blank()
    )
  
  # Save the plot
  save_dir <- file.path(plot_dir, "seasonality", info_col)
  filename <- paste0("seasonality_bar_", bsn, "_", stat, "_", value_col, info_text, ".pdf")
  save_plot(p, save_dir, filename, width = 14, height = 6)
}





