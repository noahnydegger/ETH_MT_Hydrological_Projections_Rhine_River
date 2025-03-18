library(data.table)
library(zoo)
library(hydroGOF)


source(here("R_scripts", "data_processing_functions", "gof_metrics.R"))
source(here("R_scripts", "plotting_functions", "plot_metadata_knmi.R"))

compute_rolling_stats <- function(dt, group_cols, value_cols, stat = "mean", width = 30) {
  # Create a copy of the data to avoid modifying the original
  dt <- copy(dt)
  
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
plot_seasonality_ts <- function(dt, basin, column, y_label, y_unit, q_bot = 0.10, q_top = 0.90, show_ensemble = FALSE, show_range = FALSE, gof_pairs = NULL, stat) {
  stat_col <- paste0(stat, "_", column)
  q_bot_col <- paste0("q_bot_", column)
  q_top_col <- paste0("q_top_", column)
  
  # Combine 'scenario' and 'variant' into a new column for the color
  dt[, scenario_variant := paste(scenario, variant, sep = "_")]
  
  if (!is.null(gof_pairs)) {

    obs_data <- dt[scenario_variant == gof_pairs[1] & basin == basin, .(get(stat_col))]
    sim_data <- dt[scenario_variant == gof_pairs[2] & basin == basin, .(get(stat_col))]
    obs_name <- plot_info$scenario$labels[gof_pairs[1]]
    sim_name <- plot_info$scenario$labels[gof_pairs[2]]
    subtitle_text <- paste0(
      sim_name, " vs ", obs_name, ", NSE = ", round(NSE(sim_data, obs_data), 2), " | ",
      "KGE", " = ", round(KGE(sim_data, obs_data), 2), " | ",
      "ME", " = ", round(me(sim_data, obs_data), 2)
    )
  } else {
    subtitle_text <- NULL
  }

  # Add month lines and labels
  month_lines <- seq(as.Date("2023-01-01"), as.Date("2023-12-01"), by = "1 month")
  month_labels <- seq(as.Date("2023-01-15"), as.Date("2023-12-15"), by = "1 month")

  # Plot
  p <- ggplot(dt, aes(x = as.Date(DayOfYear - 1, origin = "2023-01-01"),
                                     group = scenario_variant, color = scenario_variant)) +
    geom_vline(xintercept = as.numeric(month_lines), color = "gray90")

  if (show_range) {
    p <- p + geom_ribbon(aes(ymin = .data[[q_bot_col]], ymax = .data[[q_top_col]], fill = scenario), alpha = 0.4)
  }

  if (show_ensemble) {
    p <- p + geom_line(aes(y = .data[[stat_col]], color = "KNMI Ensembles"), linewidth = 1)
  }

  p <- p + geom_line(aes(y = .data[[stat_col]]), linewidth = 2) +
    scale_x_date(date_labels = "%b", breaks = month_labels, expand = c(0, 0)) +
    labs(
      title = paste("30-day Moving Average", stat, y_label, basin),
      subtitle = subtitle_text,
      x = "Month",
      y = paste(y_label, y_unit),
      color = "Dataset",
      fill = "Dataset"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "gray90"),
      panel.grid.minor = element_blank(),
      text = element_text(color = "black"),
      axis.title.x = element_blank(),
      axis.text = element_text(size = 14, color = "black"),
      axis.title = element_text(size = 16, face = "bold", color = "black"),
      legend.text = element_text(size = 14, color = "black"),
      legend.title = element_text(size = 16, face = "bold", color = "black"),
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5, color = "black")
    ) +
    scale_color_manual(
      values = plot_info$scenario$colors,
      labels = plot_info$scenario$labels
    ) +
    if (show_range) scale_fill_manual(
      values = plot_info$scenario$colors,
      labels = plot_info$scenario$labels
    ) else NULL #ylim(750, 1750) 

  # Save the plot
  save_dir <- file.path(here::here(), "Plots", "Reference_Period_Analysis", "Seasonality", basin)
  if (!dir.exists(save_dir)) {
    dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
  }

  filename <- paste0("seasonality_ts_", basin, "_", stat, "_", column, ifelse(show_ensemble,"ens", ""), ifelse(show_range, paste0("_Q", q_bot*100, "_Q", q_top*100), ""), ".pdf")
  ggsave(file.path(save_dir, filename), plot = p, device = "pdf", width = 18, height = 6)
}





