library(data.table)
library(zoo)

plot_info <- list(
  scenario = list(
    colors = c("O" = "black", "H" = "grey40", "R" = "grey70", "L" = "blue", "KNMI Ensembles" = "grey80"),
    labels = c("O" = "Observed", "H" = "Hindcast", "R" = "KNMI Reference", "L" = "KNMI L", "KNMI Ensembles" = "KNMI ens")
  ),
  column_info = list(
    y_labels = c(
      "discharge" = "Discharge",
      "P-UK" = "Interpolated precipitation",
      "P-KOR" = "Adjusted interpolated precipitation3",
      "P-SME" = "Snowmelt",
      "EPOT" = "Potential evapotranspiration",
      "EREA" = "Actual evapotranspiration",
      "EI" = "Interception evaporation / snow evaporation",
      "EB" = "Transpiration / soil evaporation",
      "R0" = "Surface runoff",
      "R1" = "Interflow",
      "R2" = "Total baseflow",
      "RGES" = "Total runoff",
      "S-SNO" = "Snow water equivalent",
      "SI" = "Interception storage",
      "SSM" = "Plant available soil moisture storage",
      "SUZ" = "Runoff generation storage (unsaturated zone)",
      "SLZ" = "Runoff generation storage (saturated zone)",
      "BIL" = "Balance from previous time step",
      "GLAC" = "Ice melt",
      "RG1" = "Fast response baseflow",
      "RG2" = "Slow response baseflow",
      "RG3" = "Third component baseflow",
      "DIFGA" = "Input to DIFGA"
    ),
    units = c(
      "discharge" = "m³/s",
      "P-UK" = "mm/d",
      "P-KOR" = "mm/d",
      "P-SME" = "mm/d",
      "EPOT" = "mm/d",
      "EREA" = "mm/d",
      "EI" = "mm/d",
      "EB" = "mm/d",
      "R0" = "mm/d",
      "R1" = "mm/d",
      "R2" = "mm/d",
      "RGES" = "mm/d",
      "S-SNO" = "mm",
      "SI" = "mm",
      "SSM" = "mm",
      "SUZ" = "mm",
      "SLZ" = "mm",
      "BIL" = "mm/d",
      "GLAC" = "mm/d",
      "RG1" = "mm/d",
      "RG2" = "mm/d",
      "RG3" = "mm/d",
      "DIFGA" = "mm/d"
    )
  )
)

statistics <- c("mean", "min", "max")

basins <- c("ThS200")

dt <- knmi_mit_output_dt[basin %in% basins]

# group_cols <- c("station", "horizon", "scenario", "variant", "hydro_model")  # discharge_dt
group_cols <- c("basin", "scenario", "variant", "hydro_model")  # knmi_mit_output_dt
value_cols <- c("RGES", "P-SME", "GLAC", "EREA")



show_range <- FALSE
show_ensemble <- FALSE

q_bot <- 0.25
q_top <- 0.75

source(here("R_scripts", "data_processing_functions", "gof_metrics.R"))

compute_daily_stats <- function(dt, group_cols, value_cols, stat = "mean") {
  # Create a copy of the data to avoid modifying the original
  dt <- copy(dt)
  
  # Compute rolling statistic for each column in value_cols
  dt[, paste0("rm_", value_cols) := lapply(.SD, function(x) zoo::rollapply(
    x, 
    width = 30, 
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
compute_stats <- function(dt, group_cols, value_cols, stat = "mean", q_bot = 0.25, q_top = 0.75) {
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
plot_runoff_statistics <- function(data, column, y_label, y_unit, q_bot = 0.10, q_top = 0.90, show_ensemble = FALSE, show_range = FALSE, stat) {
  mean_name <- paste0(stat, "_", column)
  q_bot_name <- paste0("q_bot_", column)
  q_top_name <- paste0("q_top_", column)
  
  # nse_obs_hindcast <- compute_nse(scenario_stats_DT, "observed", "hindcast")
  # nse_obs_reference <- compute_nse(scenario_stats_DT, "observed", "reference")
  # nse_hindcast_reference <- compute_nse(scenario_stats_DT, "hindcast", "reference")
  # 
  # # Subtitle text
  # subtitle_text <- paste0(
  #   "NSE(obs vs hindcast) = ", round(nse_obs_hindcast, 2), " | ",
  #   "NSE(obs vs KNMI) = ", round(nse_obs_reference, 2), " | ",
  #   "NSE(hindcast vs KNMI) = ", round(nse_hindcast_reference, 2)
  # )
  subtitle_text <- "Placeholder"

  # Add month lines and labels
  month_lines <- seq(as.Date("2023-01-01"), as.Date("2023-12-01"), by = "1 month")
  month_labels <- seq(as.Date("2023-01-15"), as.Date("2023-12-15"), by = "1 month")

  # Plot
  p <- ggplot(data, aes(x = as.Date(DayOfYear - 1, origin = "2023-01-01"),
                                     group = scenario, color = scenario)) +
    geom_vline(xintercept = as.numeric(month_lines), color = "gray90")

  if (show_range) {
    p <- p + geom_ribbon(aes(ymin = .data[[q_bot_name]], ymax = .data[[q_top_name]], fill = scenario), alpha = 0.4)
  }

  if (show_ensemble) {
    p <- p + geom_line(aes(y = .data[[mean_name]], color = "KNMI Ensembles"), size = 1)
  }

  p <- p + geom_line(aes(y = .data[[mean_name]]), size = 2) +
    scale_x_date(date_labels = "%b", breaks = month_labels, expand = c(0, 0)) +
    labs(
      title = paste("30-day Moving Average", stat, y_label),
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
  save_dir <- file.path(here::here(), "Plots", "Reference_Period_Analysis", "TimeSeries")
  if (!dir.exists(save_dir)) {
    dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
  }

  filename <- paste0("TS_Test_", stat, "_", y_label, ifelse(show_ensemble,"ens", ""), ifelse(show_range, paste0("_Q", q_bot*100, "_Q", q_top*100), ""), ".pdf")
  ggsave(file.path(save_dir, filename), plot = p, device = "pdf", width = 18, height = 6)
}

rolling_stats_dt <- compute_daily_stats(dt, group_cols, value_cols)

# Add "rm_" prefix to each value column
group_cols <- c(group_cols, "DayOfYear")
value_cols <- paste0("rm_", value_cols)

for (column in value_cols) {
  for (stat in statistics) {
    seasonality_dt <- compute_stats(rolling_stats_dt, group_cols = group_cols, value_cols = column, stat = stat)
    col <- sub("rm_", "", column)
    y_label <- plot_info$column_info$y_labels[[col]]
    y_unit <- plot_info$column_info$units[[col]]
    plot_runoff_statistics(seasonality_dt, column, y_label = y_label, y_unit = y_unit, show_ensemble = show_ensemble, show_range = show_range, stat = stat)
  }
}




