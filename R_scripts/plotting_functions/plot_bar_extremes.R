library(data.table)
library(patchwork)

source(here("R_scripts", "plotting_functions", "knmi_plot_metadata.R"))

compute_percentile_ref_value <- function(dt, bsn, value_col, comparison_col, comparison_ref, q_bot, q_top) {
  # Subset the data based on basin and comparison reference
  dt_sub <- dt[basin == bsn & get(comparison_col) == comparison_ref]
  
  # Compute percentiles
  q_values <- quantile(dt_sub[[value_col]], probs = c(q_bot, q_top), na.rm = TRUE)
  
  # Return as a named list or vector
  ref_value <- list(
    q_bot = q_values[1],
    q_top = q_values[2]
  )
  
  return(ref_value)
}

compute_number_of_days <- function(dt, bsn, group_cols, value_col, comparison_col, comparison_ref, q_bot, q_top) {
  # Compute reference thresholds
  ref_values <- compute_percentile_ref_value(dt, bsn, value_col, comparison_col, comparison_ref, q_bot, q_top)
  
  # Add date parts
  dt[, year := year(date)]
  dt[, month := month(date)]
  
  # Assign seasons
  dt[, season := fifelse(month %in% c(12, 1, 2), "DJF",
                             fifelse(month %in% c(3, 4, 5), "MAM",
                                     fifelse(month %in% c(6, 7, 8), "JJA", "SON")))]
  
  dt[, season := factor(season, levels = c("DJF", "MAM", "JJA", "SON"))]
  
  # Compare values to reference percentiles
  dt[, below_qbot := get(value_col) < ref_values$q_bot]
  dt[, above_qtop := get(value_col) > ref_values$q_top]
  
  # Prepare output list
  result_list <- list()
  
  # Group by season
  group_season <- c(group_cols, "season")
  result_season <- dt[, .(
    days_below_qbot = sum(below_qbot, na.rm = TRUE),
    days_above_qtop = sum(above_qtop, na.rm = TRUE)
  ), by = group_season]
  result_list[["season"]] <- result_season
  
  # Group by year (entire year summary)
  dt[, season := "Year"]  # Overwrite with "Year"
  result_year <- dt[, .(
    days_below_qbot = sum(below_qbot, na.rm = TRUE),
    days_above_qtop = sum(above_qtop, na.rm = TRUE)
  ), by = group_cols]
  result_year[, season := "Year"]
  
  # Combine and return
  result_combined <- rbind(result_season, result_year, use.names = TRUE, fill = TRUE)
  result_combined[, season := factor(season, levels = c("DJF", "MAM", "JJA", "SON", "Year"))]
  
  return(result_combined)
}

plot_seasonal_bars_with_error <- function(dt_seasonal, plot_dir, bsn, color_col, color_col_levels, value_col, group_cols, info_text = "", rel = FALSE) {
  
  # Fetch metadata
  value_name <- plot_info$column_info$names[[value_col]]
  value_unit <- plot_info$column_info$units[[value_col]]
  y_text <- paste(value_name, value_unit)
  
  if (rel) {
    value_unit <- "[%]"
    y_text <- "Change [%]"
    info_text <- paste0(info_text, "change")
  }
  
  # Ensure proper factor ordering
  dt_seasonal[, (color_col) := factor(get(color_col), levels = color_col_levels)]
  dt_seasonal[, season := factor(season, levels = c("DJF", "MAM", "JJA", "SON", "Year"))]
  dt_seasonal[, group_combined := do.call(paste, c(.SD, sep = "_")), .SDcols = group_cols]
  
  # Compute summary statistics per group
  dt_summary <- dt_seasonal[, .(
    mean = mean(days_below_qbot, na.rm = TRUE),
    q10 = quantile(days_below_qbot, 0.1, na.rm = TRUE),
    q90 = quantile(days_below_qbot, 0.9, na.rm = TRUE)
  ), by = c(group_cols, "season")]
  
  dt_summary[, group_combined := do.call(paste, c(.SD, sep = "_")), .SDcols = group_cols]
  
  # Plot barplot with error bars (q10 - q90)
  p <- ggplot(dt_summary, aes(x = season, y = mean, fill = .data[[color_col]])) +
    geom_bar(stat = "identity", 
             position = position_dodge(width = 0.8), 
             width = 0.6, color = "black") +
    geom_errorbar(aes(ymin = q10, ymax = q90), 
                  position = position_dodge(width = 0.8), 
                  width = 0.2, color = "black") +
    scale_fill_manual(
      values = plot_info[[color_col]]$colors,
      labels = plot_info[[color_col]]$labels
    ) +
    labs(
      x = NULL,
      y = y_text,
      fill = "Dataset"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.major.x = element_blank(),
      axis.line = element_line(color = "black", linewidth = 0.5),
      legend.position = "top",
      legend.key.width = unit(1.5, "cm"),
      axis.text.x = element_text(size = 12)
    )
  
  # Save the plot
  save_dir <- file.path(plot_dir, "seasonal_barplots", value_col)
  filename <- paste0("seasonal_bar_", bsn, "_", value_col, info_text, ".pdf")
  save_plot(p, save_dir, filename, width = 10, height = 6)
}



