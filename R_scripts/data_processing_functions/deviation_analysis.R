library(data.table)
library(ggplot2)

source(here("R_scripts", "plotting_functions", "knmi_plot_metadata.R"))

compute_monthly_differences <- function(dt, group_cols, value_cols, comparison_col, comparison_list) {
  
  # Ensure the comparison list has valid structure
  if (!is.character(comparison_list) || length(comparison_list) < 2) {
    stop("comparison_list must be a character vector with at least two scenarios.")
  }
  
  # Extract month directly from the date column
  dt[, MM := month(date)]  # Extract month as numeric (1-12)
  
  # Identify the reference scenario
  ref_scenario <- comparison_list[1]
  
  # Compute mean values by month and grouping columns
  dt_mean <- dt[, lapply(.SD, mean, na.rm = TRUE), 
                by = c(group_cols, comparison_col, "MM"),
                .SDcols = value_cols]
  
  # Compute absolute differences
  dt_mean[, (paste0(value_cols, "_abs_dev")) := 
            lapply(value_cols, function(col) .SD[[col]] - .SD[[col]][get(comparison_col) == ref_scenario]),
          by = c(group_cols, "MM"), .SDcols = value_cols]
  
  # Compute relative differences
  dt_mean[, (paste0(value_cols, "_rel_dev")) := 
            lapply(value_cols, function(col) 
              (.SD[[col]] - .SD[[col]][get(comparison_col) == ref_scenario]) /
                .SD[[col]][get(comparison_col) == ref_scenario]),
          by = c(group_cols, "MM"), .SDcols = value_cols]
  
  # Optionally drop the Month column if not needed
  dt_mean[, MM := factor(month.abb[MM], levels = month.abb[1:12])]
  
  # Exclude the reference scenario from the dt_mean
  dt_mean <- dt_mean[get(comparison_col) != ref_scenario]
  
  return(dt_mean)
}

compute_overall_difference <- function(dt, group_cols, value_cols, comparison_col, comparison_list) {
  
  # Ensure the comparison list has valid structure
  if (!is.character(comparison_list) || length(comparison_list) < 2) {
    stop("comparison_list must be a character vector with at least two scenarios.")
  }
  
  # Identify the reference scenario
  ref_scenario <- comparison_list[1]
  
  # Compute mean values by year and grouping columns
  dt_mean <- dt[, lapply(.SD, mean, na.rm = TRUE), 
                by = c(group_cols, comparison_col),
                .SDcols = value_cols]
  
  # Compute absolute deviations
  dt_mean[, (paste0(value_cols, "_abs_dev")) := 
            lapply(value_cols, function(col) .SD[[col]] - .SD[[col]][get(comparison_col) == ref_scenario]),
          by = c(group_cols), .SDcols = value_cols]
  
  # Compute relative deviations
  dt_mean[, (paste0(value_cols, "_rel_dev")) := 
            lapply(value_cols, function(col) 
              (.SD[[col]] - .SD[[col]][get(comparison_col) == ref_scenario]) /
                .SD[[col]][get(comparison_col) == ref_scenario]),
          by = c(group_cols), .SDcols = value_cols]
  
  # Exclude the reference scenario from the dt_mean
  dt_mean <- dt_mean[get(comparison_col) != ref_scenario]
  
  return(dt_mean)
}



# Function to plot monthly and yearly boxplots from daily data.table
plot_monthly_yearly_boxplots <- function(dt_month, dt_overall, info_col, color_col, value_col, dev_type) {
  
  value_name <- plot_info$column_info$names[[info_col]]
  value_unit <- plot_info$column_info$units[[info_col]]
  
  # Plotting
  p <- ggplot(dt_month, aes(x = MM, y = .data[[value_col]], fill = .data[[color_col]])) +
    geom_boxplot(position = position_dodge(width = 0.8)) +
    labs(title = paste(dev_type, "difference to hindcast", value_name, "over all basins"),
         x = "Month",
         y = paste(value_name, value_unit),
         fill = "Dataset"
    ) +
    custom_theme() +
    theme(
      axis.title.x = element_blank()  # Remove the x-axis title
    ) +
    scale_fill_manual(
      values = plot_info[[color_col]]$colors,
      labels = plot_info[[color_col]]$labels
    )
  
  # Add yearly boxplot
  p <- p +
    geom_boxplot(
      data = dt_overall,
      aes(x = "Year", y = .data[[value_col]], fill = .data[[color_col]]),
      position = position_dodge(width = 0.8)
    )
  
  # Save the plot
  save_dir <- file.path(here::here(), "Plots", "Reference_Period_Analysis", "bias_correction", paste0(info_col, "_diff"))
  filename <- paste0(value_col, ".pdf")
  save_plot(p, save_dir, filename, width = 18, height = 6)
}

