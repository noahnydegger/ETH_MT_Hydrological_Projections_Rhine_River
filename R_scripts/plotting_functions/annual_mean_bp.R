library(ggplot2)

source(here("R_scripts", "plotting_functions", "knmi_plot_metadata.R"))

plot_annual_boxplots <- function(dt, bsn, color_col, value_col, group_cols, info_text = "") {
  
  value_name <- plot_info$column_info$names[[value_col]]
  value_unit <- plot_info$column_info$units[[value_col]]
  
  # Ensure grouping column is a factor with proper order
  #dt[[group_col]] <- factor(dt[[group_col]], levels = unique(dt[[group_col]]))
  
  # Extract the year from the date column
  dt[, YYYY := year(date)]
  
  # Compute annual means by year and group_cols
  dt_annual <<- dt[, .(annual_mean = mean(get(value_col), na.rm = TRUE)), 
                  by = c("YYYY", group_cols)]
  
  # Convert group_cols to a single grouping variable for x-axis if needed
  dt_annual[, group_combined := do.call(paste, c(.SD, sep = "_")), .SDcols = group_cols]
  
  # Plot annual boxplots
  p <- ggplot(dt_annual, aes(x = group_combined, y = annual_mean, fill = .data[[color_col]])) +
    geom_boxplot(position = position_dodge(width = 0.8), fatten = 2, size = 0.8) +  
    labs(
      title = paste("Annual mean", value_name),
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
    ylim(600, 1600) +
    scale_x_discrete(labels = NULL)  
  
  # Save the plot
  save_dir <- file.path(here::here(), "Plots", "Model_Comparison", value_col)
  filename <- paste0("annual_mean_", bsn, "_", value_col, info_text, ".pdf")
  save_plot(p, save_dir, filename, width = 8, height = 6)
  
}

std = "Basel Rheinhalle" # Basel Rheinhalle
group_cols <- c("station", "scen_var_hor", "hydro_model")
value_col = "discharge"
color_col = "hydro_model"
horizons = c("ref")
scen_var_hors <- c("none_none_ref", "obs_none_ref")

dt_subset <- knmi_discharge_dt_all[station == std & scen_var_hor %in% scen_var_hors]

plot_annual_boxplots(dt_subset, std, color_col, value_col, group_cols, info_text = "_no_ensmem")