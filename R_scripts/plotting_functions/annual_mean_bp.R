plot_annual_boxplots_old <- function(dt, scenarios, geb, c_name, stat, y_label, unit) {
  
  # Initialize a list to store yearly data
  yearly_list <- list()
  
  for (scenario in scenarios) {
    # Select only the reference ensemble mean and hindcast scenario
    if (scenario == "reference") {
      yearly_data <- ref_rhine_list[[scenario]][["ensMean"]][["yearly"]]
      
    } else {
      yearly_data <- ref_rhine_list[[scenario]][["yearly"]]
    }
    
    yearly_data <- yearly_data %>%
      select(YYYY, all_of(c_name)) %>%
      mutate(Scenario = scenario)  # Add Scenario column
    
    yearly_list[[length(yearly_list) + 1]] <- yearly_data
  }
  
  # Combine extracted data
  yearly_df <- do.call(rbind, yearly_list)
  
  # Ensure Scenario is a factor with correct order
  yearly_df$Scenario <- factor(yearly_df$Scenario, levels = c("observed", "hindcast", "reference"))
  
  # Plot annual boxplots
  p <- ggplot(yearly_df, aes(x = Scenario, y = .data[[c_name]], fill = Scenario)) +
    geom_boxplot(position = position_dodge(width = 0.8), fatten = 2, size = 0.8) +  
    labs(
      title = paste("Annual", stat, y_label),
      x = "Dataset",
      y = paste(y_label, unit),
      fill = "Dataset"
    ) +
    theme_minimal(base_size = 16) +  
    theme(
      text = element_text(color = "black"),
      axis.title.x = element_blank(),
      axis.text = element_text(size = 14, color = "black"),  
      axis.title = element_text(size = 16, face = "bold", color = "black"),  
      legend.text = element_text(size = 14, color = "black"),  
      legend.title = element_text(size = 16, face = "bold"),  
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5, color = "black"),
      panel.grid.major.x = element_blank(),  # Remove vertical gridlines
      panel.grid.minor.x = element_blank()   # Remove minor vertical gridlines  
    ) +
    scale_fill_manual(values = ref_colors, labels = ref_labels) +
    scale_x_discrete(labels = NULL)  # Remove x-axis labels (only the legend will show)
  
  # save the plot as a pdf file
  save_dir <- file.path(here::here(), "Plots","Reference_Period_Analysis", "Boxplots")
  if (!dir.exists(save_dir)) {
    dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
  }
  ggsave(file.path(save_dir, paste0(geb,"_", y_label, "_annual.pdf")), plot = p, device = "pdf", width = 8, height = 6)
}

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
                  by = c("YYYY", group_cols, "member")]
  
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
    scale_x_discrete(labels = NULL)  
  
  # Save the plot
  save_dir <- file.path(here::here(), "Plots", "Model_Comparison", info_col)
  filename <- paste0("annual_mean_", bsn, "_", value_col, info_text, ".pdf")
  save_plot(p, save_dir, filename, width = 8, height = 6)
  
}

std = "Basel Rheinhalle"
group_cols <- c("station", "scen_var_hor", "hydro_model")
value_col = "discharge"
color_col = "hydro_model"
horizons = c("ref")
scen_var_hors <- c("none_none_ref", "obs_none_ref")

dt_subset <- knmi_discharge_dt_all[station == std & scen_var_hor %in% scen_var_hors]

plot_annual_boxplots(dt_subset, std, color_col, value_col, group_cols)