# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(here)

areas <- c("ThS200")
scenarios <- c(
  "reference", "Hd_2100"
)
scenario_colors <- c("reference" = "grey40", "Hd_2100" = "#B15928")
c_names <- c("RTOT")
stats <- c("Mean")

plot_monthly_with_yearly_boxplots <- function(mit_knmi_list, scenarios, area, c_name, stat, y_label, unit) {
  # Initialize lists to store monthly and yearly data
  monthly_list <- list()
  yearly_list <- list()
  
  for (scenario in scenarios) {
    # Select only ensemble members (ens1, ens2, ens3, etc.), not ensMean, ensStd, etc.
    ensemble_members <- grep("^ens[0-9]+$", names(mit_knmi_list[[scenario]]), value = TRUE)
    for (ens in ensemble_members) {
      
      # Extract monthly data
      if (!is.null(mit_knmi_list[[scenario]][[ens]][[area]][["monthly"]])) {
        monthly_data <- mit_knmi_list[[scenario]][[ens]][[area]][["monthly"]] %>%
          select(MM, all_of(c_name)) %>%
          mutate(
            MonthAbb = factor(month.abb[MM], levels = month.abb),  # Convert MM to MonthAbb
            Scenario = scenario  # Add Scenario column
          )
        monthly_list[[length(monthly_list) + 1]] <- monthly_data
      }
      
      # Extract yearly data and compute mean
      if (!is.null(mit_knmi_list[[scenario]][[ens]][[area]][["yearly"]])) {
        yearly_data <- mit_knmi_list[[scenario]][[ens]][[area]][["yearly"]] %>%
          select(YYYY, all_of(c_name)) %>%
          mutate(Scenario = scenario)  # Add Scenario column
        yearly_list[[length(yearly_list) + 1]] <- yearly_data
      }
    }
  }
  
  # Combine extracted data
  monthly_df <- do.call(rbind, monthly_list)
  yearly_df <- do.call(rbind, yearly_list)
  
  monthly_df$Scenario <- factor(monthly_df$Scenario, levels = c("reference", "Hd_2100"))
  yearly_df$Scenario <- factor(yearly_df$Scenario, levels = c("reference", "Hd_2100"))
  
  # Plot monthly boxplots
  p <- ggplot(monthly_df, aes(x = MonthAbb, y = .data[[c_name]] / 30, fill = Scenario)) +
    geom_boxplot(position = position_dodge(width = 0.8)) +
    labs(
      title = paste("Monthly", stat, y_label, "in subbasin", area),
      x = "Month",
      y = paste(y_label, unit),
      fill = "Scenario"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      text = element_text(color = "black"),  # Make all text black
      axis.title.x = element_blank(),
      axis.text = element_text(size = 14, color = "black"),  
      axis.title = element_text(size = 16, face = "bold", color = "black"),  
      legend.text = element_text(size = 14, color = "black"),  
      legend.title = element_text(size = 16, face = "bold", color = "black"),  
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5, color = "black")  
    ) +
    scale_fill_manual(values = scenario_colors)
  
  # Add yearly boxplot
  p <- p +
    geom_boxplot(
      data = yearly_df,
      aes(x = "Year", y = .data[[c_name]] / 365, fill = Scenario),
      position = position_dodge(width = 0.8)
    )
  
  # save the plot as a pdf file
  save_dir <- file.path(here::here(), "Plots","TestPlots", "Boxplots")
  if (!dir.exists(save_dir)) {
    dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
  }
  ggsave(file.path(save_dir, paste0(area,"_", y_label, ".pdf")), plot = p, device = "pdf", width = 18, height = 6)
}

# Loop through all combinations of areas, variables, and statistics
for (area in areas) {
  for (c_name in c_names) {
    for (stat in stats) {
      cat("Processing:", area, "-", c_name, "-", stat, "\n")  # Print progress
      plot_monthly_with_yearly_boxplots(mit_knmi_list, scenarios, area, c_name, stat, "Runoff", "[mm/d]")
    }
  }
}