# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(here)

data_dir <- file.path("Data", "R_KNMI")
hindcast_file <- file.path(here::here(), "Data", "Rhein", "CTRL_RUN_WSL_F_2021_g73_RhB200.mit_cut")

# Define the list of scenarios
scenarios <- c("hindcast", "reference")

ensenmbles <- c(
  "ens1", "ens2", "ens3", "ens4", "ens5", "ens6", "ens7", "ens8"
)

ref_colors <- c("hindcast" = "grey40","reference" = "grey70", "KNMI Ensembles" = "grey80")
ref_labels <- c("observed" = "Observed", "hindcast" = "Hindcast", "reference" = "KNMI Mean", 
                "KNMI Ensembles" = "KNMI ens")

gebiete <- c("RhB200")


c_names <- c("RGES", "P_uk", "P_SME", "GLAC", "EREA")
y_labels <- c("RGES" = "Runoff", "P_uk" = "Precipitation", "P_SME" = "Snowmelt", "GLAC" = "Icemelt", "EREA" = "actual Evapotranspiration")
stats <- c("Mean")

plot_monthly_with_yearly_boxplots <- function(ref_mit_list, scenarios, geb, c_name, stat, y_label, unit) {
  # Initialize lists to store monthly and yearly data
  monthly_list <- list()
  yearly_list <- list()
  
  for (scenario in scenarios) {
    
    # Select only the reference ensemble mean and hindcast scenario
    if (scenario == "reference") {
      monthly_data <- ref_mit_list[[scenario]][["ensMean"]][[geb]][["monthly"]]
      yearly_data <- ref_mit_list[[scenario]][["ensMean"]][[geb]][["yearly"]]
      
    } else if (scenario == "hindcast") {
      monthly_data <- ref_mit_list[[scenario]][[geb]][["monthly"]]
      yearly_data <- ref_mit_list[[scenario]][[geb]][["yearly"]]
    }
    monthly_data <- monthly_data %>%
      select(MM, all_of(c_name)) %>%
      mutate(
        MonthAbb = factor(month.abb[MM], levels = month.abb),  # Convert MM to MonthAbb
        Scenario = scenario  # Add Scenario column
      )
    monthly_list[[length(monthly_list) + 1]] <- monthly_data
    
    yearly_data <- yearly_data %>%
      select(YYYY, all_of(c_name)) %>%
      mutate(Scenario = scenario)  # Add Scenario column
    yearly_list[[length(yearly_list) + 1]] <- yearly_data
  }
  
  # Combine extracted data
  monthly_df <- do.call(rbind, monthly_list)
  yearly_df <- do.call(rbind, yearly_list)
  
  # Ensure correct scenario labeling
  monthly_df$Scenario <- factor(monthly_df$Scenario, levels = c("hindcast", "reference"))
  yearly_df$Scenario <- factor(yearly_df$Scenario, levels = c("hindcast", "reference"))
  
  # Plot monthly boxplots
  p <- ggplot(monthly_df, aes(x = MonthAbb, y = .data[[c_name]], fill = Scenario)) +
    geom_boxplot(position = position_dodge(width = 0.8)) +
    labs(
      title = paste(stat, y_label, "for total Rhine basin"),
      x = "Month",
      y = paste(y_label, unit),
      fill = "Dataset"
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
    scale_fill_manual(values = ref_colors, labels = ref_labels)
  
  # Add yearly boxplot
  p <- p +
    geom_boxplot(
      data = yearly_df,
      aes(x = "Year", y = .data[[c_name]], fill = Scenario),
      position = position_dodge(width = 0.8)
    )
  
  # save the plot as a pdf file
  save_dir <- file.path(here::here(), "Plots","Reference_Period_Analysis", "Boxplots")
  if (!dir.exists(save_dir)) {
    dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
  }
  ggsave(file.path(save_dir, paste0(geb,"_", y_label, ".pdf")), plot = p, device = "pdf", width = 18, height = 6)
}

plot_annual_boxplots <- function(ref_mit_list, scenarios, geb, c_name, stat, y_label, unit) {

  # Initialize a list to store yearly data
  yearly_list <- list()
  
  for (scenario in scenarios) {
    # Select only the reference ensemble mean and hindcast scenario
    if (scenario == "reference") {
      yearly_data <- ref_mit_list[[scenario]][["ensMean"]][[geb]][["yearly"]]
      
    } else if (scenario == "hindcast") {
      yearly_data <- ref_mit_list[[scenario]][[geb]][["yearly"]]
    }

    yearly_data <- yearly_data %>%
      select(YYYY, all_of(c_name)) %>%
      mutate(Scenario = scenario)  # Add Scenario column
    
    yearly_list[[length(yearly_list) + 1]] <- yearly_data
  }
  
  # Combine extracted data
  yearly_df <- do.call(rbind, yearly_list)
  
  # Ensure Scenario is a factor with correct order
  yearly_df$Scenario <- factor(yearly_df$Scenario, levels = c("hindcast", "reference"))
  
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

# Loop through all combinations of areas, variables, and statistics
for (geb in gebiete) {
  for (c_name in c_names) {
    for (stat in stats) {
      cat("Processing:", geb, "-", c_name, "-", stat, "\n")  # Print progress
      plot_annual_boxplots(ref_mit_list, scenarios, geb, c_name, stat, y_labels[c_name], "[mm/d]")
      plot_monthly_with_yearly_boxplots(ref_mit_list, scenarios, geb, c_name, stat, y_labels[c_name], "[mm/d]")
    }
  }
}