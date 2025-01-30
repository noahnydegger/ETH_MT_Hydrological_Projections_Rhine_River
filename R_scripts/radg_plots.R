# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(here)

# change what should be plotted
out_var <- "Global Radiation"
knmi_var <- "radg_full.stats"
stan_var <- "rad__full.stat"

data_dir <- "Data"
gebiet <- "TGl200"

knmi_file <- file.path(here::here(), data_dir, gebiet,"KNMItest", knmi_var)
stan_file <- file.path(here::here(), data_dir, gebiet,"Standard", stan_var)

# Load data from the file path
knmi_data <- read.table(knmi_file, header = TRUE)
stan_data <- read.table(stan_file, header = TRUE)

# Function to process data: combine date columns and filter by year range
process_data <- function(data) {
  # Combine date columns into a Date object
  data$Date <- as.Date(paste(data$YYYY, data$MM, data$DD, sep = "-"), format = "%Y-%m-%d")
  
  # Create a YearMonth column in "YYYY-MM" format
  data <- data %>% mutate(YearMonth = format(Date, "%Y-%m"))
  
  # Filter rows between 1991 and 2020
  data <- subset(data, format(Date, "%Y") >= 1991 & format(Date, "%Y") <= 2020)
  
  # Return processed data
  return(data)
}

monthly_means <- function(data) {
  # Calculate monthly means for each variable
  monthly_data <- data %>%
    group_by(YearMonth) %>%
    summarise(
      Month = first(MM),
      MeanMax = mean(MAX, na.rm = TRUE),
      MeanMin = mean(MIN, na.rm = TRUE),
      MeanAvg = mean(AVG, na.rm = TRUE),
      MeanStd = mean(STDEV, na.rm = TRUE)
    )
  
  # Convert 'MM' (numeric month) to a factor and label with month abbreviations (e.g., Jan, Feb)
  monthly_data$MonthAbb <- factor(monthly_data$Month, levels = 1:12, labels = month.abb)
  
  # Return the monthly data
  return(monthly_data)
}

yearly_means <- function(data) {
  # Compute yearly summary values for plotting
  yearly_data <- data %>%
    group_by(YYYY) %>%
    summarise(
      MeanMax = mean(MAX, na.rm = TRUE),
      MeanMin = mean(MIN, na.rm = TRUE),
      MeanAvg = mean(AVG, na.rm = TRUE),
      MeanStd = mean(STDEV, na.rm = TRUE)
    )
  yearly_data <- yearly_data %>%
    rename(Year = YYYY) %>%
    mutate(Year = as.character(Year))
  
  return(yearly_data)
}

# Apply the function to both datasets
knmi_data <- process_data(knmi_data)
stan_data <- process_data(stan_data)

# Calculate monthly means for both datasets
knmi_months <- monthly_means(knmi_data)
stan_months <- monthly_means(stan_data)

# Calculate monthly means for both datasets
knmi_years <- yearly_means(knmi_data)
stan_years <- yearly_means(stan_data)

# Add a 'Source' column to distinguish between the datasets
knmi_months$Source <- "KNMI"
knmi_years$Source <- "KNMI"
stan_months$Source <- "STAN"
stan_years$Source <- "STAN"

# Combine the two datasets
combined_months <- bind_rows(stan_months, knmi_months)
combined_months$Source <- factor(combined_months$Source, levels = c("STAN", "KNMI"))
combined_years <- bind_rows(stan_years, knmi_years)
combined_years$Source <- factor(combined_years$Source, levels = c("STAN", "KNMI"))

# Sort combined_data by YearMonth in place
combined_months <- combined_months %>%
  arrange(YearMonth)

# Sort combined_data by Year in place
combined_years <- combined_years %>%
  arrange(Year)

plot_monthly_with_yearly_boxplots <- function(monthly_data, yearly_data, variable_name, stat, daily_value, y_label) {
  # Plot monthly boxplots
  p <- ggplot(monthly_data, aes_string(x = "MonthAbb", y = variable_name, fill = "Source")) +
    geom_boxplot(position = position_dodge(width = 0.8)) +
    labs(
      title = paste("Monthly and Yearly", stat, "of Daily", daily_value, "Global Radiation"),
      x = "Time Period",
      y = y_label,
      fill = "Dataset"
    ) +
    theme_minimal() +
    scale_fill_brewer(palette = "Set1")
  
  # Add yearly boxplot to the existing plot
  p + 
    geom_boxplot(
      data = yearly_data, 
      aes(x = "Year", y = !!sym(variable_name), fill = Source), 
      position = position_dodge(width = 0.8)
    )
  
  # save the plot as a pdf file
  save_dir <- file.path(here::here(), "Plots", "Radiation")
  if (!dir.exists(save_dir)) {
    dir.create(save_dir)
  }
  ggsave(file.path(save_dir, paste0(variable_name , "_plot.pdf")), plot = last_plot(), device = "pdf", width = 12, height = 6)
}

# Plot monthly boxplots for each variable
plot_monthly_with_yearly_boxplots(combined_months, combined_years, "MeanAvg", "Mean", "avg.", "Global Radiation [W/m^2]")
plot_monthly_with_yearly_boxplots(combined_months, combined_years, "MeanMax", "Mean", "max.", "Global Radiation [W/m^2]")
plot_monthly_with_yearly_boxplots(combined_months, combined_years, "MeanMin", "Mean", "min.", "Global Radiation [W/m^2]")
plot_monthly_with_yearly_boxplots(combined_months, combined_years, "MeanStd", "Mean", "std.", "Global Radiation [W/m^2]")


