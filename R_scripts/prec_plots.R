# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(here)

# change what should be plotted
out_var <- "Precipitation"
knmi_var <- "prec_full.stats"
stan_var <- "prec_full.stat"

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
      MaxMax = max(MAX, na.rm = TRUE),
      MinMin = min(MIN, na.rm = TRUE),
      SumAvg = sum(AVG, na.rm = TRUE),
      MeanStd = mean(STDEV, na.rm = TRUE),
      # Number of wet days (prec > 0)
      WetDays = sum(AVG > 0, na.rm = TRUE),
      
      # Wet day intensity (average precipitation for days with prec > 0)
      WetDayIntensity = mean(AVG[AVG > 0], na.rm = TRUE)
    )
  
  # Convert 'MM' (numeric month) to a factor and label with month abbreviations (e.g., Jan, Feb)
  monthly_data$MonthAbb <- factor(monthly_data$Month, levels = 1:12, labels = month.abb)
  
  # Return the monthly data
  return(monthly_data)
}

# Apply the function to both datasets
knmi_data <- process_data(knmi_data)
stan_data <- process_data(stan_data)

# Calculate monthly means for both datasets
knmi_means <- monthly_means(knmi_data)
stan_means <- monthly_means(stan_data)

# Add a 'Source' column to distinguish between the datasets
knmi_means$Source <- "KNMI"
stan_means$Source <- "STAN"

# Combine the two datasets
combined_data <- bind_rows(stan_means, knmi_means)
combined_data$Source <- factor(combined_data$Source, levels = c("STAN", "KNMI"))

# Sort combined_data by YearMonth in place
combined_data <- combined_data %>%
  arrange(YearMonth)

# Define the plot function
plot_monthly_boxplots <- function(data, variable_name, y_label) {
  ggplot(data, aes_string(x = "MonthAbb", y = variable_name, fill = "Source")) +
    geom_boxplot(position = position_dodge(width = 0.8)) +
    labs(
      title = paste("Monthly", variable_name, "of Daily Precipitation"),
      x = "Month",
      y = y_label,
      fill = "Dataset"
    ) +
    theme_minimal() +
    scale_fill_brewer(palette = "Set1")
  
  # save the plot as a pdf file
  save_dir <- file.path(here::here(), "Plots", "Precipitation")
  if (!dir.exists(save_dir)) {
    dir.create(save_dir)
  }
  ggsave(file.path(save_dir, paste0(variable_name , "_plot.pdf")), plot = last_plot(), device = "pdf", width = 10, height = 6)
}

# Plot monthly boxplots for each variable
# For each variable, call the function and specify the title and y-label
plot_monthly_boxplots(combined_data, "SumAvg", "Precipitation (mm/month)")
plot_monthly_boxplots(combined_data, "MaxMax", "Precipitation (mm/day)")
plot_monthly_boxplots(combined_data, "MinMin", "Precipitation (mm/day)")
plot_monthly_boxplots(combined_data, "MeanStd", "Precipitation (mm/day)")
plot_monthly_boxplots(combined_data, "WetDays", "Occurence (-)")
plot_monthly_boxplots(combined_data, "WetDayIntensity", "Precipitation (mm/day)")

