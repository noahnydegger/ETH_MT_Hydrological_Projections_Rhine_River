# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(slider)
library(here)

# change what should be plotted
gebiet <- "Rhein"
variable <- "Runoff"

knmi_filename <- "KNMItest_2021_g73_RhB200.mit_cut"
stan_filename <- "CTRL_RUN_WSL_F_2021_g73_RhB200.mit_cut"

data_dir <- file.path("Data")

knmi_file <- file.path(here::here(), data_dir, gebiet, knmi_filename)
stan_file <- file.path(here::here(), data_dir, gebiet, stan_filename)

# Load data from the file path
knmi_data <- read.table(knmi_file, header = TRUE, skip = 1)
stan_data <- read.table(stan_file, header = TRUE, skip = 1)

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

monthly_indicators <- function(data) {
  # Calculate monthly means for each variable
  monthly_data <- data %>%
    group_by(YearMonth) %>%
    summarise(
      Month = first(MM),
      MQ = mean(RGES, na.rm = TRUE),
      MNQ = min(RGES, na.rm = TRUE),
      MHQ = max(RGES, na.rm = TRUE),
      MS = mean(P.SME, na.rm = TRUE),
      MNS = min(P.SME, na.rm = TRUE),
      MHS = max(P.SME, na.rm = TRUE),
      MGL = mean(GLAC, na.rm = TRUE),
      MNG = min(GLAC, na.rm = TRUE),
      MHG = max(GLAC, na.rm = TRUE),
    )
  
  # Convert 'MM' (numeric month) to a factor and label with month abbreviations (e.g., Jan, Feb)
  monthly_data$MonthAbb <- factor(monthly_data$Month, levels = 1:12, labels = month.abb)
  
  # Return the monthly data
  return(monthly_data)
}

seasonal_indicators <- function(data) {
  # Define seasons based on month numbers
  data <- data %>%
    mutate(
      Season = case_when(
        MM %in% c(5, 6, 7, 8, 9, 10) ~ "S",
        MM %in% c(11, 12, 1, 2, 3, 4b) ~ "W",
        TRUE ~ "Other"
      )
    ) %>%
    filter(Season != "Other") %>%  # Keep only Summer and Winter
    mutate(
      Year_Season = paste(YYYY, Season, sep = "_")
    )
  
  # Compute seasonal summaries
  seasonal_data <- data %>%
    group_by(Year_Season) %>%
    summarise(
      Season = first(Season),
      MQ = mean(RGES, na.rm = TRUE),
      MN7Q = min(slide_dbl(RGES, mean, .before = 3, .after = 3, na.rm = TRUE), na.rm = TRUE),
      MH7Q = max(slide_dbl(RGES, mean, .before = 3, .after = 3, na.rm = TRUE), na.rm = TRUE),
      MS = mean(P.SME, na.rm = TRUE),
      MN7S = min(slide_dbl(P.SME, mean, .before = 3, .after = 3, na.rm = TRUE), na.rm = TRUE),
      MH7S = max(slide_dbl(P.SME, mean, .before = 3, .after = 3, na.rm = TRUE), na.rm = TRUE),
      MG = mean(GLAC, na.rm = TRUE),
      MN7G = min(slide_dbl(GLAC, mean, .before = 3, .after = 3, na.rm = TRUE), na.rm = TRUE),
      MH7G = max(slide_dbl(GLAC, mean, .before = 3, .after = 3, na.rm = TRUE), na.rm = TRUE)
    )
  
  return(seasonal_data)
}

annual_indicators <- function(data) {
  # Compute yearly summary values
  yearly_data <- data %>%
    group_by(YYYY) %>%
    summarise(
      MQ = mean(RGES, na.rm = TRUE),
      MNQ = min(RGES, na.rm = TRUE),
      MHQ = max(RGES, na.rm = TRUE),
      MS = mean(P.SME, na.rm = TRUE),
      MNS = min(P.SME, na.rm = TRUE),
      MHS = max(P.SME, na.rm = TRUE),
      MG = mean(GLAC, na.rm = TRUE),
      MNG = min(GLAC, na.rm = TRUE),
      MHG = max(GLAC, na.rm = TRUE),
    )
  yearly_data <- yearly_data %>%
    rename(Year = YYYY)
  
  return(yearly_data)
}

# Apply the function to both datasets
knmi_data <- process_data(knmi_data)
stan_data <- process_data(stan_data)

# Calculate monthly indicators for both datasets
knmi_months <- monthly_indicators(knmi_data)
stan_months <- monthly_indicators(stan_data)

# Calculate seasonal indicators for both datasets
knmi_seasons <- seasonal_indicators(knmi_data)
stan_seasons <- seasonal_indicators(stan_data)

# Calculate yearly indicators for both datasets
knmi_years <- annual_indicators(knmi_data)
stan_years <- annual_indicators(stan_data)

# Add a 'Source' column to distinguish between the datasets
knmi_months$Source <- "KNMI"
knmi_seasons$Source <- "KNMI"
knmi_years$Source <- "KNMI"
stan_months$Source <- "STAN"
stan_seasons$Source <- "STAN"
stan_years$Source <- "STAN"

# Combine the datasets
combined_months <- bind_rows(stan_months, knmi_months)
combined_months$Source <- factor(combined_months$Source, levels = c("STAN", "KNMI"))
combined_seasons <- bind_rows(stan_seasons, knmi_seasons)
combined_seasons$Source <- factor(combined_seasons$Source, levels = c("STAN", "KNMI"))
combined_years <- bind_rows(stan_years, knmi_years)
combined_years$Source <- factor(combined_years$Source, levels = c("STAN", "KNMI"))

# Sort combined_data by YearMonth in place
combined_months <- combined_months %>%
  arrange(YearMonth)

# Sort combined_data by Year_Season in place
combined_seasons <- combined_seasons %>%
  arrange(Year_Season)

# Sort combined_data by Year in place
combined_years <- combined_years %>%
  arrange(Year)

plot_annual_indicators_boxplots <- function(data, indicator, stat, y_label) {
  unit = "[mm/d]"
  p <- ggplot(data, aes(x = indicator, y = !!sym(indicator), fill = Source)) +
    geom_boxplot(position = position_dodge(width = 0.8)) +
    labs(
      title = paste("Annual", stat, y_label),
      x = "Indicator",
      y = paste(y_label, unit),
      fill = "Dataset"
    ) +
    theme_minimal() +
    theme(axis.title.x = element_blank()) +  # Remove the x-axis title
    scale_fill_brewer(palette = "Set1")
  
  # save the plot as a pdf file
  save_dir <- file.path(here::here(), "Plots", gebiet, y_label)
  if (!dir.exists(save_dir)) {
    dir.create(save_dir, recursive = TRUE)
  }
  ggsave(file.path(save_dir, paste0("Annual_",indicator, ".pdf")), plot = p, device = "pdf", width = 12, height = 6)
}

plot_seasonal_indicators_boxplots <- function(data, indicator, stat, y_label) {
  unit <- "[mm/d]"
  
  # Prepare data to ensure seasonal separation
  p <- ggplot(data, aes(x = Season, y = !!sym(indicator), fill = Source)) +
    geom_boxplot(position = position_dodge(width = 0.8)) +
    labs(
      title = paste("Seasonal", stat, y_label),
      x = "Season",
      y = paste(y_label, unit),
      fill = "Dataset"
    ) +
    theme_minimal() +
    scale_fill_brewer(palette = "Set1")
  
  # Save the plot as a PDF file
  save_dir <- file.path(here::here(), "Plots", gebiet, y_label)
  if (!dir.exists(save_dir)) {
    dir.create(save_dir, recursive = TRUE)
  }
  
  ggsave(file.path(save_dir, paste0("Seasonal_",indicator, ".pdf")), plot = p, device = "pdf", width = 12, height = 6)
}

plot_seasonal_indicators_boxplots(combined_seasons, "MQ", "Mean","Runoff")
plot_seasonal_indicators_boxplots(combined_seasons, "MN7Q", "Lowest 7-day","Runoff")
plot_seasonal_indicators_boxplots(combined_seasons, "MH7Q", "Highest 7-day","Runoff")
plot_seasonal_indicators_boxplots(combined_seasons, "MS", "Mean","Snow melt")
plot_seasonal_indicators_boxplots(combined_seasons, "MN7S", "Lowest 7-day","Snow melt")
plot_seasonal_indicators_boxplots(combined_seasons, "MH7S", "Highest 7-day","Snow melt")
plot_seasonal_indicators_boxplots(combined_seasons, "MG", "Mean","Ice melt")
plot_seasonal_indicators_boxplots(combined_seasons, "MN7G", "Lowest 7-day","Ice melt")
plot_seasonal_indicators_boxplots(combined_seasons, "MH7G", "Highest 7-day","Ice melt")

plot_annual_indicators_boxplots(combined_years, "MQ", "Mean","Runoff")
plot_annual_indicators_boxplots(combined_years, "MNQ", "Lowest","Runoff")
plot_annual_indicators_boxplots(combined_years, "MHQ", "Highest","Runoff")
plot_annual_indicators_boxplots(combined_years, "MS", "Mean","Snow melt")
plot_annual_indicators_boxplots(combined_years, "MNS", "Lowest","Snow melt")
plot_annual_indicators_boxplots(combined_years, "MHS", "Highest","Snow melt")
plot_annual_indicators_boxplots(combined_years, "MG", "Mean","Ice melt")
plot_annual_indicators_boxplots(combined_years, "MNG", "Lowest","Ice melt")
plot_annual_indicators_boxplots(combined_years, "MHG", "Highest","Ice melt")


