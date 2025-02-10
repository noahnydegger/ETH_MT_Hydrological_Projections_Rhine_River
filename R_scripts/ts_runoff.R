# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(slider)
library(scales)
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

# Function to compute 30-day centered rolling mean and daily stats
compute_runoff_statistics <- function(data) {
  # Compute 30-day centered rolling mean
  data <- data %>%
    arrange(Date) %>%
    mutate(
      RMQ = slide_dbl(RGES, mean, .before = 15, .after = 14, .complete = TRUE),
      RMS = slide_dbl(P.SME, mean, .before = 15, .after = 14, .complete = TRUE),
      RMG = slide_dbl(GLAC, mean, .before = 15, .after = 14, .complete = TRUE)
    )
  
  # Identify leap years and adjust DayOfYear for them
  data <- data %>%
    mutate(DayOfYear = as.numeric(format(Date, "%j")),  # Create DayOfYear column
           Year = as.numeric(format(Date, "%Y"))) %>%   # Extract Year for leap year check
    # Remove leap days (Feb 29)
    filter(DayOfYear != 60 | !((Year %% 4 == 0) & (Year %% 100 != 0 | Year %% 400 == 0))) %>%
    # Shift the DayOfYear for leap years (after Feb 29)
    mutate(DayOfYear = if_else(DayOfYear > 60 & (Year %% 4 == 0) & (Year %% 100 != 0 | Year %% 400 == 0), 
                               DayOfYear - 1, 
                               DayOfYear)) %>%
    select(-Year)  # Drop the Year column since it's no longer needed
  
  # Compute daily statistics over all years
  daily_stats <- data %>%
    group_by(DayOfYear) %>%
    summarise(
      MQ = mean(RMQ, na.rm = TRUE),
      Q10Q = quantile(RMQ, 0.10, na.rm = TRUE),
      Q90Q = quantile(RMQ, 0.90, na.rm = TRUE),
      MS = mean(RMS, na.rm = TRUE),
      Q10S = quantile(RMS, 0.10, na.rm = TRUE),
      Q90S = quantile(RMS, 0.90, na.rm = TRUE),
      MG = mean(RMG, na.rm = TRUE),
      Q10G = quantile(RMG, 0.10, na.rm = TRUE),
      Q90G = quantile(RMG, 0.90, na.rm = TRUE)
    ) %>%
    filter(!is.na(MQ) | !is.na(MS) | !is.na(MG))
  
  return(daily_stats)
}

# Apply the function to both datasets
knmi_data <- process_data(knmi_data)
stan_data <- process_data(stan_data)

# Calculate rolling mean for both datasets
knmi_Rmean <- compute_runoff_statistics(knmi_data)
stan_Rmean <- compute_runoff_statistics(stan_data)

# Add a 'Source' column to distinguish between the datasets
knmi_Rmean$Source <- "KNMI"
stan_Rmean$Source <- "STAN"


# Function to plot the statistics
plot_runoff_statistics <- function(daily_stats1, daily_stats2, y_label, variable) {
  
  meancol <- paste0("M", variable)
  q10col <- paste0("Q10", variable)
  q90col <- paste0("Q90", variable)
  
  combined_stats <- bind_rows(daily_stats1, daily_stats2)
  combined_stats$Source <- factor(combined_stats$Source, levels = c("STAN", "KNMI"))
  
  # Compute NSE for the selected variable
  compute_nse <- function(observed, simulated) {
    1 - (sum((observed - simulated)^2, na.rm = TRUE) /
           sum((observed - mean(observed, na.rm = TRUE))^2, na.rm = TRUE))
  }
  
  nse_value <- compute_nse(daily_stats1[[meancol]], daily_stats2[[meancol]])
  
  # Convert DayOfYear to date-like values (use 2023 as a dummy year)
  combined_stats <- combined_stats %>%
    mutate(DateLabel = as.Date(DayOfYear - 1, origin = "2023-01-01"))
  
  # Create breaks for the start of each month
  month_breaks <- seq(as.Date("2023-01-15"), as.Date("2023-12-15"), by = "1 month")
  
  ggplot(combined_stats, aes(x = DateLabel, group = Source, color = Source)) +
    geom_ribbon(aes(ymin = !!sym(q10col), ymax = !!sym(q90col), fill = Source), alpha = 0.2) +
    geom_line(aes(y = !!sym(meancol)), size = 1) +
    scale_x_date(
      date_labels = "%b",
      breaks = month_breaks
    ) +
    labs(
      title = paste("30-day Moving Average (with Q10-Q90) of daily mean", y_label, " over 30 Years"),
      subtitle = paste("NSE between STAN and KNMI:", round(nse_value, 3)),
      x = "Month",
      y = paste(y_label, "[mm/day]"),
      color = "Dataset",
      fill = "Dataset"
    ) +
    theme_minimal() +
    # Manually set colors and fills
    scale_color_manual(values = c("STAN" = "red", "KNMI" = "blue")) +
    scale_fill_manual(values = c("STAN" = "red", "KNMI" = "blue"))
  
  # save the plot as a pdf file
  save_dir <- file.path(here::here(), "Plots", gebiet, y_label)
  if (!dir.exists(save_dir)) {
    dir.create(save_dir, recursive = TRUE)
  }
  ggsave(file.path(save_dir, paste0("TS_", y_label, ".pdf")), plot = last_plot(), device = "pdf", width = 12, height = 6)
}

plot_runoff_statistics(stan_Rmean, knmi_Rmean, "Runoff", "Q")
plot_runoff_statistics(stan_Rmean, knmi_Rmean, "Snow melt", "S")
plot_runoff_statistics(stan_Rmean, knmi_Rmean, "Ice melt", "G")
