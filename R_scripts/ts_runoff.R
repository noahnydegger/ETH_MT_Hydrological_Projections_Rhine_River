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
    mutate(RollingMean30 = slide_dbl(
      RGES,
      mean,
      .before = 15,
      .after = 14,
      .complete = TRUE
    ))
  
  # Extract the day of the year
  data <- data %>%
    mutate(DayOfYear = as.numeric(format(Date, "%j")))
  
  # Compute daily statistics over all years
  daily_stats <- data %>%
    group_by(DayOfYear) %>%
    summarise(
      MeanQ = mean(RollingMean30, na.rm = TRUE),
      MinQ = min(RollingMean30, na.rm = TRUE),
      MaxQ = max(RollingMean30, na.rm = TRUE)
    ) %>%
    filter(!is.na(MeanQ))
  
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
plot_runoff_statistics <- function(daily_stats1, daily_stats2, label1 = "Dataset 1", label2 = "Dataset 2") {
  daily_stats1$Dataset <- label1
  daily_stats2$Dataset <- label2
  
  combined_stats <- bind_rows(daily_stats1, daily_stats2)
  combined_stats$Source <- factor(combined_stats$Source, levels = c("STAN", "KNMI"))
  
  # Convert DayOfYear to date-like values (use 2023 as a dummy year)
  combined_stats <- combined_stats %>%
    mutate(DateLabel = as.Date(DayOfYear - 1, origin = "2023-01-01"))
  
  # Create breaks for the start of each month
  month_breaks <- seq(as.Date("2023-01-15"), as.Date("2023-12-15"), by = "1 month")
  
  ggplot(combined_stats, aes(x = DateLabel, group = Source, color = Source)) +
    geom_ribbon(aes(ymin = MinQ, ymax = MaxQ, fill = Source), alpha = 0.2) +
    geom_line(aes(y = MeanQ), size = 1) +
    scale_x_date(
      date_labels = "%b",
      breaks = month_breaks
    ) +
    labs(
      title = "30-Day Moving Mean (with Max-Min) Runoff over 30 Years",
      x = "Month",
      y = "Runoff [mm/day]",
      color = "Dataset",
      fill = "Dataset"
    ) +
    theme_minimal() +
    # Manually set colors and fills
    scale_color_manual(values = c("STAN" = "red", "KNMI" = "blue")) +
    scale_fill_manual(values = c("STAN" = "red", "KNMI" = "blue"))
  
  # save the plot as a pdf file
  save_dir <- file.path(here::here(), "Plots", gebiet, "Runoff")
  if (!dir.exists(save_dir)) {
    dir.create(save_dir, recursive = TRUE)
  }
  ggsave(file.path(save_dir, paste0("TS_Runoff", ".pdf")), plot = last_plot(), device = "pdf", width = 12, height = 6)
}

plot_runoff_statistics(stan_Rmean, knmi_Rmean, "STAN", "KNMI")
