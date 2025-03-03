# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(here)

import_mit_data <- function(file_path) {
  # Load the data from the file
  mit_data <- read.table(file_path, header = TRUE, skip = 1)
  
  # Extract the horizon from the file)
  horizon <- extract_horizon(file_path)
  
  # Process the data
  mit_data <- process_data(mit_data, min_year = horizon - 14, max_year = horizon + 15)
  
  return(mit_data)
}

import_stats_data <- function(file_path) {
  # Load the data from the file
  stats_data <- read.table(file_path, header = TRUE)
  
  # Extract the horizon from the file)
  horizon <- extract_horizon(file_path)
  
  # Process the data
  stats_data <- process_data(stats_data, min_year = horizon - 14, max_year = horizon + 15)
  
  return(stats_data)
}

extract_horizon <- function(file_path) {
  # Extract the scenario_ensemble folder (two levels up from the file)
  scen_ens_folder <- dirname(dirname(file_path))
  
  # Check if "reference" is in the folder name, return 2005 if true
  if (grepl("reference", scen_ens_folder)) {
    return(2005)
  } else if (grepl("\\d{4}", scen_ens_folder)) {
    # Extract the year using regex if not "reference"
    horizon_year <- as.numeric(sub(".*(\\d{4}).*", "\\1", scen_ens_folder))
    return(horizon_year)
  } else {
    stop("Error: No year found in folder name and no 'reference' found.")
  }
}

# Function to process data: combine date columns and filter by year range
process_data <- function(data, min_year = NULL, max_year = NULL) {
  # Combine date columns into a Date object
  data$Date <- as.Date(paste(data$YYYY, data$MM, data$DD, sep = "-"), format = "%Y-%m-%d")
  
  # Replace '.' with '_' in column names
  colnames(data) <- gsub("\\.", "_", colnames(data))
  
  # Create a YearMonth column in "YYYY-MM" format
  data <- data %>% mutate(YearMonth = format(Date, "%Y-%m"))
  
  # Filter rows between 30 year periods
  if (!is.null(min_year)) {
    data <- subset(data, format(Date, "%Y") >= min_year)
  }
  if (!is.null(max_year)) {
    data <- subset(data, format(Date, "%Y") <= max_year)
  }
  
  # Return processed data
  return(data)
}


compute_monthly_means <- function(mit_data) {
  # Compute monthly means for each column
  monthly_means <- mit_data %>%
    select(-c(DD)) %>%
    group_by(YearMonth) %>%
    summarize(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))
  
  return(monthly_means)
}

compute_yearly_means <- function(mit_data) {
  # Compute yearly means for each column
  yearly_means <- mit_data %>%
    select(-c(DD, MM)) %>%
    group_by(YYYY) %>%
    summarize(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))
  
  return(yearly_means)
}


