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

import_pri_data <- function(file_path) {
  # Define column names according to metadata
  column_names <- c("BASINID", "YYYY", "MM", "BASINID_DUP", "P", "ADJ_P", "SWA", "ETP", "ETR", 
                    "EI", "EB", "R0", "R1", "R2", "RTOT", "SSO", "SI", "SSM", "SUZ", "SLZ", "ICEM")
  # Load the data from the file
  pri_data <- read.table(file_path, header = FALSE, fill = TRUE)
  
  # Remove rows where the month (MM) or year (YYYY) are 0 or negative
  pri_data <- pri_data %>% 
    filter(V3 >= 0, V3 <= 12, V2 > 0)
  
  # Assign column names after filtering
  colnames(pri_data) <- column_names
  
  # Ensure year (YYYY) and month (MM) are treated as numeric
  pri_data <- pri_data %>% mutate(YYYY = as.numeric(YYYY), MM = as.numeric(MM))
  
  # Extract the horizon from the file
  horizon <- extract_horizon(file_path)
  
  # select 30 year period
  pri_data <- filter(pri_data, YYYY >= (horizon - 14), YYYY <= (horizon + 15))
  
  # Separate monthly and yearly data
  monthly_data <- pri_data %>% 
    filter(MM != 0) %>%   # Exclude yearly summary rows
    select(-BASINID_DUP, -BASINID)  # Remove redundant ID columns
  
  yearly_data <- pri_data %>%
    filter(MM == 0) %>%   # Select only yearly summary rows
    select(-c(MM, BASINID, BASINID_DUP))  # Remove MM (since it's always 0) and ID columns
  
  # Return the processed datasets as a list
  return(list(monthly = monthly_data, yearly = yearly_data))
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



