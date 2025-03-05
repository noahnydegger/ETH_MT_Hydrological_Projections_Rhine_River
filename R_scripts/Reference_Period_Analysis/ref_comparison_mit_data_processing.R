# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(here)

data_dir <- file.path("Data", "R_KNMI")
hindcast_file <- file.path(here::here(), "Data", "Rhein", "CTRL_RUN_WSL_F_2021_g73_RhB200.mit_cut")

# Define the list of scenarios
scenarios <- c("reference")

ensenmbles <- c(
  "ens1", "ens2", "ens3", "ens4", "ens5", "ens6", "ens7", "ens8"
)

gebiete <- c("RhB200")

source(here("R_scripts", "data_import_functions.R"))

process_mit_data <- function(file_path) {
  # Load the data from the file
  mit_data <- read.table(file_path, header = TRUE, skip = 1)
  
  mit_data <- process_data(mit_data)
  
  # Filter rows between 30 year periods
  
  mit_data <- subset(mit_data, format(Date, "%Y") >= 1991)
  mit_data <- subset(mit_data, format(Date, "%Y") <= 2020)
  
  return(mit_data)
}


ref_mit_list <- list()

# import the .mit file for each scenario, ensemble, and area
for (scen in scenarios) {
  ref_mit_list[[scen]] <- list()
  for (ens in ensenmbles) {
    ref_mit_list[[scen]][[ens]] <- list()
    for (geb in gebiete) {
      # Construct the file path
      mit_file <- file.path(here::here(), data_dir, paste0(scen, "_", ens), geb, paste0(geb, ".mit"))
      
      # Check if the mit file exists before reading
      if (file.exists(mit_file)) {
        
        # Import data from the .mit and .pri files
        mit_data <- process_mit_data(mit_file)
        
        # Store the loaded data in the list
        ref_mit_list[[scen]][[ens]][[geb]][["daily"]] <- mit_data
        ref_mit_list[[scen]][[ens]][[geb]][["monthly"]] <- compute_monthly_means(mit_data)
        ref_mit_list[[scen]][[ens]][[geb]][["yearly"]] <- compute_yearly_means(mit_data)
        
      } else {
        message(paste("Mit file not found:", mit_file))
        ref_mit_list[[scen]][[ens]][[geb]] <- NULL
      }
    }
  }
}

# import hindcast data
hindcast_data <- process_mit_data(hindcast_file)
ref_mit_list[["hindcast"]][[geb]][["daily"]] <- hindcast_data
ref_mit_list[["hindcast"]][[geb]][["monthly"]] <- compute_monthly_means(hindcast_data)
ref_mit_list[["hindcast"]][[geb]][["yearly"]] <- compute_yearly_means(hindcast_data)

# compute the ensemble statistics (mean, std, max, min) for each scenario, area, and time scale

for (scenario in scenarios) {
  cat("Scenario processing started:", scenario, "\n")
  # Select only ensemble members (ens1, ens2, ens3, etc.), not ensMean, ensStd, etc.
  ensemble_members <- grep("^ens[0-9]+$", names(ref_mit_list[[scenario]]), value = TRUE)
  
  # Get all area names dynamically from the first ensemble
  areas <- names(ref_mit_list[[scenario]][[ensemble_members[1]]])
  
  for (area in areas) {
    
    # Process "daily", "monthly", "yearly" data
    time_scale_names <- c("monthly", "yearly")# c("daily", "monthly", "yearly")
    for (time_scale in time_scale_names) {
      # Extract data frame for the first ensemble member for the given area and time scale
      first_df <- ref_mit_list[[scenario]][[ensemble_members[1]]][[area]][[time_scale]]
      
      if (is.null(first_df)) next  # Skip if time scale doesn't exist
      
      # Extract the correct time column based on time scale
      if (time_scale == "daily") {
        times <- first_df$Date
      } else if (time_scale == "monthly") {
        times <- first_df$YearMonth
      } else if (time_scale == "yearly") {
        times <- first_df$YYYY
      } 
      
      # Initialize the list to store statistics (Mean, Std, Max, Min) of all ensembles
      stat_list <- list(ensMean = list(), ensStd = list(), ensMax = list(), ensMin = list())
      
      # Extract unique times (dates, YearMonth or YYYY)
      unique_times <- unique(times)
      
      # Loop over each unique time group (date, YearMonth, year)
      for (time_id in seq_along(unique_times)) {
        time_filter <- unique_times[time_id]
        
        # Filter the data based on time_scale and time_filter
        filtered_data <- do.call(rbind, lapply(ensemble_members, function(ens) {
          data <- ref_mit_list[[scenario]][[ens]][[area]][[time_scale]]
          if (time_scale == "daily") {
            data <- data[data$Date == time_filter, , drop = FALSE]
            data <- data[, !colnames(data) %in% c("YYYY", "MM", "DD"), drop = FALSE]  # Remove YYYY, MM, DD
            
            # Apply a 30-day centered rolling mean to numeric columns
            numeric_columns <- data[, sapply(data, is.numeric), drop = FALSE]  # Select only numeric columns
            
            # Apply rolling mean
            rolling_data <- as.data.frame(lapply(numeric_columns, function(x) {
              rollapply(x, width = 30, FUN = mean, align = "center", fill = NA)
            }))
            
            # Replace original data with rolling data (mean of 30 days centered)
            data <- cbind(data, rolling_data)
            
          } else if (time_scale == "monthly") {
            data <- data[data$YearMonth == time_filter, , drop = FALSE]  # Monthly data remains unchanged
          } else if (time_scale == "yearly") {
            data <- data[data$YYYY == time_filter, , drop = FALSE]
            data <- data[, !colnames(data) %in% c("YYYY"), drop = FALSE]  # Remove YYYY
          }
        }))
        
        # Compute statistics for each column, excluding the time column
        numeric_data <- filtered_data[, sapply(filtered_data, is.numeric)]  # Only select numeric columns
        
        # Compute statistics for each column
        stat_list$ensMean[[time_id]] <- colMeans(numeric_data, na.rm = TRUE)
        stat_list$ensStd[[time_id]]  <- apply(numeric_data, 2, sd, na.rm = TRUE)
        stat_list$ensMax[[time_id]]  <- apply(numeric_data, 2, max, na.rm = TRUE)
        stat_list$ensMin[[time_id]]  <- apply(numeric_data, 2, min, na.rm = TRUE)
      }
      
      # Convert lists into data frames and store in the correct list structure
      for (stat_name in names(stat_list)) {
        stat_df <- do.call(rbind, stat_list[[stat_name]])
        
        # Add the correct time column (Year or Year-Month) to the data frame
        if (time_scale == "yearly") {
          stat_df <- data.frame(YYYY = unique_times, stat_df, row.names = NULL)
        } else {
          stat_df <- data.frame(YearMonth = unique_times, stat_df, row.names = NULL)
        }
        
        # Store the computed statistics in the correct location
        ref_mit_list[[scenario]][[stat_name]][[area]][[time_scale]] <- stat_df
      }
      
    } # time_scale loop
    cat("Finished processing:", scenario, area, "\n")
  } # area loop
  
  cat("Scenario processing completed:", scenario, "\n")
} # scenario loop

cat("All scenarios successfully processed!\n")
