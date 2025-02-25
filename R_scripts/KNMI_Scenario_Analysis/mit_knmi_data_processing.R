# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(here)

data_dir <- file.path("Data", "R_KNMI")

scenarios <- c(
  "reference", "Hd_2100"
)

ensenmbles <- c(
  "ens1", "ens2", "ens3", "ens4", "ens5", "ens6", "ens7", "ens8"
)

gebiete <- c(
  #"TGl200",
  "ThS200"#, "BEN200", "BiS200", "Bod400", "EmW200", "HiR200", "LaP200", "Lim200", "NeS200", "NoW200", "Rhb200", "KEm200", "SeD200","SSG200", "Thu200", "VoA200", "VoR200", "WaS200",
)


source(here("R_scripts", "data_import_functions.R"))

mit_knmi_list <- list()

# import the .mit and .pri files for each scenario, ensemble, and area
for (scen in scenarios) {
  mit_knmi_list[[scen]] <- list()
  for (ens in ensenmbles) {
    mit_knmi_list[[scen]][[ens]] <- list()
    for (geb in gebiete) {
      # Construct the file path
      mit_file <- file.path(here::here(), data_dir, paste0(scen, "_", ens), geb, paste0(geb, ".mit"))
      pri_file <- file.path(here::here(), data_dir, paste0(scen, "_", ens), geb, paste0(geb, ".pri"))
      
      # Check if the mit and pri files exist before reading
      if (file.exists(mit_file) && file.exists(pri_file)) {
        
        # Import data from the .mit and .pri files
        mit_data <- import_mit_data(mit_file)
        pri_data <- import_pri_data(pri_file)
        
        # Store the loaded data in the list
        mit_knmi_list[[scen]][[ens]][[geb]][["daily"]] <- mit_data
        mit_knmi_list[[scen]][[ens]][[geb]][["monthly"]] <- pri_data$monthly
        mit_knmi_list[[scen]][[ens]][[geb]][["yearly"]] <- pri_data$yearly
        
      } else {
        # Handle missing file case
        if (!file.exists(mit_file)) {
          message(paste("Mit file not found:", mit_file))
        }
        if (!file.exists(pri_file)) {
          message(paste("Pri file not found:", pri_file))
        }
        mit_knmi_list[[scen]][[ens]][[geb]] <- NULL
      }
    }
  }
}

# compute the ensemble statistics (mean, std, max, min) for each scenario, area, and time scale
scenario_names <- c(
  "reference", "Hd_2100"
)

for (scenario in scenario_names) {
  # Select only ensemble members (ens1, ens2, ens3, etc.), not ensMean, ensStd, etc.
  ensemble_members <- grep("^ens[0-9]+$", names(mit_knmi_list[[scenario]]), value = TRUE)
  
  # Get all area names dynamically from the first ensemble
  areas <- names(mit_knmi_list[[scenario]][[ensemble_members[1]]])
  
  for (area in areas) {
    
    # Process "daily", "monthly", "yearly" data
    time_scale_names <- c("monthly", "yearly")# c("daily", "monthly", "yearly")
    for (time_scale in time_scale_names) {
      # Extract data frame for the first ensemble member for the given area and time scale
      first_df <- mit_knmi_list[[scenario]][[ensemble_members[1]]][[area]][[time_scale]]
      
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
          data <- mit_knmi_list[[scenario]][[ens]][[area]][[time_scale]]
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
        mit_knmi_list[[scenario]][[stat_name]][[area]][[time_scale]] <- stat_df
      }
      
      # Print message indicating completion
      cat("Finished processing:", area, "for", time_scale, "\n")
      
    } # time_scale loop
  } # area loop
  
  cat("Scenario processing completed:", scenario, "\n")
} # scenario loop

cat("All scenarios successfully processed!\n")

# compute the scenario statistics (mean, std, max, min) for each area and time scale



