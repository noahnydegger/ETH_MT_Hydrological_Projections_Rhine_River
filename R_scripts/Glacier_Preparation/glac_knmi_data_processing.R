library(tidyverse)
library(ggplot2)
library(dplyr)
library(here)

period_lenght <- 10 # years for running average

gebiete <- c("ThS200")

# Define the variables to process
meteo_variables <- c("tair", "prec", "radg")

data_dir <- file.path("Data", "R_KNMI")

scenarios <- c(
  "L_2033"#, "Ld_2100", "Ln_2100", "Hd_2100"
)

ensenmbles <- c(
  "ens1", "ens2", "ens3", "ens4", "ens5", "ens6", "ens7", "ens8"
)

source(here("R_scripts", "data_import_functions.R"))

knmi_meteo_list <- list()

# import the meteo data for each scenario, ensemble, and area
for (scen in scenarios) {
  knmi_meteo_list[[scen]] <- list()
  for (ens in ensenmbles) {
    knmi_meteo_list[[scen]][[ens]] <- list()
    for (geb in gebiete) {
      knmi_meteo_list[[scen]][[ens]][[geb]] <- list()
      
      for (var in meteo_variables) {
        # Construct the file path
        meteo_file <- file.path(here::here(), data_dir, paste0(scen, "_", ens), geb, paste0(var, "_full.stats"))
        
        # Check if the meteo file exists before reading
        if (file.exists(meteo_file)) {
          
          # Import data from the .stats file
          meteo_data <- import_stats_data(meteo_file)
          
          # Store the loaded data in the list
          knmi_meteo_list[[scen]][[ens]][[geb]][[var]][["daily"]] <- meteo_data
          knmi_meteo_list[[scen]][[ens]][[geb]][[var]][["monthly"]] <- compute_monthly_means(meteo_data)
          knmi_meteo_list[[scen]][[ens]][[geb]][[var]][["yearly"]] <- compute_yearly_means(meteo_data)
          
        } else {
          message(paste("Meteo file not found:", meteo_file))
          knmi_meteo_list[[scen]][[ens]][[geb]][[var]] <- NULL
        }
      }
    }
  }
}

# compute the ensemble mean, 10-year running average and 30 year mean

for (scen in scenarios) {
  cat("Scenario processing started:", scen, "\n")
  # Select only ensemble members (ens1, ens2, ens3, etc.), not ensMean, ensStd, etc.
  ensemble_members <- grep("^ens[0-9]+$", names(knmi_meteo_list[[scen]]), value = TRUE)
  
  for (geb in gebiete) {
    for (var in meteo_variables) {
    
      # Process "monthly", "yearly" data
      time_scale_names <- c("monthly", "yearly")
      for (time_scale in time_scale_names) {
        # Extract data frame for the first ensemble member for the given area and time scale
        first_df <- knmi_meteo_list[[scen]][[ensemble_members[1]]][[geb]][[var]][[time_scale]]
        
        if (is.null(first_df)) next  # Skip if time scale doesn't exist
        
        # Extract the correct time column based on time scale
         if (time_scale == "monthly") {
          times <- first_df$YearMonth
        } else if (time_scale == "yearly") {
          times <- first_df$YYYY
        } 
        
        # Initialize the list to store ensemble means
        ensMean_list <- list()
        
        # Extract unique times (YearMonth or YYYY)
        unique_times <- unique(times)
        
        # Loop over each unique time group (YearMonth for monthly, YYYY for yearly)
        for (time_id in seq_along(unique_times)) {
          time_filter <- unique_times[time_id]
          
          # Filter the data based on time_scale and time_filter
          filtered_data <- do.call(rbind, lapply(ensemble_members, function(ens) {
            data <- knmi_meteo_list[[scen]][[ens]][[geb]][[var]][[time_scale]]
            
            if (time_scale == "monthly") {
              data <- data[data$YearMonth == time_filter, , drop = FALSE]
            } else if (time_scale == "yearly") {
              data <- data[data$YYYY == time_filter, , drop = FALSE]
              data <- data[, !colnames(data) %in% c("YYYY"), drop = FALSE]  # Remove YYYY column
            }
          }))
          
          # Compute ensemble mean for each numeric column
          numeric_data <- filtered_data[, sapply(filtered_data, is.numeric)]  # Select only numeric columns
          ensMean_list[[time_id]] <- colMeans(numeric_data, na.rm = TRUE)
        }
        
        # Convert the list into a data frame
        ensMean_df <- do.call(rbind, ensMean_list)
        
        # Add the correct time column (Year or Year-Month)
        if (time_scale == "yearly") {
          ensMean_df <- data.frame(YYYY = unique_times, ensMean_df, row.names = NULL)
        } else {
          ensMean_df <- data.frame(YearMonth = unique_times, ensMean_df, row.names = NULL)
        }
        
        # Store the computed ensemble mean in the correct location
        knmi_meteo_list[[scen]][["ensMean"]][[geb]][[var]][[time_scale]] <- ensMean_df
        
      } # time_scale loop
      # Extract the monthly ensemble mean data
      ensMean_df <- knmi_meteo_list[[scen]][["ensMean"]][[geb]][[var]][["yearly"]]
      
      # Ensure YearMonth is in Date format for proper ordering
      ensMean_df$YYYY <- as.Date(paste0(ensMean_df$YYYY, "-01-01"), format = "%Y-%m-%d")  # Convert YYYY to Date
      
      # Compute the overall mean for the 30-year period
      overall_mean_df <- colMeans(ensMean_df[, -1], na.rm = TRUE)  # Exclude YearMonth column
      
      # Compute 10-year running average
      rolling_mean_df <- as.data.frame(lapply(ensMean_df[, -1], function(x) {
        rollapply(x, width = period_lenght, FUN = mean, align = "center", fill = NA, na.rm = TRUE)
      }))
      
      # Add back the time column for proper visualization
      rolling_mean_df$YearMonth <- ensMean_df$YearMonth
      
      # Store results in the list structure
      knmi_meteo_list[[scen]][["overall_mean_30yr"]][[geb]][[var]] <- overall_mean_df
      knmi_meteo_list[[scen]][["rolling_mean_10yr"]][[geb]][[var]] <- rolling_mean_df
      
    } # meteo variable loop
    
  } # area loop
  
  cat("Scenario processing completed:", scen, "\n")
} # scenario loop

cat("All scenarios successfully processed!\n")