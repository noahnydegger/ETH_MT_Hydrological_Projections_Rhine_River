# Loop over each scenario and area

scenario_names <- c(
  "reference"#, "Hd_2100"
)

for (scenario in scenario_names) {
  # Select only ensemble members (ens1, ens2, ens3, etc.), not ensMean, ensStd, etc.
  ensemble_members <- grep("^ens[0-9]+$", names(mit_list[[scenario]]), value = TRUE)
  
  # Get all area names dynamically from the first ensemble
  areas <- names(mit_list[[scenario]][[ensemble_members[1]]])
  
  for (area in areas) {
    
    # Process "daily", "monthly", "yearly" data
    time_scale_names <- c("monthly", "yearly")# c("daily", "monthly", "yearly")
    for (time_scale in time_scale_names) {
      # Extract data frame for the first ensemble member for the given area and time scale
      first_df <- mit_list[[scenario]][[ensemble_members[1]]][[area]][[time_scale]]
      
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
          data <- mit_list[[scenario]][[ens]][[area]][[time_scale]]
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
        mit_list[[scenario]][[stat_name]][[area]][[time_scale]] <- stat_df
      }
      
      # Print message indicating completion
      cat("Finished processing:", area, "for", time_scale, "\n")
      
    } # time_scale loop
  } # area loop
  
  cat("Scenario processing completed:", scenario, "\n")
} # scenario loop

cat("All scenarios successfully processed!\n")