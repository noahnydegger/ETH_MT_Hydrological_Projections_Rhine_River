# Loop over each scenario and area
for (scenario in scenario_names) {
  for (area in areas) {
    
    # Process both "yearly" and "monthly" data
    for (time_scale in c("monthly", "yearly")) {
      # Extract data frame for the first ensemble member for the given area and time scale
      first_df <- mit_list[[scenario]][[ensemble_members[1]]][[area]][[time_scale]]
      
      if (is.null(first_df)) next  # Skip if time scale doesn't exist
      
      # Extract the correct time column based on time scale
      if (time_scale == "yearly") {
        times <- first_df$YYYY
      } else {
        times <- first_df$YearMonth
      }
      
      # Initialize the list to store statistics (Mean, Max, Min, Q10, Q90)
      stat_list <- list(Mean = list(), Max = list(), Min = list(), Q10 = list(), Q90 = list())
      
      # Extract unique times (either YYYY or YearMonth)
      unique_times <- unique(times)
      
      # Loop over each unique time group (year or year-month)
      for (time_id in seq_along(unique_times)) {
        time_filter <- unique_times[time_id]
        
        # Filter the data based on time_scale and time_filter
        filtered_data <- do.call(rbind, lapply(ensemble_members, function(ens) {
          data <- mit_list[[scenario]][[ens]][[area]][[time_scale]]
          if (time_scale == "yearly") {
            data[data$YYYY == time_filter, , drop = FALSE]
          } else {
            data[data$YearMonth == time_filter, , drop = FALSE]
          }
        }))
        
        # Compute statistics for each column, excluding the time column
        numeric_data <- filtered_data[, sapply(filtered_data, is.numeric)]  # Only select numeric columns
        
        # Compute statistics for each column
        stat_list$Mean[[time_id]] <- colMeans(numeric_data, na.rm = TRUE)
        stat_list$Max[[time_id]]  <- apply(numeric_data, 2, max, na.rm = TRUE)
        stat_list$Min[[time_id]]  <- apply(numeric_data, 2, min, na.rm = TRUE)
        stat_list$Q10[[time_id]]  <- apply(numeric_data, 2, quantile, probs = 0.10, na.rm = TRUE)
        stat_list$Q90[[time_id]]  <- apply(numeric_data, 2, quantile, probs = 0.90, na.rm = TRUE)
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
    }
  }
}