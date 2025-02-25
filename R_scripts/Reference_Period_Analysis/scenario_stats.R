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
      
      # Initialize the list to store statistics (Mean, Std, Max, Min) of all ensembles
      stat_list <- list(scenMean = list(), scenStd = list(), scenMax = list(), scenMin = list(), scenQ90 = list(), scenQ10 = list())
      
        
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