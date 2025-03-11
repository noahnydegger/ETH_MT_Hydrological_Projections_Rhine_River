library(here)
library(reshape2)
library(data.table)

home_dir <- file.path(here::here())
input_dir <- file.path(home_dir, "Data", "Rheinblick2027", "raw_prevah_output", "R_KNMI")
output_dir <- file.path(home_dir, "Data", "Rheinblick2027", "processed_prevah_output")

mit_output_file_suffix <- ".mit"
meteo_stat_file_suffix <- "_full.stats"

output_name_mit_output <- "prevah_mit_output_knmi"
output_name_meteo_stat <- "prevah_meteo_stat_knmi"

scenario_horizons <- c(
  "reference", 
  "L_2033",
  "Md_2050", "Mn_2050", "Hd_2050", "Hn_2050",
  "Ld_2100", "Ln_2100", "Md_2100", "Mn_2100", "Hd_2100", "Hn_2100",
  "Md_2150", "Mn_2150", "Hd_2150", "Hn_2150"
)

ensenmbles <- c(
  "ens1", "ens2", "ens3", "ens4", "ens5", "ens6", "ens7", "ens8"
)

meteo_variables <- c("tair", "prec", "radg", "sund", "rhum", "wspd")

no_meteo_gebiete <- c(
  "RhB200", "RhD200", "RhN200", "RhR200"
)

source(here("R_scripts", "data_import_functions.R"))

knmi_mit_output_list <- list()
knmi_meteo_stat_list <- list()

# Initialize an empty list to store the reshaped data.tables
all_mit_data_list <- list()
all_meteo_data_list <- list()

if (dir.exists(geb_path)) {
  
  # List all subfolders inside the scenario folder
  scen_hor_folders <- list.dirs(input_dir, recursive = FALSE)
}

# import the .mit file for each scenario, ensemble, and area
for (scen in scenario_horizons) {
  
  if (scen == "reference") {
    scenario <- "R"
    horizon <- 2005
  } else {
    scenario <- substr(scen, 1, 1)
    horizon <- as.numeric(sub(".*([0-9]{4})$", "\\1", scen))
  }
  
  # Extract `variant` (2nd character of scenario, "d", "n", or "none")
  variant <- ifelse(nchar(scen) >= 2 && substr(scen, 2, 2) %in% c("d", "n"), substr(scen, 2, 2), "none")
  
  # Skip if `scen` is not found in any scenario-horizon folder
  matching_folders <- grep(scen, scen_hor_folders, value = TRUE)
  if (length(matching_folders) == 0) next
  
  # Create an entry for the scenario in the output list
  knmi_mit_output_list[[scen]] <- list()
  knmi_meteo_stat_list[[scen]] <- list()
  
  # Loop over matching folders
  for (scen_ensm_dir in matching_folders) {
    
    # Extract the ensemble member number (ens1 to ens8) as a numeric value and as a string
    member <- as.numeric(sub(".*_ens([1-8])$", "\\1", basename(scen_ensm_dir)))
    ensm <- paste0("ens", member)
    
    knmi_mit_output_list[[scen]][[ensm]] <- list()
    knmi_meteo_stat_list[[scen]][[ensm]] <- list()
    
    # List all subfolders (gebiete) in the matched scenario-ensemble folder
    gebiete_folders <- list.dirs(scen_ensm_dir, recursive = FALSE)
    
    # Loop over the gebiete folders
    for (ezg_dir in gebiete_folders) {
      ezg <- basename(ezg_dir)
      # First process .mit files
      mit_file <- file.path(ezg_dir, paste0(ezg, mit_output_file_suffix))
      
      # Check if the mit file exists before reading
      if (file.exists(mit_file)) {
        
        # Import data from the .mit and .pri files
        mit_data_d <- import_mit_data(mit_file)
        
        # Add metadata columns for this specific folder
        mit_data_d[, horizon := horizon]
        mit_data_d[, scenario := scenario]
        mit_data_d[, variant := variant]
        mit_data_d[, member := member]
        mit_data_d[, hydro_model := "PREVAH"]
        mit_data_d[, basin := ezg]
        
        # # Reorder columns as needed
        # mit_data_long <- mit_data_long[, .(station, date, discharge, horizon, scenario, 
        #                                    ensm, ezg, hydro_model)]
        
        # Append this to the list of all mit data
        all_mit_data_list[[length(all_mit_data_list) + 1]] <- mit_data_d
      
        # Store the loaded data in the list
        # knmi_mit_output_list[[scen]][[ensm]][[ezg]][["daily"]] <- mit_data_d
        # knmi_mit_output_list[[scen]][[ensm]][[ezg]][["monthly"]] <- compute_monthly_means(mit_data)
        # knmi_mit_output_list[[scen]][[ensm]][[ezg]][["yearly"]] <- compute_yearly_means(mit_data)
        
      } else {
        message(paste(".mit file not found:", mit_file))
        knmi_mit_output_list[[scen]][[ensm]][[ezg]] <- NULL
      }
      
      if (!(ezg %in% no_meteo_gebiete)) {
        # Initialize an empty data.table to store combined meteo data
        all_meteo_data_dt <- data.table()
        
        for (var in meteo_variables) {
          # Construct the file path
          meteo_file <- file.path(ezg_dir, paste0(var, "_full.stats"))
          
          # Check if the meteo file exists before reading
          if (file.exists(meteo_file)) {
            
            # Import data from the .stats file
            meteo_data_d <- import_stats_data(meteo_file)
            
            # Rename meteo-specific columns with 'var_' prefix
            old_meteo_cols <- c("MIN", "MAX", "AVG", "STDEV")
            new_meteo_cols <- paste0(var, "_", c("min", "max", "avg", "std"))
            setnames(meteo_data_d, old = old_meteo_cols, new = new_meteo_cols)
            
            # Add metadata columns
            meteo_data_d[, `:=`(
              horizon = horizon,
              scenario = scenario,
              variant = variant,
              member = member,
              hydro_model = "PREVAH",
              basin = ezg
            )]
            
            # Merge only meteo columns on "date"
            if (nrow(all_meteo_data_dt) == 0) {
              all_meteo_data_dt <- meteo_data_d  # First dataset initializes the structure
            } else {
              all_meteo_data_dt <- merge(
                all_meteo_data_dt, 
                meteo_data_d[, c("date", new_meteo_cols), with = FALSE], 
                by = "date", 
                all = TRUE
              )
            }
            
            # Store the loaded data in the list
            # knmi_meteo_stat_list[[scen]][[ensm]][[ezg]][[var]][["daily"]] <- meteo_data
            # knmi_meteo_stat_list[[scen]][[ensm]][[ezg]][[var]][["monthly"]] <- compute_monthly_means(meteo_data)
            # knmi_meteo_stat_list[[scen]][[ensm]][[ezg]][[var]][["yearly"]] <- compute_yearly_means(meteo_data)
            
          } else {
            message(paste("Meteo file not found:", meteo_file))
            knmi_meteo_stat_list[[scen]][[ensm]][[ezg]][[var]] <- NULL
          }
        } # meteo_variables loop
        # Append this to the list of all mit data
        all_meteo_data_list[[length(all_meteo_data_list) + 1]] <- all_meteo_data_dt
      } # no_meteo_gebiete check
    } # gebiete_folders loop
  } # scen_ensm loop
} # scenario_horizons loop

# Combine all the data.tables into one long data.table
knmi_mit_output_dt <- rbindlist(all_mit_data_list)
knmi_meteo_stat_dt <- rbindlist(all_meteo_data_list)

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
}

# Export to CSV
write.csv2(knmi_mit_output_dt, file.path(output_dir, paste0(output_name_mit_output, ".csv")), row.names = FALSE, quote = FALSE)
write.csv2(knmi_meteo_stat_dt, file.path(output_dir, paste0(output_name_meteo_stat, ".csv")), row.names = FALSE, quote = FALSE)

# Export to .RDS format
saveRDS(knmi_mit_output_dt, file.path(output_dir, paste0(output_name_mit_output, ".rds")))
saveRDS(knmi_meteo_stat_dt, file.path(output_dir, paste0(output_name_meteo_stat, ".rds")))


# # compute the ensemble statistics (mean, std, max, min) for each scenario, area, and time scale
# 
# for (scenario in scenarios) {
#   cat("Scenario processing started:", scenario, "\n")
#   # Select only ensemble members (ens1, ens2, ens3, etc.), not ensMean, ensStd, etc.
#   ensemble_members <- grep("^ens[0-9]+$", names(knmi_mit_output_list[[scenario]]), value = TRUE)
#   
#   # Get all area names dynamically from the first ensemble
#   areas <- names(knmi_mit_output_list[[scenario]][[ensemble_members[1]]])
#   
#   for (area in areas) {
#     
#     # Process "daily", "monthly", "yearly" data
#     time_scale_names <- c("monthly", "yearly")# c("daily", "monthly", "yearly")
#     for (time_scale in time_scale_names) {
#       # Extract data frame for the first ensemble member for the given area and time scale
#       first_df <- knmi_mit_output_list[[scenario]][[ensemble_members[1]]][[area]][[time_scale]]
#       
#       if (is.null(first_df)) next  # Skip if time scale doesn't exist
#       
#       # Extract the correct time column based on time scale
#       if (time_scale == "daily") {
#         times <- first_df$Date
#       } else if (time_scale == "monthly") {
#         times <- first_df$YearMonth
#       } else if (time_scale == "yearly") {
#         times <- first_df$YYYY
#       } 
#       
#       # Initialize the list to store statistics (Mean, Std, Max, Min) of all ensembles
#       stat_list <- list(ensMean = list(), ensStd = list(), ensMax = list(), ensMin = list())
#       
#       # Extract unique times (dates, YearMonth or YYYY)
#       unique_times <- unique(times)
#       
#       # Loop over each unique time group (date, YearMonth, year)
#       for (time_id in seq_along(unique_times)) {
#         time_filter <- unique_times[time_id]
#         
#         # Filter the data based on time_scale and time_filter
#         filtered_data <- do.call(rbind, lapply(ensemble_members, function(ensm) {
#           data <- knmi_mit_output_list[[scenario]][[ensm]][[area]][[time_scale]]
#           if (time_scale == "daily") {
#             data <- data[data$Date == time_filter, , drop = FALSE]
#             data <- data[, !colnames(data) %in% c("YYYY", "MM", "DD"), drop = FALSE]  # Remove YYYY, MM, DD
#             
#             # Apply a 30-day centered rolling mean to numeric columns
#             numeric_columns <- data[, sapply(data, is.numeric), drop = FALSE]  # Select only numeric columns
#             
#             # Apply rolling mean
#             rolling_data <- as.data.frame(lapply(numeric_columns, function(x) {
#               rollapply(x, width = 30, FUN = mean, align = "center", fill = NA)
#             }))
#             
#             # Replace original data with rolling data (mean of 30 days centered)
#             data <- cbind(data, rolling_data)
#             
#           } else if (time_scale == "monthly") {
#             data <- data[data$YearMonth == time_filter, , drop = FALSE]  # Monthly data remains unchanged
#           } else if (time_scale == "yearly") {
#             data <- data[data$YYYY == time_filter, , drop = FALSE]
#             data <- data[, !colnames(data) %in% c("YYYY"), drop = FALSE]  # Remove YYYY
#           }
#         }))
#         
#         # Compute statistics for each column, excluding the time column
#         numeric_data <- filtered_data[, sapply(filtered_data, is.numeric)]  # Only select numeric columns
#         
#         # Compute statistics for each column
#         stat_list$ensMean[[time_id]] <- colMeans(numeric_data, na.rm = TRUE)
#         stat_list$ensStd[[time_id]]  <- apply(numeric_data, 2, sd, na.rm = TRUE)
#         stat_list$ensMax[[time_id]]  <- apply(numeric_data, 2, max, na.rm = TRUE)
#         stat_list$ensMin[[time_id]]  <- apply(numeric_data, 2, min, na.rm = TRUE)
#       }
#       
#       # Convert lists into data frames and store in the correct list structure
#       for (stat_name in names(stat_list)) {
#         stat_df <- do.call(rbind, stat_list[[stat_name]])
#         
#         # Add the correct time column (Year or Year-Month) to the data frame
#         if (time_scale == "yearly") {
#           stat_df <- data.frame(YYYY = unique_times, stat_df, row.names = NULL)
#         } else {
#           stat_df <- data.frame(YearMonth = unique_times, stat_df, row.names = NULL)
#         }
#         
#         # Store the computed statistics in the correct location
#         knmi_mit_output_list[[scenario]][[stat_name]][[area]][[time_scale]] <- stat_df
#       }
#       
#     } # time_scale loop
#     cat("Finished processing:", scenario, area, "\n")
#   } # area loop
#   
#   cat("Scenario processing completed:", scenario, "\n")
# } # scenario loop
# 
# cat("All scenarios successfully processed!\n")
# 
# # compute the scenario statistics (mean, std, max, min) for each area and time scale

