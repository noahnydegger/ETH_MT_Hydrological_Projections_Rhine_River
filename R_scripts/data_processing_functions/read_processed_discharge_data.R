library(here)
library(data.table)

# project directory
home_dir <- file.path(here::here())

# input directories
input_dir_prevah <- file.path(home_dir, "Data", "Rheinblick2027", "processed_prevah_output", "discharge", "Rheinblick_stations","future_V1")
input_dir_larsim <- file.path(home_dir, "Data", "Rheinblick2027", "processed_larsim_output", "all_stations")
input_dir_wflow <- file.path(home_dir, "Data", "Rheinblick2027", "processed_wflow_output", "new_structure")

station_list <- c(
  "Basel Rheinhalle", "Lobith", "Maxau", "Kaub", "Diepoldsau", "Andelfingen", "Brugg", "Bruegg-Aegerten", "Gisingen", "Rekingen"
)

stations_wflow <- paste0(station_list, "_Deltares")

# output directory
output_dir <- file.path(home_dir, "Data", "Rheinblick2027", "processed_discharge_data")

input_file_suffix_knmi <- ".csv"

output_file_name <- "knmi_discharge_data"


# functions --------------------------------------------------
# Function to add 'scenario_variant' and 'scenario_variant_horizon' columns with custom ordering
add_scenario_horizon_grouping_columns <- function(dt) {
  dt[, scen_var := paste(scenario, variant, sep = "_")]
  dt[, scen_var_hor := paste(scen_var, horizon, sep = "_")]
  
  # Define custom order for scenario
  scenario_order <- c("H", "M", "L", "none")
  
  # Define custom order for scen_var (including the variants: dry, wet, none)
  scen_var_order <- c(
    "H_dry", "H_wet", "M_dry", "M_wet", "L_dry", "L_wet",
    "L_none", "none_none"
  )
  
  # Define custom order for scen_var_hor (with horizon)
  scen_var_hor_order <- c(
    "H_dry_2150", "H_dry_2100", "H_dry_2050", 
    "H_wet_2150", "H_wet_2100", "H_wet_2050", 
    "M_dry_2150", "M_dry_2100", "M_dry_2050", 
    "M_wet_2150", "M_wet_2100", "M_wet_2050", 
    "L_dry_2100",
    "L_wet_2100",
    "L_none_2033",
    "none_none_ref", "none_none_hindcast", "none_none_observation"
  )
  
  # Convert scen_var and scen_var_hor to factors with defined levels
  dt[, scenario := factor(scenario, levels = scenario_order)]
  dt[, scen_var := factor(scen_var, levels = scen_var_order)]
  dt[, scen_var_hor := factor(scen_var_hor, levels = scen_var_hor_order)]
  
  return(dt)
}

add_time_period_column <- function(dt, date_col = "date", horizon_col = "horizon", default_horizon = 2005) {
  # Get the numeric year from the date column
  dt[, year := as.numeric(format(get(date_col), "%Y"))]
  
  # Convert horizon to numeric and use default if conversion fails
  vals <- dt[[horizon_col]]
  dt[, horizon_num := ifelse(grepl("^[0-9]{4}$", vals), as.integer(vals), default_horizon)]
  
  # Classify period
  dt[, period := ifelse(
    year >= horizon_num - 14 & year <= horizon_num + 15,
    "simulation",
    "warmup"
  )]
  
  # Optional cleanup
  dt[, c("year", "horizon_num") := NULL]
  
  return(dt)
}


# code to read data from each input dir -------------------------------------
input_dirs <- c(input_dir_prevah, input_dir_larsim, input_dir_wflow)
# Initialize an empty list to store data from each directory
all_discharge_data_list <- list()

# Loop through each input directory
for (dir in input_dirs) {
  cat("Processing directory:", dir, "\n")
  
  for (station_folder in station_list) {
    
    if (grepl("wflow", dir)) {
      # If the directory is for wflow, use the modified station names
      station_folder <- paste0(station_folder, "_Deltares")
    }
    
    # Construct the path to the current subfolder
    station_folder_dir <- file.path(dir, station_folder)
    
    # Check if the subfolder exists
    if (!dir.exists(station_folder_dir)) {
      warning(paste("Subfolder", station_folder, "not found in", dir))
      next  # Skip this subfolder if it doesn't exist
    }
    
    # List all CSV files in the subfolder
    csv_files <- list.files(station_folder_dir, pattern = "\\.csv$", full.names = TRUE)
    
    # Read each CSV file and append the data
    if (length(csv_files) > 0) {
      all_discharge_data_list[[paste0(dir, "_", station_folder)]] <- rbindlist(
        lapply(csv_files, fread), 
        fill = TRUE
      )
    }
  }
}


# Combine all data into one long data.table
knmi_discharge_dt_all <- rbindlist(all_discharge_data_list, fill = TRUE)

# clean the data from Deltares

knmi_discharge_dt_all[scenario == "Hd", scenario := "H"]
knmi_discharge_dt_all[scenario == "Hn", scenario := "H"]
knmi_discharge_dt_all[scenario == "Md", scenario := "M"]
knmi_discharge_dt_all[scenario == "Mn", scenario := "M"]
knmi_discharge_dt_all[scenario == "Ld", scenario := "L"]
knmi_discharge_dt_all[scenario == "Ln", scenario := "L"]

knmi_discharge_dt_all <- add_scenario_horizon_grouping_columns(knmi_discharge_dt_all)
knmi_discharge_dt_all <- add_time_period_column(knmi_discharge_dt_all)

# export processed data --------------------------------------------------
cat("Exporting combined data to RDS and CSV...\n")
file_path_rds <- file.path(output_dir, paste0(output_file_name, ".rds"))
file_path_csv <- file.path(output_dir, paste0(output_file_name, ".csv"))
# Export to .RDS format
#saveRDS(knmi_discharge_dt_all, file_path_rds)

# Export to CSV
#fwrite(knmi_discharge_dt_all, file_path_csv)
