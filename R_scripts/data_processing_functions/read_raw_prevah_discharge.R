library(here)
library(reshape2)
library(data.table)

home_dir <- file.path(here::here())

# input directories
input_dir_knmi <- file.path(home_dir, "Data", "Rheinblick2027", "raw_prevah_output", "routing")
input_dir_hind <- file.path(home_dir, "Data", "Rheinblick2027", "raw_prevah_output", "routing", "hindcast", "CTRL_RUN_WSL_F_2021_g73")
input_dir_obse <- file.path(home_dir, "Data", "Rheinblick2027", "discharge_measurements", "CHBILANZ")

# output directory
output_dir <- file.path(home_dir, "Data", "Rheinblick2027", "processed_prevah_output")

input_file_prefix_knmi <- "Swissrhine200_"
input_file_suffix_knmi <- ".dat"

input_file_hind <- "Swissrhine200_CTRL_RUN_WSL_F_2021_g73.dat"
input_file_obse <- "2289.daily.mean.dat" # Basel station 

output_file_name <- "prevah_discharge"

column_names_prevah <- c("YYYY", "MM", "DD", "Rekingen", "Untersiggenthal", "Rheinfelden", "Basel", "Wiese")
column_names_obse <- c("YYYY", "MM", "DD", "Basel")

# Define which stations to keep (leave empty `c()` to keep all)
selected_stations <- c("Basel")

gebiete <- c(
  "NoW200"
)

scenarios <- c(
  "reference"
)

read_raw_discharge_data <- function(file_path, column_names, horizon = NULL) {
  # Read the raw discharge data from the file
  raw_data <- fread(file_path, header = FALSE)
  
  # Assign column names
  setnames(raw_data, column_names)
  
  # Create Date column
  raw_data[, date := as.Date(paste(YYYY, MM, DD, sep = "-"), format = "%Y-%m-%d")]
  
  # Filter rows between 30 year periods
  if (!is.null(horizon)) {
    raw_data <- raw_data[YYYY >= horizon - 14 & YYYY <= horizon + 15]
  }
  
  return(raw_data)
}

process_discharge_data <- function(data_file, column_names, selected_stations, horizon, scenario, variant, member) {
  # Check if the file exists before reading
  if (file.exists(data_file)) {
    
    # Read the discharge data
    discharge_data <- read_raw_discharge_data(data_file, column_names, horizon)
    
    # Convert from wide to long format
    discharge_long <- melt(discharge_data, id.vars = c("YYYY", "MM", "DD", "date"),
                           variable.name = "station", value.name = "discharge")
    
    # Filter selected stations if specified
    if (length(selected_stations) > 0) {
      discharge_long <- discharge_long[station %in% selected_stations]
    }
    
    # Add metadata columns
    discharge_long[, `:=`(
      horizon = horizon,
      scenario = scenario,
      variant = variant,
      member = member,
      hydro_model = "PREVAH"
    )]
    
    # Select required columns in correct order
    discharge_long <- discharge_long[, .(station, date, discharge, horizon, scenario, 
                                         variant, member, hydro_model)]
    
    return(discharge_long)
    
  } else {
    stop(paste("File not found:", data_file))
  }
}

# Initialize an empty list to store all processed data
discharge_data_list <- list()

# Loop over each area (gebiete)
for (geb in gebiete) {
  # Define the base path for the gebiet
  geb_path <- file.path(input_dir_knmi, geb)
  
  # Loop over each scenario
  for (scen in scenarios) {
    # Extract `horizon` from scenario name (last 4 digits) or use 2005 for "reference"
    if (scen == "reference") {
      scenario <- "R"
      horizon <- 2005
    } else {
      scenario <- substr(scen, 1, 1)
      horizon <- as.numeric(sub(".*([0-9]{4})$", "\\1", scen))
    }
    
    # Extract `variant` (2nd character of scenario, "d", "n", or "none")
    variant <- ifelse(nchar(scen) >= 2 && substr(scen, 2, 2) %in% c("d", "n"), substr(scen, 2, 2), "none")
    
    # Check if the scenario directory exists
    if (dir.exists(geb_path)) {
      
      # List all subfolders inside the scenario folder
      subfolders <- list.dirs(geb_path, recursive = FALSE)
      
      # Filter subfolders matching "scenario_ens1" to "scenario_ens8"
      matching_subfolders <- subfolders[grepl(paste0(scen, "_ens[1-8]$"), basename(subfolders))]
      
      # Loop through the matching subfolders
      for (ens_folder in matching_subfolders) {
        
        # Extract the ensemble member number (ens1 to ens8) as a numeric value
        member <- as.numeric(sub(".*_ens([1-8])$", "\\1", basename(ens_folder)))
        
        # Define the expected file path inside the subfolder (adjust filename if needed)
        data_file <- file.path(ens_folder, paste0(input_file_prefix_knmi, basename(ens_folder), input_file_suffix_knmi))  # Adjust filename if needed
        
        discharge_long <- process_discharge_data(data_file, column_names_prevah, selected_stations, 
                               horizon, 
                               scenario, 
                               variant, 
                               member)
        
        # Append to the list
        discharge_data_list[[length(discharge_data_list) + 1]] <- discharge_long
      }
    } else {
      print(paste("Scenario path does not exist:", geb_path))
    }
  }
}

# hindcast data
data_file <- file.path(input_dir_hind, input_file_hind)
discharge_long <- process_discharge_data(data_file, column_names_prevah, selected_stations, 
                       horizon = 2005, 
                       scenario = "H", 
                       variant = "none",
                       member = 1)

# Append to the list
discharge_data_list[[length(discharge_data_list) + 1]] <- discharge_long

# observed data
data_file <- file.path(input_dir_obse, input_file_obse)
discharge_long <- process_discharge_data(data_file, column_names_obse, selected_stations, 
                       horizon = 2005, 
                       scenario = "O", 
                       variant = "none",
                       member = 1)

# Append to the list
discharge_data_list[[length(discharge_data_list) + 1]] <- discharge_long

# Combine all knmi data into a single data.table
discharge_dt <- rbindlist(discharge_data_list, use.names = TRUE, fill = TRUE)

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
}

# Export to CSV
write.csv2(discharge_dt, file.path(output_dir, paste0(output_file_name, ".csv")), row.names = FALSE, quote = FALSE)

# Export to .RDS format
saveRDS(discharge_dt, file.path(output_dir, paste0(output_file_name, ".rds")))
        
        