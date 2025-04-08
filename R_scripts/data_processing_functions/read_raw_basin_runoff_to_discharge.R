library(here)
library(data.table)

# project directory
home_dir <- file.path(here::here())

# input directories
input_dir_knmi <- file.path(home_dir, "Data", "Rheinblick2027", "raw_prevah_output", "R_KNMI")
input_dir_hind <- file.path(home_dir, "Data", "Rheinblick2027", "raw_prevah_output", "R_KNMI", "hindcast", "CTRL_RUN_WSL_F_2021_g73")

# output directory
output_dir <- file.path(home_dir, "Data", "Rheinblick2027", "processed_prevah_output")

mit_output_file_suffix <- ".mit"

output_name_mit_output <- "prevah_mit_output_knmi"

scenario_horizons <- c(
  "reference", 
  "Hd_2100", "Hn_2100"
)

bsn_stn <- c("Thu200" = "Andelfingen")

stn_area <- c(
  "Andelfingen" = 2000
)

ensenmbles <- c(
  "ens1", "ens2", "ens3", "ens4", "ens5", "ens6", "ens7", "ens8"
)

hydro_model <- "PREVAH"
source <- "WSL"


read_raw_data <- function(file_path) {
  # Read the raw discharge data from the file
  raw_data <- fread(file_path)
  
  # Create Date column
  raw_data[, date := as.Date(paste(YYYY, MM, DD, sep = "-"), format = "%Y-%m-%d")]
  
  return(raw_data)
}

process_mit_data <- function(data_file, horizon, scenario, variant, member, ezg) {
  # Check if the file exists before reading
  if (file.exists(data_file)) {
    
    # Read the discharge data
    mit_data <- read_raw_data(data_file)
    
    value_columns <- setdiff(names(mit_data), c("YYYY", "MM", "DD", "date"))
    
    # Add metadata columns for this specific folder
    mit_data[, `:=`(
      horizon = horizon,
      scenario = scenario,
      variant = variant,
      member = member,
      hydro_model = "PREVAH",
      basin = ezg
    )]
    
    # Select required columns in correct order
    mit_data <- mit_data[, c("basin", "date", "horizon", "scenario", "variant", "member", "hydro_model", value_columns), with = FALSE]
    
    return(mit_data)
    
  } else {
    stop(paste("File not found:", data_file))
  }
}

compute_discharge_from_runoff <- function(dt, stn) {
  
  dt_station <- dt[basin == stn]
  # Compute discharge from runoff
  dt[, discharge := RGES * bsn_area * 24 * 3600 / 1000] # (mm/d) * (m^2) * h/d * s/h / (mm/m)
  
  # Set unit
  dt[, unit := "m3/s"]
  
  return(dt)
}

export_discharge_data <- function(dt, output_dir) {
  
  # Ensure the output directory exists
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # Add filtered date ranges
  dt[, export_start := ifelse(horizon == "ref", as.Date("1991-01-01"), 
                              as.Date(paste0(as.numeric(horizon) - 14, "-01-01")))]
  
  dt[, export_end := ifelse(horizon == "ref", as.Date("2020-12-31"), 
                            as.Date(paste0(as.numeric(horizon) + 15, "-12-31")))]
  
  # Filter data based on export ranges
  dt_filtered <- dt[date >= export_start & date <= export_end]
  
  # Export logic
  dt_filtered[, {
    # Create folder path for the station
    station_dir <- file.path(output_dir, station)
    if (!dir.exists(station_dir)) dir.create(station_dir, recursive = TRUE)
    
    # Determine filename based on conditions
    file_name <- if (horizon == "ref" && scenario == "none") {
      "Reference"
    } else if (horizon == "ref" && scenario == "contr") {
      "Hindcast"
    } else if (horizon == "ref" && scenario == "obs") {
      "Observation"
    } else {
      paste0(scenario, 
             ifelse(variant == "dry", "d", 
                    ifelse(variant == "wet", "n", "")),
             "_", horizon)
    }
    
    # Full file path
    file_path <- file.path(station_dir, paste0(file_name, ".csv"))
    
    # Add station, scenario, variant, and horizon to the data before export
    export_data <- .SD[, .(station, date, discharge, unit, horizon, scenario, 
                           variant, member, hydro_model, source)]
    
    # Export data
    fwrite(export_data, file_path)
    
  }, by = .(station, scenario, variant, horizon)]
  
  message("Export completed successfully.")
}

cat("Processing knmi_mit_output from:", input_dir_knmi, "\n")

# Initialize an empty list to store the data.tables
all_mit_data_list <- list()

if (dir.exists(input_dir_knmi)) {
  
  # List all subfolders inside the scenario folder
  scen_hor_folders <- list.dirs(input_dir_knmi, recursive = FALSE)
}

# import the .mit file for each scenario, ensemble, and area
for (scen in scenario_horizons) {
  
  # Skip if `scen` is not found in any scenario-horizon folder
  matching_folders <- grep(scen, scen_hor_folders, value = TRUE)
  if (length(matching_folders) == 0) next
  
  cat("Processing scenario-horizon:", scen, "\n")
  
  if (scen == "reference") {
    scenario <- "none"
    horizon <- "ref"
  } else {
    scenario <- substr(scen, 1, 1)
    horizon <- sub(".*([0-9]{4})$", "\\1", scen)
  }
  
  # Extract `variant` (2nd character of scenario, "d", "n", or "none")
  variant <- ifelse(nchar(scen) >= 2 & substr(scen, 2, 2) == "d", 
                    "dry", 
                    ifelse(substr(scen, 2, 2) == "n", "wet", 
                           "none"))
  
  # Loop over matching folders
  for (scen_ensm_dir in matching_folders) {
    cat("Processing scenario-ensemble:", basename(scen_ensm_dir), "\n")
    
    # Extract the ensemble member number (ens1 to ens8) as a numeric value and as a string
    member <- as.numeric(sub(".*_ens([1-8])$", "\\1", basename(scen_ensm_dir)))
    ensm <- paste0("ens", member)
    
    # List all subfolders (gebiete) in the matched scenario-ensemble folder
    gebiete_folders <- list.dirs(scen_ensm_dir, recursive = FALSE)
    
    # Loop over the gebiete folders
    for (ezg_dir in gebiete_folders) {
      ezg <- basename(ezg_dir)
      
      # First process .mit files
      mit_file <- file.path(ezg_dir, paste0(ezg, mit_output_file_suffix))
      
      mit_data <- process_mit_data(mit_file, horizon, scenario, variant, member, ezg)
      
      # Append this to the list of all mit data
      all_mit_data_list[[length(all_mit_data_list) + 1]] <- mit_data
      
    } # gebiete_folders loop
  } # scen_ensm loop
} # scenario_horizons loop

# hindcast data
cat("Processing hindcast data\n")
horizon <- "hindcast"
scenario <- "none" # for control run
variant <- "none"
member <- "none"
hydro_model <- "PREVAH"
source <- "WSL"
# List all subfolders (gebiete) in the matched scenario-ensemble folder
gebiete_folders <- list.dirs(input_dir_hind, recursive = FALSE)
# Loop over the gebiete folders
for (ezg_dir in gebiete_folders) {
  ezg <- basename(ezg_dir)
  
  if (ezg %in% no_knmi_gebiete) next # skip ezg that are not part of the Rhine
  
  # First process .mit files
  mit_file <- file.path(ezg_dir, paste0(ezg, mit_output_file_suffix))
  
  mit_data <- process_mit_data(mit_file, horizon, scenario, variant, member, ezg)
  
  # Append this to the list of all mit data
  all_mit_data_list[[length(all_mit_data_list) + 1]] <- mit_data
  
} # gebiete_folders loop

# Combine all the data.tables into one long data.table
knmi_mit_output_dt <- rbindlist(all_mit_data_list)

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
}

# Export to CSV
write.csv2(knmi_mit_output_dt, file.path(output_dir, paste0(output_name_mit_output, ".csv")), row.names = FALSE, quote = FALSE)

# Export to .RDS format
saveRDS(knmi_mit_output_dt, file.path(output_dir, paste0(output_name_mit_output, ".rds")))
