library(here)
library(data.table)

# settings ---------------------------------------------------------------

# project directory
home_dir <- file.path(here::here())

run_type <- "future_V1"

# input directories
input_dir_knmi <- file.path(home_dir, "Data", "Rheinblick2027", "raw_prevah_output", paste0("routing", "_", run_type))
input_dir_hind <- file.path(home_dir, "Data", "Rheinblick2027", "raw_prevah_output", "routing_hindcast", "CHBILANZ")
input_dir_obse <- file.path(home_dir, "Data", "Rheinblick2027", "discharge_measurements", "CHBILANZ")

# output directory
output_dir <- file.path(home_dir, "Data", "Rheinblick2027", "processed_prevah_output")

output_file_name <- "prevah_discharge_knmi"

input_file_suffix_knmi <- ".dat"

# import the lists with the filenames and column names to import the routing discharge data
source(file.path(home_dir, "R_scripts", "data_processing_functions", "discharge_measurement_stations.R"))

# Define which stations to keep (leave empty `c()` to keep all)
selected_stations <- c()

all_gebiete <- c(
  "BEN200", "BiS200", "EmW200", "Lim200", "NoW200", "Reu200", "RhN200", "SSG200", "TGl200", "ThS200", "Thu200", "WaS200"
)

gebiete <- c(
  "BEN200", "BiS200", "EmW200", "Lim200", "NoW200", "Reu200", "RhN200", "SSG200", "TGl200", "ThS200", "Thu200", "WaS200"
)

all_scenario_horizons <- c(
  "Hd_2050", "Hd_2100", "Hd_2150",
  "Hn_2050", "Hn_2100", "Hn_2150",
  "Md_2050", "Md_2100", "Md_2150",
  "Mn_2050", "Mn_2100", "Mn_2150",
  "Ld_2100", "Ln_2100",
  "L_2033",
  "reference"
)

scenario_horizons <- c(
  "Hd_2050", "Hd_2100", "Hd_2150",
  "Hn_2050", "Hn_2100", "Hn_2150",
  "Md_2050", "Md_2100", "Md_2150",
  "Mn_2050", "Mn_2100", "Mn_2150",
  "Ld_2100", "Ln_2100"
)
# "Hd_2150" problem with ens 4 Swissrhine

read_hindcast <- FALSE
read_observation <- FALSE

# functions ---------------------------------------------------------------
read_raw_discharge_data <- function(file_path, column_names) {
  # Read the raw discharge data from the file
  raw_data <- fread(file_path, header = FALSE)
  
  # If there are more columns than column_names, append "no_name" placeholders
  if (ncol(raw_data) > length(column_names)) {
    extras <- rep("no_name", ncol(raw_data) - length(column_names))
    full_names <- c(column_names, extras)
  } else {
    full_names <- column_names
  }
  
  # Assign column names
  setnames(raw_data, full_names)
  
  # Create Date column
  raw_data[, date := as.Date(paste(YYYY, MM, DD, sep = "-"), format = "%Y-%m-%d")]
  
  # Identify and remove all "no_name" columns by index
  no_name_cols <- which(names(raw_data) == "no_name")
  if (length(no_name_cols) > 0) {
    raw_data <- raw_data[, -no_name_cols, with = FALSE]
  }
  
  return(raw_data)
}

process_discharge_data <- function(data_file, column_names, selected_stations, horizon, scenario, variant, member, hydro_model, source, run_type) {
  # Check if the file exists before reading
  if (file.exists(data_file)) {
    
    # Read the discharge data
    discharge_data <- read_raw_discharge_data(data_file, column_names)
    
    # Convert from wide to long format
    discharge_long <- melt(discharge_data, id.vars = c("YYYY", "MM", "DD", "date"),
                           variable.name = "station", value.name = "discharge")
    
    # Filter selected stations if specified
    if (length(selected_stations) > 0) {
      discharge_long <- discharge_long[station %in% selected_stations]
    }
    
    # Add metadata columns
    discharge_long[, `:=`(
      unit = "m3/s",
      horizon = horizon,
      scenario = scenario,
      variant = variant,
      member = member,
      hydro_model = hydro_model,
      source = source,
      run_type = run_type
    )]
    
    discharge_long <- add_scenario_horizon_grouping_columns(discharge_long)
    discharge_long <- add_time_period_column(discharge_long)
    
    prevah_date_cols <- c("YYYY", "MM", "DD")
    prevah_general_cols <- c("station")
    rblick_date_cols <- c("date")
    rblick_cols <- c("horizon", "scenario", "variant", "member", "scen_var", "scen_hor", "scen_var_hor", "period", "run_type", "hydro_model", "source")
    
    non_value_col <- c(prevah_date_cols, prevah_general_cols, rblick_date_cols, rblick_cols)
    
    value_cols <- c("discharge", "unit")
    
    # Select required columns in correct order
    col_order <- c(prevah_general_cols, rblick_date_cols, value_cols, rblick_cols)
    
    # Select required columns in correct order
    discharge_long <- discharge_long[, col_order, with = FALSE]
    
    return(discharge_long)
    
  } else {
    warning(paste("File not found:", data_file))
  }
}

# Function to add 'scenario_variant' and 'scenario_variant_horizon' columns with custom ordering
add_scenario_horizon_grouping_columns <- function(dt) {
  dt[, scen_var := paste(scenario, variant, sep = "_")]
  dt[, scen_hor := paste(scenario, horizon, sep = "_")]
  dt[, scen_var_hor := paste(scen_var, horizon, sep = "_")]
  
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

export_discharge_per_scenario_horizon <- function(dt, output_dir, source = "discharge", run_type_name) {
  # Create output folder
  export_dir <- file.path(output_dir, source, run_type_name)
  
  if (!dir.exists(export_dir)) {
    dir.create(export_dir, recursive = TRUE)
  }
  
  # Export one file per scenario-variant-horizon combo
  dt[, {
    # Define filename based on horizon + scenario + variant
    file_name <- if (horizon == "ref") {
      paste0("Reference_", run_type_name)
    } else if (horizon == "hindcast") {
      "Hindcast"
    } else if (horizon == "observation") {
      "Observation"
    } else {
      paste0(scenario,
             ifelse(variant == "dry", "d",
                    ifelse(variant == "wet", "n", "")),
             "_", horizon, "_", run_type_name)
    }
    
    # Define file paths
    file_path_csv <- file.path(export_dir, paste0(file_name, ".csv"))
    file_path_rds <- file.path(export_dir, paste0(file_name, ".rds"))
    
    # Select and write export data
    export_data <- .SD[, .(station, date, discharge, unit, horizon, scenario,
                           variant, member, scen_var, scen_hor, scen_var_hor, period, run_type, hydro_model, source)]
    
    saveRDS(export_data, file_path_rds)
    fwrite(export_data, file_path_csv)
    
  }, by = .(scenario, variant, horizon)]
  
  message("Export discharge per scenario-horizon successful for run_type: ", run_type_name)
}

export_discharge_per_station <- function(dt, output_dir, rblick_stations, run_type_name) {
  
  export_dir <- file.path(output_dir, "discharge", "Rheinblick_stations", run_type_name)
  
  # Ensure the output directory exists
  if (!dir.exists(export_dir)) dir.create(export_dir, recursive = TRUE)
  
  # Filter data based on export ranges
  dt_filtered <- dt[station %in% rblick_stations & period == "simulation"]
  
  
  # Export logic
  dt_filtered[, {
    # Create folder path for the station
    station_dir <- file.path(export_dir, station)
    if (!dir.exists(station_dir)) dir.create(station_dir, recursive = TRUE)
    
    # Determine filename based on conditions
    file_name <- if (horizon == "ref") {
      "Reference"
    } else if (horizon == "hindcast") {
      "Hindcast"
    } else if (horizon == "observation") {
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
  
  # Create a subfolder for the zip files
  zip_dir <- file.path(export_dir, "all_stations")
  if (!dir.exists(zip_dir)) dir.create(zip_dir, recursive = TRUE)
  
  # Zip each station folder and save to all_stations
  stations <- unique(dt_filtered$station)
  for (stn in stations) {
    cat("Zipping station:", stn, "\n")
    stn_dir <- file.path(export_dir, stn)
    zip_file <- file.path(zip_dir, paste0(stn, ".zip"))
    
    # Remove existing zip file if it exists
    if (file.exists(zip_file)) file.remove(zip_file)
    
    # Create zip without path structure
    zip(zipfile = zip_file,
        files = list.files(stn_dir, full.names = TRUE),
        flags = "-j")
  }
  
  message("Export discharge per station successful.")
}

# code to read data -------------------------------------------------------
message("Processing knmi_discharge from:", input_dir_knmi)

# Initialize an empty list to store all processed data
discharge_data_list <- list()

# read knmi discharge data
if (length(scenario_horizons) > 0) {
  # Loop over each area (gebiete)
  for (geb in gebiete) {
    message("Processing gebiet:", geb)
    # Define the base path for the gebiet
    geb_path <- file.path(input_dir_knmi, geb)
    
    # Loop over each scenario
    for (scen in scenario_horizons) {
      # Extract `horizon` from scenario name (last 4 digits) or use 2005 for "reference"
      if (scen == "reference") {
        scenario <- "ref"
        horizon <- "ref"
      } else {
        scenario <- substr(scen, 1, 1)
        horizon <- sub(".*([0-9]{4})$", "\\1", scen)
      }
      
      # Extract `variant` (2nd character of scenario, "d", "n", or "none")
      variant <- ifelse(nchar(scen) >= 2 & substr(scen, 2, 2) == "d", 
                        "dry", 
                        ifelse(substr(scen, 2, 2) == "n", "wet", 
                               "ref"))
      
      if (scen == "L_2033") {
        variant <- "Paris"
      }
      
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
          
          for (prefix in names(knmi_routing_files)) {
            
            station_names <- knmi_routing_files[[prefix]]  # station names for this prefix
            column_names_prevah <- c("YYYY", "MM", "DD", station_names)  # add date columns
            
            # Build the expected data file
            data_file <- file.path(ens_folder, paste0(prefix, "_", basename(ens_folder), input_file_suffix_knmi))
            
            if (file.exists(data_file)) {
              # Process the file
              discharge_long <- process_discharge_data(
                data_file, 
                column_names_prevah, 
                selected_stations,
                horizon,
                scenario,
                variant,
                member,
                hydro_model = "PREVAH",
                source = "WSL",
                run_type = run_type
              )
              
              # Append discharge_long to the list
              discharge_data_list[[length(discharge_data_list) + 1]] <- discharge_long
            }
          } # prefix loop
        } # ens_folder loop
      } else {
        cat("Scenario path does not exist:", geb_path, "\n")
      }
    } # scenario loop
  } # gebiete loop
  scenario_discharge_dt <- rbindlist(discharge_data_list)
  export_discharge_per_scenario_horizon(scenario_discharge_dt, output_dir, "discharge", run_type)
  rm(scenario_discharge_dt)
  gc()
}


if (read_hindcast) {
  message("Processing hindcast data from:", input_dir_hind)
  # hindcast data
  hind_discharge_list <- list()
  for (file_name in names(hind_routing_files)) {
    station_names <- hind_routing_files[[file_name]]  # station names for this file
    column_names_prevah <- c("YYYY", "MM", "DD", station_names)  # add date columns
  
    # Build the expected data file
    data_file <- file.path(input_dir_hind, file_name)
    
    # Process the file
    discharge_long <- process_discharge_data(data_file, column_names_prevah, selected_stations,
                                             horizon = "hindcast",
                                             scenario = "hindcast",
                                             variant = "hindcast",
                                             member = "none",
                                             hydro_model = "PREVAH",
                                             source = "WSL",
                                             run_type = "hindcast")
    
    # Append to the list
    discharge_data_list[[length(discharge_data_list) + 1]] <- discharge_long
    hind_discharge_list[[length(hind_discharge_list) + 1]] <- discharge_long
  }
  
  hind_discharge_dt <- rbindlist(hind_discharge_list)
  export_discharge_per_scenario_horizon(hind_discharge_dt, output_dir, "discharge", run_type_name = "hindcast")
  
  rm(hind_discharge_dt)
  rm(hind_discharge_list)
  gc()
}

if (read_observation) {
  message("Processing observation data from:", input_dir_obse)
  # observed data
  obse_discharge_list = list()
  for (file_name in names(observation_files)) {
    station_names <- observation_files[[file_name]]  # station names for this file
    column_names_obse <- c("YYYY", "MM", "DD", station_names)  # add date columns
  
    data_file <- file.path(input_dir_obse, file_name)
    discharge_long <- process_discharge_data(data_file, column_names_obse, selected_stations,
                                             horizon = "observation",
                                             scenario = "observation",
                                             variant = "observation",
                                             member = "none",
                                             hydro_model = "observation",
                                             source = "BAFU",
                                             run_type = "observation")
    
    # Append to the list
    discharge_data_list[[length(discharge_data_list) + 1]] <- discharge_long
    obse_discharge_list[[length(obse_discharge_list) + 1]] <- discharge_long
  }
  obse_discharge_dt <- rbindlist(obse_discharge_list)
  export_discharge_per_scenario_horizon(obse_discharge_dt, output_dir, "discharge", run_type_name = "observation")
  
  rm(obse_discharge_dt)
  rm(obse_discharge_list)
  gc()
}

# Combine all knmi data into a single data.table
knmi_discharge_dt <- rbindlist(discharge_data_list)

rm(discharge_data_list)
gc()

# change scenario, variant columns for Rheinblick
knmi_discharge_dt[variant %in% c("ref", "Paris", "hindcast", "observation"),
                  c("scenario", "variant") := "none"]

# export processed data --------------------------------------------------
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
}

# Export to individual CSV files per station for KNMI data exchange for scenario-variant-horizon combination
#export_discharge_per_station(knmi_discharge_dt, output_dir, rblick_stations, run_type)


        