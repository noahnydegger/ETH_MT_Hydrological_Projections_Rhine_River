library(here)
library(data.table)

# project directory
home_dir <- file.path(here::here())

# input directories
input_dir_knmi <- file.path(home_dir, "Data", "Rheinblick2027", "raw_prevah_output", "routing")
input_dir_hind <- file.path(home_dir, "Data", "Rheinblick2027", "raw_prevah_output", "routing", "hindcast", "CTRL_RUN_WSL_F_2021_g73")
input_dir_obse <- file.path(home_dir, "Data", "Rheinblick2027", "discharge_measurements", "CHBILANZ")

# output directory
output_dir <- file.path(home_dir, "Data", "Rheinblick2027", "processed_prevah_output")

output_file_name <- "prevah_discharge_knmi"

input_file_suffix_knmi <- ".dat"

# file prefix = c(station names)
knmi_routing_files <- list(
  "Swissrhine200_" = c("Rekingen", "Untersiggenthal", "Rheinfelden", "Basel Rheinhalle", "Wiese"),
  #"Birs200_" = c(),
  "Thur200_" = c("hal", "mur", "rem", "Andelfingen")
)

# file name = c(station names)
hind_routing_files <- list(
  "Swissrhine200_CTRL_RUN_WSL_F_2021_g73.dat" = c("Rekingen", "Untersiggenthal", "Rheinfelden", "Basel Rheinhalle", "Wiese")
)

# file name = c(station names)
observation_files <- list(
  "2289.daily.mean.dat" = c("Basel Rheinhalle")
)

# Define which stations to keep (leave empty `c()` to keep all)
selected_stations <- c()
rblick_stations <- c("Gisingen", "Diepoldsau", "Kennelbach", "Rekingen", "Brugg", 
                     "Mellingen", "Brienzwiler", "Bruegg-Aegerten", "Basel Rheinhalle", 
                     "Riegel", "Schwaibach", "Bad Rotenfels", "Maxau", "Rockenau-SKA", 
                     "Worms", "Raunheim", "Mainz", "Grolsheim", "Kaub", "Kalkofen", 
                     "Cochem", "Andernach", "Menden", "Koeln", "Duesseldorf", 
                     "Hattingen", "Schermbeck", "Lobith", "Andelfingen")

all_gebiete <- c(
  "BEN200", "BiS200", "EmW200", "Lim200", "NoW200", "Reu200", "RhN200", "SSG200", "TGl200", "ThS200", "Thu200", "WaS200"
)

gebiete <- c(
  "NoW200","Thu200"
)

all_scenario_horizons <- c(
  "reference", 
  "L_2033",
  "Md_2050", "Mn_2050", "Hd_2050", "Hn_2050",
  "Md_2100", "Mn_2100", "Hd_2100", "Hn_2100", "Ld_2100", "Ln_2100",
  "Md_2150", "Mn_2150", "Hd_2150", "Hn_2150"
)

scenario_horizons <- c(
  "reference"
)

read_hindcast <- FALSE
read_observation <- TRUE

# functions ---------------------------------------------------------------
read_raw_discharge_data <- function(file_path, column_names) {
  # Read the raw discharge data from the file
  raw_data <- fread(file_path, header = FALSE)
  
  # Assign column names
  setnames(raw_data, column_names)
  
  # Create Date column
  raw_data[, date := as.Date(paste(YYYY, MM, DD, sep = "-"), format = "%Y-%m-%d")]
  
  return(raw_data)
}

process_discharge_data <- function(data_file, column_names, selected_stations, horizon, scenario, variant, member, hydro_model, source) {
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
      source = source
    )]
    
    discharge_long <- add_scenario_horizon_grouping_columns(discharge_long)
    discharge_long <- add_time_period_column(discharge_long)
    
    prevah_date_cols <- c("YYYY", "MM", "DD")
    prevah_general_cols <- c("station")
    rblick_date_cols <- c("date")
    rblick_cols <- c("horizon", "scenario", "variant", "member", "scen_var", "scen_var_hor", "period", "hydro_model", "source")
    
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

export_discharge_per_scenario_horizon <- function(dt, output_dir, source = "discharge", scenario_horizons) {
  # Create output folder
  export_dir <- file.path(output_dir, source)
  
  if (!dir.exists(export_dir)) {
    dir.create(export_dir, recursive = TRUE)
  }
  
  # Export one file per scenario-variant-horizon combo
  dt[, {
    # Define filename based on horizon + scenario + variant
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
    
    # Define file paths
    file_path_csv <- file.path(export_dir, paste0(file_name, ".csv"))
    file_path_rds <- file.path(export_dir, paste0(file_name, ".rds"))
    
    # Select and write export data
    export_data <- .SD[, .(station, date, discharge, unit, horizon, scenario,
                           variant, member, scen_var, scen_var_hor, period, hydro_model, source)]
    
    fwrite(export_data, file_path_csv)
    saveRDS(export_data, file_path_rds)
  }, by = .(scenario, variant, horizon)]
  
  message("Export discharge per scenario-horizon successful for: ", paste(scenario_horizons, collapse = " "))
}

export_discharge_per_station <- function(dt, output_dir, rblick_stations) {
  
  export_dir <- file.path(output_dir, "discharge", "Rheinblick_stations")
  
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
# Loop over each area (gebiete)
for (geb in gebiete) {
  message("Processing gebiet:", geb)
  # Define the base path for the gebiet
  geb_path <- file.path(input_dir_knmi, geb)
  
  # Loop over each scenario
  for (scen in scenario_horizons) {
    # Extract `horizon` from scenario name (last 4 digits) or use 2005 for "reference"
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
          data_file <- file.path(ens_folder, paste0(prefix, basename(ens_folder), input_file_suffix_knmi))
          
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
              source = "WSL"
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

if (read_hindcast) {
  message("Processing hindcast data from:", input_dir_hind)
  # hindcast data
  for (file_name in names(hind_routing_files)) {
    station_names <- hind_routing_files[[file_name]]  # station names for this file
    column_names_prevah <- c("YYYY", "MM", "DD", station_names)  # add date columns
  
    # Build the expected data file
    data_file <- file.path(input_dir_hind, file_name)
    
    # Process the file
    discharge_long <- process_discharge_data(data_file, column_names_prevah, selected_stations,
                                             horizon = "hindcast",
                                             scenario = "none",
                                             variant = "none",
                                             member = "none",
                                             hydro_model = "PREVAH",
                                             source = "WSL")
    
    # Append to the list
    discharge_data_list[[length(discharge_data_list) + 1]] <- discharge_long
  }
}
  
if (read_observation) {
  message("Processing observation data from:", input_dir_obse)
  # observed data
  for (file_name in names(observation_files)) {
    station_names <- observation_files[[file_name]]  # station names for this file
    column_names_obse <- c("YYYY", "MM", "DD", station_names)  # add date columns
  
    data_file <- file.path(input_dir_obse, file_name)
    discharge_long <- process_discharge_data(data_file, column_names_obse, selected_stations,
                                             horizon = "observation",
                                             scenario = "none",
                                             variant = "none",
                                             member = "none",
                                             hydro_model = "observed",
                                             source = "BAFU")
    
    # Append to the list
    discharge_data_list[[length(discharge_data_list) + 1]] <- discharge_long
  }
}

# Combine all knmi data into a single data.table
knmi_discharge_dt <- rbindlist(discharge_data_list, use.names = TRUE, fill = TRUE)

# export processed data --------------------------------------------------
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
}

# Export to CSV files per scenario-variant-horizon combination
export_discharge_per_scenario_horizon(knmi_discharge_dt, output_dir, "discharge", scenario_horizons)

# Export to individual CSV files per station for scenario-variant-horizon combination
export_discharge_per_station(knmi_discharge_dt, output_dir, rblick_stations)


        