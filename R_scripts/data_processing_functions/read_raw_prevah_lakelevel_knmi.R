library(here)
library(data.table)

# project directory
home_dir <- file.path(here::here())

run_type <- "future_V1"

# input directories
input_dir_knmi <- file.path(home_dir, "Data", "Rheinblick2027", "raw_prevah_output", paste0("routing", "_", run_type))
input_dir_hind <- file.path(home_dir, "Data", "Rheinblick2027", "raw_prevah_output", "routing_hindcast", "CHBILANZ")

# output directory
output_dir <- file.path(home_dir, "Data", "Rheinblick2027", "processed_prevah_output")

output_file_name <- "prevah_lakelevel_knmi"

input_file_suffix_knmi <- ".dat"

# import the lists with the filenames and column names to import the routing discharge data
#source(file.path(home_dir, "R_scripts", "data_processing_functions", "discharge_measurement_stations.R"))
knmi_routing_files <- list(
  "Neuhausen200" = c(
    "no_name",    # col 1
    "no_name",    # col 2
    "Bodensee",   # col 3
    "no_name"     # col 4
  )
)

# Define which stations to keep (leave empty `c()` to keep all)
selected_lakes <- c()

all_gebiete <- c(
  "BEN200", "BiS200", "EmW200", "Lim200", "NoW200", "Reu200", "RhN200", "SSG200", "TGl200", "ThS200", "Thu200", "WaS200"
)

gebiete <- c(
  "RhN200"
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
  "Ld_2100", "Ln_2100",
  "L_2033",
  "reference"
)

read_hindcast <- FALSE
read_observation <- FALSE

# functions ---------------------------------------------------------------
read_raw_lakelevel_data <- function(file_path, column_names) {
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

process_lakelevel_data <- function(data_file, column_names, selected_lakes, horizon, scenario, variant, member, hydro_model, source, run_type) {
  # Check if the file exists before reading
  if (file.exists(data_file)) {
    
    # Read the discharge data
    lake_data <- read_raw_lakelevel_data(data_file, column_names)
    
    # Convert from wide to long format
    lake_long <- melt(lake_data, id.vars = c("YYYY", "MM", "DD", "date"),
                           variable.name = "lake", value.name = "level")
    
    # Filter selected lakes if specified
    if (length(selected_lakes) > 0) {
      lake_long <- lake_long[lake %in% selected_lakes]
    }
    
    # Add metadata columns
    lake_long[, `:=`(
      unit = "m",
      horizon = horizon,
      scenario = scenario,
      variant = variant,
      member = member,
      hydro_model = hydro_model,
      source = source,
      run_type = run_type
    )]
    
    lake_long <- add_scenario_horizon_grouping_columns(lake_long)
    lake_long <- add_time_period_column(lake_long)
    
    prevah_date_cols <- c("YYYY", "MM", "DD")
    prevah_general_cols <- c("lake")
    rblick_date_cols <- c("date")
    rblick_cols <- c("horizon", "scenario", "variant", "member", "scen_var", "scen_hor", "scen_var_hor", "period", "run_type", "hydro_model", "source")
    
    non_value_col <- c(prevah_date_cols, prevah_general_cols, rblick_date_cols, rblick_cols)
    
    value_cols <- c("level", "unit")
    
    # Select required columns in correct order
    col_order <- c(prevah_general_cols, rblick_date_cols, value_cols, rblick_cols)
    
    # Select required columns in correct order
    lake_long <- lake_long[, col_order, with = FALSE]
    
    return(lake_long)
    
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

export_lakelevel_per_scenario_horizon <- function(dt, output_dir, source = "lakelevel", run_type_name) {
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
    export_data <- .SD[, .(lake, date, level, unit, horizon, scenario,
                           variant, member, scen_var, scen_hor, scen_var_hor, period, run_type, hydro_model, source)]
    
    saveRDS(export_data, file_path_rds)
    fwrite(export_data, file_path_csv)
    
  }, by = .(scenario, variant, horizon)]
  
  message("Export lakelevel per scenario-horizon successful for run_type: ", run_type_name)
}

# code to read data -------------------------------------------------------
message("Processing knmi_lakelevel from:", input_dir_knmi)

# Initialize an empty list to store all processed data
lakelevel_data_list <- list()

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
            
            lake_names <- knmi_routing_files[[prefix]]  # lake names for this prefix
            column_names_prevah <- c("YYYY", "MM", "DD", lake_names)  # add date columns
            
            # Build the expected data file
            data_file <- file.path(ens_folder, paste0(prefix, input_file_suffix_knmi))
            
            if (file.exists(data_file)) {
              # Process the file
              lake_long <- process_lakelevel_data(
                data_file, 
                column_names_prevah, 
                selected_lakes,
                horizon,
                scenario,
                variant,
                member,
                hydro_model = "PREVAH",
                source = "WSL",
                run_type = run_type
              )
              
              # Append discharge_long to the list
              lakelevel_data_list[[length(lakelevel_data_list) + 1]] <- lake_long
            }
          } # prefix loop
        } # ens_folder loop
      } else {
        cat("Scenario path does not exist:", geb_path, "\n")
      }
    } # scenario loop
  } # gebiete loop
  scenario_lakelevel_dt <- rbindlist(lakelevel_data_list)
  export_lakelevel_per_scenario_horizon(scenario_lakelevel_dt, output_dir, "lakelevel", run_type)
}


if (read_hindcast) {
  message("Processing hindcast data from:", input_dir_hind)
  # hindcast data
  hind_lakelevel_list <- list()
  for (file_name in names(hind_routing_files)) {
    lake_names <- hind_routing_files[[file_name]]  # lake names for this file
    column_names_prevah <- c("YYYY", "MM", "DD", lake_names)  # add date columns
  
    # Build the expected data file
    data_file <- file.path(input_dir_hind, file_name)
    
    # Process the file
    lake_long <- process_discharge_data(data_file, column_names_prevah, selected_lakes,
                                             horizon = "hindcast",
                                             scenario = "hindcast",
                                             variant = "hindcast",
                                             member = "none",
                                             hydro_model = "PREVAH",
                                             source = "WSL",
                                             run_type = "hindcast")
    
    # Append to the list
    lakelevel_data_list[[length(lakelevel_data_list) + 1]] <- lake_long
    hind_lakelevel_list[[length(hind_lakelevel_list) + 1]] <- lake_long
  }
  
  hind_lakelevel_dt <- rbindlist(hind_lakelevel_list)
  export_lakelevel_per_scenario_horizon(hind_lakelevel_dt, output_dir, "lakelevel", run_type_name = "hindcast")
}

if (read_observation) {
  message("Processing observation data from:", input_dir_obse)
  # observed data
  obse_lakelevel_list = list()
  for (file_name in names(observation_files)) {
    lake_names <- observation_files[[file_name]]  # lake names for this file
    column_names_obse <- c("YYYY", "MM", "DD", lake_names)  # add date columns
  
    data_file <- file.path(input_dir_obse, file_name)
    lake_long <- process_lakelevel_data(data_file, column_names_obse, selected_lakes,
                                             horizon = "observation",
                                             scenario = "observation",
                                             variant = "observation",
                                             member = "none",
                                             hydro_model = "observation",
                                             source = "BAFU",
                                             run_type = "observation")
    
    # Append to the list
    lakelevel_data_list[[length(lakelevel_data_list) + 1]] <- lake_long
    obse_lakelevel_list[[length(obse_lakelevel_list) + 1]] <- lake_long
  }
  obse_lakelevel_dt <- rbindlist(obse_lakelevel_list)
  export_lakelevel_per_scenario_horizon(obse_lakelevel_dt, output_dir, "lakelevel", run_type_name = "observation")
}

# Combine all knmi data into a single data.table
knmi_lakelevel_dt <- rbindlist(lakelevel_data_list)

# export processed data --------------------------------------------------
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
}

        