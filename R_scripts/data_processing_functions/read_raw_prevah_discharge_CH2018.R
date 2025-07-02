library(here)
library(data.table)

# settings ---------------------------------------------------------------

# project directory
home_dir <- file.path(here::here())

run_type <- "CH2018"

# input directories
input_dir_knmi <- file.path(home_dir, "Data", "Rheinblick2027", "raw_prevah_output", paste0("routing", "_", run_type))
input_dir_ch18 <- file.path(home_dir, "Data", "Rheinblick2027", "raw_prevah_output", paste0("routing", "_", run_type))

# output directory
output_dir <- file.path(home_dir, "Data", "Rheinblick2027", "processed_prevah_output")

output_file_name <- "prevah_discharge_CH2018"

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
c(
  
)
all_chains <- c(
  "CLMCOM-CCLM4-HADGEM-EUR44-RCP85",
  "CLMCOM-CCLM5-ECEARTH-EUR44-RCP85",
  "CLMCOM-CCLM5-HADGEM-EUR44-RCP85",
  "CLMCOM-CCLM5-MIROC-EUR44-RCP85",
  "CLMCOM-CCLM5-MPIESM-EUR44-RCP85",
  "DMI-HIRHAM-ECEARTH-EUR11-RCP26",
  "DMI-HIRHAM-ECEARTH-EUR11-RCP45",
  "DMI-HIRHAM-ECEARTH-EUR11-RCP85",
  "DMI-HIRHAM-ECEARTH-EUR44-RCP45",
  "DMI-HIRHAM-ECEARTH-EUR44-RCP85",
  "KNMI-RACMO-ECEARTH-EUR44-RCP45",
  "KNMI-RACMO-ECEARTH-EUR44-RCP85",
  "KNMI-RACMO-HADGEM-EUR44-RCP26",
  "KNMI-RACMO-HADGEM-EUR44-RCP45",
  "KNMI-RACMO-HADGEM-EUR44-RCP85",
  "SMHI-RCA-CCCMA-EUR44-RCP45",
  "SMHI-RCA-CCCMA-EUR44-RCP85",
  "SMHI-RCA-ECEARTH-EUR11-RCP26",
  "SMHI-RCA-ECEARTH-EUR11-RCP45",
  "SMHI-RCA-ECEARTH-EUR11-RCP85",
  "SMHI-RCA-ECEARTH-EUR44-RCP26",
  "SMHI-RCA-ECEARTH-EUR44-RCP45",
  "SMHI-RCA-ECEARTH-EUR44-RCP85",
  "SMHI-RCA-HADGEM-EUR11-RCP45",
  "SMHI-RCA-HADGEM-EUR11-RCP85",
  "SMHI-RCA-HADGEM-EUR44-RCP26",
  "SMHI-RCA-HADGEM-EUR44-RCP45",
  "SMHI-RCA-HADGEM-EUR44-RCP85",
  "SMHI-RCA-MIROC-EUR44-RCP26",
  "SMHI-RCA-MIROC-EUR44-RCP45",
  "SMHI-RCA-MIROC-EUR44-RCP85",
  "SMHI-RCA-MPIESM-EUR11-RCP45",
  "SMHI-RCA-MPIESM-EUR11-RCP85",
  "SMHI-RCA-MPIESM-EUR44-RCP26",
  "SMHI-RCA-MPIESM-EUR44-RCP45",
  "SMHI-RCA-MPIESM-EUR44-RCP85",
  "SMHI-RCA-NORESM-EUR44-RCP26",
  "SMHI-RCA-NORESM-EUR44-RCP45",
  "SMHI-RCA-NORESM-EUR44-RCP85"
)

chains_sel <- c(
  "CLMCOM-CCLM4-HADGEM-EUR44-RCP85",
  "CLMCOM-CCLM5-ECEARTH-EUR44-RCP85",
  "CLMCOM-CCLM5-HADGEM-EUR44-RCP85",
  "CLMCOM-CCLM5-MIROC-EUR44-RCP85",
  "CLMCOM-CCLM5-MPIESM-EUR44-RCP85",
  "DMI-HIRHAM-ECEARTH-EUR11-RCP26",
  "DMI-HIRHAM-ECEARTH-EUR11-RCP45",
  "DMI-HIRHAM-ECEARTH-EUR11-RCP85",
  "DMI-HIRHAM-ECEARTH-EUR44-RCP45",
  "DMI-HIRHAM-ECEARTH-EUR44-RCP85",
  "KNMI-RACMO-ECEARTH-EUR44-RCP45",
  "KNMI-RACMO-ECEARTH-EUR44-RCP85",
  "KNMI-RACMO-HADGEM-EUR44-RCP26",
  "KNMI-RACMO-HADGEM-EUR44-RCP45",
  "KNMI-RACMO-HADGEM-EUR44-RCP85",
  "SMHI-RCA-CCCMA-EUR44-RCP45",
  "SMHI-RCA-CCCMA-EUR44-RCP85",
  "SMHI-RCA-ECEARTH-EUR11-RCP26",
  "SMHI-RCA-ECEARTH-EUR11-RCP45",
  "SMHI-RCA-ECEARTH-EUR11-RCP85",
  "SMHI-RCA-ECEARTH-EUR44-RCP26",
  "SMHI-RCA-ECEARTH-EUR44-RCP45",
  "SMHI-RCA-ECEARTH-EUR44-RCP85",
  "SMHI-RCA-HADGEM-EUR11-RCP45",
  "SMHI-RCA-HADGEM-EUR11-RCP85",
  "SMHI-RCA-HADGEM-EUR44-RCP26",
  "SMHI-RCA-HADGEM-EUR44-RCP45",
  "SMHI-RCA-HADGEM-EUR44-RCP85",
  "SMHI-RCA-MIROC-EUR44-RCP26",
  "SMHI-RCA-MIROC-EUR44-RCP45",
  "SMHI-RCA-MIROC-EUR44-RCP85",
  "SMHI-RCA-MPIESM-EUR11-RCP45",
  "SMHI-RCA-MPIESM-EUR11-RCP85",
  "SMHI-RCA-MPIESM-EUR44-RCP26",
  "SMHI-RCA-MPIESM-EUR44-RCP45",
  "SMHI-RCA-MPIESM-EUR44-RCP85",
  "SMHI-RCA-NORESM-EUR44-RCP26",
  "SMHI-RCA-NORESM-EUR44-RCP45"
  #"SMHI-RCA-NORESM-EUR44-RCP85"
)
# "Hd_2150" problem with ens 4 Swissrhine

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

process_discharge_data <- function(data_file, column_names, selected_stations, chain, team, rcm, gcm, res, rcp, hydro_model, source, run_type) {
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
      chain = chain,
      team = team,
      rcm = rcm,
      gcm = gcm,
      resolution = res,
      rcp = rcp,
      hydro_model = hydro_model,
      source = source,
      run_type = run_type
    )]
    
    discharge_long <- add_time_period_column(discharge_long)
    
    prevah_date_cols <- c("YYYY", "MM", "DD")
    prevah_general_cols <- c("station")
    ch2018_date_cols <- c("date")
    ch2018_cols <- c("chain", "period", "run_type", "hydro_model", "source")
    
    non_value_col <- c(prevah_date_cols, prevah_general_cols, ch2018_date_cols, ch2018_cols)
    
    value_cols <- c("discharge", "unit")
    
    # Select required columns in correct order
    col_order <- c(prevah_general_cols, ch2018_date_cols, value_cols, ch2018_cols)
    
    # Select required columns in correct order
    discharge_long <- discharge_long[, col_order, with = FALSE]
    
    return(discharge_long)
    
  } else {
    warning(paste("File not found:", data_file))
  }
}

add_time_period_column <- function(dt, date_col = "date") {
  # Extract year
  dt[, year := as.numeric(format(get(date_col), "%Y"))]
  
  # Classify periods
  dt[, period := fifelse(
    year < 1981, "warmup",
    fifelse(year <= 2010, "reference", "simulation")
  )]
  
  # Clean up
  dt[, year := NULL]
  
  return(dt)
}

export_discharge_per_chain <- function(dt, output_dir, source = "discharge", chain, run_type_name) {
  # Create output folder
  export_dir <- file.path(output_dir, source, run_type_name)
  if (!dir.exists(export_dir)) {
    dir.create(export_dir, recursive = TRUE)
  }
  
  file_name <- chain
  
  # Define file paths
  file_path_csv <- file.path(export_dir, paste0(file_name, ".csv"))
  file_path_rds <- file.path(export_dir, paste0(file_name, ".rds"))
  
  # Select export columns
  export_data <- dt[, .(
    station, date, discharge, unit, chain,
    team, rcm, gcm, resolution, rcp,
    period, run_type, hydro_model, source
  )]
  
  # Write to file
  saveRDS(export_data, file_path_rds)
  fwrite(export_data, file_path_csv)
  
  message("Export discharge per chain successful:", chain)
}

# code to read data -------------------------------------------------------
message("Processing CH2018 discharge from:", input_dir_ch18)

# Initialize an empty list to store all processed data
discharge_data_list <- list()

# read knmi discharge data
if (length(chains_sel) > 0) {
  # Loop over each scenario
  for (chain in chains_sel) {
    message("Processing chain:", chain)
    
    # Split by "-"
    parts <- strsplit(chain, "-", fixed = TRUE)[[1]]
    
    # Assign to named variables
    team <- parts[1]
    rcm  <- parts[2]
    gcm  <- parts[3]
    res  <- parts[4]
    rcp  <- parts[5]
    
    chain_g73 <- paste0(chain, "_g73")
    
    # Loop over each area (gebiete)
    for (geb in gebiete) {
      message("Processing gebiet:", geb)
      # Define the base path for the gebiet
      geb_chain_path <- file.path(input_dir_ch18, geb, chain_g73)
    
      # Check if the scenario directory exists
      if (dir.exists(geb_chain_path)) {
        
        for (prefix in names(knmi_routing_files)) {
          
          station_names <- knmi_routing_files[[prefix]]  # station names for this prefix
          column_names_prevah <- c("YYYY", "MM", "DD", station_names)  # add date columns
          
          # Build the expected data file
          data_file <- file.path(geb_chain_path, paste0(prefix, "_", chain_g73, input_file_suffix_knmi))
          
          if (file.exists(data_file)) {
            # Process the file
            discharge_long <- process_discharge_data(
              data_file, 
              column_names_prevah, 
              selected_stations,
              chain = chain,
              team = team,
              rcm = rcm,
              gcm = gcm,
              res = res,
              rcp = rcp,
              hydro_model = "PREVAH",
              source = "WSL",
              run_type = run_type
            )
            
            # Append discharge_long to the list
            discharge_data_list[[length(discharge_data_list) + 1]] <- discharge_long
          }
        } # prefix loop
      } else {
        cat("gebiet-chain path does not exist:", geb_chain_path, "\n")
      }
    } # scenario loop
    chain_discharge_dt <- rbindlist(discharge_data_list)
    export_discharge_per_chain(chain_discharge_dt, output_dir, "discharge", chain, run_type)
    discharge_data_list <- list()  # Reset the list for the next chain
  } # gebiete loop
}

        