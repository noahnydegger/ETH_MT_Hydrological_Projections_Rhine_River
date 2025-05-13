library(here)
library(data.table)

# project directory
home_dir <- file.path(here::here())

run_type <- "with_glac_sdbc"

# input directories
input_dir_knmi <- file.path(home_dir, "Data", "Rheinblick2027", "raw_prevah_output", paste0("R_KNMI", "_", run_type))
input_dir_hind <- file.path(home_dir, "Data", "Rheinblick2027", "raw_prevah_output", "R_KNMI_hindcast", "CTRL_RUN_WSL_F_2021_g73")

# output directory
output_dir <- file.path(home_dir, "Data", "Rheinblick2027", "processed_prevah_output")

mit_output_file_suffix <- ".mit"
meteo_stat_file_suffix_knmi <- "_full.stats"
meteo_stat_file_suffix_hind <- "_full.stat"

output_name_mit_output <- "prevah_mit_output_knmi"
output_name_meteo_stat <- "prevah_meteo_stat_knmi"

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
  "Ld_2100", "Ln_2100",
  "Md_2150"
)
read_hindcast <- FALSE

ensembles <- paste0("ens", 1:8)

meteo_variables_knmi <- c(
  "tair", "prec", "radg", "sund", "rhum", "wspd", "sdbc"
)

meteo_variables_hind <- c(
  "temp", "prec", "rad_", "ssd_", "relh", "wind", "sdbc"
)

no_meteo_gebiete <- c(
  "RhB200", "RhD200", "RhN200", "RhR200", "AaB200", "AaH200", "AaU200", "ASe200", "CH_200", "Inn200", "JBN200", "ReM200", "Rho200", "Tic200", "TTB200"
)

knmi_gebiete <- c(
  "TGl200", "ThS200", "BEN200", "BiS200", "Bod400", "EmW200", "HiR200", "LaP200", "Lim200", "NeS200", "NoW200", "Rhb200", "KEm200", "SeD200","SSG200", "Thu200", "VoA200", "VoR200", "WaS200"

)

no_knmi_gebiete <- c(
  "Brg500", "Eng200", "Gen500", "Jur200", "Kru200", "LaL200", "MaV200", "Mer500", "Pos200", "Rom500", "TiB200", "Tre200", "Wal200", # rest of switzerland
  "AaB200", "AaH200", "AaU200", "ASe200", "CH_200", "Inn200", "JBN200", "ReM200", "Rho200", "Tic200", "TTB200" # routing
)

# functions -------------------------------------------------
read_raw_data <- function(file_path) {
  # Read the raw discharge data from the file
  raw_data <- fread(file_path)
  
  # Create Date column
  raw_data[, date := as.Date(paste(YYYY, MM, DD, sep = "-"), format = "%Y-%m-%d")]
  
  return(raw_data)
}

process_mit_data <- function(data_file, horizon, scenario, variant, member, ezg, run_type) {
  # Check if the file exists before reading
  if (file.exists(data_file)) {
    
    # Read the discharge data
    mit_data <- read_raw_data(data_file)
    
    # Add metadata columns for this specific folder
    mit_data[, `:=`(
      horizon = horizon,
      scenario = scenario,
      variant = variant,
      member = member,
      hydro_model = "PREVAH",
      source = "WSL",
      basin = ezg,
      run_type = run_type
    )]
    
    mit_data <- add_scenario_horizon_grouping_columns(mit_data)
    mit_data <- add_time_period_column(mit_data)
    
    prevah_date_cols <- c("YYYY", "MM", "DD")
    prevah_general_cols <- c("basin")
    rblick_date_cols <- c("date")
    rblick_cols <- c("horizon", "scenario", "variant", "member", "scen_var", "scen_var_hor", "period", "run_type", "hydro_model", "source")
    
    non_value_col <- c(prevah_date_cols, prevah_general_cols, rblick_date_cols, rblick_cols)
    
    value_cols <- setdiff(names(mit_data), non_value_col)
    
    # Select required columns in correct order
    col_order <- c(prevah_general_cols, rblick_date_cols, rblick_cols, value_cols)
    
    # Select required columns in correct order
    mit_data <- mit_data[, col_order, with = FALSE]
    
    return(mit_data)
    
  } else {
    stop(paste("File not found:", data_file))
  }
}

process_meteo_stats_data <- function(ezg_dir, meteo_variables, meteo_stat_file_suffix, horizon, scenario, variant, member, ezg, run_type) {
  
  # Initialize an empty data.table to store combined meteo data
  all_meteo_data_dt <- data.table()
  # Check if the file exists before reading
  
  for (var in meteo_variables) {
    # Construct the file path
    meteo_file <- file.path(ezg_dir, paste0(var, meteo_stat_file_suffix))
    
    # Check if the meteo file exists before reading
    if (file.exists(meteo_file)) {
      
      # Import data from the .stats file
      meteo_data <- read_raw_data(meteo_file)
      
      # If var is 'sdbc', rename it to 'sund'
      if (var == "sdbc") var <- "sund"
      
      # Rename meteo-specific columns with 'var_' prefix
      old_meteo_cols <- c("MIN", "MAX", "AVG", "STDEV")
      new_meteo_cols <- paste0(var, "_", c("min", "max", "avg", "std"))
      setnames(meteo_data, old = old_meteo_cols, new = new_meteo_cols)
      
      # Add metadata columns
      meteo_data[, `:=`(
        horizon = horizon,
        scenario = scenario,
        variant = variant,
        member = member,
        hydro_model = "PREVAH",
        source = "WSL",
        basin = ezg,
        run_type = run_type
      )]
      
      # Merge only meteo columns on "date"
      if (nrow(all_meteo_data_dt) == 0) {
        all_meteo_data_dt <- meteo_data  # First dataset initializes the structure
      } else {
        all_meteo_data_dt <- merge(
          all_meteo_data_dt, 
          meteo_data[, c("date", new_meteo_cols), with = FALSE], 
          by = "date", 
          all = TRUE
        )
      }
    } else {
      warning(paste("File not found:", meteo_file))
    }
  } # meteo_variables loop
  
  all_meteo_data_dt <- add_scenario_horizon_grouping_columns(all_meteo_data_dt)
  all_meteo_data_dt <- add_time_period_column(all_meteo_data_dt)
  
  prevah_date_cols <- c("YYYY", "MM", "DD")
  prevah_general_cols <- c("basin")
  rblick_date_cols <- c("date")
  rblick_cols <- c("horizon", "scenario", "variant", "member", "scen_var", "scen_var_hor", "period", "run_type", "hydro_model", "source")
  
  non_value_col <- c(prevah_date_cols, prevah_general_cols, rblick_date_cols, rblick_cols)
  
  value_cols <- setdiff(names(all_meteo_data_dt), non_value_col)
  
  # Select required columns in correct order
  col_order <- c(prevah_general_cols, rblick_date_cols, rblick_cols, value_cols)
  all_meteo_data_dt <- all_meteo_data_dt[, col_order, with = FALSE]
  
  
  return(all_meteo_data_dt)
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

# Function to change row entries in a column based on old-to-new value mapping
change_row_entries <- function(dt, column, row_value_map) {
  # Loop through each old value and replace it with the corresponding new value
  for (old_value in names(row_value_map)) {
    new_value <- row_value_map[[old_value]]
    dt[get(column) == old_value, (column) := new_value]
  }
  
  return(dt)
}

export_to_rds_csv <- function(dt, output_dir, source_folder, scen_hor, run_type) {
  
  output_dir <- file.path(output_dir, source_folder, run_type)
  
  # Ensure the output directory exists
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # Define file paths
  rds_path <- file.path(output_dir, paste0(scen_hor, "_", source_folder, "_", run_type, ".rds"))
  csv_path <- file.path(output_dir, paste0(scen_hor, "_", source_folder, "_", run_type, ".csv"))
  
  # Export to .RDS format
  saveRDS(dt, rds_path)
  
  # Export to CSV using fwrite with comma separator
  fwrite(dt, csv_path)
  
  cat(scen_hor, source_folder ,"exported", "\n")
}

# code --------------------------------------------------
cat("Processing knmi_mit_output from:", input_dir_knmi, "\n")

# Initialize an empty list to store the data.tables
all_mit_data_list <- list()
all_meteo_data_list <- list()

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
  
  scenario_mit_output_list <- list()
  scenario_meteo_stat_list <- list()
  
  if (scen == "reference") {
    scenario <- "none"
    horizon <- "ref"
  } else {
    scenario <- substr(scen, 1, 1)
    horizon <- as.numeric(sub(".*([0-9]{4})$", "\\1", scen))
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
      
      mit_data <- process_mit_data(mit_file, horizon, scenario, variant, member, ezg, run_type)
      
      # Append this to the list of all mit data
      all_mit_data_list[[length(all_mit_data_list) + 1]] <- mit_data
      scenario_mit_output_list[[length(scenario_mit_output_list) + 1]] <- mit_data
      
      if (!(ezg %in% no_meteo_gebiete)) {
        # Process meteo statistics data
        all_meteo_data_dt <- process_meteo_stats_data(ezg_dir, meteo_variables_knmi, meteo_stat_file_suffix_knmi, horizon, scenario, variant, member, ezg, run_type)
        
        all_meteo_data_list[[length(all_meteo_data_list) + 1]] <- all_meteo_data_dt
        scenario_meteo_stat_list[[length(scenario_meteo_stat_list) + 1]] <- all_meteo_data_dt
        
      } # no_meteo_gebiete check
    } # gebiete_folders loop
  } # scen_ensm loop
  
  # combine ens data to one data.table, add columns
  # export dt to .RDS and .csv
  export_to_rds_csv(
    rbindlist(scenario_mit_output_list), 
    output_dir, 
    "mit_output", 
    scen,
    run_type
  )
  export_to_rds_csv(
    rbindlist(scenario_meteo_stat_list), 
    output_dir, 
    "meteo_stat", 
    scen,
    run_type
  )
} # scenario_horizons loop

if (read_hindcast) {
  # hindcast data
  cat("Processing hindcast data\n")
  horizon <- "hindcast"
  scenario <- "none" # for control run
  variant <- "none"
  member <- "none"
  run_type_hind <- "hindcast"
  
  hind_mit_output_list <- list()
  hind_meteo_stat_list <- list()
  
  # List all subfolders (gebiete) in the matched scenario-ensemble folder
  gebiete_folders <- list.dirs(input_dir_hind, recursive = FALSE)
  # Loop over the gebiete folders
  for (ezg_dir in gebiete_folders) {
    ezg <- basename(ezg_dir)
    
    if (ezg %in% no_knmi_gebiete) next # skip ezg that are not part of the Rhine
    
    # First process .mit files
    mit_file <- file.path(ezg_dir, paste0(ezg, mit_output_file_suffix))
    
    mit_data <- process_mit_data(mit_file, horizon, scenario, variant, member, ezg, run_type_hind)
    
    # Append this to the list of all mit data
    all_mit_data_list[[length(all_mit_data_list) + 1]] <- mit_data
    hind_mit_output_list[[length(hind_mit_output_list) + 1]] <- mit_data
    
    if (!(ezg %in% no_meteo_gebiete)) {
      # Process meteo statistics data
      all_meteo_data_dt <- process_meteo_stats_data(ezg_dir, meteo_variables_hind, meteo_stat_file_suffix_hind, horizon, scenario, variant, member, ezg, run_type_hind)
      
      # Create a named vector for mapping
      replacement_map <- setNames(meteo_variables_knmi, meteo_variables_hind)
      
      # Update column names in the data table
      setnames(all_meteo_data_dt, 
               old = names(all_meteo_data_dt), 
               new = stringr::str_replace_all(names(all_meteo_data_dt), replacement_map)
      )
      
      all_meteo_data_list[[length(all_meteo_data_list) + 1]] <- all_meteo_data_dt
      hind_meteo_stat_list[[length(hind_meteo_stat_list) + 1]] <- all_meteo_data_dt
      
    } # no_meteo_gebiete check
  } # gebiete_folders loop
  
  hind_mit_output_dt <- rbindlist(hind_mit_output_list)
  hind_meteo_stat_dt <- rbindlist(hind_meteo_stat_list)
  
  row_value_map <- c(  # old_value = new_value
    "Bod200" = "Bod400"
  )
  hind_mit_output_dt <- change_row_entries(hind_mit_output_dt, "basin", row_value_map)
  hind_meteo_stat_dt <- change_row_entries(hind_meteo_stat_dt, "basin", row_value_map)
  
  # add columns and export
  export_to_rds_csv(
    hind_mit_output_dt, 
    output_dir, 
    "mit_output", 
    "hindcast",
    run_type_hind
  )
  export_to_rds_csv(
    hind_meteo_stat_dt, 
    output_dir, 
    "meteo_stat", 
    "hindcast",
    run_type_hind
  )
}

