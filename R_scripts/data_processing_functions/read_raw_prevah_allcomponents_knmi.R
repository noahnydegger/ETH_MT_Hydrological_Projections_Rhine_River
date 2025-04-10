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
meteo_stat_file_suffix_knmi <- "_full.stats"
meteo_stat_file_suffix_hind <- "_full.stat"

output_name_mit_output <- "prevah_mit_output_knmi"
output_name_meteo_stat <- "prevah_meteo_stat_knmi"

scenario_horizons <- c(
  "reference", 
  "L_2033",
  "Md_2050", "Mn_2050", "Hd_2050", "Hn_2050",
  "Ld_2100", "Ln_2100", "Md_2100", "Mn_2100", "Hd_2100", "Hn_2100",
  "Md_2150", "Mn_2150", "Hd_2150", "Hn_2150"
)

ensembles <- paste0("ens", 1:8)

meteo_variables_knmi <- c(
  "tair", "prec", "radg", "sund", "rhum", "wspd"
)

meteo_variables_hind <- c(
  "temp", "prec", "rad_", "ssd_", "relh", "wind"
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

source(here("R_scripts", "data_import_functions.R"))

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

process_meteo_stats_data <- function(ezg_dir, meteo_variables, meteo_stat_file_suffix, horizon, scenario, variant, member, ezg) {
  
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
        basin = ezg
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
      stop(paste("File not found:", meteo_file))
    }
  } # meteo_variables loop
  
  value_columns <- setdiff(names(all_meteo_data_dt), c("YYYY", "MM", "DD", "basin", "date", "horizon", "scenario", "variant", "member", "hydro_model"))
  # Select required columns in correct order
  all_meteo_data_dt <- all_meteo_data_dt[, c("basin", "date", "horizon", "scenario", "variant", "member", "hydro_model", value_columns), with = FALSE]
  
  
  return(all_meteo_data_dt)
}

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
  
  if (scen == "reference") {
    scenario <- "R"
    horizon <- 2005
  } else {
    scenario <- substr(scen, 1, 1)
    horizon <- as.numeric(sub(".*([0-9]{4})$", "\\1", scen))
  }
  
  # Extract `variant` (2nd character of scenario, "d", "n", or "none")
  variant <- ifelse(nchar(scen) >= 2 && substr(scen, 2, 2) %in% c("d", "n"), substr(scen, 2, 2), "none")
  
  # # Create an entry for the scenario in the output list
  # knmi_mit_output_list[[scen]] <- list()
  # knmi_meteo_stat_list[[scen]] <- list()
  
  # Loop over matching folders
  for (scen_ensm_dir in matching_folders) {
    cat("Processing scenario-ensemble:", basename(scen_ensm_dir), "\n")
    
    # Extract the ensemble member number (ens1 to ens8) as a numeric value and as a string
    member <- as.numeric(sub(".*_ens([1-8])$", "\\1", basename(scen_ensm_dir)))
    ensm <- paste0("ens", member)
    
    # knmi_mit_output_list[[scen]][[ensm]] <- list()
    # knmi_meteo_stat_list[[scen]][[ensm]] <- list()
    
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
      
      if (!(ezg %in% no_meteo_gebiete)) {
        # Process meteo statistics data
        all_meteo_data_dt <- process_meteo_stats_data(ezg_dir, meteo_variables_knmi, meteo_stat_file_suffix_knmi, horizon, scenario, variant, member, ezg)
        
        all_meteo_data_list[[length(all_meteo_data_list) + 1]] <- all_meteo_data_dt
        
      } # no_meteo_gebiete check
    } # gebiete_folders loop
  } # scen_ensm loop
} # scenario_horizons loop

# hindcast data
cat("Processing hindcast data\n")
horizon <- 2005
scenario <- "C" # for control run
variant <- "none"
member <- 1
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
  
  if (!(ezg %in% no_meteo_gebiete)) {
    # Process meteo statistics data
    all_meteo_data_dt <- process_meteo_stats_data(ezg_dir, meteo_variables_hind, meteo_stat_file_suffix_hind, horizon, scenario, variant, member, ezg)
    
    # Create a named vector for mapping
    replacement_map <- setNames(meteo_variables_knmi, meteo_variables_hind)
    
    # Update column names in the data table
    setnames(all_meteo_data_dt, 
             old = names(all_meteo_data_dt), 
             new = stringr::str_replace_all(names(all_meteo_data_dt), replacement_map)
    )
    
    all_meteo_data_list[[length(all_meteo_data_list) + 1]] <- all_meteo_data_dt
    
  } # no_meteo_gebiete check
} # gebiete_folders loop

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
