
# project directory
home_dir <- file.path(here::here())

read_new_data <- FALSE

# input directory
rds_input_dir <- file.path(home_dir, "Data", "Rheinblick2027", "processed_prevah_output")

generating_scripts_dir <- file.path(home_dir, "R_scripts", "data_processing_functions")

dt_variables <- c(
  "knmi_discharge_dt",
  "knmi_mit_output_dt",
  "knmi_meteo_stat_dt"
  )

rds_files <- c(
  "prevah_discharge_knmi.rds",
  "prevah_mit_output_knmi.rds",
  "prevah_meteo_stat_knmi.rds"
)

generating_scripts <- c(
  "read_raw_prevah_discharge_knmi.R",
  "read_raw_prevah_allcomponents_knmi.R",
  "read_raw_prevah_allcomponents_knmi.R"
)

generate_new_prevah_dt <- function(dt_name, script_path) {
  if (dt_name == "knmi_meteo_stat_dt") {
    return()
  } else {
    source(script_path)
    if (!exists(dt_name, envir = .GlobalEnv)) {
      stop(paste("Script", script_path, "did not generate the expected variable:", dt_name))
    }
    message(paste("Generated", dt_name, "by running", script_path))
  }
}

read_rds_prevah_dt <- function(dt_name, rds_path) {
  assign(dt_name, readRDS(rds_path), envir = .GlobalEnv)
  message(paste("Loaded", dt_name, "from", rds_path))
}

load_or_generate_data <- function(dt_name, rds_path, script_path, read_new_data = FALSE) {
  
  # Check if read_new_data is TRUE, or the variable doesn't exist
  if (read_new_data) {
    generate_new_prevah_dt(dt_name, script_path)
  }
  else if (!exists(dt_name, envir = .GlobalEnv)) {
    if (file.exists(rds_path)) {
      read_rds_prevah_dt(dt_name, rds_path)
      
    } else if (file.exists(script_path)) {
      # If RDS doesn't exist, try to generate it by sourcing the script
      generate_new_prevah_dt(dt_name, script_path)
    } else {
      stop(paste("Neither", rds_path, "nor", script_path, "was found."))
    }
  } else {
    message(paste(dt_name, "already exists in the environment."))
  }
  
}

# Function to add 'scenario_variant' and 'scenario_variant_horizon' columns with custom ordering
add_scenario_horizon_grouping_columns <- function(dt) {
  dt[, scen_var := paste0(scenario, variant)]
  dt[, scen_var_hor := paste(scen_var, horizon, sep = "_")]
  
  # Define custom order for scenario
  scenario_order <- c("H", "M", "L", "R", "C", "O")
  
  # Define custom order for scen_var (including the variants: d, n, none)
  scen_var_order <- c(
    "Hd", "Hn", "Md", "Mn", "Ld", "Ln",
    "Lnone", "Rnone", "Cnone", "Onone"
  )
  
  # Define custom order for scen_var_hor (with horizon)
  scen_var_hor_order <- c(
    "Hd_2150", "Hd_2100", "Hd_2050", 
    "Hn_2150", "Hn_2100", "Hn_2050", 
    "Md_2150", "Md_2100", "Md_2050", 
    "Mn_2150", "Mn_2100", "Mn_2050", 
    "Ld_2100",
    "Ln_2100",
    "Lnone_2033",
    "Rnone_2005", "Cnone_2005", "Onone_2005"
  )
  
  # Convert scen_var and scen_var_hor to factors with defined levels
  dt[, scenario := factor(scenario, levels = scenario_order)]
  dt[, scen_var := factor(scen_var, levels = scen_var_order)]
  dt[, scen_var_hor := factor(scen_var_hor, levels = scen_var_hor_order)]
  return(dt)
}

add_time_period_column <- function(dt, date_col = "date", horizon_col = "horizon") {
  dt[, period := ifelse(
    as.numeric(format(get(date_col), "%Y")) >= get(horizon_col) - 14 &
      as.numeric(format(get(date_col), "%Y")) <= get(horizon_col) + 15,
    "simulation", 
    "warmup"
  )]
}

# Function to change row entries in a column based on old-to-new value mapping
change_row_entries <- function(dt, column, row_value_map) {
  # Loop through each old value and replace it with the corresponding new value
  for (old_value in names(row_value_map)) {
    new_value <- row_value_map[[old_value]]
    dt[get(column) == old_value, (column) := new_value]
  }

}

# Function to change column names based on a value_map (named list)
change_column_names <- function(dt, col_value_map) {
  # Loop through each old column name and rename it to the corresponding new name
  for (old_name in names(col_value_map)) {
    # Check if the old column exists in the data.table
    if (old_name %in% names(dt)) {
      new_name <- col_value_map[[old_name]]
      setnames(dt, old_name, new_name)
    } else {
      # Print a warning if the old column does not exist
      warning(paste("Column", old_name, "does not exist in the data.table"))
    }
  }
}

# load or generate the data.tables
for (i in seq_along(dt_variables)) {
  load_or_generate_data(dt_variables[i], 
                        file.path(rds_input_dir, rds_files[i]), 
                        file.path(generating_scripts_dir, generating_scripts[i]),
                        read_new_data = read_new_data
  )
}

# add columns to the data.tables
for (dt_name in dt_variables) {
  if (exists(dt_name, envir = .GlobalEnv)) {
    dt <- get(dt_name, envir = .GlobalEnv)
    if (inherits(dt, "data.table")) {
      add_scenario_horizon_grouping_columns(dt)
      add_time_period_column(dt)
    }
  }
}


# change column and row names
for (dt_name in dt_variables) {
  if (exists(dt_name, envir = .GlobalEnv)) {
    dt <- get(dt_name, envir = .GlobalEnv)
    if (inherits(dt, "data.table")) {
      if (dt_name == "knmi_discharge_dt") {
        # Change column names
        col_value_map <- c(
          "station" = "basin"
        )
        change_column_names(dt, col_value_map)
      }
      else if (dt_name %in% c("knmi_mit_output_dt", "knmi_meteo_stat_dt")) {
        # Change column names
        row_value_map <- c(
          "Bod200" = "Bod400"
        )
        change_row_entries(dt, "basin", row_value_map)
      }
        
      add_scenario_horizon_grouping_columns(dt)  # Modified directly
      add_time_period_column(dt)  # Modified directly
    }
  }
}
