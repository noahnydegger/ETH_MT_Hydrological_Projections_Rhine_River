
# project directory
home_dir <- file.path(here::here())

# input directory
rds_input_dir <- file.path(home_dir, "Data", "Rheinblick2027", "processed_prevah_output")

generating_scripts_dir <- file.path(home_dir, "R_scripts", "data_processing_functions")

dt_variables <- c(
  "knmi_discharge_dt",
  "knmi_mit_output_dt",
  "knmi_meteo_stat_dt"
  )

rds_files <- c(
  "prevah_discharge_knmi.R",
  "prevah_mit_output_knmi.R",
  "prevah_meteo_stat_knmi.R"
)

generating_scripts <- c(
  "read_raw_prevah_discharge_knmi.R",
  "read_raw_prevah_allcomponents_knmi.R"
)

knmi_dt_list <- list()

load_or_generate_data <- function(var_name, rds_path, script_path) {
  if (!exists(var_name, envir = .GlobalEnv)) {
    if (file.exists(rds_path)) {
      assign(var_name, readRDS(rds_path), envir = .GlobalEnv)
      message(paste("Loaded", var_name, "from", rds_path))
    } else if (file.exists(script_path)) {
      source(script_path)
      if (!exists(var_name, envir = .GlobalEnv)) {
        stop(paste("Script", script_path, "did not generate the expected variable:", var_name))
      }
      message(paste("Generated", var_name, "by running", script_path))
    } else {
      stop(paste("Neither", rds_path, "nor", script_path, "was found."))
    }
  } else {
    message(paste(var_name, "already exists in the environment."))
  }
  
  # Add loaded variable to knmi_dt_list list
  knmi_dt_list[[var_name]] <<- get(var_name, envir = .GlobalEnv)
}

# Function to add 'scenario_variant' and 'scenario_variant_horizon' columns with custom ordering
add_combined_columns <- function(dt) {
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

# Function to apply add_combined_columns to each data.table in dt_list
apply_to_dt_list <- function(dt_list) {
  dt_list <- lapply(dt_list, add_combined_columns)
  return(dt_list)
}

for (i in seq_along(dt_variables)) {
  load_or_generate_data(dt_variables[i], 
                        file.path(rds_input_dir, rds_files[i]), 
                        file.path(generating_scripts_dir, generating_scripts[i])
  )
}

knmi_dt_list <- apply_to_dt_list(knmi_dt_list)
