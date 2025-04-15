library(here)
library(data.table)

# project directory
home_dir <- file.path(here::here())

# Define your variable config
knmi_variables <- list(
  knmi_mit_output_dt = list(read = FALSE, data_dir = "path/to/meteo_data.rds"),
  knmi_meteo_stat_dt = list(read = FALSE, data_dir = "path/to/discharge_data.rds"),
  knmi_discharge_dt = list(read = FALSE, data_dir = "path/to/discharge_data.rds"),
  knmi_discharge_dt_rblick = list(read = FALSE, data_dir = "path/to/discharge_data.rds")
)

ch2018_variables <- list(
  ch2018_chain_glchain_dt = list(read = TRUE, data_dir = file.path(home_dir, "Data", "Rheinblick2027", "ch2018", "ch2018_chain_glchain.rds")),
  ch2018_meteo_dt = list(read = TRUE, data_dir = file.path(home_dir, "Data", "Rheinblick2027", "ch2018", "ch2018_meteo.rds")),
  ch2018_glacier_dt = list(read = TRUE, data_dir = file.path(home_dir, "Data", "Rheinblick2027", "ch2018", "ch2018_glacier.rds"))
)

# functions ----------------------------------------------------------------
read_variables_from_list <- function(var_list) {
  # Loop over each entry in the dictionary
  for (varname in names(var_list)) {
    config <- var_list[[varname]]
    
    if (config$read) {
      if (file.exists(config$data_dir)) {
        assign(varname, as.data.table(readRDS(config$data_dir)), envir = .GlobalEnv)
        message("Loaded ", varname)
      } else {
        warning("File not found for ", varname, ": ",config$data_dir)
      }
    } else {
      message("Skipped loading ", varname)
    }
  }
}


# code -----------------------------------------------------------------
read_variables_from_list(knmi_variables)
read_variables_from_list(ch2018_variables)

