library(here)
library(data.table)

# project directory
home_dir <- file.path(here::here())

all_scenario_horizons <- c(
  "Hd_2050", "Hd_2100", "Hd_2150",
  "Hn_2050", "Hn_2100", "Hn_2150",
  "Md_2050", "Md_2100", "Md_2150",
  "Mn_2050", "Mn_2100", "Mn_2150",
  "Ld_2100", "Ln_2100",
  "L_2033",
  "reference",
  "Hindcast",
  "Observation"
)

scenario_horizons <- c(
  "Hd_2050", "Hd_2100", "Hd_2150",
  "Ld_2100", "Ln_2100",
  "Md_2150"
)

run_type_sel <- c("Observation", "Hindcast", "sund_bc", "no_sund_bc", "with_glac_sdbc", "with_glac_sund")

# To include all .rds files without filtering, uncomment the lines below:
# scenario_horizons <- character(0)
# run_type_sel <- character(0)

knmi_variables <- list(
  knmi_mit_output_dt = list(read = FALSE, read_dir = file.path(home_dir, "Data", "Rheinblick2027", "processed_prevah_output", "prevah_mit_output_knmi.rds"),
                            combine = FALSE, combine_dir = file.path(home_dir, "Data", "Rheinblick2027", "processed_prevah_output", "mit_output")),
 
  knmi_meteo_stat_dt = list(read = FALSE, read_dir = file.path(home_dir, "Data", "Rheinblick2027", "processed_prevah_output", "prevah_meteo_stat_knmi.rds"),
                            combine = FALSE, combine_dir = file.path(home_dir, "Data", "Rheinblick2027", "processed_prevah_output", "meteo_stat")),
  
  knmi_discharge_dt = list(read = TRUE, read_dir = file.path(home_dir, "Data", "Rheinblick2027", "processed_prevah_output", "prevah_discharge_knmi.rds"),
                           combine = FALSE, combine_dir = file.path(home_dir, "Data", "Rheinblick2027", "processed_prevah_output", "discharge")),
  
  knmi_discharge_dt_rblick = list(read = FALSE, read_dir = "path/to/discharge_data.rds")
)

ch2018_variables <- list(
  ch2018_chain_glchain_dt = list(read = FALSE, 
                                 read_dir = file.path(home_dir, "Data", "Rheinblick2027", "ch2018", "ch2018_chain_glchain.rds")),
  ch2018_meteo_dt = list(read = FALSE, 
                         read_dir = file.path(home_dir, "Data", "Rheinblick2027", "ch2018", "ch2018_meteo.rds")),
  ch2018_glacier_dt = list(read = FALSE, 
                           read_dir = file.path(home_dir, "Data", "Rheinblick2027", "ch2018", "ch2018_glacier.rds"))
)

# functions ----------------------------------------------------------------
read_variables_from_list <- function(var_list) {
  # Loop over each entry in the dictionary
  for (varname in names(var_list)) {
    config <- var_list[[varname]]
    
    if (config$read) {
      if (file.exists(config$read_dir)) {
        assign(varname, as.data.table(readRDS(config$read_dir)), envir = .GlobalEnv)
        message("Loaded ", varname)
      } else {
        warning("File not found for ", varname, ": ",config$data_dir)
      }
    } else {
      message("Skipped loading ", varname)
    }
  }
}

combine_rds_scenario_data <- function(var_list) {
  for (varname in names(var_list)) {
    config <- var_list[[varname]]
    
    # Only combine if "combine = TRUE" is set
    if (!isTRUE(config$combine)) next
    
    combine_dir <- config$combine_dir
    read_dir <- config$read_dir
    
    # Find all .rds files in the combine directory
    rds_files <- list.files(combine_dir, pattern = "\\.rds$", full.names = TRUE, recursive = TRUE)
    
    # Apply filtering only if scenario_horizons and run_type_sel are not empty
    if (length(scenario_horizons) > 0 && length(run_type_sel) > 0) {
      rds_files <- rds_files[
        sapply(rds_files, function(f) {
          any(sapply(scenario_horizons, function(sh) grepl(sh, f))) &&
            any(sapply(run_type_sel, function(rt) grepl(rt, f)))
        })
      ]
    }
    
    if (length(rds_files) == 0) {
      warning("No .rds files found in ", combine_dir)
      next
    }
    
    # Read and combine all RDS files
    combined_dt <- data.table::rbindlist(
      lapply(rds_files, readRDS), 
      use.names = TRUE, fill = TRUE
    )
    
    # Order the combined data
    if ("basin" %in% colnames(combined_dt)) {
      data.table::setorder(combined_dt, horizon, scenario, variant, member, basin, date)
    } else if ("station" %in% colnames(combined_dt)) {
      data.table::setorder(combined_dt, horizon, scenario, variant, member, station, date)
    } else {
      data.table::setorder(combined_dt, horizon, scenario, variant, member, date)
    }
    
    # Assign to global variable and save as .rds
    assign(varname, combined_dt, envir = .GlobalEnv)
    
    # Save as RDS only if no filtering was applied
    if (length(scenario_horizons) == 0 && length(run_type_sel) == 0) {
      saveRDS(combined_dt, file = read_dir)
    }
    
    # Also save as CSV (same base name, .csv extension)
    csv_path <- sub("\\.rds$", ".csv", read_dir)
    data.table::fwrite(combined_dt, file = csv_path)
    
    message("Combined and saved '", varname, "' to: ", read_dir)
  }
}


# code -----------------------------------------------------------------
read_variables_from_list(knmi_variables)
read_variables_from_list(ch2018_variables)

combine_rds_scenario_data(knmi_variables)

