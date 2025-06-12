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
  "eference", # upper and lower case
  "indcast", # upper and lower case
  "bservation" # upper and lower case
)

scenario_horizons <- c(
  "Hd_2050", "Hd_2100", "Hd_2150",
  "Hn_2050", "Hn_2100", "Hn_2150",
  "Md_2050", "Md_2100", "Md_2150",
  "Mn_2050", "Mn_2100", "Mn_2150",
  "Ld_2100", "Ln_2100",
  "L_2033",
  "eference", # upper and lower case
  "indcast", # upper and lower case
  "bservation" # upper and lower case
)

all_run_types <- c(
  "bservation", "indcast",  # upper and lower case
  "sund_bc", "no_sund_bc",
  "with_glac_sdbc", "with_glac_sund",
  "future_V1"
)
run_type_sel <- c("sund_bc", "with_glac_sdbc")
run_type_sel <- c("bservation", "indcast", "future_V1")

basin_sel <- c("RhB200", "RhD200", "RhN200", "RhR200", "AaU200", "ThS200", "Thu200", "TGl200", "HiR200", "VoR200", "Bod400")  # MT_sel
station_sel <- c("Basel Rheinhalle", "Diepoldsau", "Rhine_Neuhausen", "Rekingen", "Aare_Untersiggenthal", 
               "Aare_Thun", "Luetschine_Gsteig",
               "Andelfingen", "Thur_Halden",
               "Toess_Neftenbach", "Glatt_Rheinsfelden",
               "Hinterrhein_Fuerstenau", "Vorderrhein_Ilanz", "Rhine_Domat_Ems", "Gisingen", "Rhine_Rheinfelden", 
               "Brugg", "Bruegg-Aegerten", "Aare_Murgenthal", "Aare_Schoenau", 
               "Limmatt_Baden", "Mellingen", "Reuss_Seedorf")  # MT_sel

# To include all .rds files without filtering, uncomment the lines below:
# scenario_horizons <- character(0)
# run_type_sel <- character(0)

knmi_variables <- list(
  knmi_mit_output_dt = list(read = FALSE, read_dir = file.path(home_dir, "Data", "Rheinblick2027", "processed_prevah_output", "prevah_mit_output_future_V1_MT_sel.rds"),
                            combine = FALSE, combine_dir = file.path(home_dir, "Data", "Rheinblick2027", "processed_prevah_output", "mit_output")),
 
  knmi_meteo_stat_dt = list(read = FALSE, read_dir = file.path(home_dir, "Data", "Rheinblick2027", "processed_prevah_output", "prevah_meteo_stat_future_V1_MT_sel.rds"),
                            combine = FALSE, combine_dir = file.path(home_dir, "Data", "Rheinblick2027", "processed_prevah_output", "meteo_stat")),
  
  knmi_discharge_dt = list(read = FALSE, read_dir = file.path(home_dir, "Data", "Rheinblick2027", "processed_prevah_output", "prevah_discharge_knmi_future_V1_MT_sel.rds"),
                           combine = TRUE, combine_dir = file.path(home_dir, "Data", "Rheinblick2027", "processed_prevah_output", "discharge")),
  
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
    
    message("Processing variable '", varname, "' with ", length(rds_files), " files...")
    
    # Batching parameters
    batch_size <- 3
    batch_starts <- seq(1, length(rds_files), by = batch_size)
    
    batch_list <- list()
    batch_idx <- 1
    
    # Process batches
    for (batch_start in batch_starts) {
      batch_end <- min(batch_start + batch_size - 1, length(rds_files))
      batch_files <- rds_files[batch_start:batch_end]
      
      message("  Processing batch ", batch_idx, " of ", length(batch_starts), 
              " (", length(batch_files), " files)...")
      
      batch_dt <- data.table::rbindlist(lapply(batch_files, readRDS), use.names = TRUE, fill = TRUE)
      
      # Filter for desired basins
      if ("basin" %in% names(batch_dt)) {
        batch_dt <- batch_dt[basin %in% basin_sel]
      } else if ("station" %in% names(batch_dt)) {
        batch_dt <- batch_dt[station %in% station_sel]
      } else {
        warning("Column 'basin' not found in batch ", batch_idx, " — skipping filtering.")
      }
      
      batch_list[[batch_idx]] <- batch_dt
      batch_idx <- batch_idx + 1
      
      rm(batch_dt)
      gc()
    }
    
    # Final rbindlist — this is fast + memory efficient
    combined_dt <- data.table::rbindlist(batch_list, use.names = TRUE, fill = TRUE)
    
    rm(batch_list)
    gc()
    
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
    
    message("Combined data '", varname, "'")
    
    saveRDS(combined_dt, file = read_dir)
    
    # Also save as CSV (same base name, .csv extension)
    csv_path <- sub("\\.rds$", ".csv", read_dir)
    data.table::fwrite(combined_dt, file = csv_path)
    
    message("Saved combined '", varname, "' to: ", read_dir)
    
    # Save as RDS only if no filtering was applied
    if (length(scenario_horizons) == 0 && length(run_type_sel) == 0) {
      saveRDS(combined_dt, file = read_dir)
      
      # Also save as CSV (same base name, .csv extension)
      csv_path <- sub("\\.rds$", ".csv", read_dir)
      data.table::fwrite(combined_dt, file = csv_path)
      
      message("Saved combined '", varname, "' to: ", read_dir)
    }
  }
}

# code -----------------------------------------------------------------
read_variables_from_list(knmi_variables)
read_variables_from_list(ch2018_variables)

combine_rds_scenario_data(knmi_variables)

