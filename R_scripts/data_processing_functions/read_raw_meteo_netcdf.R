library(here)
library(future.apply)
library(progressr)
library(data.table)
library(stringr)
library(ncdf4)
library(terra)
library(geosphere)

# settings ---------------------------------------------------------------
# project directory
home_dir <- file.path(here::here())

# input directories
input_dir <- file.path(home_dir, "Data", "Rheinblick2027", "meteo", "netcdf_processed")

# output directory
output_dir <- file.path(home_dir, "Data", "Rheinblick2027", "processed_meteo")

input_file_suffix <- ".nc"

# functions ---------------------------------------------------------------
read_nc_raster <- function(input_dir, scenario, ensemble = NULL, variable) {
  
  var_path <- file.path(input_dir, scenario, variable)
  
  if (!is.null(ensemble)) {
    scenario <- paste0(scenario, "_", ensemble)
  }
  
  file_path <- file.path(var_path, paste0(scenario, "_", variable, input_file_suffix))
  r <- rast(file_path)
  return(r)
}

resample_to_knmi_grid_hind_ext <- function(r_stack, hind_rast_path, knmi_rast_path) {
  # 1. Read hindcast and KNMI rasters
  hind_rast <- read_and_convert_prevah_bin_raster(hind_rast_path)
  knmi_rast <- read_and_convert_prevah_bin_raster(knmi_rast_path)
  
  # 2. Crop KNMI raster to hindcast extent
  knmi_rast_crop <- crop(knmi_rast, hind_rast)
  
  # 3. Create resampling target (same extent, res, crs)
  hind_rast_res <- resample(hind_rast, knmi_rast_crop, method = "bilinear")
  
  # 4. Resample the input raster stack to match the target
  r_stack_resampled <- resample(r_stack, hind_rast_res, method = "bilinear")
  
  return(r_stack_resampled)
}

logit_transform <- function(r_stack, eps = 1e-6) {
  # Ensure values are in (0,1) interval
  r_stack_clipped <- clamp(r_stack, lower = eps, upper = 1 - eps)
  
  # Apply logit transformation
  logit_r <- log(r_stack_clipped / (1 - r_stack_clipped))
  
  return(logit_r)
}

inv_logit_transform <- function(logit_r_stack) {
  backtransformed <- 1 / (1 + exp(-logit_r_stack))
  return(backtransformed)
}

# Mean value over all layers and cells
stack_mean_value <- function(r_stack) {
  global(r_stack, "mean", na.rm = TRUE)[1, 1]
}

crop_and_mask_by_polygon <- function(r_stack, crop_shape_path) {
  crop_shape <- vect(crop_shape_path)
  crop_shape <- project(crop_shape, crs(r_stack))
  
  masked <- mask(crop(r_stack, crop_shape), crop_shape)
  return(masked)
}

# apply bias_correct_sund_rel and export_to_netcdf functions
crop_and_export <- function(
    data_dir,
    output_dir,
    crop_shape_path,
    scen_hor,
    pattern = ".*sund_rel_crop.*\\.nc$",
    varname = "",
    varunit = "",
    suffix = ""
) {
  files <- list.files(data_dir, pattern = pattern, full.names = TRUE, recursive = TRUE)
  if (length(files) == 0) stop("No files found.")
  
  plan(multisession, workers = 8)
  
  process_and_export <- function(file) {
    cat("[", format(Sys.time(), "%H:%M:%S"), "] processing ", basename(file), " \n")
    r_stack <- rast(file)
    
    # Crop and mask
    r_crop <- crop_and_mask_by_polygon(r_stack, crop_shape_path)
    
    # Extract ensemble from filename
    ens_match <- regmatches(file, regexpr("ens\\d+", file))
    ensemble <- if (length(ens_match) == 0 || ens_match == "") "none" else ens_match
    
    cat("[", format(Sys.time(), "%H:%M:%S"), "] export ", basename(file), "_bc \n")
    # Export corrected raster
    export_to_netcdf(
      r_stack = r_crop,
      out_dir = output_dir,
      scen_hor = scen_hor,
      ensemble = ensemble,
      varname = varname,
      varunit = varunit,
      suffix = suffix
    )
    
    return(file)
  }
  
  invisible(future_lapply(files, process_and_export))
}

export_to_netcdf <- function(r_stack, out_dir, scen_hor, ensemble, varname, varunit = "unit", suffix = "") {
  
  save_dir <- file.path(out_dir, scen_hor, paste0(varname, suffix))
  # Ensure the directory exists
  if (!dir.exists(save_dir)) {
    dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
  }
  file_ending <- paste0(suffix, ".nc")
  file_name <- if (scen_hor == "hindcast") {
    paste0(scen_hor, "_", varname, file_ending)
  } else {
    paste0(scen_hor, "_", ensemble, "_", varname, file_ending)
  }
  with_progress({
    writeCDF(
      x = r_stack,
      filename = file.path(save_dir, file_name),
      varname = varname,
      unit = varunit,
      overwrite = TRUE,
      zname = "time",
      compression = 4
    )
  })
  
  message("Exported NetCDF: ", file_name)
}

compute_stats_parallel <- function(
    data_dir,
    scen_hor,
    scenario,
    variant,
    horizon,
    pattern = "*\\.nc$",
    varname,
    basin
) {
  files <- list.files(data_dir, pattern = pattern, full.names = TRUE)
  if (length(files) == 0) stop("No files found.")
  
  plan(multisession, workers = 8)
  
  process_chunk <- function(file) {
    cat("[", format(Sys.time(), "%H:%M:%S"), "] processing ", basename(file), " \n")
    r_stack <- rast(file)
    
    # Compute per-layer means
    dates <- time(r_stack)
    max_value <- global(r_stack, "max", na.rm = TRUE)[, 1]
    min_value <- global(r_stack, "min", na.rm = TRUE)[, 1]
    avg_value <- global(r_stack, "mean", na.rm = TRUE)[, 1]
    std_value <- global(r_stack, "sd", na.rm = TRUE)[, 1]
    
    # Extract ensemble from filename
    ens_match <- regmatches(file, regexpr("ens\\d+", file))
    member <- if (length(ens_match) == 0 || ens_match == "") "none" else sub("ens", "", ens_match)
    
    # 4. Store results in DT
    dt <- data.table(
      basin = basin,
      date = as.Date(dates),
      scenario = scenario,
      variant = variant,
      horizon = horizon,
      member = member
    )
    
    dt[[paste0(varname, "_max")]] <- max_value
    dt[[paste0(varname, "_min")]] <- min_value
    dt[[paste0(varname, "_avg")]] <- avg_value
    dt[[paste0(varname, "_std")]] <- std_value
    
    return(dt)
  }
  
  # Process all chunks in parallel
  dt_list <- future_lapply(files, process_chunk)
  
  # Combine results
  stats_dt <- rbindlist(dt_list)
  setorder(stats_dt, member, date)
  
  fwrite(stats_dt, file.path(data_dir, paste0(scen_hor, varname, "_", basin, "_stats.csv")))
  saveRDS(stats_dt, file.path(data_dir, paste0(scen_hor, varname, "_", basin, "_stats.rds")))
  cat("[", format(Sys.time(), "%H:%M:%S"), "] stats saved ", scen_hor, varname, " \n")
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


# code to read data -------------------------------------------------------
all_scenario_horizons <- c(
  "Hd_2050", "Hd_2100", "Hd_2150",
  "Hn_2050", "Hn_2100", "Hn_2150",
  "Md_2050", "Md_2100", "Md_2150",
  "Mn_2050", "Mn_2100", "Mn_2150",
  "Ld_2100", "Ln_2100",
  "L_2033",
  "reference", # upper and lower case
  "hindcast", # upper and lower case
  "observation" # upper and lower case
)

scenario_horizons <- c(
  "Hd_2150",
  "Hn_2050", "Hn_2100", "Hn_2150",
  "Md_2050", "Md_2100", "Md_2150",
  "Mn_2050", "Mn_2100", "Mn_2150",
  "Ld_2100", "Ln_2100",
  "L_2033",
  "reference"
)

# Step 1: Crop KNMI data

variables <- list(
  tair = list(code = "tas", folder = "temperature", unit = "°C"),
  prec = list(code = "pr", folder = "precipitation", unit = "mm/day")
)

var_sel <- c("tair", "prec")

suffix <- "_RhB"

# crop raw data to rhine basin extent
for (scen_hor in scenario_horizons) {
  for (var_name in var_sel) {
    cat("[", format(Sys.time(), "%H:%M:%S"), "] cropping", scen_hor, var_name, " \n")
    var_info <- variables[[var_name]]
    var_code <- var_info$code
    var_folder <- var_info$folder
    var_unit <- var_info$unit
    
    if (scen_hor == "reference") {
      scenario <- "none"
      horizon <- "ref"
    } else {
      scenario <- substr(scen_hor, 1, 1)
      horizon <- as.numeric(sub(".*([0-9]{4})$", "\\1", scen_hor))
    }
    
    # Extract `variant` (2nd character of scenario, "d", "n", or "none")
    variant <- ifelse(nchar(scen_hor) >= 2 & substr(scen_hor, 2, 2) == "d", 
                      "dry", 
                      ifelse(substr(scen_hor, 2, 2) == "n", "wet", 
                             "none"))
    
    # Define paths
    nc_dir <- file.path(input_dir, var_folder, "all_scenarios")
    
    # Select matching files for the current scenario and horizon
    file_pattern <- paste0(".*", var_code, ".*", scen_hor, ".*\\.nc$")
    
    crop_and_export(
      data_dir = nc_dir,
      output_dir = output_dir,
      crop_shape_path = file.path(output_dir, "extents", "rhine_bsn_projected.shp"),
      scen_hor = scen_hor,
      pattern = file_pattern,
      varname = var_name,  # e.g., "tair"
      varunit = var_unit,  # e.g., "°C"
      suffix = suffix
    )
    
    cat("[", format(Sys.time(), "%H:%M:%S"), "] computing stats", scen_hor, var_name, " \n")
    
    compute_stats_parallel(
      data_dir = file.path(output_dir, scen_hor, paste0(var_name, suffix)),
      scen_hor = scen_hor,
      scenario = scenario,
      variant = variant,
      horizon = horizon,
      pattern = paste0(".*", var_name, ".*\\.nc$"),
      varname = var_name,
      basin = "RhB200"
    )
  }
}
plan(sequential)

# # compute stats ----------------------------
# message("Processing KNMI scenarios")
# 
# scenario <- "Hd_2100"
# sund_bc_export(
#   data_dir = file.path(output_dir, scenario, "sund_rel_crop"),
#   output_dir = output_dir,
#   scenario = scenario,
#   pattern = ".*sund_rel_crop.*\\.nc$",
#   suffix = "_bc"
# )
# 
# # compute mean and bc mean
# scenario <- "reference"
# message("[", format(Sys.time(), "%H:%M:%S"), "] computing ", scenario, " stats")
# reference_sund_stats <- compute_sund_stats_parallel(
#   data_dir = file.path(output_dir, scenario, "sund_rel_crop"),
#   scenario = scenario,
#   pattern = ".*sund_rel_crop.*\\.nc$"
# )
# message("[", format(Sys.time(), "%H:%M:%S"), "] stats computed")
# 
# reference_sund_stats[, `:=`(
#   basin = "hydro_CH",
#   scenario = "none",
#   variant = "none",
#   horizon = "ref"
# )]
# 
# reference_sund_stats <- add_scenario_horizon_grouping_columns(reference_sund_stats)
# 
# # compute mean and bc mean
# scenario <- "Hd_2100"
# message("[", format(Sys.time(), "%H:%M:%S"), "] computing ", scenario, " stats")
# Hd_2100_sund_stats <- compute_sund_stats_parallel(
#   data_dir = file.path(output_dir, scenario, "sund_rel_crop"),
#   scenario = scenario,
#   pattern = ".*sund_rel_crop.*\\.nc$"
# )
# message("[", format(Sys.time(), "%H:%M:%S"), "] stats computed")
# 
# Hd_2100_sund_stats[, `:=`(
#   basin = "hydro_CH",
#   scenario = "H",
#   variant = "dry",
#   horizon = "2100"
# )]
# 
# Hd_2100_sund_stats <- add_scenario_horizon_grouping_columns(Hd_2100_sund_stats)
# # ----------------------------
# # Step 4: Compute mean difference
# # ----------------------------
# sund_stats_dt <- rbind(hindcast_sund_stats, reference_sund_stats, Hd_2100_sund_stats, use.names = TRUE, fill = FALSE)
# hindcast_logit_raw_mean <- hindcast_sund_stats[, mean(sund_logit_raw, na.rm = TRUE)]
# hindcast_logit_res_mean <- hindcast_sund_stats[, mean(sund_logit_res, na.rm = TRUE)]
# 
# reference_logit_mean <- reference_sund_stats[, mean(sund_logit_raw, na.rm = TRUE)]
# 
# logit_diff_raw <- reference_logit_mean - hindcast_logit_raw_mean
# logit_diff_res <- reference_logit_mean - hindcast_logit_res_mean
# 
# # ----------------------------
# # Step 5: Create Plots
# # ----------------------------
# 
# plot_dir <- file.path(home_dir, "Plots", "Reference_Period_Analysis", "bias_correction")
# 
# # cdf plot -----------------------------------------------------
# source(here("R_scripts", "plotting_functions", "plot_pdf_cdf.R"))
# basins <- unique(sund_stats_dt$basin)
# color_col <- "scen_var_hor"
# group_cols <- c("scenario", "variant", "horizon")
# value_cols <- c("sund_rel", "sund_rel_bc")
# info_col <- c("sund_rel_mean")
# for (bsn in basins) {
#   dt <- sund_stats_dt[basin == bsn]
#   for (value_col in value_cols) {
#     cat("Plotting cdf for", bsn, value_col, "\n")
#     
#     plot_cdf(dt, plot_dir, bsn, info_col, color_col, value_col, group_cols)
#   }
# } # basin loop
# 
# # seasonality plot -----------------------------------------------------
# source(here("R_scripts", "plotting_functions", "plot_seasonality.R"))
# group_cols <- c("basin", "scen_var_hor")
# value_cols <- c("sund_rel", "sund_rel_bc")
# color_col <- "scen_var_hor"
# 
# gof_pairs <- c("none_none_hindcast", "none_none_ref")
# 
# # Compute rolling statistics
# rolling_stats_dt <- compute_rolling_stats(sund_stats_dt, group_cols, value_cols)
# 
# # Add "rm_" prefix to each value column
# group_cols <- c(group_cols, "DayOfYear")
# value_cols <- paste0("rm_", value_cols)
# 
# # compute seasonality and produce plots
# for (stat in c("mean")) {
#   seasonality_dt <- compute_seasonality(rolling_stats_dt, group_cols = group_cols, value_cols = value_cols, stat = stat)
#   for (bsn in basins) {
#     dt <- seasonality_dt[basin == bsn]
#     for (value_col in value_cols) {
#       cat("Plotting seasonality for", bsn, value_col, "\n")
#       
#       info_col <- c("sund_rel_mean")
#       
#       plot_seasonality_ts(dt, plot_dir, bsn, info_col, color_col, value_col, stat, info_text = "_rast_bc", gof_pairs = gof_pairs)
#       
#     } # value_col loop
#   } # basin loop
# } # stat loop