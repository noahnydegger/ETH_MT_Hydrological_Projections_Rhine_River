library(here)
library(future.apply)
library(data.table)
library(stringr)
library(ncdf4)
library(terra)
library(geosphere)

# project directory
home_dir <- file.path(here::here())

# input directories
input_dir <- file.path(home_dir, "Data", "Rheinblick2027", "processed_meteo")

# output directory
output_dir <- file.path(home_dir, "Data", "Rheinblick2027", "processed_meteo")

input_file_suffix <- ".nc"

scenario_horizons <- c(
  "Hd_2100"
)

ensembles <- paste0("ens", 1:8)

# Common metadata
basin <- "hydro_CH"


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

# apply bias_correct_sund_rel and export_to_netcdf functions
sund_bc_export <- function(
    data_dir,
    output_dir,
    scenario,
    pattern = ".*sund_rel_crop.*\\.nc$",
    suffix = "_bc"
) {
  files <- list.files(data_dir, pattern = pattern, full.names = TRUE, recursive = TRUE)
  if (length(files) == 0) stop("No files found.")
  
  plan(multisession, workers = 8)
  
  process_and_export <- function(file) {
    cat("[", format(Sys.time(), "%H:%M:%S"), "] bias correct file ", basename(file), " \n")
    r_stack <- rast(file)
    r_stack_bc <- bias_correct_sund_rel(r_stack)
    
    # Extract ensemble from filename
    ens_match <- regmatches(file, regexpr("ens\\d+", file))
    ensemble <- if (length(ens_match) == 0 || ens_match == "") "none" else ens_match
    
    cat("[", format(Sys.time(), "%H:%M:%S"), "] export ", basename(file), "_bc \n")
    # Export corrected raster
    export_to_netcdf(
      r_stack = r_stack_bc,
      out_dir = output_dir,
      scenario = scenario,
      ensemble = ensemble,
      varname = "sund_rel",
      varunit = "%",
      suffix = suffix
    )
    
    return(file)
  }
  
  invisible(future_lapply(files, process_and_export))
}

compute_sund_stats_parallel_hindcast <- function(
    chunk_dir,
    crop_shape_path,
    pattern = ".*sund_rel_chunk_.*\\.nc$"
) {
  files <- list.files(chunk_dir, pattern = pattern, full.names = TRUE)
  if (length(files) == 0) stop("No chunk files found.")
  
  plan(multisession, workers = 8)
  
  process_chunk <- function(file) {
    r_stack <- rast(file)
    
    #r_resample <- resample_to_knmi_grid_hind_ext(r_stack, hind_rast_path, knmi_rast_path)
    
    # 1. Crop + mask
    r_stack_crop <- crop_and_mask_by_polygon(r_stack, crop_shape_path)
    #r_resample_crop <- crop_and_mask_by_polygon(r_resample, crop_shape_path)
    
    # 2. Logit transform
    r_stack_logit <- logit_transform(r_stack_crop)
    #r_resample_logit <- logit_transform(r_resample_crop)
    
    # 3. Compute per-layer means
    dates <- time(r_stack)
    sund_means_raw <- global(r_stack_crop, "mean", na.rm = TRUE)[, 1]
    logit_means_raw <- global(r_stack_logit, "mean", na.rm = TRUE)[, 1]
    #sund_means_res <- global(r_resample_crop, "mean", na.rm = TRUE)[, 1]
    #logit_means_res <- global(r_resample_logit, "mean", na.rm = TRUE)[, 1]
    
    # Extract ensemble from filename
    ens_match <- regmatches(file, regexpr("ens\\d+", file))
    ensemble <- if (length(ens_match) == 0 || ens_match == "") "none" else ens_match
    
    # 4. Store results in DT
    data.table(
      date = as.Date(dates),
      ensemble = ensemble,
      sund_rel_raw = sund_means_raw,
      sund_rel_res = "none", #sund_means_res,
      sund_logit_raw = logit_means_raw,
      sund_logit_res = "none" #logit_means_res
    )
  }
  
  # Process all chunks in parallel
  dt_list <- future_lapply(files, process_chunk)
  
  # Combine results
  sund_stats_dt <- rbindlist(dt_list)
  setorder(sund_stats_dt, ensemble, date)
  
  fwrite(sund_stats_dt, file.path(chunk_dir, paste0(scenario, "_sund_rel_stats.csv")))
  saveRDS(sund_stats_dt, file.path(chunk_dir, paste0(scenario, "_sund_rel_stats.rds")))
  
  return(sund_stats_dt)
}

compute_sund_stats_parallel <- function(
    data_dir,
    scenario,
    pattern = ".*sund_rel.*\\.nc$"
) {
  files <- list.files(data_dir, pattern = pattern, full.names = TRUE)
  if (length(files) == 0) stop("No files found.")
  
  plan(multisession, workers = 8)
  
  process_chunk <- function(file) {
    r_stack <- rast(file)
    
    r_stack_bc <- bias_correct_sund_rel(r_stack)
    
    # 3. Compute per-layer means
    dates <- time(r_stack)
    sund_means <- global(r_stack, "mean", na.rm = TRUE)[, 1]
    sund_means_bc <- global(r_stack_bc, "mean", na.rm = TRUE)[, 1]
    #sund_means_res <- global(r_resample_crop, "mean", na.rm = TRUE)[, 1]
    #logit_means_res <- global(r_resample_logit, "mean", na.rm = TRUE)[, 1]
    
    # Extract ensemble from filename
    ens_match <- regmatches(file, regexpr("ens\\d+", file))
    ensemble <- if (length(ens_match) == 0 || ens_match == "") "none" else ens_match
    
    # 4. Store results in DT
    data.table(
      date = as.Date(dates),
      member = ensemble,
      sund_rel = sund_means,
      sund_rel_bc = sund_means_bc
    )
  }
  
  # Process all chunks in parallel
  dt_list <- future_lapply(files, process_chunk)
  
  # Combine results
  sund_stats_dt <- rbindlist(dt_list)
  setorder(sund_stats_dt, member, date)
  
  fwrite(sund_stats_dt, file.path(data_dir, paste0(scenario, "_sund_rel_stats.csv")))
  saveRDS(sund_stats_dt, file.path(data_dir, paste0(scenario, "_sund_rel_stats.rds")))
  
  return(sund_stats_dt)
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
# ----------------------------
# Step 1: Process hindcast
# ----------------------------
message("Processing hindcast scenario")

# hindcast_r_stack_raw <- read_nc_raster(
#   meteo_dir = input_dir_meteo,
#   scenario = "hindcast",
#   variable = "sund_rel"
# )

# resample to knmi 12 km grid
hindcast_r_stack_rel <- resample(hindcast_r_stack_rel, hind_rast_res, method = "bilinear")

# compute mean and bc mean
scenario <- "hindcast"
message("[", format(Sys.time(), "%H:%M:%S"), "] computing ", scenario, " stats")
hindcast_sund_stats <- compute_sund_stats_parallel(
  data_dir = file.path(output_dir, scenario, "sund_rel_crop"),
  scenario = scenario,
  pattern = ".*sund_rel_crop.*\\.nc$"
)
message("[", format(Sys.time(), "%H:%M:%S"), "] stats computed")

hindcast_sund_stats[, `:=`(
  basin = "hydro_CH",
  scenario = "none",
  variant = "none",
  horizon = "hindcast",
  sund_rel_bc = sund_rel
)]

setnames(hindcast_sund_stats, old = "ensemble", new = "member")

hindcast_sund_stats <- add_scenario_horizon_grouping_columns(hindcast_sund_stats)

# ----------------------------
# Step 3: Process KNMI reference (ens1 to ens8)
# ----------------------------
message("Processing reference scenario")

scenario <- "reference"
sund_bc_export(
  data_dir = file.path(output_dir, scenario, "sund_rel_crop"),
  output_dir = output_dir,
  scenario = scenario,
  pattern = ".*sund_rel_crop.*\\.nc$",
  suffix = "_bc"
)

# compute mean and bc mean
scenario <- "reference"
message("[", format(Sys.time(), "%H:%M:%S"), "] computing ", scenario, " stats")
reference_sund_stats <- compute_sund_stats_parallel(
  data_dir = file.path(output_dir, scenario, "sund_rel_crop"),
  scenario = scenario,
  pattern = ".*sund_rel_crop.*\\.nc$"
)
message("[", format(Sys.time(), "%H:%M:%S"), "] stats computed")

reference_sund_stats[, `:=`(
  basin = "hydro_CH",
  scenario = "none",
  variant = "none",
  horizon = "ref"
)]

reference_sund_stats <- add_scenario_horizon_grouping_columns(reference_sund_stats)

# compute mean and bc mean
scenario <- "Hd_2100"
message("[", format(Sys.time(), "%H:%M:%S"), "] computing ", scenario, " stats")
Hd_2100_sund_stats <- compute_sund_stats_parallel(
  data_dir = file.path(output_dir, scenario, "sund_rel_crop"),
  scenario = scenario,
  pattern = ".*sund_rel_crop.*\\.nc$"
)
message("[", format(Sys.time(), "%H:%M:%S"), "] stats computed")

Hd_2100_sund_stats[, `:=`(
  basin = "hydro_CH",
  scenario = "H",
  variant = "dry",
  horizon = "2100"
)]

Hd_2100_sund_stats <- add_scenario_horizon_grouping_columns(Hd_2100_sund_stats)
# ----------------------------
# Step 4: Compute mean difference
# ----------------------------
sund_stats_dt <- rbind(hindcast_sund_stats, reference_sund_stats, Hd_2100_sund_stats, use.names = TRUE, fill = FALSE)
hindcast_logit_raw_mean <- hindcast_sund_stats[, mean(sund_logit_raw, na.rm = TRUE)]
hindcast_logit_res_mean <- hindcast_sund_stats[, mean(sund_logit_res, na.rm = TRUE)]

reference_logit_mean <- reference_sund_stats[, mean(sund_logit_raw, na.rm = TRUE)]

logit_diff_raw <- reference_logit_mean - hindcast_logit_raw_mean
logit_diff_res <- reference_logit_mean - hindcast_logit_res_mean

# ----------------------------
# Step 5: Create Plots
# ----------------------------

plot_dir <- file.path(home_dir, "Plots", "Reference_Period_Analysis", "bias_correction")

# cdf plot -----------------------------------------------------
source(here("R_scripts", "plotting_functions", "plot_pdf_cdf.R"))
basins <- unique(sund_stats_dt$basin)
color_col <- "scen_var_hor"
group_cols <- c("scenario", "variant", "horizon")
value_cols <- c("sund_rel", "sund_rel_bc")
info_col <- c("sund_rel_mean")
for (bsn in basins) {
  dt <- sund_stats_dt[basin == bsn]
  for (value_col in value_cols) {
    cat("Plotting cdf for", bsn, value_col, "\n")
    
    plot_cdf(dt, plot_dir, bsn, info_col, color_col, value_col, group_cols)
  }
} # basin loop

# seasonality plot -----------------------------------------------------
source(here("R_scripts", "plotting_functions", "plot_seasonality.R"))
group_cols <- c("basin", "scen_var_hor")
value_cols <- c("sund_rel", "sund_rel_bc")
color_col <- "scen_var_hor"

gof_pairs <- c("none_none_hindcast", "none_none_ref")

# Compute rolling statistics
rolling_stats_dt <- compute_rolling_stats(sund_stats_dt, group_cols, value_cols)

# Add "rm_" prefix to each value column
group_cols <- c(group_cols, "DayOfYear")
value_cols <- paste0("rm_", value_cols)

# compute seasonality and produce plots
for (stat in c("mean")) {
  seasonality_dt <- compute_seasonality(rolling_stats_dt, group_cols = group_cols, value_cols = value_cols, stat = stat)
  for (bsn in basins) {
    dt <- seasonality_dt[basin == bsn]
    for (value_col in value_cols) {
      cat("Plotting seasonality for", bsn, value_col, "\n")
      
      info_col <- c("sund_rel_mean")
      
      plot_seasonality_ts(dt, plot_dir, bsn, info_col, color_col, value_col, stat, info_text = "_rast_bc", gof_pairs = gof_pairs)
      
    } # value_col loop
  } # basin loop
} # stat loop