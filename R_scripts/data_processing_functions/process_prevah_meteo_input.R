library(here)
library(future.apply)
library(data.table)
library(stringr)
library(ncdf4)
library(terra)
library(geosphere)

#plan(multisession, workers = 8)

# project directory
home_dir <- file.path(here::here())

# input directories
input_dir_meteo <- file.path(home_dir, "Data", "Rheinblick2027", "processed_meteo")

# output directory
output_dir <- file.path(home_dir, "Data", "Rheinblick2027", "processed_meteo")

input_file_suffix <- ".nc"

scenario_horizons <- c(
  "reference", 
  "Hd_2100"
)

meteo_variables_knmi <- c(
  "sund" = "sund_rel"
  #"radg" = "radg_abs"
)

meteo_variables_hind <- c(
  "ssd_" = "sund_abs"
  #"rad_" = "radg_abs"
)

ensembles <- paste0("ens", 1:8)

# Common metadata
basin <- "hydro_CH"


# functions ---------------------------------------------------------------
read_and_convert_prevah_bin_raster <- function(file, crop_ext_vec = NULL) {
  r <- rast(read.prevah(file))  # Read and convert
  if (!is.null(crop_ext_vec)) {
    r <- crop(r, ext(crop_ext_vec))
  }
  return(r)
}

read_nc_raster <- function(meteo_dir, scenario, ensemble = NULL, variable) {
  if (!is.null(ensemble)) {
    scenario <- paste0(scenario, "_", ensemble)
  }
  file_path <- file.path(meteo_dir, scenario, variable, paste0(scenario, "_", variable, input_file_suffix))
  r <- rast(file_path)
  return(r)
}

get_center_lat_from_raster <- function(r) {
  stopifnot(inherits(r, "SpatRaster"))
  
  # Get center of raster in native CRS using numeric index
  ex <- ext(r)
  center_coords <- c((ex[1] + ex[2]) / 2, (ex[3] + ex[4]) / 2)
  
  # Create center point geometry in raster CRS
  center_point <- vect(matrix(center_coords, ncol = 2), type = "points", crs = crs(r))
  
  # Check if CRS is already WGS84 (EPSG:4326)
  if (crs(r) != "EPSG:4326") {
    center_point <- project(center_point, "EPSG:4326")
  }
  
  # Return latitude (y)
  return(geom(center_point)[, "y"])
}

crop_by_raster <- function(r_stack, crop_shape_path) {
  crop_shape <- read_and_convert_prevah_bin_raster(crop_shape_path)
  
  return(cropped)
}

# Crop and mask
crop_and_mask_by_polygon <- function(r_stack, crop_shape_path) {
  crop_shape <- vect(crop_shape_path)
  crop_shape <- project(crop_shape, crs(r_stack))
  
  masked <- mask(crop(r_stack, crop_shape), crop_shape)
  return(masked)
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

bias_correct_sund_rel <- function(r_stack, logit_diff = 1.077914) {
  r_stack_logit <- logit_transform(r_stack)
  r_stack_logit_bc <- r_stack_logit - logit_diff
  r_stack_bc <- inv_logit_transform(r_stack_logit_bc)
  return(r_stack_bc)
}

apply_sund_bc_parallel <- function(
    chunk_dir,
    crop_shape_path,
    pattern = ".*sund_rel_chunk_.*\\.nc$"
) {
  files <- list.files(chunk_dir, pattern = pattern, full.names = TRUE)
  if (length(files) == 0) stop("No chunk files found.")
  
  plan(multisession, workers = 8)
  
  process_chunk <- function(file) {
    r_stack <- rast(file)
    
    # 1. Crop + mask
    r_stack_crop <- crop_and_mask_by_polygon(r_stack, crop_shape_path)
    #r_resample_crop <- crop_and_mask_by_polygon(r_resample, crop_shape_path)
    
    r_stack_logit <- logit_transform(r_stack_crop)
    
    r_stack_logit_bc_raw <- r_stack_logit - logit_diff_raw
    r_stack_logit_bc_res <- r_stack_logit - logit_diff_res
    
    r_stack_bc_raw <- inv_logit_transform(r_stack_logit_bc_raw)
    r_stack_bc_res <- inv_logit_transform(r_stack_logit_bc_res)
    
    # 3. Compute per-layer means
    dates <- time(r_stack)
    sund_means_bc_raw <- global(r_stack_bc_raw, "mean", na.rm = TRUE)[, 1]
    sund_means_bc_res <- global(r_stack_bc_res, "mean", na.rm = TRUE)[, 1]
    
    # Extract ensemble from filename
    ens_match <- regmatches(file, regexpr("ens\\d+", file))
    ensemble <- if (length(ens_match) == 0 || ens_match == "") "none" else ens_match
    
    # 4. Store results in DT
    data.table(
      date = as.Date(dates),
      ensemble = ensemble,
      sund_rel_bc_raw = sund_means_bc_raw,
      sund_rel_bc_res = sund_means_bc_res
    )
  }
  
  # Process all chunks in parallel
  dt_list <- future_lapply(files, process_chunk)
  
  # Combine results
  sund_stats_dt <- rbindlist(dt_list)
  setorder(sund_stats_dt, ensemble, date)
  
  fwrite(sund_stats_dt, file.path(chunk_dir, paste0(scenario, "_sund_rel_bc_stats.csv")))
  saveRDS(sund_stats_dt, file.path(chunk_dir, paste0(scenario, "_sund_rel_bc_stats.rds")))
  
  return(sund_stats_dt)
}

# Mean value over all layers and cells
stack_mean_value <- function(r_stack) {
  global(r_stack, "mean", na.rm = TRUE)[1, 1]
}

compute_stack_stats <- function(r_stack) {
  stopifnot(inherits(r_stack, "SpatRaster"))
  
  # Use global() to compute all stats per layer
  stats_dt <- data.table::as.data.table(terra::global(
    r_stack,
    fun = c("mean", "max", "min", "sd"),
    na.rm = TRUE
  ))
  
  # Add layer names for clarity
  stats_dt[, layer := names(r_stack)]
  data.table::setcolorder(stats_dt, c("layer", "mean", "max", "min", "sd"))
  
  return(stats_dt)
}

export_to_netcdf <- function(r_stack, out_dir, scenario, ensemble, varname, varunit = "units") {
  
  save_dir <- file.path(out_dir, scenario, varname)
  # Ensure the directory exists
  if (!dir.exists(save_dir)) {
    dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
  }
  file_name <- if (scenario == "hindcast") {
    paste0(scenario, "_", varname, ".nc")
  } else {
    paste0(scenario, "_", ensemble, "_", varname, ".nc")
  }
  writeCDF(
    x = r_stack,
    filename = file.path(save_dir, file_name),
    varname = varname,
    unit = varunit,
    overwrite = TRUE,
    zname = "time",
    compression = 4
  )
  message("Exported NetCDF: ", file_name)
}


# code to read data -------------------------------------------------------
# ----------------------------
# Step 1: Get center latitude from hindcast raster
# ----------------------------

extents_dir <- file.path(input_dir_meteo, "extents")

hind_rast_path <- file.path(extents_dir, "ssd_19910101.2km")
knmi_rast_path <- file.path(extents_dir, "sund19910101.2km")
rhine_bsn_path <- file.path(extents_dir, "cchydro_Rhine_basin.shp")

hind_rast <- read_and_convert_prevah_bin_raster(hind_rast_path)
knmi_rast <- read_and_convert_prevah_bin_raster(knmi_rast_path)
rhine_bsn_shp <- vect(rhine_bsn_path)

rhine_basin_shp <- project(rhine_basin_shp, crs(hind_rast))
knmi_rast_crop <- crop(knmi_rast, hind_rast)
hind_rast_res <- resample(hind_rast, knmi_rast_crop, method = "bilinear")

hind_ext_vec <- as.vector(ext(ssd__rast))
knmi_ext_vec <- as.vector(ext(knmi_rast))

center_lat <- get_center_lat_from_raster(hind_rast)

# ----------------------------
# Step 2: Process hindcast
# ----------------------------
message("Processing hindcast scenario")

# ----------------------------
# Step 3: Process KNMI reference (ens1 to ens8)
# ----------------------------
message("Processing reference scenario")

# compute bc
message("[", format(Sys.time(), "%H:%M:%S"), "] computing reference BC")
reference_sund_bc_stats <- apply_sund_bc_parallel(
  chunk_dir = file.path(input_dir, "reference", "sund_rel"),
  crop_shape_path = rhine_bsn_path,
  pattern = ".*sund_rel_chunk_.*\\.nc$"
)
message("[", format(Sys.time(), "%H:%M:%S"), "] BC computed")

reference_sund_bc_stats[, `:=`(
  basin = "hydro_CH",
  scenario = "none",
  variant = "none",
  horizon = "ref"
)]


setnames(reference_sund_bc_stats, old = "ensemble", new = "member")

reference_sund_bc_stats <- add_scenario_horizon_grouping_columns(reference_sund_bc_stats)

reference_sund_stats <- merge(reference_sund_stats, reference_sund_bc_stats[, .(date, member, sund_rel_bc_raw, sund_rel_bc_res)], by = c("date", "member"))

hindcast_sund_stats[, `:=`(
  sund_rel_bc_raw = sund_rel_raw,
  sund_rel_bc_res = sund_rel_res
)]

sund_stats_dt <- rbind(hindcast_sund_stats, reference_sund_stats,
                       use.names = TRUE, fill = FALSE)



