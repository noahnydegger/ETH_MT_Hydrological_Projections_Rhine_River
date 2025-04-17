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
input_dir <- file.path(home_dir, "Data", "Rheinblick2027", "processed_meteo")

# output directory
output_dir <- file.path(home_dir, "Data", "Rheinblick2027", "processed_meteo")

input_file_suffix <- ".nc"

scenario_horizons <- c(
  "reference"
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

read_nc_raster <- function(input_dir, scenario, ensemble = NULL, variable) {
  
  var_path <- file.path(input_dir, scenario, variable)
  
  if (!is.null(ensemble)) {
    scenario <- paste0(scenario, "_", ensemble)
  }
  
  file_path <- file.path(var_path, paste0(scenario, "_", variable, input_file_suffix))
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
  cropped <- crop(r_stack, crop_shape)
  return(cropped)
}

crop_and_mask_by_polygon <- function(r_stack, crop_shape_path) {
  crop_shape <- vect(crop_shape_path)
  crop_shape <- project(crop_shape, crs(r_stack))
  
  masked <- mask(crop(r_stack, crop_shape), crop_shape)
  return(masked)
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

compute_sund_stats_parallel <- function(
    chunk_dir,
    crop_shape_path,
    pattern = ".*sund_rel_chunk_.*\\.nc$"
) {
  files <- list.files(chunk_dir, pattern = pattern, full.names = TRUE)
  if (length(files) == 0) stop("No chunk files found.")
  
  plan(multisession, workers = 8)
  
  process_chunk <- function(file) {
    r_stack <- rast(file)
    
    r_resample <- resample_to_knmi_grid_hind_ext(r_stack, hind_rast_path, knmi_rast_path)
    
    # 1. Crop + mask
    r_stack_crop <- crop_and_mask_by_polygon(r_stack, crop_shape_path)
    r_resample_crop <- crop_and_mask_by_polygon(r_resample, crop_shape_path)
    
    # 2. Logit transform
    r_stack_logit <- logit_transform(r_stack_crop)
    r_resample_logit <- logit_transform(r_resample_crop)
    
    # 3. Compute per-layer means
    dates <- time(r_stack)
    sund_means_raw <- global(r_stack_crop, "mean", na.rm = TRUE)[, 1]
    logit_means_raw <- global(r_stack_logit, "mean", na.rm = TRUE)[, 1]
    sund_means_res <- global(r_resample_crop, "mean", na.rm = TRUE)[, 1]
    logit_means_res <- global(r_resample_logit, "mean", na.rm = TRUE)[, 1]
    
    # Extract ensemble from filename
    ens_match <- regmatches(file, regexpr("ens\\d+", file))
    ensemble <- if (length(ens_match) == 0 || ens_match == "") "none" else ens_match
    
    # 4. Store results in DT
    data.table(
      date = as.Date(dates),
      ensemble = ensemble,
      sund_rel_raw = sund_means_raw,
      sund_rel_res = sund_means_res,
      sund_logit_raw = logit_means_raw,
      sund_logit_res = logit_means_res
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

# # Create extent rectangles as SpatVector objects
# e1 <- as.polygons(ext(knmi_rast));       crs(e1) <- crs(knmi_rast)
# e2 <- as.polygons(ext(knmi_rast_crop));  crs(e2) <- crs(knmi_rast_crop)
# e3 <- as.polygons(ext(hind_rast_res));       crs(e3) <- crs(hind_rast_res)
# shp <- as.polygons(rhine_basin_shp); crs(shp) <- crs(rhine_basin_shp)
# 
# # Plot all in one figure
# plot(e1, border = "black", lwd = 2, main = "Raster Extents")  # full extent
# plot(e2, border = "blue",  lwd = 2, main = "Raster Extents")  
# plot(e3, border = "red",   lwd = 2, add = TRUE)                # hindcast
# plot(shp, border = "green", lwd = 2, add = TRUE)
# legend("topright", legend = c("KNMI crop", "Hindcast"),
#        col = c("blue", "red"), lwd = 2, bg = "white")

# ----------------------------
# Step 2: Process hindcast
# ----------------------------
message("Processing hindcast scenario")

# hindcast_r_stack_raw <- read_nc_raster(
#   meteo_dir = input_dir_meteo,
#   scenario = "hindcast",
#   variable = "sund_rel"
# )

# resample to knmi 12 km grid
hindcast_r_stack_rel <- resample(hindcast_r_stack_rel, hind_rast_res, method = "bilinear")

# compute relative and logit mean
message("[", format(Sys.time(), "%H:%M:%S"), "] computing hindcast stats")
hindcast_sund_stats <- compute_sund_stats_parallel(
  chunk_dir = file.path(input_dir, "hindcast", "sund_rel"),
  crop_shape_path = rhine_bsn_path,
  pattern = ".*sund_rel_chunk.*\\.nc$"
)
message("[", format(Sys.time(), "%H:%M:%S"), "] stats computed")


# ----------------------------
# Step 3: Process KNMI reference (ens1 to ens8)
# ----------------------------
message("Processing reference scenario")

# ens_stack_list_raw <- list()
# 
# for (i in 1:8) {
#   ens <- paste0("ens", i)
#   ens_stack_list_raw[[ens]] <- read_nc_raster(
#     meteo_dir = input_dir_meteo,
#     scenario = "reference",
#     ensemble = ens,
#     variable = "sund_rel"
#   )
# }

logit_diff <- referenece_logit_mean - hindcast_logit_mean
