library(here)
library(future.apply)
library(progressr)
library(data.table)
library(stringr)
library(ncdf4)
library(terra)
library(geosphere)

source(here("R_scripts", "data_processing_functions", "read_prevah.R"))

# functions ---------------------------------------------------------------
read_and_convert_prevah_bin_raster <- function(file, crop_ext_vec = NULL) {
  r <- rast(read.prevah(file))  # Read and convert
  if (!is.null(crop_ext_vec)) {
    r <- crop(r, ext(crop_ext_vec))
  }
  return(r)
}

get_center_lat_from_raster <- function(r) {
  stopifnot(inherits(r, c("SpatRaster", "SpatVector")))
  
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

read_and_stack_raster <- function(file_dt, reader_fun) {
  message("Reading ", nrow(file_dt), " rasters...")
  rast_list <- lapply(file_dt$file, reader_fun)
  r_stack <- rast(rast_list)
  time(r_stack) <- file_dt$date
  names(r_stack) <- format(file_dt$date, "%Y-%m-%d")
  return(r_stack)
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

compute_daylength <- function(dates, lat) {
  # Convert dates to DOY
  doy_vec <- yday(dates)
  doy_vec[doy_vec == 366] <- 365
  
  # Calculate daylengths (in hours) for each date
  daylength_vec <- daylength(lat = lat, doy = doy_vec)
  
  return(daylength_vec)
}

process_sund_rel_chunk <- function(i, raster_path, lat, chunk_size, out_dir, scenario, varname, varunit) {
  r_stack_abs <- rast(raster_path)  # Re-open inside the worker
  n <- nlyr(r_stack_abs)
  idx <- i:min(i + chunk_size - 1, n)
  
  r_chunk <- r_stack_abs[[idx]]
  dates_chunk <- time(r_chunk)
  daylength_chunk <- compute_daylength(dates_chunk, lat)
  
  # print start and end date
  start_date <- format(dates_chunk[1], "%Y-%m-%d")
  end_date <- format(dates_chunk[length(dates_chunk)], "%Y-%m-%d")
  cat("[", format(Sys.time(), "%H:%M:%S"), "] Processing layer", start_date, "to", end_date, "\n")
  
  r_chunk <- clamp(r_chunk, lower = 0)
  cat("[", format(Sys.time(), "%H:%M:%S"), "] Clamped layer   ", paste0(idx[1], ":", idx[length(idx)]), " of ", n, "\n")
  
  r_out <- app(r_chunk, function(x) pmin(x / daylength_chunk, 1))
  
  time(r_out) <- dates_chunk
  names(r_out) <- format(dates_chunk, "%Y-%m-%d")
  
  cat("[", format(Sys.time(), "%H:%M:%S"), "] Processed layer ", paste0(idx[1], ":", idx[length(idx)]), " of ", n, "\n")
  # Determine padding width based on total number of layers (n)
  pad_width <- nchar(as.character(n))
  
  # Format each index with zero-padding
  start_str <- sprintf(paste0("%0", pad_width, "d"), idx[1])
  end_str   <- sprintf(paste0("%0", pad_width, "d"), idx[length(idx)])
  
  export_to_netcdf(
    r_stack = r_out,
    out_dir = out_dir,
    scenario = scenario,
    ensemble = "none",
    varname = varname,
    varunit = varunit,
    suffix = paste0("_chunk_", start_str, "_", end_str)
  )
}

compute_relative_sund_parallel <- function(raster_path, lat, chunk_size = 100, out_dir, scenario, varname, varunit = "units") {
  cat("[", format(Sys.time(), "%H:%M:%S"), "] Computing relative sunshine...\n")
  r_stack_abs <- rast(raster_path)
  stopifnot(inherits(r_stack_abs, "SpatRaster"))
  n <- nlyr(r_stack_abs)
  cat("[", format(Sys.time(), "%H:%M:%S"), "] Number of layers: ", n, "\n")
  starts <- seq(1, n, by = chunk_size)
  
  future_lapply(
    starts,
    process_sund_rel_chunk,
    raster_path = raster_path,
    lat = lat,
    chunk_size = chunk_size,
    out_dir = out_dir,
    scenario = scenario,
    varname = varname,
    varunit = varunit
  )
}

compute_absolute_sund_raster <- function(r_stack, lat) {
  stopifnot(inherits(r_stack, "SpatRaster"))
  
  # Extract time from raster (must be set beforehand)
  dates <- time(r_stack)
  stopifnot(!any(is.na(dates)))  # Ensure time info is present
  
  # Calculate daylengths (in hours) for each date
  daylength_vec <- compute_daylength(dates, lat)
  
  # Clamp values in absolute raster: no negative sunshine
  r_stack <- clamp(r_stack, lower = 0, upper = 1)
  
  # Compute absolute sunshine raster
  stopifnot(length(daylength_vec) == nlyr(r_stack))
  
  r_stack_abs <- r_stack * daylength_vec
  
  # Preserve time and layer names
  time(r_stack_abs) <- dates
  names(r_stack_abs) <- format(dates, "%Y-%m-%d")
  
  return(r_stack_abs)
}

read_and_export_ensemble <- function(files_dt, output_dir, scenario, varname, varunit, suffix = "") {
  ens <- unique(files_dt$ensemble)
  cat("[", format(Sys.time(), "%H:%M:%S"), "] Reading ensemble:", ens, "\n")
  
  with_progress({
    r_stack <- read_and_stack_raster(files_dt, read_and_convert_prevah_bin_raster)
  })
  
  cat("[", format(Sys.time(), "%H:%M:%S"), "] Writing ensemble:", ens, "\n")
  
  export_to_netcdf(
    r_stack = r_stack,
    out_dir = output_dir,
    scenario = scenario,
    ensemble = ens,
    varname = varname,
    varunit = varunit,
    suffix = suffix
  )
}

resample_and_crop_raster <- function(r_path, output_dir, polygon_path, resample_path = NULL, suffix = "") {
  cat("[", format(Sys.time(), "%H:%M:%S"), "] Processing raster:", basename(r_path), "\n")
  # Read the NetCDF raster
  raster <- rast(r_path)
  
  # Extract metadata
  varname <- sub("_\\d+$", "", names(raster)[1])
  varunit <- terra::units(raster)[1]  # Get the unit if available
  chunk_part <- regmatches(r_path, regexpr("_chunk_[^\\.]+", r_path))
  suffix <- paste0(suffix, chunk_part)
  
  # Extract time before processing
  r_time <- time(raster)
  
  # Extract scenario and ensemble
  scenario <- basename(dirname(dirname(r_path)))
  ensemble_match <- regmatches(basename(r_path), regexpr("ens\\d+", basename(r_path)))
  ensemble <- if (length(ensemble_match) > 0) ensemble_match else "none"
  
  # Optional resample using bilinear interpolation
  if (!is.null(resample_path)) {
    resample_target <- rast(resample_path)
    raster <- resample(raster, resample_target, method = "bilinear")
  }
  
  # Load polygon shape
  polygon_shape <- vect(polygon_path)
  
  # Crop and mask to polygon
  cropped <- crop(raster, polygon_shape)
  masked <- mask(cropped, polygon_shape)
  
  time(masked) <- r_time
  
  cat("[", format(Sys.time(), "%H:%M:%S"), "] Cropped and masked raster\n")
  
  export_to_netcdf(
    r_stack = masked,
    out_dir = output_dir,
    scenario = scenario,
    ensemble = ensemble,
    varname = varname,
    varunit = varunit,
    suffix = suffix
  )
  
}

split_netcdf_to_chunks <- function(
    input_file,
    out_dir,
    chunk_size = 343,
    scenario,
    ensemble,
    varname,
    varunit,
    suffix = ""
) {
  # Ensure output directory exists
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  print(input_file)
  
  # Load full raster (lazily)
  r_full <- rast(input_file)
  n <- nlyr(r_full)
  pad_width <- nchar(as.character(n))
  
  # Set up parallel backend
  plan(multisession, workers = 8)
  
  starts <- seq(1, n, by = chunk_size)
  
  process_chunk <- function(i) {
    r_full <- rast(input_file)  # reopen safely inside worker
    
    idx <- i:min(i + chunk_size - 1, nlyr(r_full))
    r_chunk <- r_full[[idx]]
    
    dates <- time(r_chunk)
    names(r_chunk) <- format(dates, "%Y-%m-%d")
    time(r_chunk) <- dates
    
    start_str <- sprintf(paste0("%0", pad_width, "d"), idx[1])
    end_str   <- sprintf(paste0("%0", pad_width, "d"), idx[length(idx)])
    
    export_to_netcdf(
      r_stack = r_chunk,
      out_dir = out_dir,
      scenario = scenario,
      ensemble = ensemble,
      varname = varname,
      varunit = varunit,
      suffix = paste0(suffix, "_chunk_", start_str, "_", end_str)
    )
    
  }
  
  # Run all chunks in parallel
  future_lapply(starts, process_chunk)
  cat("[", format(Sys.time(), "%H:%M:%S"), "] Completed:", scenario, ensemble, "\n")
}

combine_netcdf_chunks <- function(input_dir, output_dir, pattern = "hindcast_sund_rel_crop_chunk_.*\\.nc$") {
  # List chunk files in order
  files <- list.files(input_dir, pattern = pattern, full.names = TRUE, recursive = TRUE)
  if (length(files) == 0) stop("No chunk files found.")
  
  # Sort files by chunk index (optional but helpful if file names are zero-padded)
  files <- sort(files)
  
  cat("[", format(Sys.time(), "%H:%M:%S"), "] Found", length(files), "chunk files.\n")
  
  # Read each chunk as a SpatRaster
  rasters <- lapply(files, rast)
  
  # Combine all chunks
  cat("[", format(Sys.time(), "%H:%M:%S"), "] Combining chunks into one big raster...\n")
  full_raster <- do.call(c, rasters)
  
  # Fix time metadata
  dates_all <- do.call(c, lapply(rasters, time))  # keeps date class
  time(full_raster) <- dates_all
  
  # Optional: set layer names from dates
  names(full_raster) <- format(dates_all, "%Y-%m-%d")
  
  # Write to NetCDF
  cat("[", format(Sys.time(), "%H:%M:%S"), "] Writing combined NetCDF \n")
  
  export_to_netcdf(
    r_stack = full_raster,
    out_dir = output_dir,
    scenario = "hindcast",
    ensemble = "none",
    varname = "sund_rel",
    varunit = "%",
    suffix = "_crop"
  )
  
  cat("[", format(Sys.time(), "%H:%M:%S"), "] Done! Combined file saved \n")
  
}

export_to_netcdf <- function(r_stack, out_dir, scenario, ensemble, varname, varunit = "unit", suffix = "") {
  
  save_dir <- file.path(out_dir, scenario, paste0(varname, suffix))
  # Ensure the directory exists
  if (!dir.exists(save_dir)) {
    dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
  }
  file_ending <- paste0(suffix, ".nc")
  file_name <- if (scenario == "hindcast") {
    paste0(scenario, "_", varname, file_ending)
  } else {
    paste0(scenario, "_", ensemble, "_", varname, file_ending)
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

# ----------------------------
# Step 1: Get center latitude from hindcast raster
# ----------------------------
# project directory
home_dir <- file.path(here::here())

# input directories
input_dir_meteo <- file.path(home_dir, "Data", "Rheinblick2027", "meteo")

# output directory
output_dir <- file.path(home_dir, "Data", "Rheinblick2027", "processed_meteo")

extents_dir <- file.path(output_dir, "extents")

hind_rast_path <- file.path(extents_dir, "ssd_19910101.2km")
knmi_rast_path <- file.path(extents_dir, "sund19910101.2km")
rhine_bsn_path <- file.path(extents_dir, "cchydro_Rhine_basin.shp")

hind_rast <- read_and_convert_prevah_bin_raster(hind_rast_path)
knmi_rast <- read_and_convert_prevah_bin_raster(knmi_rast_path)
rhine_bsn_shp <- vect(rhine_bsn_path)

rhine_bsn_shp <- project(rhine_bsn_shp, crs(hind_rast))
knmi_rast_crop <- crop(knmi_rast, hind_rast)
hind_rast_res <- resample(hind_rast, knmi_rast_crop, method = "bilinear")

# Create reference raster (same extent as hindcast but resolution & CRS from KNMI)
ref_rast <- crop(knmi_rast, ext(hind_rast), snap = "out")

# Fill with NA so it can be saved
values(ref_rast) <- NA

# Reproject Rhine basin to reference raster CRS
rhine_bsn_proj <- project(rhine_bsn_shp, crs(ref_rast))

# Save reference raster and projected basin shapefile
ref_rast_path <- file.path(extents_dir, "reference_raster.tif")
bsn_proj_path <- file.path(extents_dir, "rhine_bsn_projected.shp")

writeRaster(ref_rast, ref_rast_path, overwrite = TRUE)
writeVector(rhine_bsn_proj, bsn_proj_path, overwrite = TRUE)

# Output file paths for processed data
hind_rast_res_path <- file.path(extents_dir, "hind_rast_resampled.tif")
knmi_rast_crop_path <- file.path(extents_dir, "knmi_rast_cropped.tif")
rhine_bsn_proj_path <- file.path(extents_dir, "rhine_bsn_projected.shp")

# Save all to extents_dir
writeRaster(hind_rast_res, hind_rast_res_path, overwrite = TRUE)
writeRaster(knmi_rast_crop, knmi_rast_crop_path, overwrite = TRUE)
writeVector(rhine_bsn_shp, rhine_bsn_proj_path, overwrite = TRUE)

hind_ext_vec <- as.vector(ext(hind_rast))
knmi_ext_vec <- as.vector(ext(knmi_rast))

center_lat <- get_center_lat_from_raster(rhine_bsn_shp)


compare_rasters <- function(r1, r2, r3) {
  cat("----- EXTENT COMPARISON -----\n")
  print(list(
    knmi_crop = ext(r1),
    hind_res  = ext(r2),
    ref       = ext(r3)
  ))
  
  cat("\n----- RESOLUTION COMPARISON -----\n")
  print(list(
    knmi_crop = res(r1),
    hind_res  = res(r2),
    ref       = res(r3)
  ))
  
  cat("\n----- CRS COMPARISON -----\n")
  print(list(
    knmi_crop = crs(r1),
    hind_res  = crs(r2),
    ref       = crs(r3)
  ))
  
  cat("\n----- OVERLAP CHECK (intersect extents) -----\n")
  int1 <- intersect(ext(r1), ext(r2))
  int2 <- intersect(ext(r1), ext(r3))
  int3 <- intersect(ext(r2), ext(r3))
  
  print(list(
    knmi_vs_hind = !is.null(int1),
    knmi_vs_ref  = !is.null(int2),
    hind_vs_ref  = !is.null(int3)
  ))
}

# Run comparison
#compare_rasters(knmi_rast_crop, hind_rast_res, ref_rast)
