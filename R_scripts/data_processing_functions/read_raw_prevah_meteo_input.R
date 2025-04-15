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
input_dir_meteo <- file.path(home_dir, "Data", "Rheinblick2027", "meteo")
input_dir_hind <- file.path(input_dir_meteo, "hindcast")

# output directory
output_dir <- file.path(home_dir, "Data", "Rheinblick2027", "processed_meteo")

input_file_suffix <- ".2km"

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

get_file_list <- function(meteo_dir, prefix_list, scenario) {
  
  scenario_dir <- file.path(meteo_dir, scenario)
  
  # Build regex pattern from prefix list names
  if (length(prefix_list) == 1) {
    prefix_regex <- paste0("^", names(prefix_list))
  } else {
    prefix_regex <- paste0("^(", paste(names(prefix_list), collapse = "|"), ")")
  }
  
  # List all matching files recursively
  pattern <- paste0(prefix_regex, ".*\\.2km$")
  meteo_files <- list.files(scenario_dir, pattern = pattern, recursive = TRUE, full.names = TRUE)
  
  # Extract dates from filenames
  meteo_dates <- as.Date(gsub(".*(\\d{8})\\.2km$", "\\1", meteo_files), format = "%Y%m%d")
  
  # Determine valid years based on scenario
  if (scenario %in% c("reference", "hindcast")) {
    years <- 1991:2020
  } else {
    horizon <- as.integer(str_extract(scenario, "\\d{4}$"))
    if (is.na(horizon)) stop("Failed to extract horizon from scenario: ", scenario)
    years <- (horizon - 14):(horizon + 15)
  }
  
  # Filter files by year
  valid_indices <- as.integer(format(meteo_dates, "%Y")) %in% years
  meteo_files <- meteo_files[valid_indices]
  meteo_dates <- meteo_dates[valid_indices]
  
  # Extract ensemble name from path (assumes /ensX/ structure)
  ens_names <- if (scenario %in% c("hindcast")) {
    "none"
  } else {
    gsub("/", "", str_extract(meteo_files, "/ens\\d+/"))
  }
  
  # Build data.table
  files_dt <- data.table(file = meteo_files, date = meteo_dates)
  files_dt[, variable := {
    prefix <- regmatches(basename(file), regexpr(prefix_regex, basename(file)))
    prefix_list[prefix]
  }]
  files_dt[, scenario := scenario]
  files_dt[, ensemble := ens_names]
  
  return(files_dt)
}

read_and_stack_raster <- function(file_dt, reader_fun) {
  message("Reading ", nrow(file_dt), " rasters...")
  rast_list <- lapply(file_dt$file, reader_fun)
  r_stack <- rast(rast_list)
  time(r_stack) <- file_dt$date
  names(r_stack) <- format(file_dt$date, "%Y-%m-%d")
  return(r_stack)
}

read_ensemble_stacks <- function(file_dt, reader_fun) {
  ens_list <- split(file_dt, by = "ensemble", drop = TRUE)
  stack_list <- list()
  
  for (i in seq_along(ens_list)) {
    ens <- names(ens_list)[i]
    dt <- ens_list[[i]]
    message("Reading ensemble: ", ens)
    r_stack <- read_and_stack_raster(dt, reader_fun)
    stack_list[[ens]] <- r_stack
  }
  
  # # Combine: this will give time × space × ensemble ordering
  # combined <- rast(stack_list)
  # names(combined) <- unlist(lapply(names(stack_list), function(ens) {
  #   paste0(ens, "_", format(time(stack_list[[ens]]), "%Y-%m-%d"))
  # }))
  
  return(stack_list)
}

compute_relative_sund_raster <- function(r_stack_abs, lat) {
  stopifnot(inherits(r_stack_abs, "SpatRaster"))
  
  # Extract time from raster (must be set beforehand)
  dates <- time(r_stack_abs)
  stopifnot(!any(is.na(dates)))  # Ensure time info is present
  
  # Convert to DOY and cap at 365
  doy_vec <- yday(dates)
  doy_vec[doy_vec == 366] <- 365
  
  # Calculate daylengths (in hours) for each date
  daylength_vec <- daylength(lat = lat, doy = doy_vec)
  
  # Clamp values in absolute raster: no negative sunshine
  r_stack_abs <- clamp(r_stack_abs, lower = 0)
  
  # Compute relative sunshine raster
  r_stack_rel <- r_stack_abs
  stopifnot(length(daylength_vec) == nlyr(r_stack_abs))
  
  r_stack_rel <- r_stack_abs /daylength_vec
  
  # Clamp values in relative raster: max relative duration is 1
  r_stack_rel <- clamp(r_stack_rel, upper = 1)
  
  # Preserve time and layer names
  time(r_stack_rel) <- dates
  names(r_stack_rel) <- format(dates, "%Y-%m-%d")
  
  return(r_stack_rel)
}

compute_absolute_sund_raster <- function(r_stack, lat) {
  stopifnot(inherits(r_stack, "SpatRaster"))
  
  # Extract time from raster (must be set beforehand)
  dates <- time(r_stack)
  stopifnot(!any(is.na(dates)))  # Ensure time info is present
  
  # Convert to DOY and cap at 365
  doy_vec <- yday(dates)
  doy_vec[doy_vec == 366] <- 365
  
  # Calculate daylengths (in hours) for each date
  daylength_vec <- daylength(lat = lat, doy = doy_vec)
  
  # Clamp values in absolute raster: no negative sunshine
  r_stack <- clamp(r_stack, lower = 0, upper = 1)
  
  # Compute relative sunshine raster
  r_stack_abs <- r_stack
  stopifnot(length(daylength_vec) == nlyr(r_stack_abs))
  
  r_stack_abs <- r_stack * daylength_vec
  
  # Preserve time and layer names
  time(r_stack_abs) <- dates
  names(r_stack_abs) <- format(dates, "%Y-%m-%d")
  
  return(r_stack_abs)
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

# Mean value over all layers and cells
stack_mean_value <- function(r_stack) {
  global(r_stack, "mean", na.rm = TRUE)[1, 1]
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

read_and_process_ensemble <- function(files_dt) {
  ens <- unique(files_dt$ensemble)
  message("Reading ensemble: ", ens)
  
  r_stack <- read_and_stack_raster(files_dt, read_and_convert_prevah_bin_raster)
  
  export_to_netcdf(
    r_stack = r_stack,
    out_dir = output_dir,
    scenario = "reference",
    ensemble = ens,
    varname = "sund_rel",
    varunit = "%"
  )
  
  r_stack_abs <- compute_absolute_sund_raster(
    r_stack = r_stack,
    lat = center_lat
  )
  
  export_to_netcdf(
    r_stack = r_stack_abs,
    out_dir = output_dir,
    scenario = "reference",
    ensemble = ens,
    varname = "sund_abs",
    varunit = "hours/d"
  )
  
  return(setNames(list(r_stack), ens))
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
cat("Processing hindcast data...\n")

hindcast_files_dt <- get_file_list(
  meteo_dir = input_dir_meteo,
  prefix_list = meteo_variables_hind,
  scenario = "hindcast"
)

hindcast_files_subset <- head(hindcast_files_dt, 5)

hindcast_r_stack_raw <- read_and_stack_raster(
  file_dt = hindcast_files_subset,
  reader_fun = read_and_convert_prevah_bin_raster
)

export_to_netcdf(
  r_stack = hindcast_r_stack_raw,
  out_dir = output_dir,
  scenario = "hindcast",
  ensemble = "none",
  varname = "sund_abs",
  varunit = "hours/d"
)

hindcast_r_stack_rel <- compute_relative_sund_raster(
  r_stack = hindcast_r_stack_raw,
  lat = center_lat
)

export_to_netcdf(
  r_stack = hindcast_r_stack_rel,
  out_dir = output_dir,
  scenario = "hindcast",
  ensemble = "none",
  varname = "sund_rel",
  varunit = "%"
)

# ----------------------------
# Step 3: Process KNMI reference (ens1 to ens8)
# ----------------------------
cat("Processing reference data...\n")
reference_files_dt <- get_file_list(
  meteo_dir = input_dir_meteo,
  prefix_list = meteo_variables_knmi,
  scenario = "reference"
)

reference_files_subset <- reference_files_dt[, .SD[1:5], by = ensemble]

ens_files_list <- split(reference_files_subset, by = "ensemble", drop = TRUE)

# Run in parallel
plan(multisession, workers = 8)
ensemble_result_list <- future_lapply(ens_files_list, read_and_process_ensemble, future.seed = TRUE)

# Combine named list back into one
ens_stack_list_raw <- do.call(c, ensemble_result_list)

# ens_stack_list_raw <- list()
# 
# for (i in seq_along(ens_files_list)) {
#   ens <- names(ens_files_list)[i]
#   files_dt <- ens_files_list[[i]]
#   message("Reading ensemble: ", ens)
#   r_stack <- read_and_stack_raster(files_dt, read_and_convert_prevah_bin_raster)
#   ens_stack_list_raw[[ens]] <- r_stack
#   
#   export_to_netcdf(
#     r_stack = r_stack,
#     out_dir = output_dir,
#     scenario = "reference",
#     ensemble = ens,
#     varname = "sund_rel",
#     varunit = "%"
#   )
#   
#   r_stack_abs <- compute_absolute_sund_raster(
#     r_stack = r_stack,
#     lat = center_lat
#   )
#   
#   export_to_netcdf(
#     r_stack = r_stack_abs,
#     out_dir = output_dir,
#     scenario = "reference",
#     ensemble = ens,
#     varname = "sund_abs",
#     varunit = "hours/d"
#   )
# }

