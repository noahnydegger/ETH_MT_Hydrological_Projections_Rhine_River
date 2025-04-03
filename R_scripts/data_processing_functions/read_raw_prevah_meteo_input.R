library(here)
library(future.apply)
library(data.table)
library(terra)
library(geosphere)
library(sirad)

plan(multisession, workers = 8)

# project directory
home_dir <- file.path(here::here())

# input directories
# input directories
input_dir_knmi <- file.path(home_dir, "Data", "Rheinblick2027", "meteo", "reference")
input_dir_hind <- file.path(home_dir, "Data", "Rheinblick2027", "meteo", "hindcast")

# output directory
output_dir <- file.path(home_dir, "Data", "Rheinblick2027", "processed_prevah_input")

input_file_suffix <- ".2km"

output_file_name <- "prevah_meteo_input_knmi"

meteo_variables_knmi <- c(
  "sund" = "sund_rel", 
  "radg" = "radg_abs"
)

meteo_variables_hind <- c(
  "ssd_" = "sund_abs", 
  "rad_" = "radg_abs"
)

ensembles <- paste0("ens", 1:8)
years <- 1991:2020

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

get_center_lat_from_raster_center <- function(r) {
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

get_file_list <- function(dir, prefix_list, years) {
  
  # Build regex pattern from prefix list names
  prefix_regex <- paste0("^(", paste(names(prefix_list), collapse = "|"), ")")
  
  # List all matching files recursively
  pattern <- paste0(prefix_regex, ".*\\.2km$")
  
  # List all matching files
  meteo_files <- list.files(dir, pattern = pattern, recursive = TRUE, full.names = TRUE)
  
  # Extract dates from filenames
  meteo_dates <- as.Date(gsub(".*(\\d{8})\\.2km$", "\\1", meteo_files), format = "%Y%m%d")
  
  # Filter files by year
  valid_indices <- as.integer(format(meteo_dates, "%Y")) %in% years
  meteo_files <- meteo_files[valid_indices]
  meteo_dates <- meteo_dates[valid_indices]
  
  # Create data.table
  files_dt <- data.table(file = meteo_files, date = meteo_dates)
  
  # Assign variable names based on prefix
  files_dt[, variable := {
    prefix <- regmatches(basename(file), regexpr(prefix_regex, basename(file)))
    prefix_list[prefix]
  }]
  
  return(files_dt)
}

read_meteo_raster <- function(files_dt, prefix_list, crop_ext_vec = NULL) {
  stopifnot(all(c("file", "date", "variable") %in% names(files_dt)))
  
  # Define worker-safe function
  safe_read <- function(f, crop) {
    r <- read_and_convert_prevah_bin_raster(f, crop)
    c(mean = global(r, mean, na.rm = TRUE)[1, 1],
      max  = global(r, max,  na.rm = TRUE)[1, 1])
  }
  
  # Run in parallel
  result_matrix <- future_sapply(files_dt$file, safe_read, crop = crop_ext_vec, future.seed = TRUE)

  # Assign to two fixed columns in files_dt
  files_dt[, mean := result_matrix["mean", ]]
  files_dt[, max  := result_matrix["max",  ]]

  # Melt to long format (for mean and max)
  long_dt <- melt(
    files_dt,
    id.vars = c("date", "variable"),
    measure.vars = c("mean", "max"),
    variable.name = "stat",
    value.name = "value"
  )

  # Step 3: Create a combined column name like "temp_mean", "radg_abs_max", etc.
  long_dt[, var_stat := paste0(variable, "_", stat)]

  # Step 4: Cast to wide format with one row per date
  rast_stat_dt <- dcast(long_dt, date ~ var_stat, value.var = "value")
  
  return(rast_stat_dt)
}

compute_missing_sund <- function(dt, lat) {
  stopifnot("date" %in% names(dt))
  
  # Calculate day of year
  dt[, doy := yday(date)]
  dt[doy == 366, doy := 365]
  
  # Calculate daylength in hours
  dt[, daylength := daylength(lat = lat, doy = doy)]
  
  # Compute missing sunshine metric
  for (stat in c("mean", "max")) {
    abs_col <- paste0("sund_abs_", stat)
    rel_col <- paste0("sund_rel_", stat)
    
    if (abs_col %in% names(dt) && !(rel_col %in% names(dt))) {
      dt[, (rel_col) := get(abs_col) / daylength]
    } else if (!(abs_col %in% names(dt)) && rel_col %in% names(dt)) {
      dt[, (abs_col) := get(rel_col) * daylength]
    }
  }
  
  # Drop daylength column unless you want to keep it
  #dt[, daylength := NULL]
  
  return(dt)
}

compute_radg_rel <- function(dt) {
  stopifnot(all(c("radg_abs_mean", "radg_abs_max", "doy", "scenario") %in% names(dt)))
  
  # Ensure doy 366 is treated as 365 (optional)
  dt[doy == 366, doy := 365]
  
  # Compute group-wise maximum radg_abs_max
  dt[, max_radg_doy := max(radg_abs_max, na.rm = TRUE), by = .(scenario, doy)]
  
  # Compute relative radiation
  dt[, radg_rel_mean := radg_abs_mean / max_radg_doy]
  dt[, radg_rel_max  := radg_abs_max  / max_radg_doy]
  
  # Clean up helper column
  #dt[, max_radg_doy := NULL]
  
  return(dt)
}

process_meteo_raster <- function(dir, prefix_list, years, center_lat = NULL, crop_ext_vec = NULL) {
  # Get file list
  files_dt <<- get_file_list(dir, prefix_list, years)
  
  # Read and process rasters
  rast_mean_dt <- read_meteo_raster(files_dt, prefix_list, crop_ext_vec)
  
  # Convert date to Date class
  rast_mean_dt[, date := as.Date(date)]
  
  rast_mean_dt <- compute_missing_sund(rast_mean_dt, center_lat)
  
  return(rast_mean_dt)
  
}

# code to read data -------------------------------------------------------
# ----------------------------
# Step 1: Get center latitude from hindcast raster
# ----------------------------

ssd__rast_path <- file.path(input_dir_hind, "Full", "1991", "19910101", "ssd_19910101.2km")

ssd__rast <- read_and_convert_prevah_bin_raster(ssd__rast_path)

ssd__ext_vec <- as.vector(ext(ssd__rast))

center_lat <- get_center_lat_from_raster_center(ssd__rast)

# ----------------------------
# Step 2: Process hindcast
# ----------------------------
cat("Processing hindcast data...\n")
hindcast_dt <- process_meteo_raster(
  dir = file.path(input_dir_hind, "Full"),
  prefix_list = meteo_variables_hind,
  years = years,
  center_lat = center_lat
)

hindcast_dt[, `:=`(
  basin = basin,
  horizon = "ref",
  scenario = "contr",
  variant = "none",
  member = 1,
  model = "observed",
  source = "BAFU"
)]

# ----------------------------
# Step 3: Process KNMI reference (ens1 to ens8)
# ----------------------------
reference_list <- lapply(ensembles, function(ens) {
  cat("Processing reference data for", ens, "...\n")
  reference_dt <- process_meteo_raster(
    dir = file.path(input_dir_knmi, ens, "Full"),
    prefix_list = meteo_variables_knmi,
    years = years,
    center_lat = center_lat,
    crop_ext_vec = ssd__ext_vec
  )
  
  reference_dt[, `:=`(
    basin = basin,
    horizon = "ref",
    scenario = "none",
    variant = "none",
    member = as.integer(sub("ens", "", ens)),
    model = "KNMI",
    source = "KNMI"
  )]
  
  return(reference_dt)
})

# ----------------------------
# Step 4: Combine all into one long DT
# ----------------------------

knmi_meteo_rast_dt <- rbindlist(c(reference_list, list(hindcast_dt)), use.names = TRUE, fill = TRUE)
knmi_meteo_rast_dt <- compute_radg_rel(knmi_meteo_rast_dt)




# Reorder columns
setcolorder(knmi_meteo_rast_dt, c(
  "basin", "date", "doy", 
  "sund_abs_max", "sund_abs_mean", "sund_rel_max", "sund_rel_mean", "daylength", 
  "radg_abs_max", "radg_abs_mean", "radg_rel_max", "radg_rel_mean", "max_radg_doy", 
  "horizon", "scenario", "variant", "member", "model", "source"
))

plan(sequential)

# export processed data ---------------------------------------------------
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
}
# Export to .RDS format
saveRDS(knmi_meteo_rast_dt, file.path(output_dir, paste0(output_file_name, ".rds")))

# Export to CSV
write.csv2(knmi_meteo_rast_dt, file.path(output_dir, paste0(output_file_name, ".csv")), row.names = FALSE, quote = FALSE)

