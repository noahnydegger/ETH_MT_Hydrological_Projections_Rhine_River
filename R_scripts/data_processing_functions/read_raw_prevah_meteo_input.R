library(here)
library(future.apply)
library(progressr)
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
  "sund" = "sund_rel",
  "radg" = "radg_abs",
  "tair" = "tair",
  "prec" = "prec"
)

meteo_variables_hind <- c(
  "ssd_" = "sund_abs",
  "rad_" = "radg_abs",
  "temp" = "tair",
  "prec" = "prec"
)

ensembles <- paste0("ens", 1:8)

# Common metadata
basin <- "hydro_CH"

source(here("R_scripts", "data_processing_functions", "read_prevah.R"))
source(here("R_scripts", "data_processing_functions", "raster_processing_functions.R"))

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

clean_up_memory <- function(...) {
  vars <- as.character(substitute(list(...)))[-1]
  rm(list = vars, envir = .GlobalEnv)
  invisible(gc(verbose = FALSE))
}

# code to read data -------------------------------------------------------
# ----------------------------
# Step 2: Process hindcast
# ----------------------------
cat("Processing hindcast data...\n")

hindcast_files_dt <- get_file_list(
  meteo_dir = input_dir_meteo,
  prefix_list = meteo_variables_hind,
  scenario = "hindcast"
)

#hindcast_files_subset <- head(hindcast_files_dt, 5)
with_progress({
  hindcast_r_stack_raw <- read_and_stack_raster(
    file_dt = hindcast_files_dt,
    reader_fun = read_and_convert_prevah_bin_raster
  )
})

with_progress({
  export_to_netcdf(
    r_stack = hindcast_r_stack_raw,
    out_dir = output_dir,
    scenario = "hindcast",
    ensemble = "none",
    varname = "sund_abs",
    varunit = "hours/d"
  )
})
hindcast_r_stack_raw <- rast(file.path(output_dir, "hindcast", "sund_abs", "hindcast_sund_abs.nc"))
hindcast_r_stack_raw <- hindcast_r_stack_raw[[1:365]]
hindcast_r_stack_raw_sub <- hindcast_r_stack_raw[[1:10]]

export_to_netcdf(
  r_stack = hindcast_r_stack_raw,
  out_dir = output_dir,
  scenario = "hindcast",
  ensemble = "none",
  varname = "sund_abs",
  varunit = "hours/d",
  suffix = "_365"
)

plan(multisession, workers = 8)
compute_relative_sund_parallel(
  raster_path = file.path(output_dir, "hindcast", "sund_abs", "hindcast_sund_abs.nc"),
  lat = center_lat,
  chunk_size = 343,
  out_dir = output_dir, scenario = "hindcast", varname = "sund_rel", varunit = "%"
)
plan(sequential)

combine_netcdf_chunks(
  input_dir = file.path(output_dir, "hindcast", "sund_rel"),
  pattern = "hindcast_sund_rel_chunk_.*\\.nc$",
  output_file = "hindcast_sund_rel.nc",
  overwrite = TRUE
)

hindcast_r_stack_rel <- compute_relative_sund_raster(
  r_stack = hindcast_r_stack_raw,
  lat = center_lat,
  chunk_size = 100,
  out_dir = output_dir, scenario = "hindcast", varname = "sund_rel", varunit = "%"
)

with_progress({
  export_to_netcdf(
    r_stack = hindcast_r_stack_rel,
    out_dir = output_dir,
    scenario = "hindcast",
    ensemble = "none",
    varname = "sund_rel",
    varunit = "%"
  )
})

# crop raw data to rhine basin extent
scenario <- "hindcast"
variable <- "sund_rel"
var_folder <- "sund_rel"
file_pattern <- "sund_rel_chunk"
nc_dir <- file.path(output_dir, scenario, var_folder)
nc_files <- list.files(nc_dir, pattern = file_pattern, full.names = TRUE)
plan(multisession, workers = 8)
future_lapply(nc_files, resample_and_crop_raster,
              output_dir = output_dir,
              polygon_path = bsn_proj_path,
              resample_path = ref_rast_path,
              suffix = "_crop",
              future.seed = TRUE)

plan(sequential)

combine_netcdf_chunks(
  input_dir = file.path(output_dir, "hindcast"),
  output_dir
)

# Load raster and vector
r <- rast("/Volumes/MT_case_sensitive/ETH_MT_Hydrological_Projections_Rhine_River/Data/Rheinblick2027/processed_meteo/hindcast/sund_rel/hindcast_sund_rel_chunk_00001_00343.nc")
v <- vect(rhine_bsn_proj_path)

# Get and print CRS
r_crs <- crs(r)
v_crs <- crs(v)

cat("Raster CRS:\n", r_crs, "\n\n")
cat("Polygon CRS:\n", v_crs, "\n")


# ----------------------------
# Step 3: Process KNMI scenario (ens1 to ens8)
# ----------------------------
scenario <- "reference"
cat("Processing", scen, "data...\n")
knmi_scenario_files_dt <- get_file_list(
  meteo_dir = input_dir_meteo,
  prefix_list = meteo_variables_knmi,
  scenario = scenario
)

variable = "sund_rel"
date_range <- c("2086-01-01", "2115-12-31")

knmi_files_sub_dt <- knmi_scenario_files_dt[
  variable == var & date >= date_range[1] & date <= date_range[2]
]

knmi_files_sub_dt<- knmi_files_sub_dt[, .SD[1:5], by = ensemble]

ens_files_list <- split(knmi_files_sub_dt, by = "ensemble", drop = TRUE)

# read raw data from .2km files and export as NetCDF
plan(multisession, workers = 8)
future_lapply(ens_files_list, read_and_export_ensemble,
              output_dir,
              scen,
              "sund_rel",
              "%",
              "_raw", 
              future.seed = TRUE)

plan(sequential)

# split raw data into chunks
for (ens in paste0("ens", 1:1)) {
  
  scenario <- scen
  suffix <- "_raw"
  
  message("[", format(Sys.time(), "%H:%M:%S"), "] Splitting", variable, "ensemble:", ens)
  
  split_netcdf_to_chunks(
    input_file = file.path(output_dir, scenario, paste0(variable, suffix), paste0(scenario, "_", ens, "_", variable, suffix, ".nc")),
    out_dir = output_dir,
    chunk_size = 343,
    scenario = scenario,
    ensemble = ens,
    varname = variable,
    varunit = "%",
    suffix = suffix
  )
}

# crop raw data to rhine basin extent
variable <- "sund_rel"
var_folder <- "sund_rel"
file_pattern <- "sund_rel\\.nc$"
nc_dir <- file.path(output_dir, scenario, var_folder)
nc_files <- list.files(nc_dir, pattern = file_pattern, full.names = TRUE)
plan(multisession, workers = 8)
future_lapply(nc_files, resample_and_crop_raster,
              output_dir = output_dir,
              polygon_path = rhine_bsn_proj_path,
              resample_path = NULL,
              suffix = "_crop",
              future.seed = TRUE)

plan(sequential)


for (ens in c("ens3", "ens4", "ens5", "ens6", "ens7", "ens8")) {
  message("[", format(Sys.time(), "%H:%M:%S"), "] Processing ensemble: ", ens)
  
  scenario <- "reference"
  variable <- "sund_rel"
  
  split_netcdf_to_chunks(
    input_file = file.path(output_dir, scenario, variable, paste0("reference_", ens, "_", variable, ".nc")),
    out_dir = output_dir,
    chunk_size = 343,
    scenario = scenario,
    ensemble = ens,
    varname = variable,
    varunit = "%"
  )
}

# add hindcast and reference sund data
hind_sund_dt <- readRDS("/Volumes/MT_case_sensitive/ETH_MT_Hydrological_Projections_Rhine_River/Data/Rheinblick2027/processed_meteo/hindcast/sund_rel_crop/hindcast_sund_rel_stats.rds")
setDT(hind_sund_dt)

hind_sund_dt[, sund_rel_bc := NULL]
setnames(hind_sund_dt, "ensemble", "member")

hind_sund_dt[, basin := "RhB200"]
hind_sund_dt[, date := as.Date(date)]
hind_sund_dt[, scenario := "observation"]
hind_sund_dt[, variant := "observation"]
hind_sund_dt[, horizon := "observation"]
hind_sund_dt[, run_type := "observation"]
hind_sund_dt[, hydro_model := "none"]
hind_sund_dt[, source := "WSL"]

hind_sund_dt <- add_scenario_horizon_grouping_columns(hind_sund_dt)
hind_sund_dt <- add_time_period_column(hind_sund_dt)

ref_sund_raw_dt <- readRDS("/Volumes/MT_case_sensitive/ETH_MT_Hydrological_Projections_Rhine_River/Data/Rheinblick2027/processed_meteo/reference/sund_rel_crop/reference_sund_rel_stats.rds")
setDT(ref_sund_raw_dt)

ref_sund_dt <- rbindlist(list(
  ref_sund_raw_dt[, .(date, member, sund_rel = sund_rel, run_type = "no_sund_bc")],
  ref_sund_raw_dt[, .(date, member, sund_rel = sund_rel_bc, run_type = "sund_bc")]
), use.names = TRUE)

ref_sund_dt[, member := as.integer(sub("ens", "", member))]

ref_sund_dt[, basin := "RhB200"]
ref_sund_dt[, date := as.Date(date)]
ref_sund_dt[, scenario := "ref"]
ref_sund_dt[, variant := "ref"]
ref_sund_dt[, horizon := "ref"]
ref_sund_dt[, hydro_model := "none"]
ref_sund_dt[, source := "KNMI"]

ref_sund_dt <- add_scenario_horizon_grouping_columns(ref_sund_dt)
ref_sund_dt <- add_time_period_column(ref_sund_dt)

# combine hindcast, ref sund data
knmi_sund_dt <- data.table::rbindlist(list(hind_sund_dt, ref_sund_dt), use.names = TRUE)

setorder(knmi_sund_dt, scen_var_hor, member, date)

output_sund_rds <- file.path(output_dir, "knmi_sund.rds")
output_sund_csv <- file.path(output_dir, "knmi_sund.csv")

# Save as .rds (binary format)
saveRDS(knmi_sund_dt, file = output_sund_rds)
# Save as .csv (readable text format)
fwrite(knmi_sund_dt, file = output_sund_csv)

cat("Exported KNMI sund dataset to:\n", output_dir, "\n")

