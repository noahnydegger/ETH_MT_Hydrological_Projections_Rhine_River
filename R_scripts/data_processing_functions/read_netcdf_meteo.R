library(terra)
library(geosphere)

# Function to read and convert PREVAH binary files
read_and_convert <- function(file) {
  prevah_bin_file <- read.prevah(file)  # Read binary file
  terra::rast(prevah_bin_file)          # Convert to SpatRaster
}

# Define the file path
file_p_sund <- "/Volumes/MT_case_sensitive/ETH_MT_Hydrological_Projections_Rhine_River/Data/Rheinblick2027/meteo/reference/ens1/Full/1991/19910101/sund19910101.2km"
file_p_ssd <- "/Volumes/MT_case_sensitive/ETH_MT_Hydrological_Projections_Rhine_River/Data/Rheinblick2027/meteo/control/Full/1991/19910101/ssd_19910101.2km"

raster_sund <- read_and_convert(file_p_sund)
raster_ssd <- read_and_convert(file_p_ssd)

ext_sund <- ext(raster_sund)
ext_ssd <- ext(raster_ssd)

# function to check if one extent is contained in another using [1,2,3,4]
ext1_in_ext2 <- function(ext1, ext2) {
  all(ext1[1] >= ext2[1] & ext1[2] <= ext2[2] & ext1[3] >= ext2[3] & ext1[4] <= ext2[4])
}

# Plot the first raster
plot(raster_sund, main="Raster 1 with Raster 2 Extent Overlay")

# Overlay the extent of r2 using a red rectangle
rect(ext_ssd[1], ext_ssd[3], ext_ssd[2], ext_ssd[4], border="red", lwd=2)

# Extract metadata from file path
path_parts <- strsplit(file_path, "/")[[1]]
scenario <- path_parts[length(path_parts) - 4]  # "reference"
#ensemble <- path_parts[length(path_parts) - 3]  # "ens1"
year <- path_parts[length(path_parts) - 2]      # "1991"
date <- path_parts[length(path_parts) - 1]      # "19910101" (from parent folder)

# Define the directory for the entire year
year_dir <- dirname(dirname(file_path))  # Removes "YYYYMMDD" folder

# List all .2km files in the year directory that start with "sund"
year_files <- list.files(year_dir, pattern = paste0("^ssd_", year, ".*\\.2km$"), 
                         full.names = TRUE, recursive = TRUE)

# Load all "sund" rasters for that year
if (length(year_files) > 0) {
  # Read each file and convert to SpatRaster
  raster_list <- lapply(year_files, read_and_convert)
  
  # Stack all rasters into a single SpatRaster object
  year_rasters <- rast(raster_list)
  
  # Add metadata as layer names
  names(year_rasters) <- gsub(".2km", "", basename(year_files))  # Remove .2km extension
  
  # Attach metadata as attributes
  attr(year_rasters, "scenario") <- scenario
  attr(year_rasters, "ensemble") <- ensemble
  attr(year_rasters, "year") <- year
  attr(year_rasters, "date") <- date
  
  # Print summary
  print(year_rasters)
} else {
  print("No matching 'sund' rasters found for the given year.")
}

# Extract dates from filenames
dates <- as.Date(gsub("sund(\\d{8})\\.2km", "\\1", basename(year_files)), format = "%Y%m%d")
# Compute daily mean for each raster in the stack
sund_means <- global(year_rasters, mean, na.rm = TRUE)

# Create data.table with date and mean values
sund_means_dt <- data.table(date = dates, sund_mean = sund_means)

plot(year_rasters[[176]], main='Raster with 50 cells')


abs_sund <- daylength(46.947456, 1)
