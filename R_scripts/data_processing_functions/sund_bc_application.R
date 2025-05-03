library(terra)

scenario_dir <- file.path("/Volumes/MT_case_sensitive/ETH_MT_Hydrological_Projections_Rhine_River/Data/Rheinblick2027/meteo/reference")

#' @param file_p
#' @param crs either "lv_03" or a valid crs object
#' @export
read.prevah <- function(file_p, crs = "lv_03") {
  
  #First, process the crs input
  crs_prevah <- NA
  if(crs == "lv_03") crs_prevah <- "+proj=somerc +lat_0=46.95240555555556 +lon_0=7.439583333333333 +k_0=1 +x_0=600000 +y_0=200000 +ellps=bessel +towgs84=674.4,15.1,405.3,0,0,0,0 +units=m +no_defs"
  
  if(class(crs)=="crs") crs_prevah <- crs
  if(is.na(crs_prevah)) warning("No valid crs object provided")
  
  
  files <- file_p
  #print(files)
  cols <- new.env()
  rows <- new.env()
  lons <- new.env()
  lats <- new.env()
  lengths <- new.env()
  maxlon <- c()
  minlon <- c()
  maxlat <- c()
  minlat <- c()
  
  #Read in the header data
  to.read <- file(files, "rb")
  col <- readBin(to.read, double(),
                 size = 4, n = 1, endian = "little")
  row <- readBin(to.read, double(),
                 size = 4, n = 1, endian = "little")
  xu <- readBin(to.read, double(),
                size = 4, n = 1, endian = "little")
  yu <- readBin(to.read, double(),
                size = 4, n = 1, endian = "little")
  dist <- readBin(to.read, double(),
                  size = 4, n = 1, endian = "little")
  nodata <- readBin(to.read, double(),
                    size = 4, n = 1, endian = "little")
  close(to.read)
  
  swiss.lon <- seq(xu,xu+(dist*(col-1)),by=dist)
  swiss.lat <- seq(yu,yu+(dist*(row-1)),by=dist)
  maxlon    <- max(maxlon,swiss.lon)
  minlon    <- min(minlon,swiss.lon)
  maxlat    <- max(maxlat,swiss.lat)
  minlat    <- min(minlat,swiss.lat)
  
  
  leftdow = c(chx = minlon, chy = minlat)
  rightup = c(chx = maxlon, chy = maxlat)
  
  lonlim = c(leftdow[1], rightup[1])
  latlim = c(leftdow[2], rightup[2])
  alldata = array(NA, dim = c(col,row))
  
  to.read <- file(files, "rb")
  a <- readBin(to.read, double(),
               size = 4, n = (col*row)+12,
               endian = "little")
  close(to.read)
  
  a <- a[13:length(a)]
  a <- array(a, dim = c(col, row))
  a[a == nodata] <- NA
  
  rotate <- function(x) apply(x, 1, rev)
  a1 <- t(rotate(a))
  
  
  test2 <- stars::st_as_stars(a1)
  attr(test2, "dimensions")[[2]]$offset = latlim[1]
  attr(test2, "dimensions")[[1]]$offset = lonlim[1]
  attr(test2, "dimensions")[[1]]$delta = dist
  attr(test2, "dimensions")[[2]]$delta = dist
  
  sf::st_crs(test2) <- crs_prevah
  
  
  # rf <- raster::t(raster::raster(a, ymn = lonlim[1],
  #                                xmn = latlim[1], 
  #                                ymx = lonlim[2]+dist,
  #                                xmx = latlim[2]+dist))
  
  return(test2)
}


#' @export
write.prevah <- function(file, data,
                         na_value = -9999) {
  
  sd <- global(data, fun = "sd", na.rm = TRUE) %>% as.numeric()
  mean <- global(data, fun = "mean", na.rm = TRUE) %>% as.numeric()
  min <- global(data, fun = "min", na.rm = TRUE) %>% as.numeric()
  max <- global(data, fun = "max", na.rm = TRUE) %>% as.numeric()
  sum <- global(data, fun = "sum", na.rm = TRUE) %>% as.numeric()
  
  data[is.na(data)] <- na_value
  count.values <- ncell(data) - length(data[is.na(data)])
  
  
  # write data to new file
  data_f <- data
  zz <- file(file, "wb")
  writeBin(as.numeric(ncol(data_f)),
           zz, size = 4, endian = "little")
  writeBin(as.numeric(nrow(data_f)),
           zz, size = 4, endian = "little")
  writeBin(as.numeric(xmin(data_f)),
           zz, size = 4, endian = "little")
  writeBin(as.numeric(ymin(data_f)),
           zz, size = 4, endian = "little")
  writeBin(as.numeric(xres(data_f)),
           zz, size = 4, endian = "little")
  writeBin(na_value, zz, size = 4,
           endian = "little")
  writeBin(as.numeric(count.values),
           zz, size = 4, endian = "little")
  writeBin(as.numeric(max), zz, size = 4,
           endian = "little")
  writeBin(as.numeric(min), zz, size = 4,
           endian = "little")
  writeBin(as.numeric(sum), zz, size = 4,
           endian = "little")
  writeBin(as.numeric(mean), zz, size = 4,
           endian = "little")
  writeBin(as.numeric(sd), zz, size = 4, endian = "little")
  writeBin(as.vector(data_f), zz,
           size = 4, endian = "little")
  close(zz)
  
}

#' Apply logit transformation to a raster stack
#' Clamps values to (eps, 1 - eps) to avoid infinite values
logit_transform <- function(r_stack, eps = 1e-6) {
  # Ensure values are in (0,1) interval
  r_stack_clipped <- clamp(r_stack, lower = eps, upper = 1 - eps)
  
  # Apply logit transformation
  logit_r <- log(r_stack_clipped / (1 - r_stack_clipped))
  
  return(logit_r)
}

#' Apply inverse logit transformation to a raster stack
#' Converts log-odds back to probability space (0–1)
inv_logit_transform <- function(logit_r_stack) {
  # Apply back transformation
  backtransformed <- 1 / (1 + exp(-logit_r_stack))
  
  return(backtransformed)
}

#' Bias-correct a relative sunshine raster stack in logit space
#' Shifts logit-transformed values by a constant and back-transforms
bias_correct_sund_rel <- function(r_stack, logit_diff = 1.077914) {
  # transform relative values [0, 1] to logit space
  r_stack_logit <- logit_transform(r_stack)
  
  # subtract difference (of reference - hindcast) in logit space
  r_stack_logit_bc <- r_stack_logit - logit_diff
  
  # backtransform to relative values [0, 1]
  r_stack_bc <- inv_logit_transform(r_stack_logit_bc)
  return(r_stack_bc)
}

get_file_list <- function(scenario_dir, pattern = "sund\\d{8}\\.2km$") {
  all_files <- list.files(scenario_dir, full.names = TRUE, recursive = TRUE)
  filtered_files <- all_files[grepl(pattern, all_files)]
  return(filtered_files)
}

process_sund_files <- function(sund_files) {
  if (length(sund_files) == 0) {
    warning("No sund files provided.")
    return(invisible(NULL))
  }
  
  for (file in sund_files) {
    
    # Read the original PREVAH-format file
    r <- rast(read.prevah(file))
    
    # Apply bias correction
    r_bc <- bias_correct_sund_rel(r)
    
    # Create output filename by replacing 'sund' with 'sdbc'
    file_bc <- sub("sund", "sdbc", file)
    
    # Write the corrected raster
    write.prevah(file_bc, r_bc)
  }
}

# Get the list of sund files
sund_files <- get_file_list(scenario_dir)

# Process the sund files
process_sund_files(sund_files)


