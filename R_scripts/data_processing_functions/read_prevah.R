#############################################################################################################
# function to read prevah files
#
#
# autors: Dorothea Hug-Peter, Florian Lustenberger, Luzi Bernhard
# mail adresses: dorothea.hug@wsl.ch, florian.lustenberger@wsl.ch, luzi.bernhard@wsl.ch
#############################################################################################################



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
