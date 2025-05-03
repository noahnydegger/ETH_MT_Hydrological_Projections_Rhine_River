#' @export
write.prevah <- function(file, data,
                         na_value = -9999) {
  
  sd <- raster::cellStats(data,stat = sd)
  mean <- raster::cellStats(data, stat = mean)
  min <- raster::cellStats(data, stat=min)
  max <- raster::cellStats(data, stat=max)
  sum <- raster::cellStats(data, stat=sum)
  
  
  
  
  data[is.na(data)] <- na_value
  count.values <- length(data@data@values) -
    length(data@data@values[data@data@values==na_value])
  
  
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
