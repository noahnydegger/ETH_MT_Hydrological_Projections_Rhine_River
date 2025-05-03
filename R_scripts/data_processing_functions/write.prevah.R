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
