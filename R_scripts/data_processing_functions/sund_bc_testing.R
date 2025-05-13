#### testing
sund_path <- file.path("/Volumes/MT_case_sensitive/ETH_MT_Hydrological_Projections_Rhine_River/Data/Rheinblick2027/meteo/reference/ens1/Full/1983/19830101/sund19830101.2km")
sdbc_path <- file.path("/Volumes/MT_case_sensitive/ETH_MT_Hydrological_Projections_Rhine_River/Data/Rheinblick2027/meteo/reference/ens1/Full/1983/19830101/sdbc19830101.2km")

sund_r <- rast(read.prevah(sund_path))
sdbc_r <- rast(read.prevah(sdbc_path))

# color scale from 0 to 1
plot(sund_r, main = names(sund_r), zlim = c(0, 1))
plot(sdbc_r, main = names(sdbc_r), zlim = c(0, 1))

# compute mean
sund_mean <- global(sund_r, fun = "mean", na.rm = TRUE)
sdbc_mean <- global(sdbc_r, fun = "mean", na.rm = TRUE)

# print mean
print(sund_mean)
print(sdbc_mean)

print(sund_r)
print(sdbc_r)

for (ens in paste0("ens", 2:8)) {
  scen_ens_dir <- file.path(scenario_dir, ens)
  
  message("[", format(Sys.time(), "%H:%M:%S"), "] listing files ", ens)
  
  # Get the list of sund files
  sund_files <- get_file_list(scen_ens_dir)
  
  message("[", format(Sys.time(), "%H:%M:%S"), "] Processing: ", ens)
  
  # Process the sund files
  process_sund_files(sund_files)
  
  message("[", format(Sys.time(), "%H:%M:%S"), "] Finished processing: ", ens)
}

compare_rasters <- function(path1, path2) {
  # Read rasters
  r1 <- rast(read.prevah(path1))
  r2 <- rast(read.prevah(path2))
  
  # Print Extent
  cat("\n📍 Extent:\n")
  print(list(
    raster1 = ext(r1),
    raster2 = ext(r2)
  ))
  
  # Print Resolution
  cat("\n📐 Resolution:\n")
  print(list(
    raster1 = res(r1),
    raster2 = res(r2)
  ))
  
  # Print Means
  mean1 <- global(r1, "mean", na.rm = TRUE)[1, 1]
  mean2 <- global(r2, "mean", na.rm = TRUE)[1, 1]
  cat("\n📊 Mean values:\n")
  print(list(
    raster1 = mean1,
    raster2 = mean2
  ))
  
  # Print sample values
  cat("\n🔍 Sample values (first 10):\n")
  print(list(
    raster1 = values(r1)[1:10],
    raster2 = values(r2)[1:10]
  ))
  
  # Compare all properties
  same_extent <- identical(ext(r1), ext(r2))
  same_res <- identical(res(r1), res(r2))
  same_crs <- identical(crs(r1), crs(r2))
  same_values <- isTRUE(all.equal(values(r1), values(r2), check.attributes = FALSE))
  
  cat("\n🔍 Comparison results:\n")
  cat("  - Same extent: ", same_extent, "\n")
  cat("  - Same resolution: ", same_res, "\n")
  cat("  - Same CRS: ", same_crs, "\n")
  cat("  - Same values: ", same_values, "\n")
  
  identical_all <- same_extent && same_res && same_crs && same_values
  
  cat("\n✅ Rasters identical? ", identical_all, "\n")
  
  return(identical_all)
}

file_noah <- file.path("/Volumes/MT_case_sensitive/ETH_MT_Hydrological_Projections_Rhine_River/Data/Rheinblick2027/meteo/reference/ens1/Full/1983/19831231/sdbc19831231.2km")
file_schirmer <- file.path("/Volumes/MT_case_sensitive/ETH_MT_Hydrological_Projections_Rhine_River/Data/Rheinblick2027/meteo_schirmer/reference/ens1/Full/1983/19831231/sdbc19831231.2km")

compare_rasters(file_noah, file_schirmer)

