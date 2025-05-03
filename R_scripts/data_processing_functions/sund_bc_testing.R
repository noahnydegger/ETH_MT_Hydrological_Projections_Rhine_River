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