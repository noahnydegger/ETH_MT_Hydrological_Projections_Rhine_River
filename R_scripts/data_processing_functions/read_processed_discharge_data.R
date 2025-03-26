library(here)
library(data.table)

# project directory
home_dir <- file.path(here::here())

# input directories
input_dir_prevah <- file.path(home_dir, "Data", "Rheinblick2027", "processed_prevah_output")
input_dir_larsim <- file.path(home_dir, "Data", "Rheinblick2027", "processed_larsim_output")
input_dir_wflow <- file.path(home_dir, "Data", "Rheinblick2027", "processed_wflow_output")

station_list <- c(
  "Basel Rheinhalle"
)

# output directory
output_dir <- file.path(home_dir, "Data", "Rheinblick2027", "provessed_discharge_data")

input_file_suffix_knmi <- ".csv"

output_file_name <- "knmi_discharge_data"


# code to read data from each input dir -------------------------------------
input_dirs <- c(input_dir_prevah, input_dir_larsim, input_dir_wflow)
# Initialize an empty list to store data from each directory
discharge_data_list <- list()

# Loop through each input directory
for (dir in input_dirs) {
  
  for (station_folder in station_list) {
    
    # Construct the path to the current subfolder
    station_folder_dir <- file.path(dir, station_folder)
    
    # Check if the subfolder exists
    if (!dir.exists(station_folder_dir)) {
      warning(paste("Subfolder", station_folder, "not found in", dir))
      next  # Skip this subfolder if it doesn't exist
    }
    
    # List all CSV files in the subfolder
    csv_files <- list.files(station_folder_dir, pattern = "\\.csv$", full.names = TRUE)
    
    # Read each CSV file and append the data
    if (length(csv_files) > 0) {
      discharge_data_list[[paste0(dir, "_", station_folder)]] <- rbindlist(
        lapply(csv_files, fread), 
        fill = TRUE
      )
    }
  }
}

# read larsim hindcast
larsim_hindcast_dt <- fread(file.path(input_dir_larsim, "q-basel_q-kaub_hindcast-larsim", "q-basel_q-kaub_hindcast-larsim.csv"))
setnames(larsim_hindcast_dt, "runoff", "discharge")
# Add new columns
larsim_hindcast_dt[, `:=`(
  unit = "m3/s",
  horizon = "ref",
  scenario = "contr",
  variant = "none",
  member = "1",
  hydro_model = "larsim",
  source = "BfG"
)]

# Reorder columns
setcolorder(larsim_hindcast_dt, c("station", "date", "discharge", "unit", 
                  "horizon", "scenario", "variant", 
                  "member", "hydro_model", "source"))

discharge_data_list[[paste0(input_dir_larsim, "_hindcast")]] <- larsim_hindcast_dt

# Combine all data into one long data.table
knmi_discharge_dt_all <- rbindlist(discharge_data_list, fill = TRUE)

# clean the data
# Change values from "old" to "new"
knmi_discharge_dt_all[station == "Basel", station := "Basel Rheinhalle"]

knmi_discharge_dt_all[scenario == "Hd", scenario := "H"]
knmi_discharge_dt_all[scenario == "Hn", scenario := "H"]
knmi_discharge_dt_all[scenario == "Md", scenario := "M"]
knmi_discharge_dt_all[scenario == "Mn", scenario := "M"]
knmi_discharge_dt_all[scenario == "Ld", scenario := "L"]
knmi_discharge_dt_all[scenario == "Ln", scenario := "L"]


# export processed data --------------------------------------------------
# Export to .RDS format
saveRDS(knmi_discharge_dt_all, file.path(output_dir, paste0(output_file_name, ".rds")))

# Export to CSV
write.csv2(knmi_discharge_dt_all, file.path(output_dir, paste0(output_file_name, ".csv")), row.names = FALSE, quote = FALSE)
