library(here)
library(reshape2)

home_dir <- file.path(here::here())
input_dir <- file.path(home_dir, "Data", "Rheinblick2027", "raw_prevah_output", "routing")
output_dir <- file.path(home_dir, "Data", "Rheinblick2027", "processed_prevah_output")

input_file_prefix <- "Swissrhine200_"
input_file_suffix <- ".dat"

output_file_name <- "prevah_discharge_knmi"

column_names_prevah <- c("YYYY", "MM", "DD", "Rekingen", "Untersiggenthal", "Rheinfelden", "Basel", "Wiese")

# Define which stations to keep (leave empty `c()` to keep all)
selected_stations <- c("Basel")

gebiete <- c(
  "NoW200"
)

scenarios <- c(
  "reference"
)

read_raw_discharge_data <- function(file_path, column_names, horizon) {
  # Read the raw discharge data from the file
  raw_data <- read.table(file_path, header = FALSE)
  
  # Assign column names
  colnames(raw_data) <- column_names
  
  # Create Date column
  raw_data$date <- as.Date(paste(raw_data$YYYY, raw_data$MM, raw_data$DD, sep = "-"), format = "%Y-%m-%d")
  
  # Filter rows between 30 year periods
  if (!is.null(horizon)) {
    raw_data <- subset(raw_data, YYYY >= horizon - 14)
    raw_data <- subset(raw_data, YYYY <= horizon + 15)
  }
  
  return(raw_data)
}

# Initialize an empty list to store all processed data
all_data <- list()

# Loop over each area (gebiete)
for (geb in gebiete) {
  # Define the base path for the gebiet
  geb_path <- file.path(input_dir, geb)
  
  # Loop over each scenario
  for (scen in scenarios) {
    # Extract `horizon` from scenario name (last 4 digits) or use 2005 for "reference"
    if (scen == "reference") {
      scenario <- "R"
      horizon <- 2005
    } else {
      scenario <- substr(scen, 1, 1)
      horizon <- as.numeric(sub(".*([0-9]{4})$", "\\1", scen))
    }
    
    # Extract `variant` (2nd character of scenario, "d", "n", or "none")
    variant <- ifelse(nchar(scen) >= 2 && substr(scen, 2, 2) %in% c("d", "n"), substr(scen, 2, 2), "none")
    
    # Check if the scenario directory exists
    if (dir.exists(geb_path)) {
      
      # List all subfolders inside the scenario folder
      subfolders <- list.dirs(geb_path, recursive = FALSE)
      
      # Filter subfolders matching "scenario_ens1" to "scenario_ens8"
      matching_subfolders <- subfolders[grepl(paste0(scen, "_ens[1-8]$"), basename(subfolders))]
      
      # Loop through the matching subfolders
      for (ens_folder in matching_subfolders) {
        
        # Extract the ensemble member number (ens1 to ens8) as a numeric value
        member <- as.numeric(sub(".*_ens([1-8])$", "\\1", basename(ens_folder)))
        
        # Define the expected file path inside the subfolder (adjust filename if needed)
        data_file <- file.path(ens_folder, paste0(input_file_prefix, basename(ens_folder), input_file_suffix))  # Adjust filename if needed
        
        # Check if the file exists before reading
        if (file.exists(data_file)) {
          
          # Read the discharge data
          discharge_data <- read_raw_discharge_data(data_file, column_names_prevah, horizon)
          
          # Convert from wide to long format (assumes station names are column headers)
          discharge_long <- reshape2::melt(discharge_data, id.vars = c("YYYY", "MM", "DD", "date"),
                                           variable.name = "station", value.name = "discharge")
          
          # Filter selected stations if specified
          if (length(selected_stations) > 0) {
            discharge_long <- discharge_long[discharge_long$station %in% selected_stations, ]
          }
          
          # Add metadata columns
          discharge_long$horizon <- horizon
          discharge_long$scenario <- scenario
          discharge_long$variant <- variant
          discharge_long$member <- member
          discharge_long$hydro_model <- "PREVAH"
          
          # Select required columns and ensure correct order
          discharge_long <- discharge_long[, c("station", "date", "discharge", "horizon", "scenario", 
                                               "variant", "member", "hydro_model")]
          
          # Store in list
          all_data <- append(all_data, list(discharge_long))
          
        } else {
          print(paste("File not found:", data_file))
        }
      }
    } else {
      print(paste("Scenario path does not exist:", geb_path))
    }
  }
}

# Combine all results into a single data frame
combined_data <- do.call(rbind, all_data)

# Display final structured data
print(head(combined_data))

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
}

# Export to CSV
write.csv2(combined_data, file.path(output_dir, paste0(output_file_name, ".csv")), row.names = FALSE, quote = FALSE)

# Export to .RDS format
saveRDS(combined_data, file.path(output_dir, paste0(output_file_name, ".rds")))
        
        