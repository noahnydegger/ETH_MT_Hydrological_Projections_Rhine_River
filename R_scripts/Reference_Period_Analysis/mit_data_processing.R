# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(here)

data_dir <- file.path("Data", "R_KNMI")

scenarios <- c(
  "reference", "Hd_2100"
)

ensenmbles <- c(
  "ens1", "ens2", "ens3", "ens4", "ens5", "ens6", "ens7", "ens8"
)

gebiete <- c(
  #"TGl200",
  "ThS200"#, "BEN200", "BiS200", "Bod400", "EmW200", "HiR200", "LaP200", "Lim200", "NeS200", "NoW200", "Rhb200", "KEm200", "SeD200","SSG200", "Thu200", "VoA200", "VoR200", "WaS200",
)


source(here("R_scripts", "data_import_functions.R"))

mit_list <- list()

for (scen in scenarios) {
  mit_list[[scen]] <- list()
  for (ens in ensenmbles) {
    mit_list[[scen]][[ens]] <- list()
    for (geb in gebiete) {
      # Construct the file path
      mit_file <- file.path(here::here(), data_dir, paste0(scen, "_", ens), geb, paste0(geb, ".mit"))
      pri_file <- file.path(here::here(), data_dir, paste0(scen, "_", ens), geb, paste0(geb, ".pri"))
      
      # Check if the mit and pri files exist before reading
      if (file.exists(mit_file) && file.exists(pri_file)) {
        
        # Import data from the .mit and .pri files
        mit_data <- import_mit_data(mit_file)
        pri_data <- import_pri_data(pri_file)
        
        # Store the loaded data in the list
        mit_list[[scen]][[ens]][[geb]][["daily"]] <- mit_data
        mit_list[[scen]][[ens]][[geb]][["monthly"]] <- pri_data$monthly
        mit_list[[scen]][[ens]][[geb]][["yearly"]] <- pri_data$yearly
        
      } else {
        # Handle missing file case
        if (!file.exists(mit_file)) {
          message(paste("Mit file not found:", mit_file))
        }
        if (!file.exists(pri_file)) {
          message(paste("Pri file not found:", pri_file))
        }
        mit_list[[scen]][[ens]][[geb]] <- NULL
      }
    }
  }
}

