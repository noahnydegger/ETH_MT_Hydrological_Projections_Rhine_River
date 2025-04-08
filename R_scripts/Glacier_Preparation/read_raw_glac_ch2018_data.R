# Load libraries
library(data.table)
library(here)
library(zoo)

# project directory
home_dir <- file.path(here::here())

period_length <- 10
gebiet <- "ThS"
meteo_variables <- c("temp" = "tair", 
                     "prec" = "prec", 
                     "rad_" = "radg",
                     "ssd_" = "sund")

source(here("R_scripts", "Glacier_Preparation", "swisscors2lonlat.R"))

data_path <- file.path(home_dir, "Data", gebiet)

chains_vs_glac_file <- "chains_vs_glac_ch2018.dat"

chains_dir <- file.path(data_path, chains_vs_glac_file)

meteo_dir <- file.path(data_path, "meteoThS")
glac_dir <- file.path(data_path, "ThSGlac")

output_dir <- file.path(home_dir, "Data", "Rheinblick2027", "ch2018")

# functions ---------------------------------------------------------------
# Parse chains and glacier chains file
parse_chains_glchains <- function(file_path) {
  # Read all lines from the file
  lines <- trimws(readLines(file_path))
  
  # Extract the chain and glchain sections
  chains_start <- grep("^chains=\\(", lines)
  glchains_start <- grep("^glchain=\\(", lines)
  
  # Extract relevant lines
  chains <- lines[(chains_start + 1):(glchains_start - 1)]
  glchains <- lines[(glchains_start + 1):length(lines)]
  chains <- chains[chains != ")" & chains != ""]
  glchains <- glchains[glchains != ")" & glchains != ""]
  data.table::data.table(chain = chains, glchain = glchains[seq_along(chains)])
}

read_chain_meteo <- function(chain, meteo_dir, meteo_variables) {
  all_vars_dt <- list()
  
  for (old_var in names(meteo_variables)) {
    new_prefix <- meteo_variables[[old_var]]
    meteo_file <- file.path(meteo_dir, paste0(chain, "_g73"), "ThS200", paste0(old_var, "_full.stats"))
    
    if (file.exists(meteo_file)) {
      meteo_dt <- fread(meteo_file)
      meteo_dt <- meteo_dt[YYYY >= 1981]
      
      # Create a proper Date column
      meteo_dt[, date := as.Date(sprintf("%04d-%02d-%02d", YYYY, MM, DD))]
      
      # Keep only Date and the statistics columns
      stats_cols <- c("MIN", "MAX", "AVG", "STDEV")
      existing_cols <- intersect(stats_cols, names(meteo_dt))
      meteo_dt <- meteo_dt[, c("date", existing_cols), with = FALSE]
      
      # Map STDEV to std, others to lowercase
      stat_renames <- tolower(existing_cols)
      stat_renames[stat_renames == "stdev"] <- "std"
      
      # Rename with prefix
      setnames(
        meteo_dt,
        old = existing_cols,
        new = paste0(new_prefix, "_", stat_renames)
      )
      
      # Add basin column
      meteo_dt[, basin := "ThS200"]
      
      all_vars_dt[[new_prefix]] <- meteo_dt
    }
  }
  
  # Merge all variables by Date
  final_dt <- Reduce(function(x, y) merge(x, y, by = "date", all = TRUE), all_vars_dt)
  
  # Add chain column
  final_dt[, chain := chain]
  
  return(final_dt)
}

# Process one glacier file
process_one_Glac_file <- function(files, i) {
  to.read <- file(files[i], "rb")
  col <- readBin(to.read, double(), size=4, n=1, endian="little")
  row <- readBin(to.read, double(), size=4, n=1, endian="little")
  xu <- readBin(to.read, double(), size=4, n=1, endian="little")
  yu <- readBin(to.read, double(), size=4, n=1, endian="little")
  dist <- readBin(to.read, double(), size=4, n=1, endian="little")
  nodata <- readBin(to.read, double(), size=4, n=1, endian="little")
  swiss.lon <- seq(xu, xu + (dist * (col - 1)), by = dist)
  swiss.lat <- seq(yu, yu + (dist * (row - 1)), by = dist)
  len <- (row * col) + 12
  close(to.read)
  
  to.read <- file(files[i], "rb")
  a <- readBin(to.read, double(), size = 4, n = len, endian = "little")
  close(to.read)
  a <- array(a[13:length(a)], dim = c(col, row))
  a_corr <- a[, ncol(a):1]
  a_corr[a_corr == nodata] <- NA
  return(a_corr)
}

# Process all glacier files in a directory
process_all_Glac_files <- function(glac_dir) {
  glac_files <- list.files(glac_dir, full.names = TRUE)
  rbindlist(lapply(glac_files, function(f) {
    year <- as.integer(gsub(".*ela(\\d{4})\\.bin$", "\\1", basename(f)))
    alldata <- process_one_Glac_file(glac_files, which(glac_files == f))
    data.table(
      YYYY = year,
      count_nogl = sum(alldata == 0, na.rm = TRUE),
      count_abla = sum(alldata == 1, na.rm = TRUE),
      count_accu = sum(alldata == 2, na.rm = TRUE),
      count_NA = sum(is.na(alldata)),
      count_glac = sum(alldata %in% 1:2, na.rm = TRUE),
      count_area = sum(!is.na(alldata))
    )
  }))
}

export_processed_data <- function(dt, output_dir, output_file_name) {
  # Ensure output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Export to .RDS format
  saveRDS(dt, file.path(output_dir, paste0(output_file_name, ".rds")))
  
  # Export to CSV format with semicolon delimiter (write.csv2)
  write.csv2(
    dt,
    file.path(output_dir, paste0(output_file_name, ".csv")),
    row.names = FALSE,
    quote = FALSE
  )
}

# code to read the data ------------------------------------
ch2018_chain_glchain_dt <- parse_chains_glchains(chains_dir)
ch2018_glacier_dt <- data.table()
ch2018_meteo_dt <- data.table()

for (i in seq_len(nrow(ch2018_chain_glchain_dt))) {
  chain <- ch2018_chain_glchain_dt$chain[i]
  glchain <- ch2018_chain_glchain_dt$glchain[i]
  
  cat("Processing chain:", chain, "\n")
  # Process meteo files using the new function
  meteo_dt <- read_chain_meteo(chain, meteo_dir, meteo_variables)
  ch2018_meteo_dt <- rbind(ch2018_meteo_dt, meteo_dt, fill = TRUE)
  
  # Process glacier data
  glac_chain_dir <- file.path(glac_dir, glchain)
  glac_dt <- process_all_Glac_files(glac_chain_dir)
  glac_dt[, chain := chain]
  glac_dt[, glchain := glchain]
  ch2018_glacier_dt <- rbind(ch2018_glacier_dt, glac_dt, fill = TRUE)
}

# export the processed data ---------------------------------
export_processed_data(ch2018_chain_glchain_dt, output_dir, "ch2018_chains_vs_glac")
export_processed_data(ch2018_meteo_dt, output_dir, "ch2018_meteo")
export_processed_data(ch2018_glacier_dt, output_dir, "ch2018_glacier")
