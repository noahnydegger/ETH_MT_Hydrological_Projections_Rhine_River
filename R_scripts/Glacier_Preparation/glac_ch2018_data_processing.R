library(tidyverse)
library(ggplot2)
library(dplyr)
library(here)

period_lenght <- 10
gebiet <- "ThS"

meteo_variables <- c("temp", "prec", "rad_")

source(here("R_scripts", "Glacier_Preparation", "swisscors2lonlat.R"))

chains_vs_glac_file <- "chains_vs_glac_ch2018.dat"

data_path <- file.path(here::here(), "Data", gebiet)

chains_dir <- file.path(data_path, chains_vs_glac_file)

parse_chains_glchains <- function(file_path) {
  # Read all lines from the file
  lines <- trimws(readLines(file_path))
  
  # Extract the chain and glchain sections
  chains_start <- grep("^chains=\\(", lines)
  glchains_start <- grep("^glchain=\\(", lines)
  
  # Extract relevant lines
  chains_section <- lines[(chains_start + 1):(glchains_start - 1)]
  glchains_section <- lines[(glchains_start + 1):length(lines)]
  
  # Clean sections by removing the closing parenthesis and empty strings
  chains <- chains_section[chains_section != ")" & chains_section != ""]
  glchains <- glchains_section[glchains_section != ")" & glchains_section != ""]
  
  # Ensure equal lengths for data frame creation
  if (length(chains) != length(glchains)) {
    warning("Chains and glchains lengths differ. Check the input file.")
  }
  
  # Create and return the data frame
  data.frame(chain = chains, glchain = glchains[seq_along(chains)], stringsAsFactors = FALSE)
}

process_meteo_data <- function(file) {
  # Read the data from the file
  data <- read.table(file, header = TRUE)
  
  # Filter rows before 1981 (warmup period)
  data <- data %>% filter(YYYY >= 1981)
  
  # Add a new column for 5-year periods (with the period ending in the last year)
  data$Period <- (as.numeric(data$YYYY) + period_lenght - 1) %/% period_lenght * period_lenght
  
  # Remove columns YYYY, MM, DD
  data <- data %>% select(-YYYY, -MM, -DD)
  
  # Rename the 'Period' column to 'YYYY'
  data <- data %>% rename(YYYY = Period)
  
  # Extract the base filename prefix (first 4 characters, e.g., temp or prec)
  base_prefix <- substr(basename(file), 1, 4)
  
  # Add the base prefix to all column names except the 'YYYY' column
  colnames(data)[colnames(data) != "YYYY"] <- paste0(base_prefix, "_", colnames(data)[colnames(data) != "YYYY"])
  
  # Group by decade and compute the mean
  period_means <- data %>%
    group_by(YYYY) %>%
    summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))
  
  # Return processed decade means
  return(period_means)

}

process_one_Glac_file <- function(files, i) {
  # Initialize environments and variables
  cols <- new.env()
  rows <- new.env()
  lons <- new.env()
  lats <- new.env()
  lengths <- new.env()
  
  maxlon <- c()
  minlon <- c()
  maxlat <- c()
  minlat <- c()
  
  # Process only the specified file at index i
  to.read <- file(files[i], "rb")
  col       <- readBin(to.read, double(),size=4, n = 1, endian = "little")
  row       <- readBin(to.read, double(),size=4, n = 1, endian = "little")
  xu        <- readBin(to.read, double(),size=4, n = 1, endian = "little")
  yu        <- readBin(to.read, double(),size=4, n = 1, endian = "little")
  dist      <- readBin(to.read, double(),size=4, n = 1, endian = "little")  
  nodata    <- readBin(to.read, double(),size=4, n = 1, endian = "little")
  swiss.lon <- seq(xu,xu+(dist*(col-1)),by=dist)
  swiss.lat <- seq(yu,yu+(dist*(row-1)),by=dist)
  maxlon    <- max(maxlon,swiss.lon)[1]
  minlon    <- min(minlon,swiss.lon)[1]
  maxlat    <- max(maxlat,swiss.lat)[1]
  minlat    <- min(minlat,swiss.lat)[1]
  assign( paste("swisslon",i,sep=""), swiss.lon,    envir=lons)
  assign( paste("swisslat",i,sep=""), swiss.lat,    envir=lats)
  assign( paste("len",i,sep=""),      (row*col)+12, envir=lengths)
  assign( paste("col",i,sep=""),      col,          envir=cols)
  assign( paste("row",i,sep=""),      row,          envir=rows)
  close(to.read)
  
  leftdow = swisscors2lonlat(chx=minlon,chy=minlat)
  rightup = swisscors2lonlat(chx=maxlon,chy=maxlat)
  
  lonlim = c(leftdow[1],rightup[1])
  latlim = c(leftdow[2],rightup[2])
  
  alllon  = seq(minlon,maxlon,by=dist)    
  alllat  = seq(minlat,maxlat,by=dist)
  alldata = array(NA,dim=c(length(alllon),length(alllat)))
  
  # Reading and processing the specific file
  to.read <- file(files[i], "rb")
  a <- readBin(to.read, double(), size = 4, n = get(paste("len", i, sep = ""), envir = lengths), endian = "little")
  close(to.read)
  
  col <- get(paste("col", i, sep = ""), envir = cols)
  row <- get(paste("row", i, sep = ""), envir = rows)
  a <- a[13:length(a)]
  a <- array(a, dim = c(col, row))
  
  # Correct array orientation
  a_corr <- array(NA, dim = c(col, row))
  for (lat in 1:row) {
    a_corr[, lat] <- a[, (row + 1) - lat]
  }
  
  a_corr[a_corr == nodata] <- NA
  indx <- which(alllon %in% get(paste("swisslon", i, sep = ""), envir = lons))
  indy <- which(alllat %in% get(paste("swisslat", i, sep = ""), envir = lats))
  alldata[indx, indy][!is.na(a_corr)] <- a_corr[!is.na(a_corr)]
  
  return(alldata)
}

process_all_Glac_files <- function(glac_dir) {
  
  glac_files <- list.files(glac_dir, full.names = TRUE)
  # Initialize the results data frame
  result_df <- data.frame(
    YYYY = integer(),
    count_nogl = integer(),
    count_abla = integer(),
    count_accu = integer(),
    count_NA = integer(),
    count_glac = integer(),
    count_area = integer()
  )
  
  # Loop over each file and process it
  for (i in seq_along(glac_files)) {
    # Extract the year from the filename
    filename <- basename(glac_files[i])
    year <- as.integer(substr(filename, nchar(filename) - 7, nchar(filename) - 4))
    
    # Process the file and get alldata
    alldata <- process_one_Glac_file(glac_files, i)
    
    # Count values
    count_0 <- sum(alldata == 0, na.rm = TRUE)
    count_1 <- sum(alldata == 1, na.rm = TRUE)
    count_2 <- sum(alldata == 2, na.rm = TRUE)
    count_NA <- sum(is.na(alldata))
    count_glac <- count_1 + count_2
    count_area <- count_glac + count_0
    
    # Append results to the data frame
    result_df <- rbind(result_df, data.frame(
      YYYY = year,
      count_nogl = count_0,
      count_abla = count_1,
      count_accu = count_2,
      count_NA = count_NA,
      count_glac = count_glac,
      count_area = count_area
    ))
  }
  
  return(result_df)
}

# Example function to read and process data from a chain and glchain folder
process_file <- function(meteo_dir, glac_dir) {
  # Define the variables to process
  variables <- c("temp", "prec", "rad_")
  
  # Process each variable and store the results in a list
  meteo_data_list <- lapply(variables, function(variable) {
    meteo_file <- file.path(meteo_dir, paste0(variable, "_full.stats"))
    process_meteo_data(meteo_file)
  })
  
  # Combine all meteo data frames by "YYYY"
  combined_meteo_data <- Reduce(function(x, y) merge(x, y, by = "YYYY", all = TRUE), meteo_data_list)
  
  # Process the glacier data
  glac_data <- process_all_Glac_files(glac_dir)
  
  # Merge meteo and glacier data
  combined_data <- merge(combined_meteo_data, glac_data, by = "YYYY", all = TRUE)
  
  return(combined_data)
}

chain_glchain_df <- parse_chains_glchains(chains_dir)

ch2018_meteo_glac_list <- list()

for (i in seq_len(nrow(chain_glchain_df))) {
  chain <- chain_glchain_df$chain[i]
  glchain <- chain_glchain_df$glchain[i]
  
  ch2018_meteo_glac_list[[chain]] <- list()
  
  for (var in meteo_variables) {
    meteo_file <- file.path(data_path, "meteoThS", paste0(chain, "_g73"), "ThS200", paste0(var, "_full.stats"))
    glac_dir <- file.path(data_path, "ThSGlac", glchain)
    
    # Check if the meteo file exists before reading
    if (file.exists(meteo_file)) {
      
      # Read the data from the file
      meteo_data <- read.table(meteo_file, header = TRUE)
      
      # Filter rows before 1981 (warmup period)
      meteo_data <- meteo_data %>% 
        mutate(Date = as.Date(paste(meteo_data$YYYY, meteo_data$MM, meteo_data$DD, sep = "-"), format = "%Y-%m-%d")) %>%
        filter(YYYY >= 1981) %>%
        mutate(YearMonth = format(Date, "%Y-%m"))
      
      # Store the loaded data in the list
      ch2018_meteo_glac_list[[chain]][[var]][["daily"]] <- meteo_data
      ch2018_meteo_glac_list[[chain]][[var]][["monthly"]] <- compute_monthly_means(meteo_data)
      ch2018_meteo_glac_list[[chain]][[var]][["yearly"]] <- compute_yearly_means(meteo_data)
      
      # compute stats
      monthly_df <- ch2018_meteo_glac_list[[chain]][[var]][["monthly"]]
      
      # Ensure YearMonth is in Date format for proper ordering
      monthly_df$YearMonth <- as.Date(paste0(monthly_df$YearMonth, "-01"), format = "%Y-%m-%d")  # Convert YYYYMM to Date
      
      # Compute 10-year running average (using a 120-month window)
      rolling_mean_df <- as.data.frame(lapply(monthly_df[, -1], function(x) {
        rollapply(x, width = 12 * period_lenght, FUN = mean, align = "center", fill = NA, na.rm = TRUE)
      }))
      
      # Add back the time column for proper visualization
      rolling_mean_df$YearMonth <- monthly_df$YearMonth
      
      # Store results in the list structure
      ch2018_meteo_glac_list[[chain]][[var]][["rolling_mean_10yr"]] <- rolling_mean_df
      
    } else {
      message(paste("Meteo file not found:", meteo_file))
      ch2018_meteo_glac_list[[chain]][[var]] <- NULL
    }
    
  } # meteo var loop
  glac_data <- process_all_Glac_files(glac_dir)
  
  ch2018_meteo_glac_list[[chain]][["glchain"]] <- glchain
  ch2018_meteo_glac_list[[chain]][["glac_data"]] <- glac_data
}

# # Use apply to loop over each row of chain_glchain_df (ignoring the header row)
# processed_data_list <- apply(chain_glchain_df, 1, function(row) {
#   # Construct the folder paths
#   meteo_dir <- file.path(data_path, "meteoThS", paste0(row['chain'], "_g73"), "ThS200")
#   glac_dir <- file.path(data_path, "ThSGlac", row['glchain'])
#   
#   # Process the files in the directories
#   data <- process_file(meteo_dir, glac_dir)
#   
#   return(data)  # Return the processed data for this row
# })
# 
# # The result will be a list with the chain names as list names
# names(processed_data_list) <- chain_glchain_df$chain  # Use the chain column as the names for the list

# Now, processed_data_list will contain data for each chain (with key as the chain name)
