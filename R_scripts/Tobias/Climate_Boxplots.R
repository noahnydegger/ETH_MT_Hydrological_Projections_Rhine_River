
library(dplyr)
library(reshape2)
library(ggplot2)
library(ggplotify)

names <- c("Bri_P", "Bri_Q", "Thu_P", "Thu_Q", "Wal_P", "Wal_Q", "Zur_P", "Zur_Q") # catchment names (left to right)
titles <- c("(c) lake Brienz", "(c) lake Brienz", "(d) lake Thun", "(d) lake Thun",
            "(a) lake Walen", "(a) lake Walen", "(b) lake Zurich", "(b) lake Zurich") # catchment names (they are used as plot titles)

source("functions.R", encoding = "utf-8")
mymonths <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")


##______________________________________________________________________________________________________
## read in data  ---------------------------------------------------------------------------------------

files <- list.files("../Data/Data_for_Results", pattern = ".csv", full.names = T) # list files

ezg <- list() # empty list

for(file in files){ # loop over all files
  data <- read.csv(file, header = T, sep = ",")[, -1] # read in file
  names(data)[1] <- "date" # rename date column
  data$date <- as.Date(data$date, format = "%Y-%m-%d") # format as date
  name <- gsub(".*Results/(.*)_CH2018.*", "\\1", file) # extract catchemnt name and variable
  ezg[[name]] <- data # safe in list
}

rm(data, file, files, name)



##______________________________________________________________________________________________________
## create monthly and yearly data based on periods  ----------------------------------------------------

data <- list()

for(name in names){
  catchment <- ezg[[name]] # load data
  ref <- catchment[which(catchment$date == "1991-01-01"):which(catchment$date == "2020-12-31"), ] # period ref
  early <- catchment[which(catchment$date == "2020-01-01"):which(catchment$date == "2049-12-31"), ] # period early
  mid <- catchment[which(catchment$date == "2045-01-01"):which(catchment$date == "2074-12-31"), ] # period mid
  late <- catchment[which(catchment$date == "2070-01-01"):which(catchment$date == "2099-12-30"), ] # period late
  
  month <- monthly_reformat(monthly(ref), monthly(early), monthly(mid), monthly(late), mymonths) # monthly mean values
  year <- yearly_reformat(yearly(ref), yearly(early), yearly(mid), yearly(late)) # yearly mean values
  
  data[[name]][["abs"]] <- rbind(month[[1]], year[[1]]) # combine monthly and yearly data (absolute values)
  data[[name]][["diff"]] <- rbind(month[[2]], year[[2]]) # combine monthly and yearly data (diff values)
  data[[name]][["rel"]] <- rbind(month[[3]], year[[3]]) # combine monthly and yearly data (relative values)
}

rm(catchment, ref, early, mid, late, month, year, name)


##______________________________________________________________________________________________________
## create boxplots -------------------------------------------------------------------------------------

# 
type <- c("diff", "rel", "diff", "rel", "diff", "rel", "diff", "rel")

for(i in 1:length(data)){
  pl <- boxpl(data, names[i], titles[i], type[i])
  pdf(paste0("../Figures/monthlymean_", names[i], ".pdf"), width = 8.27, height = 5.5)
  print(pl)
  dev.off()
}





