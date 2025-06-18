
library(dplyr)
library(reshape2)
library(ggplot2)
library(ggplotify)
library(cowplot)

names <- c("Bri_P", "Bri_Q", "Thu_P", "Thu_Q", "Wal_P", "Wal_Q", "Zur_P", "Zur_Q") # catchment names (left to right)
titles <- c("(c) lake Brienz", "(c) lake Brienz", "(d) lake Thun", "(d) lake Thun",
            "(a) lake Walen", "(a) lake Walen", "(b) lake Zurich", "(b) lake Zurich") # catchment names (they are used as plot titles)

source("functions.R", encoding = "utf-8")
myseasons <- c("DJF", "MAM", "JJA", "SON")


##______________________________________________________________________________________________________
## read in data  ---------------------------------------------------------------------------------------

files <- list.files("../Data/Data_for_Results", pattern = ".csv", full.names = T) # list files

ezg <- list() # empty list

for(file in files){ # loop over all files
  data <- read.csv(file, header = T, sep = ",")[, -1] # read in file
  names(data)[1] <- "date" # rename date column
  data$date <- as.Date(data$date, format = "%Y-%m-%d") # format as date
  data$month <- format(as.Date(data$date), "%m")
  data$season <- ifelse(data$month %in% c("12", "01", "02"), "DJF", 
                        ifelse(data$month %in% c("03", "04", "05"), "MAM",
                               ifelse(data$month %in% c("06", "07", "08"), "JJA", "SON")))
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
  
  season <- seasonaly_reformat(seasonaly(ref), seasonaly(early), seasonaly(mid), seasonaly(late), myseasons) # seasonaly mean values
  year <- yearly_reformat(yearly(ref[, -c(1, 41, 42)]), yearly(early[, -c(1, 41, 42)]), yearly(mid[, -c(1, 41, 42)]), yearly(late[, -c(1, 41, 42)])) # yearly mean values
  
  data[[name]][["abs"]] <- rbind(season[[1]], setNames(year[[1]], names(season[[1]]))) # combine seasonaly and yearly data (absolute values)
  data[[name]][["diff"]] <- rbind(season[[2]], setNames(year[[2]], names(season[[2]]))) # combine seasonaly and yearly data (diff values)
  data[[name]][["rel"]] <- rbind(season[[3]], setNames(year[[3]], names(season[[3]]))) # combine seasonaly and yearly data (relative values)
}

rm(catchment, ref, early, mid, late, season, year, name)


##______________________________________________________________________________________________________
## create boxplots -------------------------------------------------------------------------------------

type <- c("diff", "rel", "diff", "rel", "diff", "rel", "diff", "rel") # plot type list
lim <- list(c(-0.6, 0.3), c(-60, 30), c(-0.2, 0.1), c(-60, 30), c(-0.6, 0.3), c(-60, 30), c(-0.2, 0.1), c(-60, 30))
lg <- c(rep(F, 6), T, T) # legend list
lab <- c(T, T, F, F, T, T, F, F) # y-label list
pls <- list() # empty list

# create all plots
for(i in 1:length(data)){
  pls[[i]] <- boxpl_season(data, names[i], titles[i], type[i], y_lim = lim[[i]], legend = lg[i], y_label = lab[i])
}

pdf(paste0("../Figures/seasonalmean_Q.pdf"), width = 8.27, height = 5.5)
plot_grid(pls[[6]], pls[[8]], pls[[2]], pls[[4]])
dev.off()

pdf(paste0("../Figures/seasonalmean_P.pdf"), width = 8.27, height = 5.5)
plot_grid(pls[[5]], pls[[7]], pls[[1]], pls[[3]])
dev.off()




