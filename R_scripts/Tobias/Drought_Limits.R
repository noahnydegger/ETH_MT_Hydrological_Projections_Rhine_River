
library(dplyr)
library(reshape2)
library(ggplot2)
library(ggplotify)

source("functions.R")
mymonths <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Year")


##______________________________________________________________________________________________________
## read in data  ---------------------------------------------------------------------------------------

files <- list.files("../Data/Extremes", pattern = "Extreme_.*.csv", full.names = T, recursive = T)
data <- list()

for(file in files){
  dat <- read.csv(file, header = T, sep = ",")[-1]
  dat$Time_Period <- factor(dat$Time_Period, levels = c("Tref", "T1", "T2", "T3"))
  year <- dat %>% 
    group_by(Time_Period) %>%
    summarise(across(names(dat)[3]:names(dat)[ncol(dat)], sum))
  year$Month <- 13
  dat_combined <- rbind(dat, year)
  dat_combined$Month <- factor(mymonths[as.numeric(dat_combined$Month)], levels = mymonths)
  dat_combined[,-(1:2)] <- dat_combined[,-(1:2)] / 30
  dat_combined_long <- melt(dat_combined, id.vars = c("Time_Period", "Month"), variable.name = "rcp", )
  dat_combined_long$rcp <- gsub(".*(RCP..)", "\\1", dat_combined_long$rcp)
  dat_calculated <- dat_combined_long %>%
    group_by(Time_Period, Month, rcp) %>%
    summarise(mean = mean(value), q10 = quantile(value, probs = 0.1), q90 = quantile(value, probs = 0.9))
  name <- gsub(".*simulated_(.*).csv", "\\1", file)
  variable <- gsub(".*Extreme_(.*)_limit.*", "\\1", file)
  data[[paste(variable, name, sep = "_")]] <- dat_calculated
}



##______________________________________________________________________________________________________
## plotting  -------------------------------------------------------------------------------------------

titles <- c("(c) lake Brienz", "(c) lake Brienz", "(c) lake Brienz", "(d) lake Thun", "(d) lake Thun", 
            "(a) lake Walen", "(a) lake Walen", "(b) lake Zurich", "(b) lake Zurich")

for(i in 1:length(data)){
  if(grepl("Drought", names(data)[i]) == T){
    type <- "Drought"
  } else if(grepl("Flood", names(data)[i]) == T){
    type <- "Flood"
  }
  pl <- barplot(data[[i]], titles[i], type)
  pdf(paste0("../Figures/drought_limits_", names(data)[i], ".pdf"), width = 8.27, height = 5.5)
  print(pl)
  dev.off()
}



