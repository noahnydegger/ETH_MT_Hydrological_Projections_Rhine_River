
library(dplyr)
library(reshape2)
library(ggplot2)
library(ggplotify)
library(cowplot)

source("functions.R")
myseasons <- c("DJF", "MAM", "JJA", "SON", "Year")


##______________________________________________________________________________________________________
## read in data  ---------------------------------------------------------------------------------------

files <- list.files("../Data/Extremes", pattern = "Extreme_.*.csv", full.names = T, recursive = T)
data <- list()

for(file in files){
  dat <- read.csv(file, header = T, sep = ",")[-1]
  dat$Time_Period <- factor(dat$Time_Period, levels = c("Tref", "T1", "T2", "T3"))
  dat$season <- ifelse(dat$Month %in% c(12, 1, 2), "DJF", 
                        ifelse(dat$Month %in% c(3, 4, 5), "MAM",
                               ifelse(dat$Month %in% c(6, 7, 8), "JJA", "SON")))
  dat <- dat[, c(1, 42, 3:41)]
  dat_season <- dat %>%
    group_by(Time_Period, season) %>%
    summarise(across(names(dat)[3]:names(dat)[ncol(dat)], sum))
  year <- dat %>% 
    group_by(Time_Period) %>%
    summarise(across(names(dat)[3]:names(dat)[ncol(dat)], sum))
  year$season <- "Year"
  dat_combined <- rbind(dat_season, year)
  dat_combined$season <- factor(dat_combined$season, levels = myseasons)
  dat_combined[,-c(1:2)] <- dat_combined[,-c(1:2)] / 30
  dat_combined_long <- melt(dat_combined, id.vars = c("Time_Period", "season"), variable.name = "rcp", )
  dat_combined_long$rcp <- gsub(".*(RCP..)", "\\1", dat_combined_long$rcp)
  dat_calculated <- dat_combined_long %>%
    group_by(Time_Period, season, rcp) %>%
    summarise(mean = mean(value), q10 = quantile(value, probs = 0.1), q90 = quantile(value, probs = 0.9))
  name <- gsub(".*simulated_(.*).csv", "\\1", file)
  variable <- gsub(".*Extreme_(.*)_limit.*", "\\1", file)
  data[[paste(variable, name, sep = "_")]] <- dat_calculated
}



##______________________________________________________________________________________________________
## plotting  -------------------------------------------------------------------------------------------

titles <- c("(c) lake Brienz", "(d) lake Thun", "(a) lake Walen", "(b) lake Zurich")
lg <- c(F, F, F, T) # legend list
lab <- c(T, F, T, F) # y-label list
r_number <- c(1, 4, 6, 8)

# create all plots
pls <- list() # empty list
for(i in 1:4){
  pls[[i]] <- barplot_season(data[[r_number[i]]], titles[i], legend = lg[i], y_label = lab[i])
}

pdf(paste0("../Figures/drought_limits_season.pdf"), width = 8.27, height = 5.5)
plot_grid(pls[[3]], pls[[4]], pls[[1]], pls[[2]])
dev.off()


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



