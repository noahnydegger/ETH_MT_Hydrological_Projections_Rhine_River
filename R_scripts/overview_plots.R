# Load necessary libraries
library(tidyverse)             # Load the entire tidyverse package
library(ggplot2)
library(dplyr)

# Load data from the full path
data <- read.table("/Users/noahnydegger/GitHub/ETH/ETH_MT_Hydrological_Projections_Rhine_River/Data/TGl200/KNMItest/tair_full.stats", header = TRUE)

# Combine date columns into a Date object
data$Date <- as.Date(paste(data$YYYY, data$MM, data$DD, sep = "-"), format = "%Y-%m-%d")

# Compute upper and lower bounds for the shaded region (AVG ± STDEV)
data <- data %>%
  mutate(Upper = AVG + STDEV, Lower = AVG - STDEV)

# Create the plot
ggplot(data, aes(x = Date)) +
  # Shaded area for MIN-MAX range
  geom_ribbon(aes(ymin = MIN, ymax = MAX), fill = "lightblue", alpha = 0.3) +
  # Shaded area for AVG ± STDEV
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "gray", alpha = 0.4) +
  # Line plot for AVG values
  #geom_line(aes(y = AVG), color = "blue", size = 1) +
  labs(
    title = "Precipitation Statistics Over Time",
    x = "Date",
    y = "Precipitation (Value)",
    caption = "Shaded areas represent MIN-MAX and AVG ± STDEV"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.position = "none"
  )