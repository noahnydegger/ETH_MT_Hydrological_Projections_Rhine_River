# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(here)

# change what should be plotted
out_var <- "all components"
knmi_var <- "KNMItest_2021_g73_RhB200.mit_cut"
stan_var <- "CTRL_RUN_WSL_F_2021_g73_RhB200.mit_cut"

data_dir <- "Data"
gebiet <- "Rhein"

knmi_file <- file.path(here::here(), data_dir, gebiet, knmi_var)
stan_file <- file.path(here::here(), data_dir, gebiet, stan_var)

# Load data from the file path
knmi_data <- read.table(knmi_file, header = TRUE, skip = 1)
stan_data <- read.table(stan_file, header = TRUE, skip = 1)

library(dplyr)

#' Calculate Monthly Means for All Numeric Columns
#'
#' This function calculates the monthly mean of all numeric columns in a given dataset.
#' It groups data by Year-Month, calculates means for all numeric columns, and adds
#' a month abbreviation column for convenience.
#'
#' @param data A data frame containing a `Date` column and numeric columns for which monthly means are computed.
#' @return A data frame with monthly means for all numeric columns, along with `YearMonth`, `Month`, and `MonthAbb` columns.
#' @examples
#' monthly_means(data)
monthly_means <- function(data) {
  data %>%
    mutate(YearMonth = format(Date, "%Y-%m")) %>% 
    select(-Date) %>%
    select(-Day) %>%
    group_by(YearMonth) %>%
    summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop") %>% 
    mutate(Month = as.numeric(substr(YearMonth, 6, 7)),
           MonthAbb = factor(Month, levels = 1:12, labels = month.abb))
}

yearly_means <- function(data) {
  # Compute yearly summary values for plotting
  yearly_data <- data %>%
    group_by(YYYY) %>%
    summarise(
      MeanR0 = mean(RO, na.rm = TRUE),
      MeanR1 = mean(R1, na.rm = TRUE),
      MeanR2 = mean(R2, na.rm = TRUE),
      MeanRGES = mean(RGES, na.rm = TRUE),
      MeanP_SME = mean(P.SME, na.rm = TRUE),
      MeanGLAC = mean(GLAC, na.rm = TRUE),
      MeanEPOT = mean(EPOT, na.rm = TRUE),
      MeanEREA = mean(EREA, na.rm = TRUE),
      MeanP_KOR = mean(P.kor, na.rm = TRUE),
      MeanS_SNO = mean(S.SNO, na.rm = TRUE)
    )
  yearly_data <- yearly_data %>%
    rename(Year = YYYY) %>%
    mutate(Year = as.character(Year))
  
  return(yearly_data)
}

# Apply the function to both datasets
knmi_data <- process_data(knmi_data)
stan_data <- process_data(stan_data)

# Calculate monthly means for both datasets
knmi_months <- monthly_means(knmi_data)
stan_months <- monthly_means(stan_data)

# Calculate monthly means for both datasets
knmi_years <- yearly_means(knmi_data)
stan_years <- yearly_means(stan_data)

# Add a 'Source' column to distinguish between the datasets
knmi_months$Source <- "KNMI"
knmi_years$Source <- "KNMI"
stan_months$Source <- "STAN"
stan_years$Source <- "STAN"

# Combine the two datasets
combined_months <- bind_rows(stan_months, knmi_months)
combined_months$Source <- factor(combined_months$Source, levels = c("STAN", "KNMI"))
combined_years <- bind_rows(stan_years, knmi_years)
combined_years$Source <- factor(combined_years$Source, levels = c("STAN", "KNMI"))

# Sort combined_data by YearMonth in place
combined_months <- combined_months %>%
  arrange(YearMonth)

# Sort combined_data by Year in place
combined_years <- combined_years %>%
  arrange(Year)

plot_monthly_with_yearly_boxplots <- function(monthly_data, yearly_data, variable_name, stat, y_label, unit) {
  # Plot monthly boxplots
  p <- ggplot(monthly_data, aes_string(x = "MonthAbb", y = variable_name, fill = "Source")) +
    geom_boxplot(position = position_dodge(width = 0.8)) +
    labs(
      title = paste("Monthly and Yearly", stat, "of Daily", y_label),
      x = "Time Period",
      y = paste(y_label, unit),
      fill = "Dataset"
    ) +
    theme_minimal() +
    scale_fill_brewer(palette = "Set1")
  
  # Add yearly boxplot to the existing plot
  p + 
    geom_boxplot(
      data = yearly_data, 
      aes(x = "Year", y = !!sym(variable_name), fill = Source), 
      position = position_dodge(width = 0.8)
    )
  
  # save the plot as a pdf file
  save_dir <- file.path(here::here(), "Plots","Rhein", "All_Components")
  if (!dir.exists(save_dir)) {
    dir.create(save_dir)
  }
  ggsave(file.path(save_dir, paste0(variable_name , ".pdf")), plot = last_plot(), device = "pdf", width = 12, height = 6)
}

# Plot monthly boxplots for each variable
plot_monthly_with_yearly_boxplots(combined_months, combined_years, "MeanRGES", "Mean", "Total runoff", "[mm/d]")
plot_monthly_with_yearly_boxplots(combined_months, combined_years, "MeanR0", "Mean", "Surface runoff", "[mm/d]")
plot_monthly_with_yearly_boxplots(combined_months, combined_years, "MeanR1", "Mean", "Interflow", "[mm/d]")
plot_monthly_with_yearly_boxplots(combined_months, combined_years, "MeanR2", "Mean", "Total baseflow", "[mm/d]")
plot_monthly_with_yearly_boxplots(combined_months, combined_years, "MeanP_SME", "Mean", "Snowmelt", "[mm/d]")
plot_monthly_with_yearly_boxplots(combined_months, combined_years, "MeanGLAC", "Mean", "Ice melt", "[mm/d]")
plot_monthly_with_yearly_boxplots(combined_months, combined_years, "MeanEPOT", "Mean", "Potential Evapotranspiration", "[mm/d]")
plot_monthly_with_yearly_boxplots(combined_months, combined_years, "MeanEREA", "Mean", "Actual Evapotranspiration", "[mm/d]")
plot_monthly_with_yearly_boxplots(combined_months, combined_years, "MeanP_KOR", "Mean", "Adjusted interpolated precipitation", "[mm/d]")
plot_monthly_with_yearly_boxplots(combined_months, combined_years, "MeanS_SNO", "Mean", "Snow water equivalent", "[mm]")


