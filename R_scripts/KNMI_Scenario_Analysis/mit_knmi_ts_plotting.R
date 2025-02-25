library(dplyr)
library(purrr)
library(slider)

# Function to compute daily statistics for multiple scenarios
compute_runoff_statistics_scenarios <- function(mit_knmi_list, area, column_name, scenario_list) {
  # Initialize a list to store results for each scenario
  scenario_stats_list <- list()
  
  # Loop through each scenario in the scenario list
  for (scenario in scenario_list) {
    # Extract the ensemble data for the specified area and scenario
    ensemble_data_list <- lapply(grep("^ens[0-9]+$", names(mit_knmi_list[[scenario]]), value = TRUE), function(ensemble) {
      mit_knmi_list[[scenario]][[ensemble]][[area]][["daily"]]
    })
    
    # Process each ensemble data and compute statistics for the given column
    ensemble_stats <- ensemble_data_list %>%
      map(function(data) {
        # Compute 30-day centered rolling mean for the specified column
        data <- data %>%
          arrange(Date) %>%
          mutate(
            RM = slide_dbl(.data[[column_name]], mean, .before = 14, .after = 15, .complete = TRUE)
          )
        
        # Identify leap years and adjust DayOfYear for them
        data <- data %>%
          mutate(DayOfYear = as.numeric(format(Date, "%j")),  # Create DayOfYear column
                 Year = as.numeric(format(Date, "%Y"))) %>%   # Extract Year for leap year check
          # Remove leap days (Feb 29)
          filter(DayOfYear != 60 | !((Year %% 4 == 0) & (Year %% 100 != 0 | Year %% 400 == 0))) %>%
          # Shift the DayOfYear for leap years (after Feb 29)
          mutate(DayOfYear = if_else(DayOfYear > 60 & (Year %% 4 == 0) & (Year %% 100 != 0 | Year %% 400 == 0), 
                                     DayOfYear - 1, 
                                     DayOfYear))
        
        return(data)
      })
    
    # Combine data from all ensembles for this scenario
    combined_data <- bind_rows(ensemble_stats)
    
    # Compute daily statistics for the specified column across all ensembles for this scenario
    daily_stats <- combined_data %>%
      group_by(DayOfYear) %>%
      summarise(
        Mean = mean(RM, na.rm = TRUE),
        Q10 = quantile(RM, 0.10, na.rm = TRUE),
        Q90 = quantile(RM, 0.90, na.rm = TRUE)
      ) %>%
      filter(!is.na(Mean) | !is.na(Q10) | !is.na(Q90))  # Remove rows with NA values across all metrics
    
    # Add the scenario column to differentiate between scenarios
    daily_stats$Scenario <- scenario
    
    # Store the results in the list
    scenario_stats_list[[scenario]] <- daily_stats
  }
  
  return(scenario_stats_list)
}


# Function to plot the statistics for multiple scenarios
plot_runoff_statistics <- function(scenario_stats_list, y_label, variable) {
  
  # Combine the statistics from both scenarios into one data frame
  combined_stats <- bind_rows(scenario_stats_list)
  
  # Create a column for the monthly boxplot
  meancol <- paste0("Mean")
  q10col <- paste0("Q10")
  q90col <- paste0("Q90")
  
  # Convert DayOfYear to date-like values (use 2023 as a dummy year)
  combined_stats <- combined_stats %>%
    mutate(DateLabel = as.Date(DayOfYear - 1, origin = "2023-01-01"),
           Scenario = factor(Scenario, levels = c("reference", "Hd_2100")))  # Ensure reference is first
  
  # Create breaks for the start of each month
  month_breaks <- seq(as.Date("2023-01-15"), as.Date("2023-12-15"), by = "1 month")
  
  ggplot(combined_stats, aes(x = DateLabel, group = Scenario, color = Scenario)) +
    geom_ribbon(aes(ymin = !!sym(q10col), ymax = !!sym(q90col), fill = Scenario), alpha = 0.4) +
    geom_line(aes(y = !!sym(meancol)), size = 1) +
    scale_x_date(
      date_labels = "%b",
      breaks = month_breaks
    ) +
    labs(
      title = paste("30-day Moving Average (with Q10-Q90) of daily mean", y_label),
      x = "Month",
      y = paste(y_label, "[mm/day]"),
      color = "Scenario",
      fill = "Scenario"
    ) +
    theme_minimal() +
    # Manually set colors and fills for scenarios
    scale_color_manual(values = scenario_colors) +
    scale_fill_manual(values = scenario_colors)
  
  # Save the plot as a PDF file
  save_dir <- file.path(here::here(), "Plots", "TestPlots", "TimeSeries")
  if (!dir.exists(save_dir)) {
    dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
  }
  ggsave(file.path(save_dir, paste0("TS_", y_label, ".pdf")), plot = last_plot(), device = "pdf", width = 12, height = 6)
}

# Define the list of scenarios
scenario_list <- c("reference", "Hd_2100")

# Compute daily statistics for all ensembles for each scenario
scenario_stats_list <- compute_runoff_statistics_scenarios(mit_knmi_list, area = "ThS200", column_name = "RGES", scenario_list = scenario_list)

# Call the plot function with the computed statistics
plot_runoff_statistics(scenario_stats_list, y_label = "Runoff", variable = "RGES")