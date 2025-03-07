library(dplyr)
library(purrr)
library(slider)
library(here)

data_dir <- file.path("Data", "Rheinblick", "routing")
hindcast_file <- file.path(here::here(), "Data", "Rhein", "Swissrhine200_CTRL_RUN_WSL_F_2021_g73.dat")
observed_file <- file.path(here::here(), "Data", "R_KNMI", "observed", "CHBILANZ", "2289.daily.mean.dat")

col_names_prevah <- c("YYYY", "MM", "DD", "rekingen", "aareusigg", "rhinerhf", "rhinebasel", "wiese")
col_names_observed <- c("YYYY", "MM", "DD", "rhinebasel")

# Define the list of scenarios
scenarios <- c("reference")

ensenmbles <- c(
  "ens1", "ens2", "ens3", "ens4", "ens5", "ens6", "ens7", "ens8"
)

ref_colors <- c("observed" = "black","hindcast" = "grey40","reference" = "grey70", "KNMI Ensembles" = "grey80")

gebiet <- "NoW200"

source(here("R_scripts", "data_import_functions.R"))

process_rhine_data <- function(file_path, col_names) {
  # Load the data from the file
  rhine_data <- read.table(file_path, header = FALSE, skip = 1)
  
  # Assign column names
  colnames(rhine_data) <- col_names
  
  # Create Date column
  rhine_data$Date <- as.Date(paste(rhine_data$YYYY, rhine_data$MM, rhine_data$DD, sep = "-"), format = "%Y-%m-%d")
  
  # Create a YearMonth column in "YYYY-MM" format
  rhine_data <- rhine_data %>% mutate(YearMonth = format(Date, "%Y-%m"))
  
  # Filter rows between 30 year periods
  
  rhine_data <- subset(rhine_data, format(Date, "%Y") >= 1991)
  rhine_data <- subset(rhine_data, format(Date, "%Y") <= 2020)
  
  return(rhine_data)
}

compute_ensemble_mean <- function() {
  # Compute the ensemble mean for each scenario, area, and time scale
}

ref_rhine_list <- list()

# import the meteo data for each scenario, ensemble, and area
for (scen in scenarios) {
  ref_rhine_list[[scen]] <- list()
  for (ens in ensenmbles) {
    ref_rhine_list[[scen]][[ens]] <- list()
    # Construct the file path
    swissrhine_file <- file.path(here::here(), data_dir, gebiet, paste0(scen, "_", ens), paste0("Swissrhine200_", paste0(scen, "_", ens), ".dat"))
    
    # Check if the meteo file exists before reading
    if (file.exists(swissrhine_file)) {
      
      # Import data from the .stats file
      rhine_data <- process_rhine_data(swissrhine_file, col_names_prevah)
      
      # Store the loaded data in the list
      ref_rhine_list[[scen]][[ens]][["daily"]] <- rhine_data
      ref_rhine_list[[scen]][[ens]][["monthly"]] <- compute_monthly_means(rhine_data)
      ref_rhine_list[[scen]][[ens]][["yearly"]] <- compute_yearly_means(rhine_data)
      
    } else {
      message(paste("Meteo file not found:", swissrhine_file))
      ref_rhine_list[[scen]][[ens]] <- NULL
    }
  }
  time_scale_names <- c("monthly", "yearly")# c("daily", "monthly", "yearly")
  for (time_scale in time_scale_names) {
    # Extract data frame for the first ensemble member for the given area and time scale
    first_df <- ref_rhine_list[[scenario]][[ensenmbles[1]]][[time_scale]]
    
    if (is.null(first_df)) next  # Skip if time scale doesn't exist
    
    # Extract the correct time column based on time scale
    if (time_scale == "monthly") {
      times <- first_df$YearMonth
    } else if (time_scale == "yearly") {
      times <- first_df$YYYY
    } 
    
    # Initialize the list to store statistics (Mean, Std, Max, Min) of all ensembles
    stat_list <- list(ensMean = list(), ensStd = list(), ensMax = list(), ensMin = list())
    
    # Extract unique times (dates, YearMonth or YYYY)
    unique_times <- unique(times)
    
    # Loop over each unique time group (date, YearMonth, year)
    for (time_id in seq_along(unique_times)) {
      time_filter <- unique_times[time_id]
      
      # Filter the data based on time_scale and time_filter
      filtered_data <- do.call(rbind, lapply(ensenmbles, function(ens) {
        data <- ref_rhine_list[[scenario]][[ens]][[time_scale]]
        if (time_scale == "monthly") {
          data <- data[data$YearMonth == time_filter, , drop = FALSE]  # Monthly data remains unchanged
        } else if (time_scale == "yearly") {
          data <- data[data$YYYY == time_filter, , drop = FALSE]
          data <- data[, !colnames(data) %in% c("YYYY"), drop = FALSE]  # Remove YYYY
        }
      }))
      
      # Compute statistics for each column, excluding the time column
      numeric_data <- filtered_data[, sapply(filtered_data, is.numeric)]  # Only select numeric columns
      
      # Compute statistics for each column
      stat_list$ensMean[[time_id]] <- colMeans(numeric_data, na.rm = TRUE)
      stat_list$ensStd[[time_id]]  <- apply(numeric_data, 2, sd, na.rm = TRUE)
      stat_list$ensMax[[time_id]]  <- apply(numeric_data, 2, max, na.rm = TRUE)
      stat_list$ensMin[[time_id]]  <- apply(numeric_data, 2, min, na.rm = TRUE)
    }
    
    # Convert lists into data frames and store in the correct list structure
    for (stat_name in names(stat_list)) {
      stat_df <- do.call(rbind, stat_list[[stat_name]])
      
      # Add the correct time column (Year or Year-Month) to the data frame
      if (time_scale == "yearly") {
        stat_df <- data.frame(YYYY = unique_times, stat_df, row.names = NULL)
      } else {
        stat_df <- data.frame(YearMonth = unique_times, stat_df, row.names = NULL)
      }
      
      # Store the computed statistics in the correct location
      ref_rhine_list[[scenario]][[stat_name]][[time_scale]] <- stat_df
    }
    
  } # time_scale loop
}

# import hindcast data
hindcast_data <- process_rhine_data(hindcast_file, col_names_prevah)
ref_rhine_list[["hindcast"]][["daily"]] <- hindcast_data
ref_rhine_list[["hindcast"]][["monthly"]] <- compute_monthly_means(hindcast_data)
ref_rhine_list[["hindcast"]][["yearly"]] <- compute_yearly_means(hindcast_data)

# import observed data
observed_data <- process_rhine_data(observed_file, col_names_observed)
ref_rhine_list[["observed"]][["daily"]] <- observed_data
ref_rhine_list[["observed"]][["monthly"]] <- compute_monthly_means(observed_data)
ref_rhine_list[["observed"]][["yearly"]] <- compute_yearly_means(observed_data)


# Function to compute daily statistics for multiple scenarios
compute_runoff_statistics_scenarios <- function(ref_rhine_list, area, column_name, scenarios, q_bot = 0.10, q_top = 0.90) {
  
  compute_daily_stats <- function(data) {
    data <- data %>%
      arrange(Date) %>%
      mutate(RM = slide_dbl(.data[[column_name]], mean, .before = 14, .after = 15, .complete = TRUE),
             DayOfYear = as.numeric(format(Date, "%j")),
             Year = as.numeric(format(Date, "%Y"))) %>%
      filter(DayOfYear != 60 | !((Year %% 4 == 0) & (Year %% 100 != 0 | Year %% 400 == 0))) %>%
      mutate(DayOfYear = if_else(DayOfYear > 60 & (Year %% 4 == 0) & (Year %% 100 != 0 | Year %% 400 == 0), 
                                 DayOfYear - 1, DayOfYear)) %>%
      group_by(DayOfYear) %>%
      summarise(
        Mean = mean(RM, na.rm = TRUE),
        q_bot = quantile(RM, q_bot, na.rm = TRUE),
        q_top = quantile(RM, q_top, na.rm = TRUE)
      ) %>%
      filter(!is.na(Mean) | !is.na(q_bot) | !is.na(q_top))
  }
  
  # Initialize a list to store results for each scenario
  scenario_stats_list <- list()
  
  # Loop through each scenario in the scenario list
  for (scenario in scenarios) {
    if (scenario == "reference") {
      # Extract the ensemble data for the specified area and scenario
      ensemble_data_list <- lapply(grep("^ens[0-9]+$", names(ref_rhine_list[[scenario]]), value = TRUE), function(ensemble) {
        ref_rhine_list[[scenario]][[ensemble]][["daily"]]
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
          q_bot = quantile(RM, q_bot, na.rm = TRUE),
          q_top = quantile(RM, q_top, na.rm = TRUE)
        ) %>%
        filter(!is.na(Mean) | !is.na(q_bot) | !is.na(q_top))
      
      # Add the scenario column to differentiate between scenarios
      daily_stats$Scenario <- scenario
      
      # Store the results in the list
      scenario_stats_list[[scenario]] <- daily_stats
      
      # Add the rolling means for each ensemble individually
      for (ens in ensenmbles) {
        ensemble_data <- ref_rhine_list[[scenario]][[ens]][["daily"]]
        
        daily_stats <- compute_daily_stats(ensemble_data)
        
        scen_ens <- paste0(scenario, "_", ens)
        daily_stats$Scenario <- scen_ens
        scenario_stats_list[[scen_ens]] <- daily_stats
      }
      
    } else if (scenario %in% c("hindcast", "observed")) {
      # Single ensemble case
      data <- ref_rhine_list[[scenario]][["daily"]]
      
      daily_stats <- compute_daily_stats(data)
      
      daily_stats$Scenario <- scenario
      scenario_stats_list[[scenario]] <- daily_stats
    }
  }
  
  return(scenario_stats_list)
}


# Function to plot the statistics for multiple scenarios
plot_runoff_statistics <- function(scenario_stats_list, y_label, variable, q_bot = 0.10, q_top = 0.90, show_ensemble = TRUE, show_range = FALSE) {
  mean_scenarios <- c("observed", "hindcast", "reference")  # List of scenarios to include
  ind_scenarios <- grep("^reference_ens", names(scenario_stats_list), value = TRUE)
  
  # Combine the statistics for mean scenarios into one data frame
  combined_stats_mean <- bind_rows(scenario_stats_list[mean_scenarios])
  
  # Combine the statistics for individual ensemble scenarios into another data frame
  combined_stats_ensemble <- bind_rows(scenario_stats_list[ind_scenarios])
  
  # Convert DayOfYear to date-like values (use 2023 as a dummy year) for mean scenarios
  combined_stats_mean <- combined_stats_mean %>%
    mutate(DateLabel = as.Date(DayOfYear - 1, origin = "2023-01-01"),
           Scenario = factor(Scenario, levels = c("observed", "hindcast","reference")))  # Ensure reference is first
  
  # Convert DayOfYear to date-like values (use 2023 as a dummy year) for ensemble scenarios
  combined_stats_ensemble <- combined_stats_ensemble %>%
    mutate(DateLabel = as.Date(DayOfYear - 1, origin = "2023-01-01"))
           
  
  # Compute NSE values
  compute_nse <- function(df, scenario1, scenario2) {
    observed <- df %>% 
      filter(Scenario == scenario1) %>% 
      arrange(DayOfYear) %>% 
      pull(Mean)  # Select only the 'Mean' column
    
    simulated <- df %>% 
      filter(Scenario == scenario2) %>% 
      arrange(DayOfYear) %>% 
      pull(Mean)  # Select only the 'Mean' column
    
    if (length(observed) == length(simulated) && length(observed) > 0) {
      mean_obs <- mean(observed, na.rm = TRUE)
      numerator <- sum((observed - simulated)^2, na.rm = TRUE)
      denominator <- sum((observed - mean_obs)^2, na.rm = TRUE)
      return(1 - (numerator / denominator))
    } else {
      return(NA)  # Return NA if lengths are different or data is missing
    }
  }
  
  nse_obs_hindcast <- compute_nse(combined_stats_mean, "observed", "hindcast")
  nse_obs_reference <- compute_nse(combined_stats_mean, "observed", "reference")
  nse_hindcast_reference <- compute_nse(combined_stats_mean, "hindcast", "reference")
  
  # Subtitle text
  subtitle_text <- paste0(
    "NSE(obs vs hindcast) = ", round(nse_obs_hindcast, 2), " | ",
    "NSE(obs vs KNMI) = ", round(nse_obs_reference, 2), " | ",
    "NSE(hindcast vs KNMI) = ", round(nse_hindcast_reference, 2)
  )
  
  
  # Create breaks for the start of each month (gridlines)
  month_lines <- seq(as.Date("2023-01-01"), as.Date("2023-12-01"), by = "1 month")
  
  # Create breaks for month labels (placed on the 15th)
  month_labels <- seq(as.Date("2023-01-15"), as.Date("2023-12-15"), by = "1 month")
  
  # Start plot with the background elements first
  p <- ggplot(combined_stats, aes(x = DateLabel, group = Scenario, color = Scenario)) +
    geom_vline(xintercept = as.numeric(month_lines), color = "gray90")  # Gridlines for months
  
  # Add Q10-Q90 ribbon in the background if show_range is TRUE
  if (show_range) {
    p <- p + geom_ribbon(aes(ymin = q_bot, ymax = q_top, fill = Scenario), alpha = 0.4)
  }
  
  # Add the individual ensembles if show_ensemble flag is TRUE
  if (show_ensemble) {
    p <- p + 
      geom_line(data = combined_stats_ensemble, aes(y = Mean, color = "KNMI Ensembles"), size = 1)  # Individual ensembles with size 1
  }
  
  # Add the mean line on top of the ribbon
  p <- p + geom_line(aes(y = Mean), size = 2) +
    scale_x_date(
      date_labels = "%b",
      breaks = month_labels,
      expand = c(0, 0)  # Remove empty space before Jan 1 and after Dec 31
    ) +
    labs(
      title = paste0("30-day Moving Average Mean ", y_label, ifelse(show_range, paste0(" with Q", q_bot*100, "-Q", q_top*100), ""), " (1991-2020) Basel, Rheinhalle"),
      subtitle = subtitle_text,
      x = "Month",
      y = paste(y_label, "[m³/s]"),
      color = "Dataset",
      fill = "Dataset"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.major.x = element_blank(),  # Remove automatic gridlines
      panel.grid.major.y = element_line(color = "gray90"),  # Solid horizontal gridlines
      panel.grid.minor = element_blank(),  # Remove minor gridlines
      text = element_text(color = "black"),  # Make all text black
      axis.title.x = element_blank(),
      axis.text = element_text(size = 14, color = "black"),  
      axis.title = element_text(size = 16, face = "bold", color = "black"),  
      legend.text = element_text(size = 14, color = "black"),  
      legend.title = element_text(size = 16, face = "bold", color = "black"),  
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5, color = "black")  
    ) +
    scale_color_manual(
      values = ref_colors,
      labels = c("observed" = "Observed", "hindcast" = "Hindcast", "reference" = "KNMI Mean", 
                 "KNMI Ensembles" = "KNMI ens")  # Change legend labels
    ) +
    if (show_range) scale_fill_manual(
      values = ref_colors,
      labels = c("observed" = "Observed", "hindcast" = "Hindcast", "reference" = "KNMI", 
                 "KNMI Ensembles" = "KNMI ens")  # Change legend labels
    ) else ylim(750, 1750) 
  
  # Save the plot as a PDF file
  save_dir <- file.path(here::here(), "Plots", "Reference_Period_Analysis", "TimeSeries")
  if (!dir.exists(save_dir)) {
    dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
  }
  filename <- paste0("TS_", y_label, ifelse(show_ensemble,"ens", ""), ifelse(show_range, paste0("_Q", q_bot*100, "_Q", q_top*100), ""), ".pdf")
  ggsave(file.path(save_dir, filename), plot = p, device = "pdf", width = 18, height = 6)
}

plot_annual_boxplots <- function(ref_rhine_list, scenarios, geb, c_name, stat, y_label, unit) {
  
  # Initialize a list to store yearly data
  yearly_list <- list()
  
  for (scenario in scenarios) {
    # Select only the reference ensemble mean and hindcast scenario
    if (scenario == "reference") {
      yearly_data <- ref_rhine_list[[scenario]][["ensMean"]][["yearly"]]
      
    } else {
      yearly_data <- ref_rhine_list[[scenario]][["yearly"]]
    }
    
    yearly_data <- yearly_data %>%
      select(YYYY, all_of(c_name)) %>%
      mutate(Scenario = scenario)  # Add Scenario column
    
    yearly_list[[length(yearly_list) + 1]] <- yearly_data
  }
  
  # Combine extracted data
  yearly_df <- do.call(rbind, yearly_list)
  
  # Ensure Scenario is a factor with correct order
  yearly_df$Scenario <- factor(yearly_df$Scenario, levels = c("observed", "hindcast", "reference"))
  
  # Plot annual boxplots
  p <- ggplot(yearly_df, aes(x = Scenario, y = .data[[c_name]], fill = Scenario)) +
    geom_boxplot(position = position_dodge(width = 0.8), fatten = 2, size = 0.8) +  
    labs(
      title = paste("Annual", stat, y_label),
      x = "Dataset",
      y = paste(y_label, unit),
      fill = "Dataset"
    ) +
    theme_minimal(base_size = 16) +  
    theme(
      text = element_text(color = "black"),
      axis.title.x = element_blank(),
      axis.text = element_text(size = 14, color = "black"),  
      axis.title = element_text(size = 16, face = "bold", color = "black"),  
      legend.text = element_text(size = 14, color = "black"),  
      legend.title = element_text(size = 16, face = "bold"),  
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5, color = "black"),
      panel.grid.major.x = element_blank(),  # Remove vertical gridlines
      panel.grid.minor.x = element_blank()   # Remove minor vertical gridlines  
    ) +
    scale_fill_manual(values = ref_colors, labels = ref_labels) +
    scale_x_discrete(labels = NULL)  # Remove x-axis labels (only the legend will show)
  
  # save the plot as a pdf file
  save_dir <- file.path(here::here(), "Plots","Reference_Period_Analysis", "Boxplots")
  if (!dir.exists(save_dir)) {
    dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
  }
  ggsave(file.path(save_dir, paste0(geb,"_", y_label, "_annual.pdf")), plot = p, device = "pdf", width = 8, height = 6)
}

# Define the list of scenarios
scenario_list <- c("reference", "hindcast", "observed")

# Compute daily statistics for all ensembles for each scenario
scenario_stats_list <- compute_runoff_statistics_scenarios(ref_rhine_list, "Rhine Basel", column_name = "rhinebasel", scenarios = scenario_list)

# Call the plot function with the computed statistics
plot_annual_boxplots(ref_rhine_list, scenario_list, "RhineBasel", "rhinebasel", "Mean", "Discharge", "[m³/s]")

 plot_runoff_statistics(scenario_stats_list, y_label = "Discharge", variable = "rhinebasel", show_ensemble = TRUE, show_range = FALSE)
 plot_runoff_statistics(scenario_stats_list, y_label = "Discharge", variable = "rhinebasel", show_ensemble = FALSE, show_range = FALSE)
 plot_runoff_statistics(scenario_stats_list, y_label = "Discharge", variable = "rhinebasel", show_ensemble = FALSE, show_range = TRUE)
