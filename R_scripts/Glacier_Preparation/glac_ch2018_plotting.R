library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)
library(cowplot)
library(scales)
library(here)

# Set flags to control plotting behavior
plot_individual_chains <- FALSE  # Set to TRUE to make a plot for every chain.
plot_head <- FALSE               # Set to TRUE to plot only the head (first few chains), FALSE to plot all
plot_RCP_groups <- TRUE         # Set to TRUE to plot temperature by RCP groups

# Check if a variable exists
if (!exists("processed_data_list")) {
  cat("'processed_data_list' does not exist. Running data_processing script...\n")
  
  # Source another R script if the variable doesn't exist
  source(here("R_scripts", "Glacier_Preparation", "glac_ch2018_data_processing.R"))
}

chains_to_plot <- if (plot_head) {
  names(head(processed_data_list))  # Adjust the number 3 to whatever number of chains you want to test
} else {
  names(processed_data_list)  # All chains
}

# Assume processed_data_list contains combined meteo and glacier data for each chain
if (plot_individual_chains) {
  lapply(chains_to_plot, function(chain_name) {
    data <- processed_data_list[[chain_name]]
    data <- data %>% filter(!is.na(temp_AVG), !is.na(prec_AVG))
    
    # Get the min and max values for scaling the secondary y-axis (precipitation)
    temp_min <- min(data$temp_AVG, na.rm = TRUE)
    temp_max <- max(data$temp_AVG, na.rm = TRUE)
    prec_min <- min(data$prec_AVG, na.rm = TRUE)
    prec_max <- max(data$prec_AVG, na.rm = TRUE)
    
    # Scaling factor for precipitation
    scale_factor <- (temp_max-temp_min) / (prec_max-prec_min)
    # Calculate scale factor (a) and offset (b) for linear transformation
    a <- (temp_max-temp_min) / (prec_max-prec_min)
    b <- temp_min - a * prec_min
    
    # Combined plot for Temperature (Line) and Precipitation (Bar) with secondary y-axis
    meteo_plot <- ggplot(data) +
      # Temperature line
      geom_line(aes(x = YYYY, y = temp_AVG, color = "Temperature"), size = 1) +   
      # Precipitation bar
      #geom_bar(aes(x = YYYY, y = prec_AVG, fill = "Precipitation"), stat = "identity", position = "dodge", alpha = 0.5) + 
      #geom_step(aes(x = YYYY, y = (prec_AVG * a + b), color = "Precipitation"), size = 1, direction = "vh") + 
      geom_segment(aes(x = YYYY, xend = lead(YYYY), y = (prec_AVG * a + b), yend = (prec_AVG * a + b), color = "Precipitation"),
                   size = 1, na.rm = TRUE) +
      labs(title = paste("10-Year Mean Temperature, Precipitation"),
           x = "Year",
           y = "Temperature (°C)",
           color = "Legend") +
      scale_color_manual(name = "Legend", values = c("Temperature" = "red", "Precipitation" = "blue")) +
      theme_minimal() +
      theme(legend.position = "top", plot.title = element_text(hjust = 0.5)) +
      scale_y_continuous(
        name = "Temperature (°C)",
        #limits = c(min(temp_min, prec_min) * 0.95, max(temp_max, prec_max) * 1.05),
        sec.axis = sec_axis(
          trans = ~ (. - b) / a ,  # Inverse the transformation for precipitation on secondary axis
          name = "Precipitation (mm/d)"
        )
      )
    
    # Glacier plot
    data <- processed_data_list[[chain_name]]
    # Filter the data once and split it into two sets
    data_before_2015 <- data %>% filter(YYYY <= 2015, 
                                        !is.na(count_abla), 
                                        !is.na(count_accu), 
                                        !is.na(count_glac))
    data_after_2015 <- data %>% filter(YYYY >= 2015, 
                                       !is.na(count_abla), 
                                       !is.na(count_accu), 
                                       !is.na(count_glac))
    glac_plot <- ggplot() +
      # Data before 2015 (dashed lines)
      geom_line(data = data_before_2015, aes(x = YYYY, y = count_abla / count_area * 100, color = "Ablation", linetype = "Past Data"), size = 1) +
      geom_line(data = data_before_2015, aes(x = YYYY, y = count_accu / count_area * 100, color = "Accumulation", linetype = "Past Data"), size = 1) +
      geom_line(data = data_before_2015, aes(x = YYYY, y = count_glac / count_area * 100 , color = "Total Glacier", linetype = "Past Data"), size = 1) +
      
      # Data after 2015 (solid lines)
      geom_line(data = data_after_2015, aes(x = YYYY, y = count_abla / count_area * 100, color = "Ablation", linetype = "Projected Data"), size = 1) +
      geom_line(data = data_after_2015, aes(x = YYYY, y = count_accu / count_area * 100, color = "Accumulation", linetype = "Projected Data"), size = 1) +
      geom_line(data = data_after_2015, aes(x = YYYY, y = count_glac / count_area * 100 , color = "Total Glacier", linetype = "Projected Data"), size = 1) +
      
      labs(
        title = "Relative Glacier Area",
        x = "Year",
        y = "Relative Area (%)",
        color = "Legend",
        linetype = "Data Type"  # Modify legend label for linetype
      ) +
      scale_linetype_manual(values = c("Past Data" = "dashed", "Projected Data" = "solid")) +  # Define line types
      # Adjust legend appearance to make lines more distinguishable
      guides(
        linetype = guide_legend(order = 1, keywidth = 2, keyheight = 1),
        color = guide_legend(order = 2)
      ) +
      theme_minimal() +
      theme(legend.position = "top", plot.title = element_text(hjust = 0.5)) +
      scale_x_continuous(limits = range(data$YYYY))  # Align x-axis range with meteo_plot
    
    # Combine plots with vertical alignment and shared x-axis
    plot_combined <- plot_grid(
      meteo_plot, glac_plot,
      ncol = 1,
      align = "v",  # Align plots vertically
      axis = "lr"  # Align left and right y-axes
    )
    
    # Add a title using `draw_plot_label`
    plot_combined <- ggdraw(plot_combined) + 
      draw_label(chain_name, fontface = "bold", size = 15, x = 0.5, y = 1.02, hjust = 0.5) +  # Increase y to move it lower
      theme(plot.margin = margin(t = 50, r = 10, b = 10, l = 10))  # Increase the top margin for more space
    
    # Define save directory
    save_dir <- file.path(here::here(), "Plots", "glac_2018")
    
    # Create the directory if it doesn't exist
    if (!dir.exists(save_dir)) {
      dir.create(save_dir, recursive = TRUE)
    }
    
    # Save the combined plot to PDF
    ggsave(file.path(save_dir, paste0(chain_name, ".pdf")), plot = plot_combined, device = "pdf", width = 12, height = 12)
    
  })
}

# Function to plot temperature with varying color intensity
plot_temperature_by_chain <- function(processed_data_list, selection) {
  
  # Filter chain names based on the substring
  selected_chains <- grep(selection, chains_to_plot, value = TRUE)
  
  # Check if selected_chains is empty, exit if so
  if (length(selected_chains) == 0) {
    return()  # Exit the function if no chains match the selection
  }
  
  # Create an empty list to store the data for plotting
  plot_data <- list()
  
  # Loop through the selected chains and prepare the data for plotting
  for(chain_name in selected_chains) {
    data <- processed_data_list[[chain_name]]
    
    # Get the maximum temperature for each chain
    max_temp <- max(data$temp_AVG, na.rm = TRUE)
    
    # Add a new column 'Chain' for easy identification and 'MaxTemp' for color
    data$Chain <- chain_name
    data$MaxTemp <- max_temp
    
    # Append the data to the plot_data list
    plot_data[[chain_name]] <- data
  }
  
  # Combine all the data into one data frame
  combined_data <- bind_rows(plot_data)
  combined_data <- combined_data %>% filter(!is.na(YYYY), !is.na(temp_AVG), !is.na(MaxTemp))
  
  # Plot all lines in the same plot, color by MaxTemp
  ggplot(combined_data, aes(x = YYYY, y = temp_AVG, group = Chain, color = MaxTemp)) +
    geom_line(size = 0.5) +  # Plot the lines
    scale_color_gradient(low = "lightblue", high = "darkred") +  # Color gradient by max temperature
    labs(title = paste("Temperature for Chains Containing:", selection),
         x = "Year",
         y = "Temperature (°C)",
         color = "Max Temperature") +
    theme_minimal() +
    theme(legend.position = "top", plot.title = element_text(hjust = 0.5))  # Center the title
  
  # Define save directory
  save_dir <- file.path(here::here(), "Plots", "glac_2018")
  
  # Create the directory if it doesn't exist
  if (!dir.exists(save_dir)) {
    dir.create(save_dir, recursive = TRUE)
  }
  
  # Save the combined plot to PDF
  ggsave(file.path(save_dir, paste0("subset_temp_", selection, ".pdf")), plot = last_plot(), device = "pdf", width = 12, height = 12)
  
}

# Function to plot precipitation with varying color intensity
plot_precipitation_by_chain <- function(processed_data_list, selection) {
  
  # Filter chain names based on the substring
  selected_chains <- grep(selection, chains_to_plot, value = TRUE)
  
  # Check if selected_chains is empty, exit if so
  if (length(selected_chains) == 0) {
    return()  # Exit the function if no chains match the selection
  }
  
  # Create an empty list to store the data for plotting
  plot_data <- list()
  
  # Loop through the selected chains and prepare the data for plotting
  for(chain_name in selected_chains) {
    data <- processed_data_list[[chain_name]]
    
    # Get the maximum precipitation for each chain
    max_prec <- mean(data$prec_AVG, na.rm = TRUE)
    
    # Add a new column 'Chain' for easy identification and 'MaxPrecip' for color
    data$Chain <- chain_name
    data$MaxPrecip <- max_prec
    
    # Append the data to the plot_data list
    plot_data[[chain_name]] <- data
  }
  
  # Combine all the data into one data frame
  combined_data <- bind_rows(plot_data)
  combined_data <- combined_data %>% filter(!is.na(YYYY), !is.na(prec_AVG), !is.na(MaxPrecip))
  
  # Plot all lines in the same plot, color by MaxPrecip
  ggplot(combined_data, aes(x = YYYY, y = prec_AVG, group = Chain, color = MaxPrecip)) +
    geom_line(size = 0.5) +  # Plot the lines
    #geom_step(size = 0.5, direction = "hv") + 
    scale_color_gradient(low = "lightgreen", high = "darkblue") +  # Color gradient by max precipitation
    labs(title = paste("Precipitation for Chains Containing:", selection),
         x = "Year",
         y = "Precipitation (mm/d)",
         color = "Max Precipitation") +
    theme_minimal() +
    theme(legend.position = "top", plot.title = element_text(hjust = 0.5))  # Center the title
  
  # Define save directory
  save_dir <- file.path(here::here(), "Plots", "glac_2018")
  
  # Create the directory if it doesn't exist
  if (!dir.exists(save_dir)) {
    dir.create(save_dir, recursive = TRUE)
  }
  
  # Save the combined plot to PDF
  ggsave(file.path(save_dir, paste0("subset_prec_", selection, ".pdf")), plot = last_plot(), device = "pdf", width = 12, height = 12)
}

plot_mean_glacier_area <- function(processed_data_list) {
  # Define the RCP scenarios
  rcp_scenarios <- c("RCP26", "RCP45", "RCP85")
  
  # Loop through each RCP scenario and calculate the mean
  mean_glacier_data_before_2015 <- list()
  mean_glacier_data_after_2015 <- list()
  
  # Loop through each RCP scenario and calculate the mean
  for (rcp in rcp_scenarios) {
    selected_chains <- grep(rcp, chains_to_plot, value = TRUE)
    selected_data <- processed_data_list[selected_chains]
    
    # Combine all data frames for the given RCP scenario
    combined_data <- bind_rows(selected_data, .id = "Chain") %>%
      group_by(YYYY) %>%
      summarize(
        mean_glac = mean(count_glac / count_area * 100, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(RCP = rcp)  # Add column for RCP
    
    # Separate the combined data into before and after 2015
    data_before_2015 <- combined_data %>% filter(YYYY <= 2015) %>%
      mutate(DataType = "Ovserved")  # Label as "Past Data"
    
    data_after_2015 <- combined_data %>% filter(YYYY >= 2015) %>%
      mutate(DataType = "Projected")  # Label as "Projected Data"
    
    # Store the separated data
    mean_glacier_data_before_2015[[rcp]] <- data_before_2015
    mean_glacier_data_after_2015[[rcp]] <- data_after_2015
  }
  
  # Prepare past data (black dashed lines)
  data_before_2015 <- processed_data_list %>%
    bind_rows(.id = "Chain") %>%
    filter(YYYY <= 2015, !is.na(count_glac)) %>%
    group_by(YYYY) %>%
    summarize(mean_glac = mean(count_glac / count_area * 100, na.rm = TRUE), .groups = "drop") %>%
    mutate(RCP = "Observed", DataType = "Observed")  # Add column for legend
  
  # Start ggplot
  glac_plot <- ggplot()
  
  # Plot past data (black dashed lines)
  glac_plot <- glac_plot +
    geom_line(data = data_before_2015, aes(x = YYYY, y = mean_glac, linetype = DataType), 
              color = "black", size = 1)
  
  # Define colors for RCP scenarios
  rcp_colors <- c("RCP26" = "blue", "RCP45" = "orange", "RCP85" = "red")
  
  # Plot projected data for each RCP (solid lines)
  for (rcp in rcp_scenarios) {
    # Plot data after 2015 (solid lines)
    glac_plot <- glac_plot +
      geom_line(data = mean_glacier_data_after_2015[[rcp]], 
                aes(x = YYYY, y = mean_glac, color = RCP, linetype = DataType), 
                size = 1)
  }
  
  # Finalize the plot with proper legend
  glac_plot <- glac_plot +
    labs(
      title = "Mean Relative Glacier Area",
      x = "Year",
      y = "Relative Area (%)",
      color = "RCP Scenario",
      linetype = "Data Type"
    ) +
    scale_color_manual(values = rcp_colors) +  # Custom colors for RCPs
    scale_linetype_manual(values = c("Observed" = "dashed", "Projected" = "solid")) +  # Linetype legend
    guides(
      linetype = guide_legend(order = 1, keywidth = 2, keyheight = 1),
      color = guide_legend(order = 2)
    ) +
    theme_minimal(base_size = 16) +
    theme(
      legend.position = "top",
      text = element_text(color = "black"),  # Make all text black
      axis.title.x = element_blank(),
      axis.text = element_text(size = 16, color = "black"),  
      axis.title = element_text(size = 18, face = "bold", color = "black"),  
      legend.text = element_text(size = 16, color = "black"),  
      legend.title = element_text(size = 16, face = "bold", color = "black"),  
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5, color = "black")  
    )
  
  # Define save directory
  save_dir <- file.path(here::here(), "Plots", "glac_2018")
  if (!dir.exists(save_dir)) {
    dir.create(save_dir, recursive = TRUE)
  }
  
  # Save the plot
  ggsave(file.path(save_dir, "Mean_glac_Area.pdf"), plot = glac_plot, device = "pdf", width = 22, height = 6)
}


if (plot_RCP_groups) {
  # Temperature
  plot_temperature_by_chain(processed_data_list, "RCP85")
  plot_temperature_by_chain(processed_data_list, "RCP45")
  plot_temperature_by_chain(processed_data_list, "RCP26")
  
  # Precipitation
  plot_precipitation_by_chain(processed_data_list, "RCP85")
  plot_precipitation_by_chain(processed_data_list, "RCP45")
  plot_precipitation_by_chain(processed_data_list, "RCP26")
  
  # Glacier extent
  plot_mean_glacier_area(processed_data_list)
}

