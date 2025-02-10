library(ggplot2)
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
      geom_step(aes(x = YYYY, y = (prec_AVG * a + b), color = "Precipitation"), size = 1, direction = "hv") + 
      labs(title = paste("5-Year Mean Temperature and Precipitation"),
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
    
    # Filter the data once and split it into two sets
    data_before_2015 <- data %>% filter(YYYY <= 2015, 
                                        !is.na(count_abla), 
                                        !is.na(count_accu), 
                                        !is.na(count_glac))
    data_after_2015 <- data %>% filter(YYYY >= 2015, 
                                       !is.na(count_abla), 
                                       !is.na(count_accu), 
                                       !is.na(count_glac))
    
    # Glacier plot
    # Glacier plot
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
      guides(linetype = guide_legend(keywidth = 2, keyheight = 1.5)) +  # Adjust key size for better visibility
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
  ggsave(file.path(save_dir, paste0("subset_", selection, ".pdf")), plot = last_plot(), device = "pdf", width = 12, height = 12)
  
}


if (plot_RCP_groups) {
  plot_temperature_by_chain(processed_data_list, "RCP85")
  plot_temperature_by_chain(processed_data_list, "RCP45")
  plot_temperature_by_chain(processed_data_list, "RCP26")
}

