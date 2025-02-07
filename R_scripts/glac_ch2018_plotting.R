library(ggplot2)
library(gridExtra)
library(grid)
library(here)

# Assume processed_data_list contains combined meteo and glacier data for each chain
lapply(names(processed_data_list), function(chain_name) {
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
  
  # Glacier plot
  glac_plot <- ggplot(data, aes(x = YYYY)) +
    geom_line(aes(y = count_abla / count_area * 100, color = "Ablation"), size = 1) +
    geom_line(aes(y = count_accu / count_area * 100, color = "Accumulation"), size = 1) +
    geom_line(aes(y = count_glac / count_area * 100 , color = "Total Glacier"), size = 1) +
    labs(
      title = paste0("Relative Glacier Area"),
      x = "Year",
      y = "Relative Area (%)",
      color = "Legend"
    ) +
    theme_minimal() +
    theme(
      legend.position = "top",
      plot.title = element_text(hjust = 0.5)
    )
  
  # Display combined plots with chain name as the title
  plot_combined <- grid.arrange(
    meteo_plot, glac_plot,
    ncol = 1,
    top = textGrob(chain_name, gp = gpar(fontsize = 15, fontface = "bold"))
  )
  
  # Define save directory
  save_dir <- file.path(here::here(), "Plots", "glac_2018")
  
  # Create the directory if it doesn't exist
  if (!dir.exists(save_dir)) {
    dir.create(save_dir, recursive = TRUE)
  }
  
  # Save the combined plot to PDF
  ggsave(file.path(save_dir, paste0(chain_name, ".pdf")), plot = plot_combined, device = "pdf", width = 12, height = 12)
  
})

