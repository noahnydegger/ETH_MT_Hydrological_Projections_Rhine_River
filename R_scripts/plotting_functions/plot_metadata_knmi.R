library(ggplot2)

custom_theme <- function() {
  theme_minimal(base_size = 14) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "gray90"),
      panel.grid.minor = element_blank(),
      text = element_text(color = "black"),
      axis.text = element_text(size = 14, color = "black"),
      axis.title = element_text(size = 16, face = "bold", color = "black"),
      legend.text = element_text(size = 14, color = "black"),
      legend.title = element_text(size = 16, face = "bold", color = "black"),
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5, color = "black")
    )
}

save_plot <- function(plot, save_dir, file_name, width = 18, height = 6) {
  # Ensure the directory exists
  if (!dir.exists(save_dir)) {
    dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Save the plot
  ggsave(
    filename = file.path(save_dir, file_name),
    plot = plot,
    device = "pdf",
    width = width,
    height = height
  )
}

plot_info <- list(
  scenario = list(
    colors = c(
      "O" = "black",      
      "C" = "grey40",     
      "R" = "grey70",
      "L" = "blue",       
      "M" = "green",      
      "H" = "red"         
    ),
    labels = c(
      "O" = "Observed", 
      "C" = "Hindcast",  
      "R" = "KNMI Ref",
      "L" = "KNMI L",
      "M" = "KNMI M",
      "H" = "KNMI H"
    )
  ),
  scen_var = list(
    colors = c(
      "Onone" = "black",      
      "Cnone" = "grey40",     
      "Rnone" = "grey70",
      "Lnone" = "lightblue",
      "Ld" = "lightblue",     
      "Ln" = "blue",          
      "Md" = "lightgreen",    
      "Mn" = "green",         
      "Hd" = "lightcoral",    
      "Hn" = "red"           
    ),
    labels = c(
      "Onone" = "Observed", 
      "Cnone" = "Hindcast",  
      "Rnone" = "KNMI Ref",
      "Lnone" = "KNMI L",
      "Ld" = "KNMI Ld",   
      "Ln" = "KNMI Ln",   
      "Md" = "KNMI Md",   
      "Mn" = "KNMI Mn",   
      "Hd" = "KNMI Hd",   
      "Hn" = "KNMI Hn"    
    )
  ),
  scen_var_hor = list(
    colors = c(
      "Onone_2005" = "black",      
      "Cnone_2005" = "grey40",     
      "Rnone_2005" = "grey70",
      
      "Lnone_2033" = "lightblue", 
      "Ld_2100" = "dodgerblue3",    # Darker blue for 2100
      "Ln_2100" = "deepskyblue",    # Keeping `Ln` as a slightly lighter blue
      
      "Md_2050" = "lightgreen",    
      "Md_2100" = "forestgreen",    # Darker green for 2100
      "Md_2150" = "darkgreen",  # Even darker green for 2150
      
      "Mn_2050" = "mediumseagreen",         
      "Mn_2100" = "seagreen",  # Slightly darker green for 2100
      "Mn_2150" = "darkseagreen",         
      
      "Hd_2050" = "lightcoral",    # Light coral for 2050
      "Hd_2100" = "firebrick",     # Darker red for 2100
      "Hd_2150" = "darkred",       # Darkest red for 2150
      
      "Hn_2050" = "lightpink",     # Light pink for 2050
      "Hn_2100" = "pink",          # Slightly darker pink for 2100
      "Hn_2150" = "hotpink"        # Darkest pink for 2150
    ),
    labels = c(
      "Onone_2005" = "Observed", 
      "Cnone_2005" = "Hindcast", 
      "Rnone_2005" = "KNMI Ref", 
      "Lnone_2033" = "KNMI L (2033)", 
      "Ld_2100" = "KNMI Ld (2100)",  
      "Ln_2100" = "KNMI Ln (2100)",  
      "Md_2050" = "KNMI Md (2050)",  
      "Md_2100" = "KNMI Md (2100)",  
      "Md_2150" = "KNMI Md (2150)",  
      "Mn_2050" = "KNMI Mn (2050)",  
      "Mn_2100" = "KNMI Mn (2100)",  
      "Mn_2150" = "KNMI Mn (2150)",  
      "Hd_2050" = "KNMI Hd (2050)",  
      "Hd_2100" = "KNMI Hd (2100)",  
      "Hd_2150" = "KNMI Hd (2150)",  
      "Hn_2050" = "KNMI Hn (2050)",  
      "Hn_2100" = "KNMI Hn (2100)",  
      "Hn_2150" = "KNMI Hn (2150)"   
    )
  ),
  basin_info = list(
    names = c("Now200" = "Northwestern Switzerland",
              "ThS200" = "Thunersee", 
              "Thu200" = "Thur"
    ),
    area = c("ThS200" = 100, 
             "Thu200" = 100
    )
  ),
  column_info = list(
    names = c(
      # routing file
      "discharge" = "Discharge",
      
      # mit output file
      "P-UK" = "Interpolated precipitation",
      "P-KOR" = "Adjusted interpolated precipitation3",
      "P-SME" = "Snowmelt",
      "EPOT" = "Potential evapotranspiration",
      "EREA" = "Actual evapotranspiration",
      "EI" = "Interception evaporation / snow evaporation",
      "EB" = "Transpiration / soil evaporation",
      "R0" = "Surface runoff",
      "R1" = "Interflow",
      "R2" = "Total baseflow",
      "RGES" = "Total runoff",
      "S-SNO" = "Snow water equivalent",
      "SI" = "Interception storage",
      "SSM" = "Plant available soil moisture storage",
      "SUZ" = "Runoff generation storage (unsaturated zone)",
      "SLZ" = "Runoff generation storage (saturated zone)",
      "BIL" = "Balance from previous time step",
      "GLAC" = "Ice melt",
      "RG1" = "Fast response baseflow",
      "RG2" = "Slow response baseflow",
      "RG3" = "Third component baseflow",
      "DIFGA" = "Input to DIFGA",
      
      # meteo stats file
      "tair_min" = "air temperature (min)",
      "tair_max" = "air temperature (max)",
      "tair_avg" = "air temperature (avg)",
      "tair_std" = "air temperature (std)",
      
      "prec_min" = "precipitation (min)",
      "prec_max" = "precipitation (max)",
      "prec_avg" = "precipitation (avg)",
      "prec_std" = "precipitation (std)",
      
      "radg_min" = "global radiation (min)",
      "radg_max" = "global radiation (max)",
      "radg_avg" = "global radiation (avg)",
      "radg_std" = "global radiation (std)",
      
      "sund_min" = "sunshine duration (min)",
      "sund_max" = "sunshine duration (max)",
      "sund_avg" = "sunshine duration (avg)",
      "sund_std" = "sunshine duration (std)",
      
      "rhum_min" = "relative humidity (min)",
      "rhum_max" = "relative humidity (max)",
      "rhum_avg" = "relative humidity (avg)",
      "rhum_std" = "relative humidity (std)",
      
      "wspd_min" = "wind speed (min)",
      "wspd_max" = "wind speed (max)",
      "wspd_avg" = "wind speed (avg)",
      "wspd_std" = "wind speed (std)"
    ),
    units = c(
      # routing file
      "discharge" = "m³/s",
      
      # mit output file
      "P-UK" = "mm/d",
      "P-KOR" = "mm/d",
      "P-SME" = "mm/d",
      "EPOT" = "mm/d",
      "EREA" = "mm/d",
      "EI" = "mm/d",
      "EB" = "mm/d",
      "R0" = "mm/d",
      "R1" = "mm/d",
      "R2" = "mm/d",
      "RGES" = "mm/d",
      "S-SNO" = "mm",
      "SI" = "mm",
      "SSM" = "mm",
      "SUZ" = "mm",
      "SLZ" = "mm",
      "BIL" = "mm/d",
      "GLAC" = "mm/d",
      "RG1" = "mm/d",
      "RG2" = "mm/d",
      "RG3" = "mm/d",
      "DIFGA" = "mm/d",
      
      # meteo stats file
      "tair_min" = "°C",
      "tair_max" = "°C",
      "tair_avg" = "°C",
      "tair_std" = "°C",
      
      "prec_min" = "mm/d",
      "prec_max" = "mm/d",
      "prec_avg" = "mm/d",
      "prec_std" = "mm/d",
      
      "radg_min" = "W/m²",
      "radg_max" = "W/m²",
      "radg_avg" = "W/m²",
      "radg_std" = "W/m²",
      
      "sund_min" = "hours/d",
      "sund_max" = "hours/d",
      "sund_avg" = "hours/d",
      "sund_std" = "hours/d",
      
      "rhum_min" = "%",
      "rhum_max" = "%",
      "rhum_avg" = "%",
      "rhum_std" = "%",
      
      "wspd_min" = "m/s",
      "wspd_max" = "m/s",
      "wspd_avg" = "m/s",
      "wspd_std" = "m/s"
    )
  )
)