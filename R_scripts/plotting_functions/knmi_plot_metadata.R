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
  hydro_model = list(
    colors = c(
      "PREVAH" = "green",
      "wflow_sbm" = "blue",
      "larsim" = "red",
      "observation" = "black"
    ),
    labels = c(
      "PREVAH" = "PREVAH",
      "wflow_sbm" = "wflow_sbm",
      "larsim" = "larsim",
      "observation" = "Observation"
    )
  ),
  scenario = list(
    colors = c(
      "L" = "blue",       
      "M" = "green",      
      "H" = "red",
      "none" = "grey50"
    ),
    labels = c(
      "L" = "L",
      "M" = "M",
      "H" = "H",
      "none" = "none"
    )
  ),
  scen_var = list(
    colors = c(
      "L_none" = "lightblue",
      "L_dry" = "lightblue",     
      "L_wet" = "blue",          
      "M_dry" = "lightgreen",    
      "M_wet" = "green",         
      "H_dry" = "lightcoral",    
      "H_wet" = "red",
      "none_none" = "grey50"
    ),
    labels = c(
      "L_none" = "L",
      "L_dry" = "L dry",
      "L_wet" = "L wet",
      "M_dry" = "M dry",
      "M_wet" = "M wet",
      "H_dry" = "H dry",
      "H_wet" = "H wet",
      "none_none" = "none"
    )
  ),
  scen_var_hor = list(
    colors = c(
      "none_none_observation" = "black",      
      "none_none_hindcast" = "grey40",     
      "none_none_ref" = "grey70",
      
      "L_none_2033" = "lightblue",
      "L_dry_2100" = "dodgerblue3",    # Darker blue for 2100
      "L_wet_2100" = "deepskyblue",    # Keeping `Ln` as a slightly lighter blue
      
      "M_dry_2050" = "lightgreen",
      "M_dry_2050" = "forestgreen",    # Darker green for 2100
      "M_dry_2100" = "darkgreen",  # Even darker green for 2150
      
      "M_wet_2100" = "mediumseagreen",
      "M_wet_2150" = "seagreen",  # Slightly darker green for 2100
      "M_wet_2150" = "darkseagreen",
      
      "H_dry_2050" = "lightcoral",    # Light coral for 2050
      "H_dry_2050" = "firebrick",     # Darker red for 2100
      "H_dry_2100" = "darkred",       # Darkest red for 2150
      
      "H_wet_2100" = "lightpink",     # Light pink for 2050
      "H_wet_2150" = "pink",          # Slightly darker pink for 2100
      "H_wet_2150" = "hotpink"        # Darkest pink for 2150
    ),
    labels = c(
      "none_none_observation" = "Observation",
      "none_none_hindcast" = "Hindcast",
      "none_none_ref" = "KNMI ref",
      "L_none_2033" = "L 2033",
      "L_dry_2100" = "L dry 2100",
      "L_wet_2100" = "L wet 2100",
      "M_dry_2050" = "M dry 2050",
      "M_dry_2100" = "M dry 2100",
      "M_dry_2150" = "M dry 2150",
      "M_wet_2050" = "M wet 2050",
      "M_wet_2100" = "M wet 2100",
      "M_wet_2150" = "M wet 2150",
      "H_dry_2050" = "H dry 2050",
      "H_dry_2100" = "H dry 2100",
      "H_dry_2150" = "H dry 2150",
      "H_wet_2050" = "H wet 2050",
      "H_wet_2100" = "H wet 2100",
      "H_wet_2150" = "H wet 2150"
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
      "P-uk" = "Interpolated precipitation",
      "P-kor" = "Adjusted interpolated precipitation3",
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
      
      "sdbc_min" = "sunshine duration (min) bc",
      "sdbc_max" = "sunshine duration (max) bc",
      "sdbc_avg" = "sunshine duration (avg) bc",
      "sdbc_std" = "sunshine duration (std) bc",
      
      "rhum_min" = "relative humidity (min)",
      "rhum_max" = "relative humidity (max)",
      "rhum_avg" = "relative humidity (avg)",
      "rhum_std" = "relative humidity (std)",
      
      "wspd_min" = "wind speed (min)",
      "wspd_max" = "wind speed (max)",
      "wspd_avg" = "wind speed (avg)",
      "wspd_std" = "wind speed (std)",
      
      # meteo .2km files
      "sund_abs_mean" = "sunshine duration mean (absolute)",
      "sund_abs_max" = "sunshine duration max (absolute)",
      "sund_rel_mean" = "sunshine duration mean (relative)",
      "sund_rel_max" = "sunshine duration max (relative)",
      "radg_abs_mean" = "global radiation mean (absolute)",
      "radg_abs_max" = "global radiation max (absolute)",
      "radg_rel_mean" = "global radiation mean (relative)",
      "radg_rel_max" = "global radiation max (relative)",
      "sund_rel_mean_z" = "sunshine duration mean (relative z-score)",
      "sund_rel_mean_logit" = "sunshine duration mean (relative logit)",
      "sund_rel_mean_bc_logit" = "sunshine duration mean (relative logit bias corrected)"
    ),
    units = c(
      # routing file
      "discharge" = "m³/s",
      
      # mit output file
      "P-uk" = "mm/d",
      "P-kor" = "mm/d",
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
      
      "sdbc_min" = "hours/d",
      "sdbc_max" = "hours/d",
      "sdbc_avg" = "hours/d",
      "sdbc_std" = "hours/d",
      
      "rhum_min" = "%",
      "rhum_max" = "%",
      "rhum_avg" = "%",
      "rhum_std" = "%",
      
      "wspd_min" = "m/s",
      "wspd_max" = "m/s",
      "wspd_avg" = "m/s",
      "wspd_std" = "m/s",
      
      # meteo .2km files
      "sund_abs_mean" = "hours/d",
      "sund_abs_max" = "hours/d",
      "sund_rel_mean" = "%",
      "sund_rel_max" = "%",
      "radg_abs_mean" = "W/m²",
      "radg_abs_max" = "W/m²",
      "radg_rel_mean" = "%",
      "radg_rel_max" = "%",
      "sund_rel_mean_z" = "",
      "sund_rel_mean_logit" = "",
      "sund_rel_mean_bc_logit" = ""
    )
  )
)