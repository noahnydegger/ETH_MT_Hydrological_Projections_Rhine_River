library(ggplot2)

custom_theme <- function() {
  theme_minimal(base_size = 20) + # every text 20 for presentation slides
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "gray90"),
      panel.grid.minor = element_blank(),
      text = element_text(color = "black"),
      axis.text = element_text(size = 20, color = "black"),
      axis.title = element_text(size = 20, face = "bold", color = "black"),
      legend.text = element_text(size = 20, color = "black"),
      legend.title = element_text(size = 20, face = "bold", color = "black"),
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
      "PREVAH" = "red",
      "wflow_sbm" = "blue",
      "larsim" = "orange",
      "observation" = "black"
    ),
    linetypes = c(
      "PREVAH" = "solid",
      "wflow_sbm" = "solid",
      "larsim" = "solid",
      "observation" = "solid"
    ),
    labels = c(
      "PREVAH" = "PREVAH (WSL)",
      "wflow_sbm" = "wflow_sbm (Deltares)",
      "larsim" = "LARSIM-ME (BfG)",
      "observation" = "Observation (FOEN)"
    )
  ),
  scenario = list(
    colors = c(
      "L" = rgb(0, 52, 102, maxColorValue = 255),     # Deep blue
      "M" = rgb(247, 148, 32, maxColorValue = 255),   # Orange
      "H" = rgb(153, 0, 2, maxColorValue = 255),       # Deep red
      "none" = "grey50"
    ),
    labels = c(
      "L" = "Low",
      "M" = "Moderate",
      "H" = "High",
      "none" = "Reference"
    )
  ),
  variant = list(
    colors = c(
      "none" = "black",
      "dry" = "blue",
      "wet" = "red"
    ),
    linetypes = c(
      "none" = "solid",      # Solid line for observations or baseline
      "dry"  = "dashed",     # Dashed line for dry scenarios (clear but subtle)
      "wet"  = "dotted"     # Dot-dash line for wet scenarios (visibly distinct)
    ),
    labels = c(
      "none" = "Reference",
      "dry" = "dry",
      "wet" = "wet"
    )
  ),
  horizon = list(
    colors = c(
      "observation" = "black",
      "hindcast" = "grey40",
      "ref" = "grey70",
      "2033" = "#99c2e0"  # L scenario (blue)
    ),
    linetypes = c(
      "none" = "solid",      # Solid line for observations or baseline
      "dry"  = "dashed",     # Dashed line for dry scenarios (clear but subtle)
      "wet"  = "dotdash"     # Dot-dash line for wet scenarios (visibly distinct)
    ),
    labels = c(
      "observation" = "Observation",
      "hindcast" = "Hindcast",
      "ref" = "Reference",
      "2033" = "2033",
      "2050" = "2050",
      "2100" = "2100",
      "2150" = "2150"
    )
  ),
  scen_var = list(
    colors = c(
      "none_none" = "grey50",
      # L scenario (blue)
      "L_dry"  = rgb(20, 70, 130, maxColorValue = 255),      # Deep blue
      "L_wet"  = rgb(102, 153, 204, maxColorValue = 255),   # Lighter blue
      "L_none" = rgb(153, 194, 230, maxColorValue = 255),    # Light blue
      
      # M scenario (orange)
      "M_dry"  = rgb(247, 148, 32, maxColorValue = 255),    # Orange
      "M_wet"  = rgb(255, 198, 128, maxColorValue = 255),   # Lighter orange
      
      # H scenario (red)
      "H_dry"  = rgb(180, 30, 30, maxColorValue = 255),       # Deep red
      "H_wet" = rgb(222, 152, 152, maxColorValue = 255)    # Lighter red
    ),
    linetypes = c(
      "none_none" = "solid",
      # L
      "L_none" = "dashed",
      "L_dry"  = "dashed",
      "L_wet"  = "dotted",
      # M
      "M_dry"  = "dashed",
      "M_wet"  = "dotted",
      # H
      "H_dry"  = "dashed",
      "H_wet"  = "dotted"
    ),
    labels = c(
      "none_none" = "Reference",
      "L_none" = "L 2033",
      "L_dry" = "L dry",
      "L_wet" = "L wet",
      "M_dry" = "M dry",
      "M_wet" = "M wet",
      "H_dry" = "H dry",
      "H_wet" = "H wet"
    )
  ),
  scen_hor = list(
    colors = c(
      # Observation / hindcast / reference
      "none_observation" = "black",
      "none_hindcast"    = "grey40",
      "none_ref"         = "grey70",
      
      # L scenario (blue)
      "L_2033" = "#5a8db8",  # slightly lighter than base
      "L_2100" = "#003466",  # base
      "L_2150" = "#00294f",  # slightly darker
      
      # M scenario (orange)
      "M_2050" = "#fbbf72",  # slightly lighter
      "M_2100" = "#f79420",  # base
      "M_2150" = "#d47500",  # slightly darker
      
      # H scenario (red)
      "H_2050" = "#c65a5a",  # slightly lighter
      "H_2100" = "#990002",  # base
      "H_2150" = "#730001"   # slightly darker
    ),
    linetypes = c(
      "none_observation" = "solid",
      "none_hindcast"    = "solid",
      "none_ref"         = "solid",
      
      "L_2033" = "dashed",  # L scenario (blue)
      "L_2100" = "dashed", #"#66a3d2",
      
      "M_2050" = "dashed", #"#ffc266",
      "M_2100" = "dashed", #"#bf5f00",#"#ffc266",
      "M_2150" = "dashed", #"#bf5f00",#"#ffc266",
      
      "H_2050" = "dashed", #"#f4b8b8",
      "H_2100" = "dashed", #"#990002", #"#f4b8b8",
      "H_2150" = "dashed"  #"#990002"
    ),
    labels = c(
      "none_observation" = "Observation",
      "none_hindcast" = "Hindcast",
      "none_ref" = "Reference",
      "L_2033" = "L 2033",
      "L_2100" = "L 2100",
      "M_2050" = "M 2050",
      "M_2100" = "M 2100",
      "M_2150" = "M 2150",
      "H_2050" = "H 2050",
      "H_2100" = "H 2100",
      "H_2150" = "H 2150"
    )
  ),
  scen_var_hor = list(
    colors = c(
      "none_none_observation" = "black",      
      "none_none_hindcast" = "grey40",     
      "none_none_ref" = "grey70",
      
      # L scenario (blue)
      "L_none_2033" = "#99c2e0",
      "L_dry_2100"  = "#2b5580", # "#66a3d2", 
      "L_wet_2100"  = "#66a3d2", #"#80b3d9", #
      
      # M scenario (orange)
      "M_dry_2050"  = "#bf5f00",
      "M_dry_2100"  = "#994c00", #"#ffc266", #
      "M_dry_2150"  = "#663300",
      
      "M_wet_2050"  = "#ffe5b4",  # Lightest orange-beige
      "M_wet_2100"  = "#ffd591",   # Medium beige "#bf5f00",#"#ffc266", #
      "M_wet_2150"  = "#ffc266",  # Darker, but still lighter than dry
      
      # H scenario (red)
      "H_dry_2050"  = "#990002",
      "H_dry_2100"  = "#660001", # "#660001", #"#990002",#
      "H_dry_2150"  = "#400000",
      
      "H_wet_2050"  = "#e88a8a",  #"#f8c6c6",  # Very light pink
      "H_wet_2100"  = "#d15353",  #"#ee8e8e",  #"#f4b8b8", #"#990002", #"#f4b8b8", #"#f4b8b8", #  # Medium light "#990002",#
      "H_wet_2150"  = "#990002"   #"#e35a5a"   #   # Darker pink, but lighter than dry
    ),
    linetypes = c(
      # Observations / references
      "none_none_observation" = "solid",
      "none_none_hindcast"    = "solid",
      "none_none_ref"         = "solid",
      
      # L scenario
      "L_none_2033" = "dashed",
      "L_dry_2100"  = "dashed",
      "L_wet_2100"  = "dotted",
      
      # M scenario (dry)
      "M_dry_2050"  = "dashed",
      "M_dry_2100"  = "dashed",
      "M_dry_2150"  = "dashed",
      
      # M scenario (wet)
      "M_wet_2050"  = "dotted",
      "M_wet_2100"  = "dotted",
      "M_wet_2150"  = "dotted",
      
      # H scenario (dry)
      "H_dry_2050"  = "dashed",
      "H_dry_2100"  = "dashed",
      "H_dry_2150"  = "dashed",
      
      # H scenario (wet)
      "H_wet_2050"  = "dotted",
      "H_wet_2100"  = "dotted",
      "H_wet_2150"  = "dotted"
    ),
    labels = c(
      "none_none_observation" = "Observation",
      "none_none_hindcast" = "Hindcast",
      "none_none_ref" = "Reference",
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
  run_type = list(
    colors = c(
      "observation" = "black",
      "hindcast" = "grey40",
      "no_sund_bc" = "red",
      "sund_bc" = "orange",
      "with_glac_sdbc" = "darkblue",
      "with_glac_sund" = "lightblue"
    ),
    linetypes = c(
      "observation" = "solid",
      "hindcast" = "solid",
      "no_sund_bc" = "solid",
      "sund_bc" = "solid",
      "with_glac_sdbc" = "solid",
      "with_glac_sund" = "solid"
    ),
    labels = c(
      "observation" = "Observation",
      "hindcast" = "Hindcast",
      "no_sund_bc" = "without sund BC",
      "sund_bc" = "steady glacier extent", "with sund BC",
      "with_glac_sdbc" = "with CH2018 glacier",# "with CH2018 glacier, with sund BC",
      "with_glac_sund" = "with CH2018 glacier, no sund BC"
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
      "sund_rel" = "sunshine duration (relative)", 
      "sund_rel_bc" = "sunshine duration (relative)",
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
      "discharge" = "[m³/s]",
      
      # mit output file
      "P-uk" = "[mm/d]",
      "P-kor" = "[mm/d]",
      "P-SME" = "[mm/d]",
      "EPOT" = "[mm/d]",
      "EREA" = "[mm/d]",
      "EI" = "[mm/d]",
      "EB" = "[mm/d]",
      "R0" = "[mm/d]",
      "R1" = "[mm/d]",
      "R2" = "[mm/d]",
      "RGES" = "[mm/d]",
      "S-SNO" = "[mm]",
      "SI" = "[mm]",
      "SSM" = "[mm]",
      "SUZ" = "[mm]",
      "SLZ" = "[mm]",
      "BIL" = "[mm/d]",
      "GLAC" = "[mm/d]",
      "RG1" = "[mm/d]",
      "RG2" = "[mm/d]",
      "RG3" = "[mm/d]",
      "DIFGA" = "[mm/d]",
      
      # meteo stats file
      "tair_min" = "[°C]",
      "tair_max" = "[°C]",
      "tair_avg" = "[°C]",
      "tair_std" = "[°C]",
      
      "prec_min" = "[mm/d]",
      "prec_max" = "[mm/d]",
      "prec_avg" = "[mm/d]",
      "prec_std" = "[mm/d]",
      
      "radg_min" = "[W/m²]",
      "radg_max" = "[W/m²]",
      "radg_avg" = "[W/m²]",
      "radg_std" = "[W/m²]",
      
      "sund_min" = "[hours/d]",
      "sund_max" = "[hours/d]",
      "sund_avg" = "[hours/d]",
      "sund_std" = "[hours/d]",
      
      "sdbc_min" = "[hours/d]",
      "sdbc_max" = "[hours/d]",
      "sdbc_avg" = "[hours/d]",
      "sdbc_std" = "[hours/d]",
      
      "rhum_min" = "[%]",
      "rhum_max" = "[%]",
      "rhum_avg" = "[%]",
      "rhum_std" = "[%]",
      
      "wspd_min" = "[m/s]",
      "wspd_max" = "[m/s]",
      "wspd_avg" = "[m/s]",
      "wspd_std" = "[m/s]",
      
      # meteo .2km files
      "sund_rel" = "[-]", 
      "sund_rel_bc" = "[-]",
      "sund_abs_mean" = "[hours/d]",
      "sund_abs_max" = "[hours/d]",
      "sund_rel_mean" = "[-]",
      "sund_rel_max" = "[-]",
      "radg_abs_mean" = "[W/m²]",
      "radg_abs_max" = "[W/m²]",
      "radg_rel_mean" = "[-]",
      "radg_rel_max" = "[-]",
      "sund_rel_mean_z" = "",
      "sund_rel_mean_logit" = "",
      "sund_rel_mean_bc_logit" = ""
    )
  )
)