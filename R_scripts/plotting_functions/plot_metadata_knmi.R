plot_info <- list(
  scenario = list(
    colors = c(
      "O_none" = "black",      
      "C_none" = "grey40",     
      "R_none" = "grey70",
      "L_none" = "lightblue",
      "L_d" = "lightblue",     
      "L_n" = "blue",          
      "M_d" = "lightgreen",    
      "M_n" = "green",         
      "H_d" = "lightcoral",    
      "H_n" = "red"           
    ),
    labels = c(
      "O_none" = "Observed", 
      "C_none" = "Hindcast",  
      "R_none" = "KNMI Ref",
      "L_none" = "KNMI L",
      "L_d" = "KNMI Ld",   
      "L_n" = "KNMI Ln",   
      "M_d" = "KNMI Md",   
      "M_n" = "KNMI Mn",   
      "H_d" = "KNMI Hd",   
      "H_n" = "KNMI Hn"    
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
    y_labels = c(
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