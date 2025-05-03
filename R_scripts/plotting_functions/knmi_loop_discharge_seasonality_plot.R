library(data.table)
library(here)

add_scenario_horizon_grouping_columns <- function(dt) {
  dt[, scen_var := paste(scenario, variant, sep = "_")]
  dt[, scen_var_hor := paste(scen_var, horizon, sep = "_")]
  
  # Define custom order for scenario
  scenario_order <- c("H", "M", "L", "none", "contr", "obs")
  
  # Define custom order for scen_var (including the variants: d, n, none)
  scen_var_order <- c(
    "H_dry", "H_wet", "M_dry", "M_wet", "L_dry", "L_wet",
    "L_none", "none_none", "contr_none", "obs_none"
  )
  
  # Define custom order for scen_var_hor (with horizon)
  scen_var_hor_order <- c(
    "H_dry_2150", "H_dry_2100", "H_dry_2050", 
    "H_wet_2150", "H_wet_2100", "H_wet_2050", 
    "M_dry_2150", "M_dry_2100", "M_dry_2050", 
    "M_wet_2150", "M_wet_2100", "M_wet_2050", 
    "L_dry_2100",
    "L_wet_2100",
    "L_none_2033",
    "none_none_ref", "contr_none_ref", "obs_none_ref"
  )
  
  # Convert scen_var and scen_var_hor to factors with defined levels
  dt[, scenario := factor(scenario, levels = scenario_order)]
  dt[, scen_var := factor(scen_var, levels = scen_var_order)]
  dt[, scen_var_hor := factor(scen_var_hor, levels = scen_var_hor_order)]
}

#knmi_discharge_dt_all <- as.data.table(readRDS(file.path(here::here(),"Data", "Rheinblick2027", "processed_discharge_data", "knmi_discharge_data.rds")))
add_scenario_horizon_grouping_columns(knmi_discharge_dt_all)

home_dir <-file.path(here::here())

plot_dir <- file.path(home_dir, "Plots", "Reference_Period_Analysis", "no_sund_BC")

show_range <- FALSE
show_ensemble <- FALSE
q_bot <- 0.25
q_top <- 0.75

info_text <- "models_ensmem"

color_col <- "hydro_model"

stations <- c("Basel Rheinhalle") # Basel Rheinhalle

horizons <- c("ref")#, "L", "M", "H")

sources <- c("BAFU", "WSL", "BfG", "Deltares")

scen_var_hors <- c("none_none_ref", "obs_none_ref")

gof_pairs <- NULL

# set a statistic to reduce the full data to one value per DayOfYear
statistics <- c("mean")#, "min", "max")

group_cols <- c("station", "scen_var_hor", "hydro_model")
value_cols <- c("discharge")

dt_subset <- knmi_discharge_dt_all[station %in% stations & scen_var_hor %in% scen_var_hors & horizon %in% horizons & source %in% sources]

source(here("R_scripts", "plotting_functions", "plot_seasonality.R"))
source(here("R_scripts", "plotting_functions", "knmi_plot_metadata.R"))

# Compute rolling statistics
rolling_stats_dt <- compute_rolling_stats(dt_subset, group_cols, value_cols)

# Add "rm_" prefix to each value column
group_cols <- c(group_cols, "DayOfYear")
value_cols <- paste0("rm_", value_cols)

# compute seasonality and produce plots
for (stat in statistics) {
  seasonality_dt <- compute_seasonality(rolling_stats_dt, group_cols = group_cols, value_cols = value_cols, stat = stat)
  for (stn in stations) {
    dt <- seasonality_dt[station == stn]
    for (value_col in value_cols) {
      cat("Plotting seasonality for", stn, value_col, "\n")
      
      info_col <- sub("rm_", "", value_col)
      plot_seasonality_ts(dt, plot_dir, stn, info_col, color_col, value_col, stat, info_text, show_ensemble = show_ensemble, show_range = show_range, gof_pairs = gof_pairs)
    } # value_col loop
  } # station loop
} # stat loop
