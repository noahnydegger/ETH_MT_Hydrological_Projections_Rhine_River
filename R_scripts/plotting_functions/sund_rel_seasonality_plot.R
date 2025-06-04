
# Function to add 'scenario_variant' and 'scenario_variant_horizon' columns with custom ordering
add_scenario_horizon_grouping_columns <- function(dt) {
  dt[, scen_var := paste(scenario, variant, sep = "_")]
  dt[, scen_var_hor := paste(scen_var, horizon, sep = "_")]
  
  # Define custom order for scenario
  scenario_order <- c("H", "M", "L", "none")
  
  # Define custom order for scen_var (including the variants: dry, wet, none)
  scen_var_order <- c(
    "H_dry", "H_wet", "M_dry", "M_wet", "L_dry", "L_wet",
    "L_none", "none_none"
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
    "none_none_ref", "none_none_hindcast", "none_none_observation"
  )
  
  # Convert scen_var and scen_var_hor to factors with defined levels
  dt[, scenario := factor(scenario, levels = scenario_order)]
  dt[, scen_var := factor(scen_var, levels = scen_var_order)]
  dt[, scen_var_hor := factor(scen_var_hor, levels = scen_var_hor_order)]
  
  return(dt)
}

hindcast_sund_stats <- readRDS(file.path("/Volumes/MT_case_sensitive/ETH_MT_Hydrological_Projections_Rhine_River/Data/Rheinblick2027/processed_meteo/hindcast/sund_rel_crop/hindcast_sund_rel_stats.rds"))
reference_sund_stats <- readRDS(file.path("/Volumes/MT_case_sensitive/ETH_MT_Hydrological_Projections_Rhine_River/Data/Rheinblick2027/processed_meteo/reference/sund_rel_crop/reference_sund_rel_stats.rds"))

setnames(hindcast_sund_stats, old = "ensemble", new = "member")

hindcast_sund_stats[, `:=`(
  basin = "hydro_CH",
  scenario = "none",
  variant = "none",
  horizon = "observation",
  sund_rel_bc = sund_rel
)]

hindcast_sund_stats <- add_scenario_horizon_grouping_columns(hindcast_sund_stats)

reference_sund_stats[, `:=`(
  basin = "hydro_CH",
  scenario = "none",
  variant = "none",
  horizon = "ref"
)]

reference_sund_stats <- add_scenario_horizon_grouping_columns(reference_sund_stats)

sund_stats_dt <- rbind(hindcast_sund_stats, reference_sund_stats, use.names = TRUE, fill = FALSE)

# plots -------------------------------------------------
plot_dir <- file.path(home_dir, "Plots", "Reference_Period_Analysis", "2inter_meeting", "sund_bc_analysis")

# cdf plot -----------------------------------------------------
source(here("R_scripts", "plotting_functions", "plot_pdf_cdf.R"))
basins <- unique(sund_stats_dt$basin)
color_col <- "scen_var_hor"
color_col_levels <- c("none_none_observation", "none_none_ref")

group_cols <- c("scenario", "variant", "horizon")
value_cols <- c("sund_rel", "sund_rel_bc")

info_col <- c("sund_rel_mean")

for (bsn in basins) {
  dt <- sund_stats_dt[basin == bsn]
  for (value_col in value_cols) {
    cat("Plotting cdf for", bsn, value_col, "\n")
    
    plot_cdf(dt, plot_dir, bsn, color_col, color_col_levels, value_col, group_cols, info_text = "")
  }
} # basin loop

# seasonality plot -----------------------------------------------------
source(here("R_scripts", "plotting_functions", "plot_seasonality.R"))
group_cols <- c("basin", "scen_var_hor")
value_cols <- c("sund_rel", "sund_rel_bc")
color_col <- "scen_var_hor"
color_col_levels <- c("none_none_observation", "none_none_ref")

gof_pairs <- c("none_none_observation", "none_none_ref")

# Compute rolling statistics
rolling_stats_dt <- compute_rolling_stats(sund_stats_dt, group_cols, value_cols)

# Add "rm_" prefix to each value column
group_cols <- c(group_cols, "DayOfYear")
value_cols <- paste0("rm_", value_cols)

# compute seasonality and produce plots
for (stat in c("mean")) {
  seasonality_dt <- compute_seasonality(rolling_stats_dt, group_cols = group_cols, value_cols = value_cols, stat = stat)
  for (bsn in basins) {
    dt <- seasonality_dt[basin == bsn]
    for (value_col in value_cols) {
      cat("Plotting seasonality for", bsn, value_col, "\n")
      
      info_col <- c("sund_rel_mean")
      
      plot_seasonality_ts(dt, plot_dir, bsn, info_col, color_col, color_col_levels, value_col, stat, info_text = "", gof_pairs = gof_pairs)
      
    } # value_col loop
  } # basin loop
} # stat loop