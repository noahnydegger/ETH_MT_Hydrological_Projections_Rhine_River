
dataset <- "mit_output"

show_range <- FALSE
show_ensemble <- FALSE
q_bot <- 0.25
q_top <- 0.75

color_col <- "scen_var_hor"

basins <- c("ThS200", "Thu200", "RhB200")

scenarios <- c("C", "R")#, "L", "M", "H")

gof_pairs <- c("Cnone_2005", "Rnone_2005")

# set a statistic to reduce the full data to one value per DayOfYear
statistics <- c("mean")#, "min", "max")

group_cols <- c("basin", "scen_var_hor", "hydro_model")  # knmi_mit_output_dt

if (dataset == "mit_output") {
  value_cols <- c("S-SNO", "P-SME", "RGES","P-uk","GLAC", "SSM", "SUZ", "SLZ")
  dt_dataset <- knmi_mit_output_dt
  
} else if (dataset == "meteo_stat") {
  value_cols <- c("sund_avg", "tair_avg", "tair_min", "tair_max", "radg_avg", "prec_avg")#, "rhum_avg", "wspd_avg)
  dt_dataset <- knmi_meteo_stat_dt
}

dt_subset <- dt_dataset[basin %in% basins & scenario %in% scenarios]

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
  for (value_col in value_cols) {
    for (bsn in basins) {
      cat("Plotting seasonality for", bsn, value_col, "\n")
      dt <- seasonality_dt[basin == bsn]
      
      plot_seasonality_ts(dt, bsn, color_col, value_col, show_ensemble = show_ensemble, show_range = show_range, gof_pairs = gof_pairs, stat = stat)
    } # basin loop
  } # value_col loop
} # stat loop
