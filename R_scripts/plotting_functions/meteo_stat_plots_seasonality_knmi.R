
# set a statistic to reduce the full data to one value per DayOfYear
statistics <- c("mean")#, "min", "max")

basins <- c("ThS200")#, "Thu200")

scenarios <- c("C", "R", "L", "M", "H")

# group_cols <- c("station", "horizon", "scenario", "variant", "hydro_model")  # discharge_dt
group_cols <- c("basin", "scen_var_hor", "hydro_model")  # knmi_mit_output_dt
value_cols <- c("tair_avg")#, "tair_min", "tair_max", "sund_avg", "radg_avg", "prec_avg")#, "rhum_avg", "wspd_avg")

dt <- knmi_meteo_stat_dt[basin %in% basins & scenario %in% scenarios]

color_col <- "scen_var_hor"
show_range <- FALSE
show_ensemble <- FALSE
gof_pairs <- c("Cnone_2005", "Rnone_2005")

q_bot <- 0.25
q_top <- 0.75

source(here("R_scripts", "plotting_functions", "seasonality_plot.R"))
source(here("R_scripts", "plotting_functions", "plot_metadata_knmi.R"))

# Compute rolling statistics
rolling_stats_dt <- compute_rolling_stats(dt, group_cols, value_cols)

# Add "rm_" prefix to each value column
group_cols <- c(group_cols, "DayOfYear")
value_cols <- paste0("rm_", value_cols)

# compute seasonality and produce plots
for (stat in statistics) {
  seasonality_dt <- compute_seasonality(rolling_stats_dt, group_cols = group_cols, value_cols = value_cols, stat = stat)
  for (column in value_cols) {
    for (bsn in basins) {
      cat("Plotting seasonality for", basin, column, "\n")
      dt <- seasonality_dt[basin == bsn]
      col <- sub("rm_", "", column)
      y_label <- plot_info$column_info$names[[col]]
      y_unit <- plot_info$column_info$units[[col]]
      plot_seasonality_ts(dt, bsn, color_col, column, y_label = y_label, y_unit = y_unit, show_ensemble = show_ensemble, show_range = show_range, gof_pairs = gof_pairs, stat = stat)
    } # basin loop
  } # column loop
} # stat loop

