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

add_scenario_horizon_grouping_columns(knmi_meteo_rast_dt)

dt_dataset <- knmi_meteo_rast_dt

color_col <- "scen_var_hor"

basins <- c("hydro_CH")#, "Thu200", "RhB200", "Bod400")

scenarios <- c("none", "contr")#, "L", "M", "H")

value_cols <- c("sund_abs", "sund_rel", "radg_abs")
group_cols <- c("scen_var_hor")

gof_pairs <- c("contr_none_ref", "none_none_ref")
info_text <- ""

dt_subset <- dt_dataset[basin %in% basins & scenario %in% scenarios]

source(here("R_scripts", "plotting_functions", "plot_pdf_cdf.R"))
source(here("R_scripts", "plotting_functions", "knmi_plot_metadata.R"))

# generate pdf, cdf plots
for (bsn in basins) {
  dt <- dt_subset[basin == bsn]
  for (value_col in value_cols) {
    if (all(is.na(dt[[value_col]]))) next
    cat("Plotting pdf, cdf for", bsn, value_col, "\n")
    
    plot_pdf(dt, bsn, color_col, value_col, group_cols)
    plot_cdf(dt, bsn, color_col, value_col, group_cols)
  } # column loop
} # basin loop


# scatter plots -------------------------------------------------
source(here("R_scripts", "plotting_functions", "plot_scatter.R"))

value_cols <- c("radg_abs_mean" = "sund_abs_mean", "radg_rel_mean" = "sund_rel_mean")
color_col <- "scen_var_hor"
group_cols <- c("scen_var_hor")
group_list <- c("none_none_ref", "contr_none_ref")

for (bsn in basins) {
  for (group in group_list) {
    dt <- dt_subset[scen_var_hor == group & basin == bsn]

    # Plot scatter for each value column
    for (value_col_x in names(value_cols)) {
      value_col_y <- value_cols[[value_col_x]]
      cat("Plotting scatter for", group, value_col_x, "vs", value_col_y, "\n")
      
      plot_scatter_xy(dt, bsn, color_col, value_col_x, value_col_y, group)
    } # column loop
  } # group loop
} # basin loop

source(here("R_scripts", "plotting_functions", "plot_scatter.R"))

value_cols <- c("radg_rel_mean", "radg_abs_mean", "sund_abs_mean", "sund_rel_mean")
color_col <- "scen_var_hor"
group_cols <- c("scen_var_hor")
group_list <- c("none_none_ref", "contr_none_ref")

for (bsn in basins) {
  dt <- dt_subset[basin == bsn]
  # Plot scatter for each value column
  for (value_col in value_cols) {
    cat("Plotting group scatter for", value_col, "\n")
    
    plot_scatter_group(dt, bsn, color_col, value_col, group_cols, group_list)
  } # column loop
} # basin loop



source(here("R_scripts", "plotting_functions", "plot_seasonality.R"))

# Compute rolling statistics
rolling_stats_dt <- compute_rolling_stats(dt_subset, group_cols, value_cols)

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
      
      info_col <- sub("rm_", "", value_col)
      plot_seasonality_ts(dt, bsn, info_col, color_col, value_col, stat, info_text, gof_pairs = gof_pairs)
    } # value_col loop
  } # basin loop
} # stat loop

dt_mean <- dt_subset[, lapply(.SD, mean, na.rm = TRUE), by = group_cols, .SDcols = value_cols]

# Step 1: Sum by year + group
dt_subset[, date := as.IDate(date)]
dt_subset[, year := year(date)]  # 1. add year column once

# compute yearly sums and means
value_cols <- c("sund_abs", "sund_rel", "radg_abs")
group_cols <- c("scen_var_hor")

yearly_sums <- dt_subset[
  ,
  lapply(.SD, sum, na.rm = TRUE),
  by = c(group_cols, "member", "year"),    # 2. group by your columns + year
  .SDcols = value_cols           # 3. summarize the value columns
]
# Step 2: Mean of yearly sums per group (excluding year now)
mean_of_yearly_sums <- yearly_sums[, lapply(.SD, mean, na.rm = TRUE), by = group_cols, .SDcols = value_cols]
