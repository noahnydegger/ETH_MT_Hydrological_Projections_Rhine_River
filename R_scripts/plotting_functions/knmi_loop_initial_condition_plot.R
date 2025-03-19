
dataset <- "mit_output"

show_range <- FALSE
show_ensemble <- FALSE


color_col <- "scen_var_hor"
date_col <- "date"
sel_day <- "1991-01-01"

basins <- c("ThS200", "Thu200", "RhB200")

scenarios <- c("C", "R")#, "L", "M", "H")

if (dataset == "mit_output") {
  value_cols <- c("S-SNO")
  dt_dataset <- knmi_mit_output_dt
  
} else if (dataset == "meteo_stat") {
  value_cols <- c("sund_avg", "tair_avg", "tair_min", "tair_max", "radg_avg", "prec_avg")#, "rhum_avg", "wspd_avg)
  dt_dataset <- knmi_meteo_stat_dt
}

dt_subset <- dt_dataset[basin %in% basins & scenario %in% scenarios]

source(here("R_scripts", "plotting_functions", "plot_initial_condition.R"))
source(here("R_scripts", "plotting_functions", "knmi_plot_metadata.R"))

# generate pdf, cdf plots
for (bsn in basins) {
  dt <- dt_subset[basin == bsn]
  for (value_col in value_cols) {
    if (all(is.na(dt[[value_col]]))) next
    cat("Plotting one day values for", bsn, value_col, "\n")
    
    plot_one_day_values(dt, bsn, color_col, value_col, date_col, sel_day)
  } # column loop
} # basin loop

