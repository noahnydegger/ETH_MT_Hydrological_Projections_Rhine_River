
dataset <- "mit_output"

show_range <- FALSE
show_ensemble <- FALSE

q_bot <- 0.25
q_top <- 0.75

periods <- c("simulation") # or warmup

color_col <- "scen_var_hor"

basins <- c("ThS200", "Thu200", "RhB200")

scenarios <- c("C", "R")#, "L", "M", "H")

group_cols <- c("basin", "scen_var_hor", "hydro_model")

if (dataset == "mit_output") {
  value_cols <- c("RGES", "S-SNO", "P-SME", "RGES","P-uk","GLAC", "EPOT", "EREA", "SSM", "SUZ", "SLZ")
  dt_dataset <- knmi_mit_output_dt
  
} else if (dataset == "meteo_stat") {
  value_cols <- c("sund_avg", "tair_avg", "tair_min", "tair_max", "radg_avg", "prec_avg")#, "rhum_avg", "wspd_avg)
  dt_dataset <- knmi_meteo_stat_dt
} else if (dataset == "discharge") {
  basins <- "Basel"
  value_cols <- c("discharge")
  dt_dataset <- knmi_discharge_dt
} else {
  stop("Unknown dataset")
}

basins <- unique(dt_dataset$basin)

dt_subset <- dt_dataset[basin %in% basins & scenario %in% scenarios & period %in% periods]

source(here("R_scripts", "plotting_functions", "plot_duration_curve.R"))
source(here("R_scripts", "plotting_functions", "knmi_plot_metadata.R"))

# generate pdf, cdf plots
for (bsn in basins) {
  dt <- dt_subset[basin == bsn]
  for (value_col in value_cols) {
    if (all(is.na(dt[[value_col]]))) next
    cat("Plotting duration curve for", bsn, value_col, "\n")
    
    plot_fdc(dt, bsn, color_col, value_col, group_cols)
  } # column loop
} # basin loop

