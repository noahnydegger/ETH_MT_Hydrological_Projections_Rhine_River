
basins <- c("ThS200", "Thu200")
basins <- unique(knmi_meteo_stat_dt$basin)

scenarios <- c("C", "R")#, "L", "M", "H")

value_cols <- c("sund_avg", "tair_avg", "tair_min", "tair_max", "radg_avg", "prec_avg")#, "rhum_avg", "wspd_avg")

dt_subset <- knmi_meteo_stat_dt[basin %in% basins & scenario %in% scenarios]

color_col <- "scen_var_hor"
show_range <- FALSE
show_ensemble <- FALSE

q_bot <- 0.25
q_top <- 0.75

source(here("R_scripts", "plotting_functions", "pdf_cdf_plot.R"))
source(here("R_scripts", "plotting_functions", "plot_metadata_knmi.R"))

# generate pdf, cdf plots
for (bsn in basins) {
  dt <- dt_subset[basin == bsn]
  for (column in value_cols) {
    if (all(is.na(dt[[column]]))) next
    cat("Plotting pdf, cdf for", bsn, column, "\n")
    
    x_name <- plot_info$column_info$names[[column]]
    x_unit <- plot_info$column_info$units[[column]]
    
    plot_pdf(dt, bsn, color_col, column, x_name = x_name, x_unit = x_unit)
    plot_cdf(dt, bsn, color_col, column, x_name = x_name, x_unit = x_unit)
  } # column loop
} # basin loop

