
dataset <- "mit_output"

show_range <- FALSE
show_ensemble <- FALSE

q_bot <- 0.25
q_top <- 0.75

color_col <- "scen_var_hor"

basins <- c("ThS200", "Thu200")

scenarios <- c("C", "R")#, "L", "M", "H")

if (dataset == "mit_output") {
  value_cols <- c("S-SNO", "P-SME")
  dt_dataset <- knmi_mit_output_dt
  
} else if (dataset == "meteo_stat") {
  value_cols <- c("sund_avg", "tair_avg", "tair_min", "tair_max", "radg_avg", "prec_avg")#, "rhum_avg", "wspd_avg)
  dt_dataset <- knmi_meteo_stat_dt
}

dt_subset <- dt_dataset[basin %in% basins & scenario %in% scenarios]

source(here("R_scripts", "plotting_functions", "pdf_cdf_plot.R"))
source(here("R_scripts", "plotting_functions", "plot_metadata_knmi.R"))

# generate pdf, cdf plots
for (bsn in basins) {
  dt <- dt_subset[basin == bsn]
  for (value_col in value_cols) {
    if (all(is.na(dt[[value_col]]))) next
    cat("Plotting pdf, cdf for", bsn, value_col, "\n")
    
    plot_pdf(dt, bsn, color_col, value_col)
    plot_cdf(dt, bsn, color_col, value_col)
  } # column loop
} # basin loop

