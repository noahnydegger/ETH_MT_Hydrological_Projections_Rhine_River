
source(here("R_scripts", "plotting_functions", "knmi_plot_metadata.R"))

plot_dir <- file.path(home_dir, "Plots", "Reference_Period_Analysis", "2inter_meeting", "sund_bc_analysis")

info_text <- ""

period_sel <- c("simulation")
group_cols <- c("basin", "run_type")
value_cols <- c("tair_avg", "tair_min", "tair_max", "prec_avg","sund_avg", "radg_avg", "rhum_avg", "wspd_avg")
comparison_col <- "scen_var_hor"
comparison_list <- c("none_none_observation", "none_none_ref")

basins <- c("ThS200")
basins <- unique(knmi_meteo_stat_dt$basin)

dt_subset <- knmi_meteo_stat_dt[scen_var_hor %in% comparison_list & period %in% period_sel]

dt_month <- compute_monthly_differences(dt_subset, group_cols, value_cols, comparison_col, comparison_list)
dt_overall <- compute_overall_difference(dt_subset, group_cols, value_cols, comparison_col, comparison_list)

source(here("R_scripts", "data_processing_functions", "deviation_analysis.R"))

for (r_type in c("sund_bc")) {
  for (dev_type in c("abs", "rel")) {
    for (value_col in value_cols) {
      info_col <- value_col
      value_col <- paste0(value_col, "_", dev_type, "_dev")
      plot_monthly_yearly_boxplots(dt_month[run_type == r_type], dt_overall[run_type == r_type], plot_dir, info_col, comparison_col, value_col, dev_type, r_type)
    }
  }
}
