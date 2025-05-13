
# overall settings

home_dir <-file.path(here::here())

plot_dir <- file.path(home_dir, "Plots", "Reference_Period_Analysis", "glacier_scenarios", "glac_sdbc")
plot_dir <- file.path(home_dir, "Plots", "Reference_Period_Analysis", "sund_BC")

info_text = "_Ln_2100_glac_sdbc"
info_text = ""
  
# select the dataset to plot: mit_output, meteo_stat, discharge
dataset <- "discharge"

plot_selection <- c(
  "seasonality" = FALSE,
  "annual_boxplot" = TRUE,
  "pdf_cdf" = FALSE,
  "duration_curve" = FALSE,
  "initial_condition" = FALSE
)

plot_dataset <- c(
  "mit_output" = FALSE,
  "meteo_stat" = FALSE,
  "discharge" = FALSE 
)

period_sel <- c("simulation") # or warmup

# if empty c() plot all basins
basin_sel <- c("ThS200")#, "Thu200", "RhB200", "HiR200", "LaP200", "VoR200")

scen_var_hor_sel <- c("none_none_ref", "none_none_hindcast", "none_none_observation")
#scen_var_hor_sel <- c("L_wet_2100")

run_type_sel <- c("observation", "hindcast", "sund_bc", "no_sund_bc", "with_glac_sdbc", "with_glac_sund")
#run_type_sel <- c("no_sund_bc", "sund_bc","with_glac_sund", "with_glac_sdbc")
run_type_sel <- c("hindcast", "observation", "sund_bc", "no_sund_bc")

color_col <- "run_type"

if (dataset == "mit_output") {
  value_cols <- c("RGES", "GLAC", "S-SNO", "P-SME", "EPOT", "EREA", "SSM", "SUZ", "SLZ")#, "P-uk")
  dt_dataset <- knmi_mit_output_dt
  
} else if (dataset == "meteo_stat") {
  value_cols <- c("sund_avg", "tair_avg", "tair_min", "tair_max", "radg_avg", "prec_avg", "rhum_avg", "wspd_avg")
  dt_dataset <- knmi_meteo_stat_dt
  
} else if (dataset == "discharge") {
  basin_sel <- unique(knmi_discharge_dt$station)
  #basin_sel <- c("Basel Rheinhalle")
  value_cols <- c("discharge")
  dt_dataset <- knmi_discharge_dt
  setnames(dt_dataset, old = "station", new = "basin", skip_absent = TRUE)
  
} else {
  stop("Unknown dataset")
}

if (length(basin_sel) == 0) {
  basin_sel <- unique(dt_dataset$basin)
}

dt_subset <- dt_dataset[basin %in% basin_sel & period %in% period_sel & scen_var_hor %in% scen_var_hor_sel & run_type %in% run_type_sel]

dt_subset[, run_type := factor(run_type, levels = run_type_sel)]

source(here("R_scripts", "plotting_functions", "knmi_plot_metadata.R"))

# seasonality plot -----------------------------------------------------------
if (plot_selection["seasonality"]) {
  
  source(here("R_scripts", "plotting_functions", "plot_seasonality.R"))
  
  group_cols <- c("basin", "scen_var_hor", "run_type", "hydro_model")
  
  gof_pairs <- c("none_none_hindcast", "none_none_ref")
  
  show_range <- FALSE
  show_ensemble <- FALSE
  q_bot <- 0.25
  q_top <- 0.75
  
  # set a statistic to reduce the full data to one value per DayOfYear
  statistics <- c("mean")#, "min", "max")
  
  # Compute rolling statistics
  rolling_stats_dt <- compute_rolling_stats(dt_subset, group_cols, value_cols)
  
  # Add "rm_" prefix to each value column
  group_cols <- c(group_cols, "DayOfYear")
  value_cols <- paste0("rm_", value_cols)
  
  # compute seasonality and produce plots
  for (stat in statistics) {
    seasonality_dt <- compute_seasonality(rolling_stats_dt, group_cols = group_cols, value_cols = value_cols, stat = stat)
    for (bsn in basin_sel) {
      dt <- seasonality_dt[basin == bsn]
      for (value_col in value_cols) {
        cat("Plotting seasonality for", bsn, value_col, "\n")
        
        info_col <- sub("rm_", "", value_col)
        plot_seasonality_ts(dt, plot_dir, bsn, info_col, color_col, value_col, stat, info_text, show_ensemble = show_ensemble, show_range = show_range, gof_pairs = gof_pairs)
      } # value_col loop
    } # basin loop
  } # stat loop
  value_cols <- sub("rm_", "", value_cols)
} # seasonality plot

# annual boxplot plot ------------------------------------------------------
if (plot_selection["annual_boxplot"]) {
  
  source(here("R_scripts", "plotting_functions", "plot_annual_mean.R"))
  
  group_cols <- c("basin", "scen_var_hor", "hydro_model", "run_type")
  
  # generate annual boxplots
  for (bsn in basin_sel) {
    dt <- dt_subset[basin == bsn]
    for (value_col in value_cols) {
      if (all(is.na(dt[[value_col]]))) next
      cat("Plotting annual boxplot for", bsn, value_col, "\n")
      
      plot_annual_boxplots(dt, plot_dir, bsn, color_col, value_col, group_cols, info_text)
    } # column loop
  } # basin loop
} # annual boxplot plot

# pdf & cdf plot -----------------------------------------------------------
if (plot_selection["pdf_cdf"]) {
  
  source(here("R_scripts", "plotting_functions", "plot_pdf_cdf.R"))
  
  group_cols <- c("basin", "scen_var_hor", "run_type", "hydro_model")
  
  # generate pdf, cdf plots
  for (bsn in basin_sel) {
    dt <- dt_subset[basin == bsn]
    for (value_col in value_cols) {
      if (all(is.na(dt[[value_col]]))) next
      cat("Plotting pdf, cdf for", bsn, value_col, "\n")
      
      plot_pdf(dt, plot_dir, bsn, color_col, value_col, group_cols)
      plot_cdf(dt, plot_dir, bsn, color_col, value_col, group_cols)
    } # column loop
  } # basin loop
} # pdf & cdf plot

# duration curve plot -----------------------------------------------------------
if (plot_selection["duration_curve"]) {
  
  source(here("R_scripts", "plotting_functions", "plot_duration_curve.R"))
  
  group_cols <- c("basin", "scen_var_hor", "hydro_model")
  
  # generate pdf, cdf plots
  for (bsn in basin_sel) {
    dt <- dt_subset[basin == bsn]
    for (value_col in value_cols) {
      if (all(is.na(dt[[value_col]]))) next
      cat("Plotting duration curve for", bsn, value_col, "\n")
      
      plot_fdc(dt, plot_dir, bsn, color_col, value_col, group_cols)
    } # column loop
  } # basin loop
} # duration curve plot