library(data.table)
library(here)

home_dir <-file.path(here::here())

plot_dir <- file.path(home_dir, "Plots", "Model_Comparison", "June02")

info_text <- "Hindcast"

plot_selection <- c(
  "seasonality" = TRUE,
  "annual_boxplot" = TRUE,
  "seasonal_boxplot" = FALSE,
  "pdf_cdf" = FALSE,
  "duration_curve" = FALSE,
  "initial_condition" = FALSE
) 


station_sel <- c("Basel Rheinhalle", "Diepoldsau", "Andelfingen", "Brugg", "Bruegg-Aegerten", "Gisingen", "Rekingen") # Basel Rheinhalle , "Lobith", "Kaub", "Maxau"

horizon_sel <- c("ref", "observation", "hindcast")#, "L", "M", "H")

source_sel <- c("BAFU", "WSL", "BfG", "Deltares")

scen_var_hor_sel <- c("none_none_hindcast", "none_none_observation")

dt_subset <- knmi_discharge_dt_all[station %in% station_sel & scen_var_hor %in% scen_var_hor_sel & horizon %in% horizon_sel & source %in% source_sel]

# seasonality plot -----------------------------------------------------------
if (plot_selection["seasonality"]) {
  source(here("R_scripts", "plotting_functions", "plot_seasonality.R"))
  
  group_cols <- c("station", "scen_var_hor", "hydro_model")
  value_cols <- c("discharge")
  
  color_col <- "hydro_model"
  color_col_levels <- c("observation", "PREVAH", "larsim", "wflow_sbm")

  line_col <- "variant"
  line_col_levels <- c("wet", "dry", "none")
  comparison_ref = "none_none_ref"
  
  gof_pairs <- NULL
  
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
    for (stn in station_sel) {
      dt <- seasonality_dt[station == stn]
      for (value_col in value_cols) {
        cat("Plotting seasonality for", stn, value_col, "\n")
        
        info_col <- sub("rm_", "", value_col)
        plot_seasonality_ts(dt, plot_dir, stn, info_col, color_col, color_col_levels, line_col, line_col_levels, color_col, comparison_ref, value_col, stat, info_text, show_ensemble = show_ensemble, show_range = show_range, gof_pairs = gof_pairs)
      } # value_col loop
    } # basin loop
  } # stat loop
  value_cols <- sub("rm_", "", value_cols)
} # seasonality plot

# annual boxplot plot ------------------------------------------------------
if (plot_selection["annual_boxplot"]) {
  
  source(here("R_scripts", "plotting_functions", "plot_annual_mean.R"))
  
  group_cols <- c("station", "scen_var_hor", "hydro_model")
  
  statistics <- c("mean")#, "min", "max")#, "7day_low")#, "min", "max", "7day_low")
  
  # generate annual boxplots
  for (bsn in station_sel) {
    dt <- dt_subset[station == bsn]
    for (value_col in value_cols) {
      if (all(is.na(dt[[value_col]]))) next
      for (stat in statistics) {
        cat("Plotting annual boxplot for", bsn, value_col, stat, "\n")
        
        dt_annual <- compute_annual_or_seasonal(dt, value_col, group_cols, statistic = stat, seasonal = FALSE)
        
        plot_annual_boxplots(dt_annual, plot_dir, bsn, color_col, color_col_levels, value_col, group_cols, stat, info_text)
      } # stat loop
    } # column loop
  } # basin loop
} # annual boxplot plot

# seasonal boxplot plot ------------------------------------------------------
if (plot_selection["seasonal_boxplot"]) {
  
  source(here("R_scripts", "plotting_functions", "plot_annual_mean.R"))
  
  group_cols <- c("station", "scen_var_hor", "hydro_model", "run_type")
  
  statistics <- c("mean")#, "min", "max")#, "7day_low")
  
  # generate annual boxplots
  for (bsn in station_sel) {
    dt <- dt_subset[station == bsn]
    for (value_col in value_cols) {
      if (all(is.na(dt[[value_col]]))) next
      for (stat in statistics) {
        cat("Plotting seasonal boxplot for", bsn, value_col, stat, "\n")
        
        dt_seasonal <- compute_annual_or_seasonal(dt, value_col, group_cols, statistic = stat, seasonal = TRUE)
        
        plot_seasonal_boxplots(dt_seasonal, plot_dir, bsn, color_col, color_col_levels, value_col, group_cols, stat, info_text)
      } # stat loop
    } # column loop
  } # basin loop
} # seasonal boxplot plot

# pdf & cdf plot -----------------------------------------------------------
if (plot_selection["pdf_cdf"]) {
  
  source(here("R_scripts", "plotting_functions", "plot_pdf_cdf.R"))
  
  group_cols <- c("station", "scen_var_hor", "run_type", "hydro_model")
  
  # generate pdf, cdf plots
  for (bsn in station_sel) {
    dt <- dt_subset[station == bsn]
    for (value_col in value_cols) {
      if (all(is.na(dt[[value_col]]))) next
      cat("Plotting pdf, cdf for", bsn, value_col, "\n")
      
      plot_pdf(dt, plot_dir, bsn, color_col, color_col_levels, value_col, group_cols, info_text)
      plot_cdf(dt, plot_dir, bsn, color_col, color_col_levels, value_col, group_cols, info_text)
    } # column loop
  } # basin loop
} # pdf & cdf plot

# duration curve plot -----------------------------------------------------------
if (plot_selection["duration_curve"]) {
  
  source(here("R_scripts", "plotting_functions", "plot_duration_curve.R"))
  
  group_cols <- c("station", "scen_var_hor", "hydro_model")
  
  # generate pdf, cdf plots
  for (bsn in station_sel) {
    dt <- dt_subset[station == bsn]
    for (value_col in value_cols) {
      if (all(is.na(dt[[value_col]]))) next
      cat("Plotting duration curve for", bsn, value_col, "\n")
      
      plot_fdc(dt, plot_dir, bsn, color_col, value_col, group_cols)
    } # column loop
  } # basin loop
} # duration curve plot


