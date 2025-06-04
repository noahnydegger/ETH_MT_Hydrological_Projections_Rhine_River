
# overall settings

home_dir <-file.path(here::here())

plot_dir <- file.path(home_dir, "Plots", "Reference_Period_Analysis", "glacier_scenarios", "glac_sdbc")
plot_dir <- file.path(home_dir, "Plots", "Reference_Period_Analysis", "2inter_meeting", "H_wet")
plot_dir <- file.path(home_dir, "Plots", "future_V1", "all_data")

info_text = ""
#info_text = "sund_bc"
  
# select the dataset to plot: mit_output, meteo_stat, discharge
dataset <- "discharge"

plot_selection <- c(
  "seasonality" = TRUE,
  "annual_boxplot" = FALSE,
  "seasonal_boxplot" = FALSE,
  "monthly_boxplot" = FALSE,
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
basin_sel <- c("RhB200", "ThS200") #, "Bod400", "HiR200", "LaP200", "VoR200" ) #, "RhB200", "RhD200", "RhN200", "RhR200", "Rhb200", "HiR200", "LaP200", "VoR200" , "Thu200"

scen_var_hor_sel <- c("none_none_observation", "none_none_hindcast", "none_none_ref",
                      "L_none_2033",
                      "M_wet_2050", "M_dry_2050",
                      "H_wet_2050", "H_dry_2050", 
                      "L_wet_2100", "L_dry_2100",
                      "M_wet_2100", "M_dry_2100",
                      "H_wet_2100", "H_dry_2100",
                      "M_wet_2150", "M_dry_2150",
                      "H_wet_2150", "H_dry_2150"
                      )
#scen_var_hor_sel <- c("H_dry_2150")

run_type_sel <- c("observation", "hindcast", "sund_bc", "no_sund_bc")
run_type_sel <- c("sund_bc", "with_glac_sdbc")
#run_type_sel <- c("observation", "hindcast", "sund_bc", "no_sund_bc")
run_type_sel <- c("future_V1")

hydro_model_sel <- c("observation" ,"PREVAH") #, "wflow_sbm", "larsim", "PREVAH")

color_col <- "scen_var_hor"
color_col_levels <- scen_var_hor_sel
line_col <- "variant"
line_col_levels <- c("none", "wet", "dry")
comparison_ref = "none_ref"

color_col <- "scen_hor"
color_col_levels <- c("none_ref", "L_2033", "M_2050", "H_2050", "L_2100", "M_2100", "H_2100", "M_2150", "H_2150")
#scen_var_hor_sel <- c("none_none_ref", "L_none_2033", "H_wet_2150", "H_dry_2150")

if (dataset == "mit_output") {
  value_cols <- c("RGES", "GLAC", "P-SME", "EREA", "SSM", "SUZ", "SLZ", "EPOT", "S-SNO")#, "P-uk")
  dt_dataset <- knmi_mit_output_dt
  
} else if (dataset == "meteo_stat") {
  value_cols <- c("sund_avg")#"tair_avg", "tair_min", "tair_max", "prec_avg", "sund_avg", "radg_avg") #"tair_max", 
  dt_dataset <- knmi_meteo_stat_dt
  
} else if (dataset == "discharge") {
  basin_sel <- unique(knmi_discharge_dt$station)
  basin_sel <- c("Basel Rheinhalle")#, "Diepoldsau", "Rhine_Neuhausen", "Aare_Thun", "Andelfingen")#, "Limmatt_Baden", "Reuss_Luzern", "Bruegg-Aegerten", "Aare_Untersiggenthal")
  value_cols <- c("discharge")
  dt_dataset <- knmi_discharge_dt
  setnames(dt_dataset, old = "station", new = "basin", skip_absent = TRUE)
  
} else {
  stop("Unknown dataset")
}

if (length(basin_sel) == 0) {
  basin_sel <- unique(dt_dataset$basin)
}

dt_subset <- dt_dataset[basin %in% basin_sel & 
                          period %in% period_sel & 
                          scen_var_hor %in% scen_var_hor_sel & 
                          run_type %in% run_type_sel & 
                          hydro_model %in% hydro_model_sel]

source(here("R_scripts", "plotting_functions", "knmi_plot_metadata.R"))

# seasonality plot -----------------------------------------------------------
if (plot_selection["seasonality"]) {
  
  source(here("R_scripts", "plotting_functions", "plot_seasonality.R"))
  
  group_cols <- c("basin", "scenario", "variant", "horizon", "scen_var", "scen_var_hor", "run_type", "hydro_model")
  
  gof_pairs <- c("none_none_observation", "none_none_ref")
  gof_pairs <- NULL
  
  show_range <- TRUE
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
    seasonality_dt <- compute_seasonality(rolling_stats_dt, group_cols = group_cols, value_cols = value_cols, stat = stat, q_bot, q_top)
    for (bsn in basin_sel) {
      dt <- seasonality_dt[basin == bsn]
      dt[, scen_hor := interaction(as.character(scenario), as.character(horizon), sep = "_")]
      for (value_col in value_cols) {
        cat("Plotting seasonality for", bsn, value_col, "\n")
        
        info_col <- sub("rm_", "", value_col)
        plot_seasonality_ts(dt, plot_dir, bsn, info_col, color_col, color_col_levels, line_col, line_col_levels, value_col, color_col, comparison_ref, stat, info_text, 
                            q_bot, q_top, show_ensemble = show_ensemble, show_range = show_range, gof_pairs = gof_pairs)
      } # value_col loop
    } # basin loop
  } # stat loop
  value_cols <- sub("rm_", "", value_cols)
} # seasonality plot

# annual boxplot plot ------------------------------------------------------
if (plot_selection["annual_boxplot"]) {
  
  source(here("R_scripts", "plotting_functions", "plot_boxplots.R"))
  
  group_cols <- c("basin", "scen_var_hor", "hydro_model", "run_type")
  
  statistics <- c("mean")#, "min", "max", "7day_low")#, "min", "max", "7day_low")
  
  # generate annual boxplots
  for (bsn in basin_sel) {
    dt <- dt_subset[basin == bsn]
    for (value_col in value_cols) {
      if (all(is.na(dt[[value_col]]))) next
      for (stat in statistics) {
        cat("Plotting annual boxplot for", bsn, value_col, stat, "\n")
        
        dt_annual <- compute_annual_or_seasonal(dt, value_col, group_cols, statistic = stat, seasonal = FALSE)
        
        stat_col <- paste0(value_col, "_", stat)
        dt_annual_rel <- compute_relative_mean(dt_annual, stat_col, color_col, comparison_ref, season = FALSE)
        
        plot_annual_boxplots(dt_annual, plot_dir, bsn, color_col, color_col_levels, comparison_ref, value_col, group_cols, stat, info_text, rel = FALSE)
        plot_annual_boxplots(dt_annual_rel, plot_dir, bsn, color_col, color_col_levels, comparison_ref, value_col, group_cols, stat, info_text, rel = TRUE)
      } # stat loop
    } # column loop
  } # basin loop
} # annual boxplot plot

# seasonal boxplot plot ------------------------------------------------------
if (plot_selection["seasonal_boxplot"]) {
  
  source(here("R_scripts", "plotting_functions", "plot_boxplots.R"))
  
  group_cols <- c("basin", "scen_var_hor", "hydro_model", "run_type")
  
  statistics <- c("mean", "min", "max", "7day_low")
  
  # generate annual boxplots
  for (bsn in basin_sel) {
    dt <- dt_subset[basin == bsn]
    for (value_col in value_cols) {
      if (all(is.na(dt[[value_col]]))) next
      for (stat in statistics) {
        cat("Plotting seasonal boxplot for", bsn, value_col, stat, "\n")
        
        dt_seasonal <- compute_annual_or_seasonal(dt, value_col, group_cols, statistic = stat, seasonal = TRUE)
        
        stat_col <- paste0(value_col, "_", stat)
        dt_seasonal_rel <- compute_relative_mean(dt_seasonal, stat_col, color_col, comparison_ref, season = TRUE)
        
        plot_seasonal_boxplots(dt_seasonal, plot_dir, bsn, color_col, color_col_levels, value_col, group_cols, stat, info_text, rel = FALSE)
        plot_seasonal_boxplots(dt_seasonal_rel, plot_dir, bsn, color_col, color_col_levels, value_col, group_cols, stat, info_text, rel = TRUE)
      } # stat loop
    } # column loop
  } # basin loop
} # seasonal boxplot plot

# monthly boxplot plot ------------------------------------------------------
if (plot_selection["monthly_boxplot"]) {
  
  source(here("R_scripts", "plotting_functions", "plot_boxplots.R"))
  
  group_cols <- c("basin", "scen_var_hor", "hydro_model", "run_type")
  
  statistics <- c("mean")#, "min", "max", "7day_low")
  
  # generate annual boxplots
  for (bsn in basin_sel) {
    dt <- dt_subset[basin == bsn]
    for (value_col in value_cols) {
      if (all(is.na(dt[[value_col]]))) next
      for (stat in statistics) {
        cat("Plotting monthly boxplot for", bsn, value_col, stat, "\n")
        
        dt_month <- compute_annual_or_seasonal(dt, value_col, group_cols, statistic = stat, monthly = TRUE)
        dt_year <- compute_annual_or_seasonal(dt, value_col, group_cols, statistic = stat)
        
        stat_col <- paste0(value_col, "_", stat)
        dt_month_rel <- compute_relative_mean(dt_month, stat_col, color_col, comparison_ref, monthly = TRUE)
        dt_year_rel <- compute_relative_mean(dt_year, stat_col, color_col, comparison_ref)
        
        plot_month_year_boxplots(dt_month, dt_year, plot_dir, bsn, color_col, color_col_levels, comparison_ref, value_col, group_cols, stat, info_text, rel = FALSE)
        plot_month_year_boxplots(dt_month_rel, dt_year_rel, plot_dir, bsn, color_col, color_col_levels, comparison_ref, value_col, group_cols, stat, info_text, rel = TRUE)
      } # stat loop
    } # column loop
  } # basin loop
} # seasonal boxplot plot

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
      
      plot_pdf(dt, plot_dir, bsn, color_col, color_col_levels, value_col, group_cols, info_text)
      plot_cdf(dt, plot_dir, bsn, color_col, color_col_levels, value_col, group_cols, info_text)
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