
# overall settings ----------------------------------------------------

home_dir <-file.path(here::here())

plot_dir <- file.path(home_dir, "Plots", "Reference_Period_Analysis", "glacier_scenarios", "glac_sdbc")
plot_dir <- file.path(home_dir, "Plots", "Reference_Period_Analysis", "2inter_meeting", "H_wet")
plot_dir <- file.path(home_dir, "Plots", "future_V1", "all_data")

info_text = ""
#info_text = "sund_bc"
  
# select the dataset to plot: mit_output, meteo_stat, discharge, lakelevel, meteo_input, meteo_sund
dataset <- "discharge"

plot_selection <- c(
  "seasonality" = TRUE,
  "seasonality_horizon" = FALSE,
  "annual_horizon_mean_diff" = FALSE,
  "CH2018_annual_comparison" = FALSE,
  "annual_boxplot" = FALSE,
  "annual_horizon_boxplot" = FALSE,
  "annual_horizon_boxplot_combination" = FALSE,
  "seasonal_boxplot" = FALSE,
  "seasonal_mean_diff" = FALSE,
  "monthly_boxplot" = FALSE,
  "annual_vars_change_boxplot" = FALSE,
  "half_year_vars_change_boxplot" = FALSE,
  "seasonal_vars_change_boxplot" = FALSE,
  "seasonal_bar" = FALSE,
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
basin_sel <- c("RhB200", "ThS200", "Thu200")#, "RhD200", "RhN200", "RhR200", "AaU200", "ThS200", "Thu200", "TGl200", "HiR200", "VoR200", "Bod400")

scen_var_hor_sel <- c("observation_observation_observation", "hindcast_hindcast_hindcast", "ref_ref_ref",
                      "L_Paris_2033",
                      "M_wet_2050", "M_dry_2050",
                      "H_wet_2050", "H_dry_2050", 
                      "L_wet_2100", "L_dry_2100",
                      "M_wet_2100", "M_dry_2100",
                      "H_wet_2100", "H_dry_2100",
                      "M_wet_2150", "M_dry_2150",
                      "H_wet_2150", "H_dry_2150"
                      )

# scen_var_hor_sel <- c("ref_ref_ref",
#                       "H_wet_2050", "H_dry_2050",
#                       "H_wet_2100", "H_dry_2100",
#                       "H_wet_2150", "H_dry_2150"
# )

horizon_sel <- c("observation", "hindcast", "ref", "2033", "2050", "2100", "2150")
horizon_sel <- c("ref", "2100")#, "2050", "2100", "2150")

run_type_sel <- c("future_V1", "hindcast", "observation", "no_sund_bc", "sund_bc")

hydro_model_sel <- c("none","observation" ,"PREVAH") #, "wflow_sbm", "larsim", "PREVAH")

color_col <- "scen_var"
color_col_levels <- c("ref_ref","L_Paris", "L_wet", "L_dry", "M_wet", "M_dry", "H_wet", "H_dry")

#color_col <- "scen_hor"
#color_col_levels <- c("observation_observation", "hindcast_hindcast", "ref_ref", "L_2033", "M_2050", "H_2050", "L_2100", "M_2100", "H_2100", "M_2150", "H_2150")

#color_col <- "scen_var_hor"
#color_col_levels <- scen_var_hor_sel

color_col <- "scenario"
color_col_levels <- c("ref", "L", "M", "H")
#scen_var_hor_sel <- c("ref_ref_ref", "L_Paris_2033", "H_wet_2150", "H_dry_2150")

#color_col <- "run_type"
#color_col_levels <- c("observation", "hindcast", "sund_bc", "no_sund_bc", "future_V1")

comparison_ref = "ref"

line_col <- "variant"
line_col_levels <- c("observation", "hindcast", "ref", "wet", "dry", "Paris")

if (dataset == "mit_output") {
  value_cols <- c("RGES", "GLAC", "P-SME", "P-kor", "EREA", "EPOT", "S-SNO", "P-uk")
  dt_dataset <- knmi_mit_output_dt
  
} else if (dataset == "meteo_stat") {
  value_cols <- c("tair_avg")#, "prec_avg")#, "tair_min", "tair_max", "sund_avg", "radg_avg") #"tair_max", 
  dt_dataset <- knmi_meteo_stat_dt
  
} else if (dataset == "discharge") {
  basin_sel <- unique(knmi_discharge_dt$station)
  basin_sel <- c("Basel Rheinhalle")#, "Aare_Thun", "Andelfingen", "Diepoldsau", "Rhine_Neuhausen", "Rekingen", "Aare_Thun", "Aare_Untersiggenthal", "Limmatt_Baden", "Mellingen")#, "Limmatt_Baden", "Reuss_Luzern", "Bruegg-Aegerten", "Aare_Untersiggenthal")
  value_cols <- c("discharge")
  dt_dataset <- knmi_discharge_dt
  setnames(dt_dataset, old = "station", new = "basin", skip_absent = TRUE)
  
} else if (dataset == "lakelevel") {
  basin_sel <- unique(knmi_lakelevel_dt$lake)
  basin_sel <- c("Bodensee")
  value_cols <- c("level")
  dt_dataset <- knmi_lakelevel_dt
  setnames(dt_dataset, old = "lake", new = "basin", skip_absent = TRUE)
  
} else if (dataset == "meteo_input") {
  basin_sel <- c("RhB200")
  value_cols <- c("prec_avg")#"tair_avg")#, "prec_avg") #"prec_avg")#
  dt_dataset <- knmi_meteo_input_dt
  
} else if (dataset == "meteo_sund") {
  basin_sel <- c("RhB200")
  value_cols <- c("sund_rel")
  dt_dataset <- knmi_sund_dt
  
} else {
  stop("Unknown dataset")
}

if (length(basin_sel) == 0) {
  basin_sel <- unique(dt_dataset$basin)
}

dt_subset <- dt_dataset[basin %in% basin_sel & 
                          period %in% period_sel & 
                          scen_var_hor %in% scen_var_hor_sel & 
                          horizon %in% horizon_sel &
                          run_type %in% run_type_sel & 
                          hydro_model %in% hydro_model_sel]

source(here("R_scripts", "plotting_functions", "knmi_plot_metadata.R"))

# seasonality plot -----------------------------------------------------------
if (plot_selection["seasonality"]) {
  
  source(here("R_scripts", "plotting_functions", "plot_seasonality.R"))
  
  group_cols <- c("basin", "scenario", "variant", "horizon", "scen_var", "scen_var_hor", "run_type", "hydro_model")
  horizons <- c("2033", "2050", "2100", "2150")
  gof_pairs <- c("none_none_observation", "none_none_ref")
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

# seasonality_horizon plot -----------------------------------------------------------
if (plot_selection["seasonality_horizon"]) {
  
  source(here("R_scripts", "plotting_functions", "plot_seasonality.R"))
  
  group_cols <- c("basin", "scenario", "variant", "horizon", "scen_var", "scen_var_hor", "run_type", "hydro_model")
  
  show_range <- FALSE
  q_bot <- 0.25
  q_top <- 0.75
  
  # set a statistic to reduce the full data to one value per DayOfYear
  statistics <- c("mean")#, "min", "max")
  
  # Compute rolling statistics
  #rolling_stats_dt <- compute_rolling_stats(dt_subset, group_cols, value_cols)
  
  # Add "rm_" prefix to each value column
  group_cols <- c(group_cols, "DayOfYear")
  value_cols <- paste0("rm_", value_cols)
  
  # compute seasonality and produce plots
  for (stat in statistics) {
    #seasonality_dt <- compute_seasonality(rolling_stats_dt, group_cols = group_cols, value_cols = value_cols, stat = stat, q_bot, q_top)
    for (bsn in basin_sel) {
      dt <- seasonality_dt[basin == bsn]
      for (value_col in value_cols) {
        cat("Plotting seasonality for", bsn, value_col, "\n")
        
        info_col <- sub("rm_", "", value_col)
        
        # Combine horizon plots into one plot
        combined_seasonality_horizon_plot(dt, plot_dir, bsn, info_col, value_col, comparison_ref, stat, info_text, 
                                          q_bot, q_top, show_range = show_range)
        
      } # value_col loop
    } # basin loop
  } # stat loop
  value_cols <- sub("rm_", "", value_cols)
} # seasonality_horizon plot

# annual horizon mean diff ------------------------------------------------------
if (plot_selection["annual_horizon_mean_diff"]) {
  
  source(here("R_scripts", "plotting_functions", "plot_boxplots.R"))
  
  group_cols <- c("basin", "scenario", "variant", "horizon", "scen_var", "scen_var_hor", "hydro_model", "run_type")
  linking_cols <- c("basin", "run_type")
  
  statistics <- c("mean")#, "min", "max", "7day_low")#, "min", "max", "7day_low")
  
  # generate annual boxplots
  for (bsn in basin_sel) {
    dt <- dt_subset[basin == bsn]
    for (value_col in value_cols) {
      if (all(is.na(dt[[value_col]]))) next
      for (stat in statistics) {
        cat("Plotting annual mean diff for", bsn, value_col, stat, "\n")
        
        dt_annual <- compute_annual_or_seasonal(dt, value_col, group_cols, statistic = stat, seasonal = FALSE)
        dt_diff <- compute_mean_diff_se(dt, value_col, group_cols, linking_cols, comparison_col = color_col, comparison_ref = comparison_ref, stat)
        
        plot_annual_horizon_mean_diff(dt_diff, plot_dir, bsn, 
                                      color_col, color_col_levels, 
                                      line_col, line_col_levels, 
                                      comparison_ref, value_col, group_cols, stat, info_text, rel = TRUE)
        #plot_annual_horizon_mean_diff(dt_diff, plot_dir, bsn, color_col, color_col_levels, comparison_ref, value_col, group_cols, stat, info_text, y_lim = NULL, abs = TRUE)
      } # stat loop
    } # column loop
  } # basin loop
} # annual horizon mean diff plot

# ch2018 annual comparison ------------------------------------------------------
if (plot_selection["CH2018_annual_comparison"]) {
  
  source(here("R_scripts", "plotting_functions", "plot_ch2018_comparison.R"))
  
  knmi_dt <- dt_subset
  # ch2018_dt <- ch2018_discharge_dt
  # #ch2018_dt <- ch2018_dt[period %in% c("reference", "simulation")]
  # setnames(ch2018_dt, old = "station", new = "basin", skip_absent = TRUE)
  # 
  # # Extract unique horizons and remove "ref"
  # horizons <- setdiff(unique(knmi_dt$horizon), c("ref", "hindcast", "observation"))
  # horizons_num <- as.integer(horizons[grepl("^\\d{4}$", horizons)])
  # 
  # # Extract year from date
  # ch2018_dt[, year := as.integer(format(date, "%Y"))]
  # 
  # # Initialize column
  # ch2018_dt[, horizon := NA_character_]
  # 
  # # Assign numeric horizons by ±15-year range
  # for (h in horizons_num) {
  #   ch2018_dt[year >= h - 14 & year <= h + 15, horizon := as.character(h)]
  # }
  # 
  # # Treat "ref" separately (e.g., 1981–2010)
  # ch2018_dt[year >= 1981 & year <= 2010, horizon := "ref"]
  # 
  # # Clean up
  # ch2018_dt[, year := NULL]
  # ch2018_dt <- ch2018_dt[!is.na(horizon)]
  # 
  # # Map RCP values to scenario codes
  # ch2018_dt[, scenario := fifelse(rcp == "RCP85", "H",
  #                                 fifelse(rcp == "RCP45", "M",
  #                                         fifelse(rcp == "RCP26", "L", NA_character_)))]
  # ch2018_dt[, variant := "CH2018"]
  
  knmi_dt <- knmi_dt[as.numeric(format(date, "%Y")) < 2100]
  
  group_cols_knmi <- c("basin", "scenario", "variant", "horizon", "scen_var", "scen_var_hor", "hydro_model", "run_type")
  linking_cols_knmi <- c("basin", "run_type")
  
  group_cols_ch2018 <- c("basin", "chain", "scenario", "variant", "horizon", "hydro_model", "run_type")
  linking_cols_ch2018 <- c("basin", "chain")
  
  comparison_col_knmi <- "scen_var"
  comparison_ref_knmi <- "ref_ref"
  
  comparison_col_ch2018 <- "horizon"
  comparison_ref_ch2018 <- "ref"
  
  color_col <- "scenario"
  color_col_levels <- c("ref", "L", "M", "H")
  
  line_col <- "variant"
  line_col_levels <- c("ref", "wet", "dry", "Paris", "CH2018")
  
  info_text <- "_1981_2010_15"
  y_lim <- NULL #c(-30, 22)
  
  statistics <- c("mean")#, "min", "max", "7day_low")#, "min", "max", "7day_low")
  source(here("R_scripts", "plotting_functions", "plot_ch2018_comparison.R"))
  # generate annual boxplots
  for (bsn in basin_sel) {
    knmi_dt <- knmi_dt[basin == bsn]
    for (value_col in value_cols) {
      if (all(is.na(knmi_dt[[value_col]]))) next
      for (stat in statistics) {
        cat("Plotting annual ch2018 comparison", bsn, value_col, stat, "\n")
        
        #knmi_dt_annual <- compute_annual_or_seasonal(dt, value_col, group_cols, statistic = stat, seasonal = FALSE)
        knmi_dt_diff <- compute_mean_diff_se(knmi_dt, value_col, group_cols_knmi, linking_cols_knmi, comparison_col_knmi, comparison_ref_knmi, stat)
        
        #ch2018_dt_annual <- compute_annual_or_seasonal(ch2018_dt, value_col, group_cols, statistic = stat, seasonal = FALSE)
        ch2018_dt_diff <- compute_mean_diff_se(ch2018_dt, value_col, group_cols_ch2018, linking_cols_ch2018, comparison_col_ch2018, comparison_ref_ch2018, stat)
        
        plot_annual_horizon_mean_diff_ch2018(knmi_dt_diff, ch2018_dt_diff, plot_dir, bsn, color_col, color_col_levels, line_col, line_col_levels, comparison_ref, value_col, group_cols, stat, info_text, y_lim, rel = TRUE)
        #plot_annual_horizon_mean_diff(dt_diff, plot_dir, bsn, color_col, color_col_levels, comparison_ref, value_col, group_cols, stat, info_text, y_lim = NULL, abs = TRUE)
      } # stat loop
    } # column loop
  } # basin loop
} # ch2018 annual comparison plot

# annual boxplot plot ------------------------------------------------------
if (plot_selection["annual_boxplot"]) {
  
  source(here("R_scripts", "plotting_functions", "plot_boxplots.R"))
  
  group_cols <- c("basin", "scenario", "variant", "horizon", "scen_var", "scen_var_hor", "hydro_model", "run_type")
  linking_cols <- c("basin", "run_type")
  
  statistics <- c("mean")#, "min", "max", "7day_low")#, "min", "max", "7day_low")
  
  # generate annual boxplots
  for (bsn in basin_sel) {
    dt <- dt_subset[basin == bsn]
    for (value_col in value_cols) {
      if (all(is.na(dt[[value_col]]))) next
      for (stat in statistics) {
        cat("Plotting annual boxplot for", bsn, value_col, stat, "\n")
        
        dt_annual <- compute_annual_or_seasonal(dt, value_col, group_cols, statistic = stat, seasonal = FALSE)
        dt_diff <- compute_member_differences(dt, value_col, group_cols, linking_cols, comparison_col = color_col, comparison_ref = comparison_ref, stat)
        
        plot_annual_boxplots(dt_annual, plot_dir, bsn, color_col, color_col_levels, comparison_ref, value_col, group_cols, stat, info_text, rel = FALSE)
        plot_annual_boxplots(dt_diff, plot_dir, bsn, color_col, color_col_levels, comparison_ref, value_col, group_cols, stat, info_text, rel = TRUE)
        } # stat loop
    } # column loop
  } # basin loop
} # annual boxplot plot

# annual horizon boxplot ------------------------------------------------------
if (plot_selection["annual_horizon_boxplot"]) {
  
  source(here("R_scripts", "plotting_functions", "plot_boxplots.R"))
  
  group_cols <- c("basin", "scenario", "variant", "horizon", "scen_var", "scen_var_hor", "hydro_model", "run_type")
  linking_cols <- c("basin", "run_type")
  
  statistics <- c("mean")#, "min", "max", "7day_low")#, "min", "max", "7day_low")
  
  # generate annual boxplots
  for (bsn in basin_sel) {
    dt <- dt_subset[basin == bsn]
    for (value_col in value_cols) {
      if (all(is.na(dt[[value_col]]))) next
      for (stat in statistics) {
        cat("Plotting annual boxplot for", bsn, value_col, stat, "\n")
        
        dt_annual <- compute_annual_or_seasonal(dt, value_col, group_cols, statistic = stat, seasonal = FALSE)
        dt_diff <- compute_member_differences(dt, value_col, group_cols, linking_cols, comparison_col = color_col, comparison_ref = comparison_ref, stat)

        #plot_annual_horizon_boxplots(dt_annual, plot_dir, bsn, color_col, color_col_levels, comparison_ref, value_col, group_cols, stat, info_text, rel = FALSE)
        plot_annual_horizon_boxplots(dt_diff, plot_dir, bsn, color_col, color_col_levels, comparison_ref, value_col, group_cols, stat, info_text, rel = TRUE)
        plot_annual_horizon_boxplots(dt_diff, plot_dir, bsn, color_col, color_col_levels, comparison_ref, value_col, group_cols, stat, info_text, y_lim = NULL, abs = TRUE)
      } # stat loop
    } # column loop
  } # basin loop
} # annual horizon boxplot plot

# annual horizon combination boxplot ------------------------------------------------------
if (plot_selection["annual_horizon_boxplot_combination"]) {
  
  source(here("R_scripts", "plotting_functions", "plot_boxplots.R"))
  
  # generate annual boxplots
  for (bsn in basin_sel) {
    dt <- dt_subset[basin == bsn]
      cat("Plotting annual boxplot for", bsn, "tair_prec mean", "\n")
      
      combined_annual_horizon_boxplot(dt, plot_dir, bsn)
      
  } # basin loop
} # annual horizon boxplot combination

# seasonal boxplot plot ------------------------------------------------------
if (plot_selection["seasonal_boxplot"]) {
  
  source(here("R_scripts", "plotting_functions", "plot_boxplots.R"))
  
  group_cols <- c("basin", "scen_var", "scen_var_hor", "hydro_model", "run_type")
  linking_cols <- c("basin", "run_type")
  
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
        
        dt_diff <- compute_member_differences(dt, value_col, group_cols, linking_cols, comparison_col = color_col, comparison_ref = comparison_ref, stat, seasonal = TRUE)
        
        plot_seasonal_boxplots(dt_seasonal, plot_dir, bsn, color_col, color_col_levels, value_col, group_cols, stat, info_text, rel = FALSE)
        plot_seasonal_boxplots(dt_diff, plot_dir, bsn, color_col, color_col_levels, value_col, group_cols, stat, info_text, rel = TRUE)
      } # stat loop
    } # column loop
  } # basin loop
} # seasonal boxplot plot

# seasonal mean diff plot ------------------------------------------------------
if (plot_selection["seasonal_mean_diff"]) {
  
  source(here("R_scripts", "plotting_functions", "plot_boxplots.R"))
  
  group_cols <- c("basin", "scenario", "variant", "scen_var", "scen_hor", "scen_var_hor", "hydro_model", "run_type")
  linking_cols <- c("basin", "run_type")
  
  statistics <- c("mean")
  
  comparison_ref <- "ref_ref"
  y_lim <- c(-46, 32)
  
  # generate annual boxplots
  for (bsn in basin_sel) {
    dt <- dt_subset[basin == bsn]
    for (value_col in value_cols) {
      if (all(is.na(dt[[value_col]]))) next
      for (stat in statistics) {
        cat("Plotting seasonal mean diff for", bsn, value_col, stat, "\n")
        
        dt_diff <- compute_mean_diff_se(dt, value_col, group_cols, linking_cols, comparison_col = color_col, comparison_ref = comparison_ref, stat, seasonal = TRUE)
        
        #plot_seasonal_mean_diff(dt_diff, plot_dir, bsn, color_col, color_col_levels, 
        #                        line_col, line_col_levels, value_col, group_cols, stat, info_text, y_lim = y_lim, rel = TRUE)
      } # stat loop
    } # column loop
  } # basin loop
} # seasonal boxplot plot

# monthly boxplot plot ------------------------------------------------------
if (plot_selection["monthly_boxplot"]) {
  
  source(here("R_scripts", "plotting_functions", "plot_boxplots.R"))
  
  group_cols <- c("basin", "scen_var", "scen_hor", "scen_var_hor", "hydro_model", "run_type")
  linking_cols <- c("basin", "run_type")
  
  statistics <- c("mean")#, "min", "max", "7day_low")
  
  # generate annual boxplots
  for (bsn in basin_sel) {
    dt <- dt_subset[basin == bsn]
    dt[, scen_hor := interaction(as.character(scenario), as.character(horizon), sep = "_")]
    for (value_col in value_cols) {
      if (all(is.na(dt[[value_col]]))) next
      for (stat in statistics) {
        cat("Plotting monthly boxplot for", bsn, value_col, stat, "\n")
        
        dt_month <- compute_annual_or_seasonal(dt, value_col, group_cols, statistic = stat, monthly = TRUE)
        dt_year <- compute_annual_or_seasonal(dt, value_col, group_cols, statistic = stat)
        
        #dt_month_diff <- compute_member_differences(dt, value_col, group_cols, linking_cols, comparison_col = color_col, comparison_ref = comparison_ref, stat, monthly = TRUE)
        #dt_year_diff <- compute_member_differences(dt, value_col, group_cols, linking_cols, comparison_col = color_col, comparison_ref = comparison_ref, stat)
        
        plot_month_year_boxplots(dt_month, dt_year, plot_dir, bsn, color_col, color_col_levels, comparison_ref, value_col, group_cols, stat, info_text, rel = FALSE)
        #plot_month_year_boxplots(dt_month_diff, dt_year_diff, plot_dir, bsn, color_col, color_col_levels, comparison_ref, value_col, group_cols, stat, info_text, rel = TRUE)
      } # stat loop
    } # column loop
  } # basin loop
} # seasonal boxplot plot

# annual vars change boxplot ------------------------------------------------------
if (plot_selection["annual_vars_change_boxplot"]) {
  
  source(here("R_scripts", "plotting_functions", "plot_boxplots.R"))
  
  group_cols <- c("basin", "scenario", "variant", "horizon", "scen_var", "scen_var_hor", "hydro_model", "run_type")
  value_cols <- c("P-kor", "GLAC", "P-SME", "EREA", "RGES")
  linking_cols <- c("basin", "run_type")
  
  color_col <- "scen_var_hor"
  comparison_ref <- "none_none_ref"
  
  statistics <- c("mean")#, mean, "min", "max", "7day_low")#, "min", "max", "7day_low")
  
  # generate annual boxplots
  for (bsn in basin_sel) {
    dt <- dt_subset[basin == bsn]
    for (stat in statistics) {
      cat("Plotting annual variables change boxplot for", bsn, "\n")
      
      dt_diff_long <- get_combined_long_diff_dt(dt, value_cols, group_cols, linking_cols, color_col, comparison_ref, stat)
      
      combined_variables_change_boxplot_annual(dt_diff_long, plot_dir, bsn, time_period = "future")
      
    } # stat loop
  } # basin loop
} # annual vars change boxplot plot

# half year vars change boxplot ------------------------------------------------------
if (plot_selection["half_year_vars_change_boxplot"]) {
  
  source(here("R_scripts", "plotting_functions", "plot_boxplots.R"))
  
  group_cols <- c("basin", "scenario", "variant", "horizon", "scen_var", "scen_var_hor", "hydro_model", "run_type")
  value_cols <- c("P-kor", "P-SME", "EREA", "RGES")
  linking_cols <- c("basin", "run_type")
  
  color_col <- "scen_var_hor"
  comparison_ref <- "none_none_ref"
  
  statistics <- c("mean")#, mean, "min", "max", "7day_low")#, "min", "max", "7day_low")
  
  # generate annual boxplots
  for (bsn in basin_sel) {
    dt <- dt_subset[basin == bsn]
    for (stat in statistics) {
      cat("Plotting half year variables change boxplot for", bsn, "\n")
      
      dt_diff_long <- get_combined_long_diff_dt(dt, value_cols, group_cols, linking_cols, color_col, comparison_ref, stat, half_year = TRUE)
      
      combined_variables_change_boxplot_half_year(dt_diff_long, plot_dir, bsn, time_period = "future")
      
    } # stat loop
  } # basin loop
} # half year vars change boxplot plot

# seasonal vars change boxplot ------------------------------------------------------
if (plot_selection["seasonal_vars_change_boxplot"]) {
  
  source(here("R_scripts", "plotting_functions", "plot_boxplots.R"))
  
  group_cols <- c("basin", "scenario", "variant", "horizon", "scen_var", "scen_var_hor", "hydro_model", "run_type")
  value_cols <- c("P-kor", "P-SME", "EREA", "RGES")
  linking_cols <- c("basin", "run_type")
  
  color_col <- "scen_var_hor"
  comparison_ref <- "none_none_ref"
  
  statistics <- c("mean")#, mean, "min", "max", "7day_low")#, "min", "max", "7day_low")
  
  # generate annual boxplots
  for (bsn in basin_sel) {
    dt <- dt_subset[basin == bsn]
    for (stat in statistics) {
      cat("Plotting seasonal variables change boxplot for", bsn, "\n")
      
      dt_diff_long <- get_combined_long_diff_dt(dt, value_cols, group_cols, linking_cols, color_col, comparison_ref, stat, seasonal = TRUE)
      
      combined_variables_change_boxplot_seasonal(dt_diff_long, plot_dir, bsn, time_period = "future")
      
    } # stat loop
  } # basin loop
} # half year vars change boxplot plot


# seasonal bar extreme plot ------------------------------------------------------
if (plot_selection["seasonal_bar"]) {
  
  source(here("R_scripts", "plotting_functions", "plot_bar_extremes.R"))
  
  group_cols <- c("basin", "horizon", "scenario", "scen_var", "scen_var_hor", "hydro_model", "run_type")
  
  comparison_col <- "scen_var_hor"
  comparison_ref <- "ref_ref_ref"
  
  q_bot <- 0.05
  q_top <- 0.95
  
  for (bsn in basin_sel) {
    dt <- dt_subset[basin == bsn]
    for (value_col in value_cols) {
      if (all(is.na(dt[[value_col]]))) next
      cat("Plotting seasonal bar plot for", bsn, value_col, "\n")
      
      dt_dpy <- compute_number_of_days(dt, bsn, group_cols, value_col, comparison_col, comparison_ref, q_bot, q_top)
      
      #plot_seasonal_bars_with_error(dt_dpy, plot_dir, bsn, color_col, color_col_levels, value_col, group_cols, info_text = info_text, rel = FALSE)
      
      #combined_bar_plot_season(dt_dpy, plot_dir, bsn, q_bot, q_top, time_period="ref")
      combined_bar_plot_season(dt_dpy, plot_dir, bsn, q_bot, q_top, time_period="future")
    } # column loop
  } # basin loop
} # seasonal bar plot

# pdf & cdf plot -----------------------------------------------------------
if (plot_selection["pdf_cdf"]) {
  
  source(here("R_scripts", "plotting_functions", "plot_pdf_cdf.R"))
  
  group_cols <- c("basin", "scenario", "variant", "scen_var", "scen_var_hor", "run_type", "hydro_model")
  
  color_col <- "scenario"
  color_col_levels <- c("ref", "L", "M", "H")
  group_col <- "scen_var"
  
  # generate pdf, cdf plots
  for (bsn in basin_sel) {
    dt <- dt_subset[basin == bsn]
    for (value_col in value_cols) {
      if (all(is.na(dt[[value_col]]))) next
      for (month in sprintf("%02d", 1:12)) {
        dt_month <- dt[format(date, "%m") == month]
        info_text_month <- paste0(info_text, "_", month)
        cat("Plotting pdf, cdf for", bsn, value_col, month, "\n")

        plot_pdf_with_percentiles(dt_month, plot_dir, bsn, color_col, color_col_levels, line_col, line_col_levels, group_col, value_col, group_cols, info_text_month)

        #plot_pdf(dt_month, plot_dir, bsn, color_col, color_col_levels, value_col, group_cols, info_text_month)
        #plot_cdf(dt_month, plot_dir, bsn, color_col, color_col_levels, value_col, group_cols, info_text_month)
      } # month loop
      
      info_text_year <- paste0(info_text, "_", "year")
      
      plot_pdf_with_percentiles(dt, plot_dir, bsn, color_col, color_col_levels, line_col, line_col_levels, group_col, value_col, group_cols, info_text_year)
      
      #plot_pdf(dt, plot_dir, bsn, color_col, color_col_levels, value_col, group_cols, info_text)
      #plot_cdf(dt, plot_dir, bsn, color_col, color_col_levels, value_col, group_cols, info_text)
      
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