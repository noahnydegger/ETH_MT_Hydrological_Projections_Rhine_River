
source(here("R_scripts", "data_processing_functions", "deviation_analysis.R"))
source(here("R_scripts", "plotting_functions", "plot_seasonality.R"))
source(here("R_scripts", "plotting_functions", "knmi_plot_metadata.R"))

periods <- c("simulation")
group_cols <- c("basin")
value_cols <- c("sund_avg")
comparison_col <- "scen_var_hor"
comparison_list <- c("Cnone_2005", "Rnone_2005")

basins <- c("ThS200")
basins <- unique(knmi_meteo_stat_dt$basin)

dt_subset <- knmi_meteo_stat_dt[scen_var_hor %in% comparison_list & period %in% periods]

dt_month <- compute_monthly_differences(dt_subset, group_cols, value_cols, comparison_col, comparison_list)
dt_overall <- compute_overall_difference(dt_subset, group_cols, value_cols, comparison_col, comparison_list)

for (dev_type in c("abs", "rel")) {
  for (value_col in value_cols) {
    info_col <- sub("(avg).*", "\\1", value_col)
    value_col <- paste0(value_cols, "_", dev_type, "_dev")
    plot_monthly_yearly_boxplots(dt_month, dt_overall, info_col, comparison_col, value_col, dev_type)
  }
}


sund_bc <- knmi_meteo_stat_dt[basin %in% basins & scenario %in% scenarios & period %in% periods, .(basin, date, horizon, scenario, variant, member, hydro_model, scen_var, scen_var_hor, period, sund_avg)]

# Extract month directly from the date column
sund_bc[, MM := month(date)]  # Extract month as numeric (1-12)
sund_bc[, MM := factor(month.abb[MM], levels = month.abb[1:12])]  # Convert month to factor with custom levels


sund_monthly_factors <- dt_month[, .(sund_avg_abs_fac = mean(sund_avg_abs_dev, na.rm = TRUE),
                                     sund_avg_rel_fac = mean(sund_avg_rel_dev, na.rm = TRUE)), 
                                 by = .(MM, scen_var_hor)]

sund_overall_factors <- dt_overall[, .(sund_avg_abs_fac = mean(sund_avg_abs_dev, na.rm = TRUE),
                                       sund_avg_rel_fac = mean(sund_avg_rel_dev, na.rm = TRUE)), 
                                   by = .(scen_var_hor)]

# Subtract the monthly mean from each daily value with matching by MM and scen_var_hor
sund_bc[, sund_avg_bc_monthly := fifelse(
  scenario == "R",
  pmax(sund_avg - sund_monthly_factors$sund_avg_abs_fac[
    match(paste(MM, scen_var_hor), 
          paste(sund_monthly_factors$MM, sund_monthly_factors$scen_var_hor))], 
    0),
  sund_avg)]

# Subtract the overall mean with matching by scen_var_hor
sund_bc[, sund_avg_bc_overall := fifelse(
  scenario == "R",
  pmax(sund_avg - sund_overall_factors$sund_avg_abs_fac[
    match(scen_var_hor, sund_overall_factors$scen_var_hor)], 
    0),
  sund_avg)]


group_cols <- c("basin", "scen_var_hor", "hydro_model")
value_cols <- c("sund_avg", "sund_avg_bc_monthly", "sund_avg_bc_overall")
color_col <- "scen_var_hor"

gof_pairs <- c("Cnone_2005", "Rnone_2005")

# Compute rolling statistics
rolling_stats_dt <- compute_rolling_stats(sund_bc, group_cols, value_cols)

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
      info_col <- sub("(avg).*", "\\1", info_col)
      
      plot_seasonality_ts(dt, bsn, info_col, color_col, value_col, stat, gof_pairs = gof_pairs)
      
    } # value_col loop
  } # basin loop
} # stat loop

source(here("R_scripts", "plotting_functions", "plot_pdf_cdf.R"))
basins <- unique(sund_bc$basin)
color_col <- "scen_var_hor"
value_cols <- c("sund_avg", "sund_avg_bc_monthly", "sund_avg_bc_overall")
for (bsn in basins) {
  dt <- sund_bc[basin == bsn]
  cat("Plotting cdf for", bsn, value_col, "\n")
  
  plot_cdf(dt, bsn, color_col, value_cols[1], value_cols[2], value_cols[3], group_cols)
} # basin loop

group_cols <- c("basin")
value_cols <- c("sund_avg", "sund_avg_bc_monthly", "sund_avg_bc_overall")

dt_month <- compute_monthly_differences(sund_bc, group_cols, value_cols, comparison_col, comparison_list)
dt_overall <- compute_overall_difference(sund_bc, group_cols, value_cols, comparison_col, comparison_list)

for (dev_type in c("abs", "rel")) {
  for (value_col in value_cols) {
    info_col <- sub("(avg).*", "\\1", value_col)
    value_col <- paste0(value_col, "_", dev_type, "_dev")
    
    plot_monthly_yearly_boxplots(dt_month, dt_overall, info_col, comparison_col, value_col, dev_type)
  }
}

value_cols <- c("sund_avg", "sund_avg_bc_monthly", "sund_avg_bc_overall")

sund_monthly_factors <- dt_month[, .(sund_avg_abs_fac = mean(sund_avg_abs_dev, na.rm = TRUE),
                                     sund_avg_rel_fac = mean(sund_avg_rel_dev, na.rm = TRUE)), 
                                 by = .(MM, scen_var_hor)]

sund_overall_factors <- dt_overall[, .(sund_avg_abs_fac = mean(sund_avg_abs_dev, na.rm = TRUE),
                                       sund_avg_rel_fac = mean(sund_avg_rel_dev, na.rm = TRUE)), 
                                   by = .(scen_var_hor)]

# Define the value columns to loop over
value_cols <- c("sund_avg", "sund_avg_bc_monthly", "sund_avg_bc_overall")

# Compute the mean absolute and relative deviation factors dynamically for each value column
sund_monthly_factors <- dt_month[, 
                                 {
                                   # Start with the list to store results
                                   res <- list()
                                   
                                   # Loop over value_cols and calculate the factors
                                   for (col in value_cols) {
                                     res[[paste0(col, "_mean")]] <- mean(get(col), na.rm = TRUE)
                                     res[[paste0(col, "_abs_fac")]] <- mean(get(paste0(col, "_abs_dev")), na.rm = TRUE)
                                     res[[paste0(col, "_rel_fac")]] <- mean(get(paste0(col, "_rel_dev")), na.rm = TRUE)
                                   }
                                   
                                   # Return the results for each group
                                   res
                                 }, 
                                 by = .(MM, scen_var_hor)
]

# Compute the overall factors (mean absolute and relative factors dynamically)
sund_overall_factors <- dt_overall[, 
                                   {
                                     # Start with the list to store results
                                     res <- list()
                                     
                                     # Loop over value_cols and calculate the factors
                                     for (col in value_cols) {
                                       res[[paste0(col, "_mean")]] <- mean(get(col), na.rm = TRUE)
                                       res[[paste0(col, "_abs_fac")]] <- mean(get(paste0(col, "_abs_dev")), na.rm = TRUE)
                                       res[[paste0(col, "_rel_fac")]] <- mean(get(paste0(col, "_rel_dev")), na.rm = TRUE)
                                     }
                                     
                                     # Return the results for each group
                                     res
                                   }, 
                                   by = .(scen_var_hor)
]

