
compute_summary <- function(dt, value_col, group_cols, comparison_col, comparison_ref) {
  
  # Step 1: Compute summary stats per group
  summary_dt <- dt[, .(
    min  = min(get(value_col), na.rm = TRUE),
    q10  = quantile(get(value_col), 0.10, na.rm = TRUE),
    q25  = quantile(get(value_col), 0.25, na.rm = TRUE),
    q50  = quantile(get(value_col), 0.50, na.rm = TRUE),
    q75  = quantile(get(value_col), 0.75, na.rm = TRUE),
    q90  = quantile(get(value_col), 0.90, na.rm = TRUE),
    max  = max(get(value_col), na.rm = TRUE),
    mean = mean(get(value_col), na.rm = TRUE)
  ), by = group_cols]
  
  # Add IQR column
  summary_dt[, IQR := q75 - q25]
  summary_dt[, range := max - min]
  
  # Step 2: Merge with reference values (e.g., run_type == "observation")
  ref_dt <- summary_dt[get(comparison_col) == comparison_ref]
  setnames(ref_dt, setdiff(names(ref_dt), comparison_col), paste0(setdiff(names(ref_dt), comparison_col), "_ref"))
  
  # Create join keys
  join_cols <- setdiff(group_cols, comparison_col)
  setkeyv(summary_dt, join_cols)
  setkeyv(ref_dt, paste0(join_cols, "_ref"))
  
  # Step 3: Join and calculate differences
  summary_dt <- summary_dt[ref_dt, on = setNames(paste0(join_cols, "_ref"), join_cols)]
  
  stat_names <- c("min", "q10", "q25", "q50", "q75", "q90", "max", "mean", "IQR", "range")
  
  for (stat in stat_names) {
    summary_dt[, paste0(stat, "_abs") := get(stat) - get(paste0(stat, "_ref"))]
    summary_dt[, paste0(stat, "_rel") := 100 * (get(stat) - get(paste0(stat, "_ref"))) / get(paste0(stat, "_ref"))]
  }
  
  # Optional: reorder
  if ("run_type" %in% names(summary_dt)) {
    summary_dt[, run_type := factor(run_type, levels = c("observation", "hindcast", "future_V1", "no_sund_bc"))]
    setorder(summary_dt, basin, run_type)
  }
  
  return(summary_dt)
}

compute_nse_rmse_grouped <- function(dt, group_cols, value_col = "mean_rm_discharge", time_col = "DayOfYear", reference = "observation") {
  
  # Identify comparison group columns (excluding run_type)
  compare_cols <<- setdiff(group_cols, "run_type")
  
  # Split by those columns (e.g., basin)
  result_list <- dt[, {
    dt_wide <<- dcast(.SD, formula = as.formula(paste(time_col, "~ run_type")), value.var = value_col)
    
    # Identify run types to compare
    run_types <<- setdiff(names(dt_wide), c(time_col, reference))
    
    # Define NSE
    nse <- function(sim, obs) {
      1 - sum((sim - obs)^2, na.rm = TRUE) / sum((obs - mean(obs, na.rm = TRUE))^2, na.rm = TRUE)
    }
    
    # Define RMSE
    rmse <- function(sim, obs) {
      sqrt(mean((sim - obs)^2, na.rm = TRUE))
    }
    
    # KGE
    kge <- function(sim, obs) {
      sim_mean <- mean(sim, na.rm = TRUE)
      obs_mean <- mean(obs, na.rm = TRUE)
      r <- cor(sim, obs, use = "complete.obs")
      beta <- sim_mean / obs_mean
      alpha <- sd(sim, na.rm = TRUE) / sd(obs, na.rm = TRUE)
      1 - sqrt((r - 1)^2 + (alpha - 1)^2 + (beta - 1)^2)
    }
    
    raw_dt <<- data.table(
      run_type = run_types,
      NSE = sapply(run_types, function(rt) nse(dt_wide[[rt]], dt_wide[[reference]])),
      KGE = sapply(run_types, function(rt) kge(dt_wide[[rt]], dt_wide[[reference]])),
      RMSE = sapply(run_types, function(rt) rmse(dt_wide[[rt]], dt_wide[[reference]]))
      
    )
    
    # Round values before returning
    raw_dt[, `:=`(
      NSE = round(NSE, 2),
      KGE = round(KGE, 2),
      RMSE = round(RMSE, 1)
    )]
    
    raw_dt
  }, by = compare_cols]
  
  result_list[, run_type := factor(run_type, levels = c("hindcast", "future_V1", "larsim", "wflow_sbm", "no_sund_bc"))]
  setorder(result_list, basin, run_type)
  
  return(result_list)
}

compute_peak_change <- function(dt, value_col, group_cols, comparison_col, comparison_ref, date_col = "DayOfYear") {
  # Get peak (max) value and date for each group
  peak_dt <- dt[, .SD[which.max(get(value_col))], by = group_cols, .SDcols = c(value_col, date_col)]
  
  join_cols <- setdiff(group_cols, comparison_col)
  
  # Extract reference peak
  ref_peak <- peak_dt[get(comparison_col) == comparison_ref]
  ref_peak <- ref_peak[, c(join_cols, value_col, date_col), with = FALSE]
  setnames(ref_peak, c(value_col, date_col), c("value_ref", "doy_ref"))
  
  # Merge with all peaks
  
  result <- merge(peak_dt, ref_peak, by = join_cols, all.x = TRUE)
  
  # Compute differences
  result[, `:=`(
    magnitude_abs_change = get(value_col) - value_ref,
    magnitude_rel_change = 100 * (get(value_col) - value_ref) / value_ref,
    timing_shift_days = get(date_col) - doy_ref
  )]
  
  result <- result[, c(group_cols, value_col, date_col, "magnitude_abs_change", "magnitude_rel_change", "timing_shift_days"), with = FALSE]
  setorderv(result, group_cols)  # simpler and avoids dependency
  return(result)
}

# seasonality NSE, bias metrics
metrics <- compute_nse_rmse_grouped(
  dt = seasonality_dt,
  group_cols = c("basin"),
  value_col = "mean_rm_discharge",
  time_col = "DayOfYear",
  reference = "observation" # "ref_ref_ref"
)
print(metrics)

# seasonality peak changes
peak_changes <- compute_peak_change(
  dt = seasonality_dt,
  value_col = "mean_rm_discharge",
  group_cols = c("basin", "hydro_model"), # , "scen_var_hor"
  comparison_col = "hydro_model", # "scen_var_hor",
  comparison_ref = "observation", # "ref_ref_ref",
  date_col = "DayOfYear"
)

print(peak_changes)

# annual and seasonal mean diff
summary_dt <- compute_summary(dt_subset,
                value_col = "discharge",
                group_cols = c("basin", "hydro_model"),
                comparison_col = "hydro_model",
                comparison_ref = "observation")

# low and high flow days
# dt_final <- dt_dpy[horizon %in% c("ref", "hindcast", "observation")]
# 
# summary_dt <- compute_summary(
#   dt = dt_final,
#   value_col = "days_above_qtop",  # "days_below_qbot", "days_above_qtop"
#   group_cols = c("basin", "season", "run_type"),
#   comparison_col = "run_type",
#   comparison_ref = "observation"
# )

# horizon_sub <- c("2100")
# season_sub <- c("DJF") # "DJF", "MAM", "JJA", "SON"
# 
# # Define desired column order
# desired_cols <- c("basin", "horizon", "scenario", "variant", "season", "mean_diff_rel")
# remaining_cols <- setdiff(names(dt_sub), desired_cols)
# 
# # Reorder columns
# setcolorder(dt_diff, c(desired_cols, remaining_cols))
# 
# dt_sub <- dt_diff[season %in% season_sub]
# setorder(dt_sub, basin, horizon, scenario, variant, season)
# 
# # Compute the wet - dry difference
# # Subset to wet and dry variants
# dt_sub <- dt_diff[variant %in% c("wet", "dry"), .(horizon, scenario, variant, season, mean_diff_rel)]
# dt_sub <- dt_sub[season %in% season_sub]
# 
# # Reshape to wide format
# dt_wide <- dcast(dt_sub, horizon + scenario + season ~ variant, value.var = "mean_diff_rel")
# 
# # Compute difference (wet - dry)
# dt_wide[, diff := round(dry - wet, 1)]
# 
# # Print result rounded to 1 decimal
# dt_wide[, cat(sprintf("%s: %s: %s: %.1f\n", horizon, scenario, season, diff))]
