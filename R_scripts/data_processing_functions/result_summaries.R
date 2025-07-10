
compute_summary <- function(dt, value_col, group_cols, comparison_col, comparison_ref) {
  
  # Step 1: Compute summary stats per group
  summary_dt <- dt[, .(
    min  = min(get(value_col), na.rm = TRUE),
    q25  = quantile(get(value_col), 0.25, na.rm = TRUE),
    q50  = quantile(get(value_col), 0.50, na.rm = TRUE),
    q75  = quantile(get(value_col), 0.75, na.rm = TRUE),
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
  
  stat_names <- c("min", "q25", "q50", "q75", "max", "mean", "IQR", "range")
  
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
  library(data.table)
  
  # Ensure data is a data.table
  dt <- as.data.table(dt)
  
  # Identify comparison group columns (excluding run_type)
  compare_cols <- setdiff(group_cols, "run_type")
  
  # Split by those columns (e.g., basin)
  result_list <- dt[, {
    dt_wide <- dcast(.SD, formula = as.formula(paste(time_col, "~ run_type")), value.var = value_col)
    
    # Identify run types to compare
    run_types <- setdiff(names(dt_wide), c(time_col, reference))
    
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
    
    raw_dt <- data.table(
      run_type = run_types,
      NSE = sapply(run_types, function(rt) nse(dt_wide[[rt]], dt_wide[[reference]])),
      KGE = sapply(run_types, function(rt) kge(dt_wide[[rt]], dt_wide[[reference]])),
      RMSE = sapply(run_types, function(rt) rmse(dt_wide[[rt]], dt_wide[[reference]]))
      
    )
    
    # Round values before returning
    raw_dt[, `:=`(
      NSE = round(NSE, 3),
      KGE = round(KGE, 3),
      RMSE = round(RMSE, 1)
    )]
    
    raw_dt
  }, by = compare_cols]
  
  result_list[, run_type := factor(run_type, levels = c("hindcast", "future_V1", "no_sund_bc"))]
  setorder(result_list, basin, run_type)
  
  return(result_list)
}

metrics <- compute_nse_rmse_grouped(
  dt = seasonality_dt,
  group_cols = c("basin", "run_type"),
  value_col = "mean_rm_discharge",
  time_col = "DayOfYear",
  reference = "observation"
)
print(metrics)

summary_dt <- compute_summary(dt_annual, 
                value_col = "discharge_mean", 
                group_cols = c("basin", "run_type"), 
                comparison_col = "run_type", 
                comparison_ref = "observation")
