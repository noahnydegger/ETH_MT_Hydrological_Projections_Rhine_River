library(data.table)

check_last_dates <- function(dt) {
  # Get the name of the DT (as string)
  dt_name <- deparse(substitute(dt))
  
  # Ensure 'date' is Date type
  if (!inherits(dt$date, "Date")) {
    dt[, date := as.Date(date)]
  }
  
  # Get the latest date per (basin, scenario, ensemble)
  dt_last <- dt[, .(last_date = date[which.max(date)]), by = .(basin, scenario, variant, horizon, member)]
  
  # Extract horizon year safely
  dt_last[, horizon_fixed := {
    # Try to convert to integer
    h_num <- suppressWarnings(as.integer(horizon))
    ifelse(is.na(h_num), 2005L, h_num)
  }]
  
  # Compute threshold date: 31-Dec-(horizon + 15)
  dt_last[, threshold_date := as.Date(paste0(horizon_fixed + 15, "-12-31"))]
  
  # Identify problematic rows
  dt_last[, too_early := last_date < threshold_date]
  
  # Really print one message per row (new line per entry)
  bad_rows <- dt_last[too_early == TRUE]
  message(dt_name)
  for (i in seq_len(nrow(bad_rows))) {
    row <- bad_rows[i]
    
    # Extract variant: d for dry (Ld_, Md_, Hd_), n for wet (Ln_, Mn_, Hn_), empty otherwise
    variant <- ""
    if (grepl("dry", row$variant)) {
      variant <- "d"
    } else if (grepl("wet", row$variant)) {
      variant <- "n"
    }
    
    # Build the formatted scenario_variant_horizon_ensemble string
    scenario_variant <- paste0(row$scenario, variant, "_", row$horizon, "_ens", row$member)
    
    # Print one message per row
    message(sprintf("%s, %s, last date: %s", 
                    row$basin, scenario_variant, format(row$last_date)))
  }
  
  # Return the resulting dt_last
  return(dt_last)
}

dt_last_date <- check_last_dates(knmi_meteo_stat_dt)