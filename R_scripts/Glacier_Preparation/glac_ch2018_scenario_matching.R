# Load libraries
library(data.table)
library(here)

# project directory
home_dir <- file.path(here::here())

ch2018_meteo_dir <- file.path(home_dir, "Data", "Rheinblick2027", "ch2018", "ch2018_meteo.rds")
knmi_meteo_dir <- file.path(home_dir, "Data", "Rheinblick2027", "processed_prevah_output", "prevah_meteo_stat_knmi.rds")

# Load only if not already in global environment
if (!exists("ch2018_meteo_dt", envir = .GlobalEnv)) {
  ch2018_meteo_dt <- as.data.table(readRDS(ch2018_meteo_dir))
}

if (!exists("knmi_meteo_stat_dt", envir = .GlobalEnv)) {
  knmi_meteo_stat_dt <- as.data.table(readRDS(knmi_meteo_dir))
}

# functions ----------------------------------------------------------------
# compute KNMI mean tair_avg for each (scenario, variant, horizon) over 30-year periods
horizon_summary_knmi <- function(knmi_dt, bsn = "ThS200") {
  knmi_dt <- knmi_dt[basin == bsn]
  knmi_dt[, date := as.Date(date)]
  knmi_dt[, year := year(date)]
  
  # Calculate start and end years directly on knmi_dt
  knmi_dt[, start_year := horizon - 14]
  knmi_dt[, end_year := pmin(horizon + 15, 2099)]
  
  # Filter rows within their horizon's 30-year window
  knmi_dt <- knmi_dt[year >= start_year & year <= end_year]
  
  # Calculate mean tair_avg for each group
  knmi_avg_dt <- knmi_dt[,
                         .(start_year = start_year[1],
                           end_year = end_year[1],
                           tair_avg = mean(tair_avg, na.rm = TRUE)),
                         by = .(basin, scenario, variant, member, horizon)
  ]
  
  # Remove horizon 2015
  knmi_avg_dt <- knmi_avg_dt[horizon != 2150]
  
  return(knmi_avg_dt)
}

# Step 2: compute CH2018 mean tair_avg per chain for the same time spans as KNMI
horizon_summary_ch2018 <- function(ch2018_dt, knmi_avg_dt, bsn = "ThS200") {
  ch2018_dt <- ch2018_dt[basin == bsn]
  ch2018_dt[, date := as.Date(date)]
  ch2018_dt[, year := year(date)]
  
  chains <- unique(ch2018_dt$chain)
  
  # Create a cross join between chains and knmi horizons
  chain_horizons <- CJ(chain = chains,
                       scenario = knmi_avg_dt$scenario,
                       variant = knmi_avg_dt$variant,
                       horizon = knmi_avg_dt$horizon,
                       member = knmi_avg_dt$member,
                       unique = TRUE)
  
  # Join start and end year info
  chain_horizons <- merge(chain_horizons, knmi_avg_dt[, .(scenario, variant, horizon, member, start_year, end_year)],
                          by = c("scenario", "variant", "horizon"), allow.cartesian = TRUE)
  
  # Join to ch2018_dt
  full_dt <- merge(chain_horizons, ch2018_dt, by = "chain", allow.cartesian = TRUE)
  
  # Filter by year range and calculate means
  ch2018_avg_dt <- full_dt[year >= start_year & year <= end_year,
                       .(start_year = start_year[1],
                         end_year = end_year[1],
                         tair_avg = mean(tair_avg, na.rm = TRUE)),
                       by = .(chain, basin, scenario, variant, member, horizon)]
  
  return(ch2018_avg_dt)
}

# Step 3: combine and calculate difference and a general score column
combine_and_compare <- function(knmi_avg_dt, ch2018_avg_dt, weight_mean_diff = 1.0) {
  # Rename tair_avg in each DT before merge
  setnames(knmi_avg_dt, "tair_avg", "tair_avg_knmi")
  setnames(ch2018_avg_dt, "tair_avg", "tair_avg_ch2018")
  
  # Perform merge
  comparison_dt <- merge(
    knmi_avg_dt,
    ch2018_avg_dt,
    by = c("basin", "scenario", "variant", "horizon", "member", "start_year", "end_year"),
    allow.cartesian = TRUE
  )
  
  # Compute difference and score
  comparison_dt[, diff_mean := abs(tair_avg_knmi - tair_avg_ch2018)]
  comparison_dt[, score := diff_mean * weight_mean_diff]
  
  return(comparison_dt)
}

# Step 4: get best-matching chain for each scenario/variant/horizon by score
get_best_matches <- function(comparison_dt) {
  best_match_dt <- comparison_dt[, .SD[which.min(score)], by = .(scenario, variant, horizon, member)]
  return(best_match_dt)
}

# Step 5: full wrapper function
match_chains_to_knmi <- function(knmi_dt, ch_dt, basin = "ThS200", weight_mean_diff = 1.0) {
  knmi_avg_dt <- horizon_summary_knmi(knmi_dt, basin)
  ch2018_avg_dt <- temp_summary_ch2018(ch_dt, knmi_avg_dt, basin)
  comparison_dt <- combine_and_compare(knmi_avg_dt, ch2018_avg_dt, weight_mean_diff)
  best_match_dt <- get_best_matches(comparison_dt)
  return(list(all_comparisons = comparison_dt, best_matches = best_match_dt))
}


# code -------------------------------------------------------

# Compute summaries
knmi_avg_dt <- horizon_summary_knmi(knmi_meteo_stat_dt)
ch2018_avg_dt <- horizon_summary_ch2018(ch2018_meteo_dt, knmi_avg_dt)

# Compare all combinations
comparison_dt <- combine_and_compare(knmi_avg_dt, ch2018_avg_dt)

# Get best match per scenario/variant/horizon
best_match_dt <- get_best_matches(comparison_dt)
