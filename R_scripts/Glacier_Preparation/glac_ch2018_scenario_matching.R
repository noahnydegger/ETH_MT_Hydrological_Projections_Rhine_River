# Load libraries
library(data.table)
library(here)

# project directory
home_dir <- file.path(here::here())

ch2018_meteo_dir <- file.path(home_dir, "Data", "Rheinblick2027", "ch2018", "ch2018_meteo.rds")
knmi_meteo_dir <- file.path(home_dir, "Data", "Rheinblick2027", "processed_prevah_output", "prevah_meteo_stat_knmi.rds")

output_dir <- file.path(home_dir, "hyperion")

# Load only if not already in global environment
if (!exists("ch2018_meteo_dt", envir = .GlobalEnv)) {
  ch2018_meteo_dt <- as.data.table(readRDS(ch2018_meteo_dir))
}

if (!exists("knmi_meteo_stat_dt", envir = .GlobalEnv)) {
  knmi_meteo_stat_dt <- as.data.table(readRDS(knmi_meteo_dir))
}

# functions ----------------------------------------------------------------

compute_delta_n_years <- function(values, years, start_year, end_year, n_years = 5) {
  early_mean <- mean(values[years >= start_year & years < start_year + n_years], na.rm = TRUE)
  late_mean  <- mean(values[years > end_year - n_years & years <= end_year], na.rm = TRUE)
  late_mean - early_mean
}

# compute KNMI mean tair_avg for each (scenario, variant, horizon) over 30-year periods
horizon_summary_knmi <- function(knmi_dt, bsn = "ThS200") {
  # Filter for specified basin
  knmi_dt <- knmi_dt[basin == bsn]
  
  # Keep only rows where horizon is numeric
  knmi_dt <- knmi_dt[suppressWarnings(!is.na(as.numeric(horizon)))]
  knmi_dt[, horizon := as.integer(horizon)]
  
  # Exclude 2150
  knmi_dt <- knmi_dt[horizon != 2150]
  
  knmi_dt[, date := as.Date(date)]
  knmi_dt[, year := year(date)]
  
  # Calculate start and end years directly on knmi_dt
  knmi_dt[, start_year := horizon - 14]
  knmi_dt[, end_year := pmin(horizon + 15, 2099)]
  
  # Filter rows within their horizon's 30-year window
  knmi_dt <- knmi_dt[year >= start_year & year <= end_year]
  
  # Calculate mean tair_avg for each group
  n_years <- 5  # change to 10, etc. if needed
  
  knmi_avg_dt <- knmi_dt[,
                         {
                           start_y <- start_year[1]
                           end_y <- end_year[1]
                           
                           list(
                             start_year = start_y,
                             end_year = end_y,
                             tair_avg_knmi = mean(tair_avg, na.rm = TRUE),
                             radg_avg_knmi = mean(radg_avg, na.rm = TRUE),
                             prec_avg_knmi = mean(prec_avg, na.rm = TRUE),
                             
                             tair_trend_knmi = compute_delta_n_years(tair_avg, year, start_y, end_y, n_years),
                             radg_trend_knmi = compute_delta_n_years(radg_avg, year, start_y, end_y, n_years),
                             prec_trend_knmi = compute_delta_n_years(prec_avg, year, start_y, end_y, n_years)
                           )
                         },
                         by = .(basin, scenario, variant, member, horizon)
  ]
  
  return(knmi_avg_dt)
}

# Step 2: compute CH2018 mean tair_avg per chain for the same time spans as KNMI
horizon_summary_ch2018 <- function(ch2018_dt, knmi_avg_dt, bsn = "ThS200") {
  ch2018_dt <- ch2018_dt[basin == bsn]
  ch2018_dt[, date := as.Date(date)]
  ch2018_dt[, year := year(date)]
  
  # Get unique start–end year periods from KNMI
  periods <- unique(knmi_avg_dt[, .(horizon, start_year, end_year)])
  
  # Apply per period
  n_years <- 5  # configurable window size
  
  period_avg_list <- lapply(seq_len(nrow(periods)), function(i) {
    hor <- periods$horizon[i]
    sy <- periods$start_year[i]
    ey <- periods$end_year[i]
    
    ch2018_dt[year >= sy & year <= ey,
              .(
                horizon = hor,
                start_year = sy,
                end_year = ey,
                tair_avg_ch2018 = mean(tair_avg, na.rm = TRUE),
                radg_avg_ch2018 = mean(radg_avg, na.rm = TRUE),
                prec_avg_ch2018 = mean(prec_avg, na.rm = TRUE),
                
                tair_trend_ch2018 = compute_delta_n_years(tair_avg, year, sy, ey, n_years),
                radg_trend_ch2018 = compute_delta_n_years(radg_avg, year, sy, ey, n_years),
                prec_trend_ch2018 = compute_delta_n_years(prec_avg, year, sy, ey, n_years)
              ),
              by = .(basin, chain, glchain)
    ]
  })
  
  # Combine all period-wise summaries
  ch2018_avg_dt <- rbindlist(period_avg_list)
  
  # Add RCP column
  ch2018_avg_dt[, RCP := regmatches(chain, regexpr("RCP[0-9]{2}", chain))]
  
  # Add scenario mapping
  rcp_map <- c("RCP26" = "L", "RCP45" = "M", "RCP85" = "H")
  ch2018_avg_dt[, scenario_match := rcp_map[RCP]]
  
  return(ch2018_avg_dt)
}

# Step 3: combine period average data from KNMI and CH2018
combine_avg_data <- function(knmi_avg_dt, ch2018_avg_dt) {
  # Perform merge
  comparison_dt <- merge(
    knmi_avg_dt,
    ch2018_avg_dt[, .(
      basin, horizon, chain, glchain,
      tair_avg_ch2018, radg_avg_ch2018, prec_avg_ch2018,
      tair_trend_ch2018, radg_trend_ch2018, prec_trend_ch2018,
      RCP, scenario_match
    )],
    by = c("basin", "horizon"),
    allow.cartesian = TRUE
  )
  
  # Add short variant code
  comparison_dt[, variant_code := fifelse(variant == "dry", "d",
                                          fifelse(variant == "wet", "n", ""))]
  
  # Create scenario_id (e.g. Md_2050_ens1)
  comparison_dt[, scenario_id := paste0(
    scenario, variant_code, "_", horizon, "_ens", member
  )]
  
  # Clean up
  comparison_dt[, variant_code := NULL]
  
  return(comparison_dt)
}

# Step 4: compute absolute and relative differences and an overall score
compute_differences_and_score <- function(comparison_dt,
                                          weight_tair = 0.8,
                                          weight_radg = 0.2,
                                          weight_prec = 0.0) {
  # Absolute differences
  comparison_dt[, tair_avg_diff := tair_avg_knmi - tair_avg_ch2018]
  comparison_dt[, radg_avg_diff := radg_avg_knmi - radg_avg_ch2018]
  comparison_dt[, prec_avg_diff := prec_avg_knmi - prec_avg_ch2018]
  
  comparison_dt[, tair_trend_diff := tair_trend_knmi - tair_trend_ch2018]
  comparison_dt[, radg_trend_diff := radg_trend_knmi - radg_trend_ch2018]
  comparison_dt[, prec_trend_diff := prec_trend_knmi - prec_trend_ch2018]
  
  # Relative differences (averages)
  comparison_dt[, tair_avg_diff_rel := tair_avg_diff / tair_avg_knmi]
  comparison_dt[, radg_avg_diff_rel := radg_avg_diff / radg_avg_knmi]
  comparison_dt[, prec_avg_diff_rel := prec_avg_diff / prec_avg_knmi]
  
  # Relative differences (trends)
  comparison_dt[, tair_trend_diff_rel := tair_trend_diff / tair_trend_knmi]
  comparison_dt[, radg_trend_diff_rel := radg_trend_diff / radg_trend_knmi]
  comparison_dt[, prec_trend_diff_rel := prec_trend_diff / prec_trend_knmi]
  
  # General score: mean of rel avg + rel trend per variable
  comparison_dt[, score := (
    weight_tair * (abs(tair_avg_diff_rel)) + 
      weight_radg * (abs(radg_avg_diff_rel)) + 
      weight_prec * (abs(prec_avg_diff_rel)) 
  ) / 2]
  
  return(comparison_dt)
}


# Step 5: get best-matching chain for each scenario/variant/horizon by score
get_best_matches <- function(comparison_dt, match_scenario = FALSE) {
  dt <- if (match_scenario) {
    comparison_dt[scenario == scenario_match]
  } else {
    comparison_dt
  }
  
  dt[, .SD[which.min(score)], by = .(scenario, variant, member, horizon)]
}

compute_annual_means <- function(dt, start_year = 1990, end_year = 2100, n_years = 5) {
  dt[, date := as.Date(date)]
  dt[, year := year(date)]
  
  # Filter to time window
  dt <- dt[year >= start_year & year <= end_year]
  
  # Extract RCP from chain name
  dt[, RCP := regmatches(chain, regexpr("RCP[0-9]{2}", chain))]
  
  # Step 1: Compute annual mean per chain
  annual_dt <- dt[, .(tair_ann = mean(tair_avg, na.rm = TRUE)), by = .(RCP, chain, glchain, year)]
  
  # Step 2: Compute n-year moving average per chain
  annual_dt[, tair_smooth := zoo::rollapply(
    tair_ann,
    width = n_years,
    FUN = mean,
    fill = NA,
    align = "center",
    partial = TRUE,
    na.rm = TRUE
  ), by = .(RCP, chain)]
  
  return(annual_dt)
}

select_mean_chain_by_rcp <- function(annual_dt) {
  # Step 3: Reshape to wide format
  wide_dt <- dcast(annual_dt, year + RCP ~ chain, value.var = "tair_smooth")
  
  # Get all chain columns (excluding 'year' and 'RCP')
  all_chain_cols <- setdiff(names(wide_dt), c("year", "RCP"))
  
  # Step 4a: Compute distances to ensemble mean per RCP
  distance_table <- wide_dt[, {
    mat <- as.matrix(.SD)
    valid <- colSums(!is.na(mat)) > 0
    mat <- mat[, valid, drop = FALSE]
    
    if (ncol(mat) < 2) return(NULL)
    
    ens_mean <- rowMeans(mat, na.rm = TRUE)
    dists_sum <- apply(mat, 2, function(col) sum((col - ens_mean)^2, na.rm = TRUE))
    dists_rmse <- apply(mat, 2, function(col) sqrt(mean((col - ens_mean)^2, na.rm = TRUE)))
    
    data.table(chain = colnames(mat), dist_sum = dists_sum, dist_rmse = dists_rmse)
  }, by = RCP, .SDcols = all_chain_cols]
  
  # Step 4b: Select closest chain per RCP
  selected_chains <- distance_table[, .SD[which.min(dist_rmse)], by = RCP]
  
  # Step 4c: Merge glchain from original annual_dt
  chain_glchain_map <- unique(annual_dt[, .(RCP, chain, glchain)])
  selected_chains <- merge(selected_chains, chain_glchain_map, by = c("RCP", "chain"), all.x = TRUE)
  
  
  return(selected_chains)
}

# export glchain as text file
write_best_glchains_to_file <- function(best_match_dt, mean_chain_dt, mean_chain_ref_dt, output_dir, file_name) {
  # Create output directories if they don't exist
  dir.create(file.path(output_dir, "scenario_ens_glchain"), recursive = TRUE, showWarnings = FALSE)
  
  # Define scenario-horizon groups
  scenario_groups <- list(
    "# High" = c("Hd_2050", "Hd_2100", "Hd_2150", "Hn_2050", "Hn_2100", "Hn_2150"),
    "# Low" = c("L_2033", "Ld_2100", "Ln_2100"),
    "# Moderate" = c("Md_2050", "Md_2100", "Md_2150", "Mn_2050", "Mn_2100", "Mn_2150"),
    "# reference" = c("reference")
  )
  
  # For the full list file
  lines <- c("glchain=(")
  
  # Loop through all scenario groups
  for (group in names(scenario_groups)) {
    lines <- c(lines, group)
    
    for (scenario in scenario_groups[[group]]) {
      lines <- c(lines, paste("#", scenario))
      
      if (scenario == "reference") {
        # Special handling for reference scenario
        gl <- mean_chain_ref_dt[RCP == "RCP26", glchain][1]
        
        for (ens in paste0("ens", 1:8)) {
          # Write single file for each scenario_ens
          filename <- file.path(output_dir, "scenario_ens_glchain", paste0(scenario, "_", ens, ".txt"))
          writeLines(gl, filename)
          
          # Add to the big glchain list
          lines <- c(lines, gl)
        }
        
      } else if (grepl("2150$", scenario)) {
        # Special handling for 2150 horizon scenarios
        rcp <- if (grepl("^M", scenario)) "RCP45" else if (grepl("^H", scenario)) "RCP85" else NA_character_
        gl <- mean_chain_dt[RCP == rcp, glchain][1]  # use the first match
        
        for (ens in paste0("ens", 1:8)) {
          # Write single file for each scenario_ens
          filename <- file.path(output_dir, "scenario_ens_glchain", paste0(scenario, "_", ens, ".txt"))
          writeLines(gl, filename)
          
          # Add to the big glchain list
          lines <- c(lines, gl)
        }
        
      } else {
        # Normal handling for other scenarios
        for (ens in paste0("ens", 1:8)) {
          id <- paste0(scenario, "_", ens)
          gl <- best_match_dt[scenario_id == id, glchain]
          
          if (length(gl) == 1 && !is.na(gl)) {
            # Write single file
            filename <- file.path(output_dir, "scenario_ens_glchain", paste0(scenario, "_", ens, ".txt"))
            writeLines(gl, filename)
            
            # Add to the big glchain list
            lines <- c(lines, gl)
          } else {
            cat("Missing glchain for: ", id, "\n")
          }
        }
      }
    }
    lines <- c(lines, "")  # Spacer between groups
  }
  
  lines <- c(lines, ")")
  
  # Write the full glchain list to the main file
  file_path <- file.path(output_dir, file_name)
  writeLines(lines, file_path)
  message("Written full glchain list to: ", file_path)
  message("Written all individual scenario_ens glchain files to: ", file.path(output_dir, "scenario_ens_glchain/"))
}

plot_rcp_chains <- function(annual_dt, mean_chain_dt, rcp_name, value_col = "tair_smooth", info_text = "") {
  source(here("R_scripts", "plotting_functions", "knmi_plot_metadata.R"))
  # Filter annual data for the given RCP
  rcp_data <- annual_dt[RCP == rcp_name]
  
  # Compute MaxTemp per chain for color mapping
  rcp_data[, MaxTemp := max(get(value_col), na.rm = TRUE), by = chain]
  
  # Get the selected chain for this RCP
  selected_chain <- mean_chain_dt[RCP == rcp_name, chain]
  selected_glchain <- mean_chain_dt[RCP == rcp_name, glchain]
  
  # Plot
  p <- ggplot(rcp_data, aes(x = year, y = get(value_col), group = chain, color = MaxTemp)) +
    geom_line(linewidth = 0.5) +  # Plot all chains
    geom_line(data = rcp_data[chain == selected_chain], aes(x = year, y = get(value_col)),
              color = "black", linewidth = 1.2) +  # Highlight selected chain
    scale_color_gradient(low = "lightblue", high = "darkred") +
    labs(
      title = paste("Temperature", info_text, "window for Chains in", rcp_name),
      subtitle = paste("Selected chain / glchain:", selected_chain, "/", selected_glchain),
      x = "Year",
      y = "Temperature (°C)",
      color = "Max Temperature"
    ) +
    custom_theme() +
    theme(
      legend.position = "top",
      plot.title = element_text(hjust = 0.5)
    )
  
  # Save the plot
  save_dir <- file.path(here::here(), "Plots", "glac_ch2018", "chain_selection")
  filename <- paste0("ts_", value_col, "_", info_text, "_", rcp_name, ".pdf")
  save_plot(p, save_dir, filename, width = 12, height = 6)
}

# code -------------------------------------------------------

# Compute summaries
knmi_avg_dt <- horizon_summary_knmi(knmi_meteo_stat_dt)
ch2018_avg_dt <- horizon_summary_ch2018(ch2018_meteo_dt, knmi_avg_dt)

# Compare all combinations
comparison_dt <- combine_avg_data(knmi_avg_dt, ch2018_avg_dt)
comparison_dt <- compute_differences_and_score(comparison_dt)

# Get best match per scenario/variant/horizon
best_match_overall_dt <- get_best_matches(comparison_dt)
best_match_scenario_dt <- get_best_matches(comparison_dt, match_scenario = TRUE)

# compute annual means
annual_5yr_dt <- compute_annual_means(
  ch2018_meteo_dt,
  start_year = 1990,
  end_year = 2100,
  n_years = 5
)

annual_10yr_dt <- compute_annual_means(
  ch2018_meteo_dt,
  start_year = 1990,
  end_year = 2100,
  n_years = 10
)

annual_10yr_ref_dt <- compute_annual_means(
  ch2018_meteo_dt,
  start_year = 1990,
  end_year = 2020,
  n_years = 10
)

# Select median chain by RCP for horizon 2150
mean_chain_5yr_dt <- select_mean_chain_by_rcp(annual_5yr_dt)
mean_chain_10yr_dt <- select_mean_chain_by_rcp(annual_10yr_dt)
mean_chain_10yr_ref_dt <- select_mean_chain_by_rcp(annual_10yr_ref_dt)

for (rcp in unique(annual_5yr_dt$RCP)) {
  plot_rcp_chains(
    annual_5yr_dt,
    mean_chain_5yr_dt,
    rcp_name = rcp,
    value_col = "tair_smooth",
    info_text = "5yr_rmse"
  )
  plot_rcp_chains(
    annual_10yr_dt,
    mean_chain_10yr_dt,
    rcp_name = rcp,
    value_col = "tair_smooth",
    info_text = "10yr_rmse"
  )
  
  plot_rcp_chains(
    annual_10yr_ref_dt,
    mean_chain_10yr_ref_dt,
    rcp_name = rcp,
    value_col = "tair_smooth",
    info_text = "10yr_rmse_ref"
  )
}

write_best_glchains_to_file(
  best_match_overall_dt,
  mean_chain_10yr_dt,
  mean_chain_10yr_ref_dt,
  output_dir,
  "glchains_overall.txt"
)

write_best_glchains_to_file(
  best_match_scenario_dt,
  mean_chain_10yr_dt,
  mean_chain_10yr_ref_dt,
  output_dir,
  "glchains_scenario.txt"
)
