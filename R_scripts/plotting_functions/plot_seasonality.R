library(data.table)
library(zoo)
library(hydroGOF)
library(grid)
library(patchwork)


source(here("R_scripts", "data_processing_functions", "gof_metrics.R"))
source(here("R_scripts", "plotting_functions", "knmi_plot_metadata.R"))

compute_rolling_stats_old <- function(dt, group_cols, value_cols, stat = "mean", width = 30) {
  # Create a copy of the data to avoid modifying the original
  dt <- copy(dt)
  
  group_cols <- c(group_cols, "member")
  
  # Compute rolling statistic for each column in value_cols
  dt[, paste0("rm_", value_cols) := lapply(.SD, function(x) zoo::rollapply(
    x, 
    width = width, 
    FUN = match.fun(stat),  # Flexible statistics (mean, min, max)
    fill = NA, 
    align = "center", 
    partial = TRUE,
    na.rm = TRUE
  )), by = group_cols, .SDcols = value_cols]
  
  # Add DayOfYear column and filter out Day 366
  dt <- dt[as.numeric(format(date, "%j")) != 366]
  dt[, DayOfYear := as.numeric(format(date, "%j"))]
  
  return(dt)
}

compute_rolling_stats <- function(dt, group_cols, value_cols, stat = "mean", width = 30) {
  stopifnot(stat %in% c("mean"))  # Only "mean" is supported in this optimized version
  
  group_cols <- c(group_cols, "member")
  # Keep date and group_cols for the new result table
  keep_cols <- unique(c("date", group_cols))
  result_dt <- unique(dt[, ..keep_cols])  # only one row per date-group
  result_dt <- copy(dt)
  
  compute_frollmean_partial <- function(x, n = width) {
    len <- length(x)
    half <- floor(n / 2)
    result <- frollmean(x, n = n, align = "center", fill = NA, na.rm = TRUE)
    
    # Fill start
    for (i in seq_len(half)) {
      result[i] <- mean(x[1:(i + half)], na.rm = TRUE)
    }
    
    # Fill end
    for (i in (len - half + 1):len) {
      result[i] <- mean(x[(i - half):len], na.rm = TRUE)
    }
    
    return(result)
  }
  
  for (val_col in value_cols) {
    new_col <- paste0("rm_", val_col)
    result_dt[, (new_col) := compute_frollmean_partial(get(val_col), n = width), by = group_cols]
  }
  
  result_dt <- result_dt[as.numeric(format(date, "%j")) != 366]
  result_dt[, DayOfYear := as.numeric(format(date, "%j"))]
  
  return(result_dt)
}

# Function to compute mean of selected columns grouped by specified columns
compute_seasonality <- function(dt, group_cols, value_cols, stat = "mean", q_bot = 0.25, q_top = 0.75) {
  # Ensure required columns are present
  if (!all(c(group_cols, value_cols) %in% names(dt))) {
    stop("Some specified columns are not in the data.table")
  }
  
  # Compute specified statistics
  dt[, c(
    setNames(lapply(.SD, match.fun(stat), na.rm = TRUE), paste0(stat, "_", value_cols)),
    setNames(lapply(.SD, quantile, probs = q_bot, na.rm = TRUE), paste0("q_bot_", value_cols)),
    setNames(lapply(.SD, quantile, probs = q_top, na.rm = TRUE), paste0("q_top_", value_cols))
  ), 
  by = group_cols, 
  .SDcols = value_cols]
}

# Function to plot the statistics
plot_seasonality_ts <- function(
    dt, plot_dir, bsn, 
    info_col, 
    color_col, color_col_levels, 
    line_col, line_col_levels, 
    value_col, 
    comparison_col, comparison_ref, 
    stat, info_text, 
    q_bot = 0.25, q_top = 0.75, show_ensemble = FALSE, show_range = FALSE, gof_pairs = NULL,
    save_p = TRUE) {
  
  value_name <- plot_info$column_info$names[[info_col]]
  value_unit <- plot_info$column_info$units[[info_col]]
  
  stat_col <- paste0(stat, "_", value_col)
  q_bot_col <- paste0("q_bot_", value_col)
  q_top_col <- paste0("q_top_", value_col)
  
  # Ensure color column has defined factor levels
  dt[, (color_col) := factor(get(color_col), levels = color_col_levels)]
  dt[, (line_col) := factor(get(line_col), levels = line_col_levels)]
  
  if (!is.null(gof_pairs)) {

    obs_data <- dt[scen_var_hor == gof_pairs[1] & basin == bsn, .(get(stat_col))]
    sim_data <- dt[scen_var_hor == gof_pairs[2] & basin == bsn, .(get(stat_col))]
    obs_name <- plot_info[[color_col]]$labels[gof_pairs[1]]
    sim_name <- plot_info[[color_col]]$labels[gof_pairs[2]]
    # Ensure both datasets have equal length
    if (nrow(obs_data) == nrow(sim_data) && nrow(obs_data) > 0) {
      subtitle_text <- paste0(
        sim_name, " vs ", obs_name, ", NSE = ", round(NSE(sim_data, obs_data), 2), " | ",
        "KGE", " = ", round(KGE(sim_data, obs_data), 2), " | ",
        "ME", " = ", round(me(sim_data, obs_data), 2)
      )
    } else {
      subtitle_text <- NULL
    }
  } else {
    subtitle_text <- NULL
  }

  # Add month lines and labels
  month_lines <- seq(as.Date("2023-01-01"), as.Date("2023-12-01"), by = "1 month")
  month_labels <- seq(as.Date("2023-01-15"), as.Date("2023-12-15"), by = "1 month")

  # Plot
  p <- ggplot(dt, aes(x = as.Date(DayOfYear - 1, origin = "2023-01-01"),
                      group = .data[[color_col]], 
                      color = .data[[color_col]], 
                      linetype = .data[[line_col]])) +
    geom_vline(xintercept = as.numeric(month_lines), color = "gray90")

  if (show_range) {
    p <- p + geom_ribbon(
      data = dt[get(comparison_col) == comparison_ref],
      aes(
        x = as.Date(DayOfYear - 1, origin = "2023-01-01"),
        ymin = .data[[q_bot_col]],
        ymax = .data[[q_top_col]],
        fill = .data[[color_col]]
      ),
      alpha = 0.4,
      inherit.aes = FALSE
    )
  }

  if (show_ensemble) {
    p <- p + geom_line(aes(y = .data[[stat_col]], color = .data[[color_col]]), linewidth = 0.7)
  }

  p <- p + geom_line(
    aes(
      y = .data[[stat_col]],
      color = .data[[color_col]],
      linetype = .data[[line_col]],
      group = interaction(.data[[color_col]], .data[[line_col]])
    ),
    linewidth = 2
  ) +
    scale_x_date(date_labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"), breaks = month_labels, expand = c(0, 0)) +
    labs(
      title = paste("30-day Moving Average", value_name, bsn, info_text),
      subtitle = subtitle_text,
      x = "Month",
      y = paste(value_name, value_unit),
      color = "Scenario",
      linetype = "Variant",
      fill = paste0(q_bot * 100, "-", q_top * 100, " % Quantile")
    ) +
    custom_theme() +
    theme(
      legend.key.width = unit(2, "cm"),  # Adjust to your liking (default ~1.2cm)
      axis.title.x = element_blank()  # Remove the x-axis title
    ) +
    scale_color_manual(
      values = plot_info[[color_col]]$colors,
      labels = plot_info[[color_col]]$labels
    ) +
    scale_linetype_manual(
      values = plot_info[[line_col]]$linetypes,
      labels = plot_info[[line_col]]$labels
    ) +
    guides(
      color = guide_legend(title.position = "top", nrow = 1),
      linetype = guide_legend(title.position = "top", nrow = 1)
    ) +
    ylim(450, 1450) +
    (if (show_range) scale_fill_manual(
      values = plot_info[[color_col]]$colors,
      labels = plot_info[[color_col]]$labels
    ) else NULL)
    

  if (save_p) {
    # Save the plot
    save_dir <- file.path(plot_dir, "seasonality", info_col)
    filename <- paste0("seasonality_ts_", bsn, "_", stat, "_", value_col, ifelse(show_ensemble,"ens", ""), ifelse(show_range, paste0("_Q", q_bot*100, "_Q", q_top*100), ""), info_text, ".pdf")
    save_plot(p, save_dir, filename, width = 18, height = 6)
  } else {
    return(p)
  }
  
}

combine_seasonality_horizon_plots <- function(plot_list, plot_dir, bsn, stat, value_col, info_text) {
  combined_plot <- wrap_plots(plot_list, ncol = 2, guides = "collect") &
    theme(legend.position = "bottom")
  
  # Dynamic height
  n_horizons <- length(plot_list)
  base_height <- 2
  total_height <- n_horizons * base_height
  
  # Save
  save_dir <- file.path(plot_dir, "seasonality_horizon", info_col)
  filename <- paste0("seasonality_ts_horizon_", bsn, "_", stat, "_", value_col, info_text, ".pdf")
  
  save_plot(combined_plot, save_dir, filename, width = 18, height = total_height)
}

plot_seasonality_bars <- function(dt, plot_dir, bsn, info_col, color_col, value_col, stat, info_text) {
  
  value_name <- plot_info$column_info$names[[info_col]]
  value_unit <- plot_info$column_info$units[[info_col]]
  
  stat_col <- paste0(stat, "_", value_col)
  
  # Extract month from DayOfYear (assumes 2023 calendar for mapping)
  dt[, month := format(as.Date(DayOfYear - 1, origin = "2023-01-01"), "%b")]
  dt[, month := factor(month, levels = format(seq(as.Date("2023-01-01"), by = "1 month", length.out = 12), "%b"))]
  
  # Plot
  p <- ggplot(dt, aes(
    x = month,
    y = .data[[stat_col]],
    fill = .data[[color_col]],
    group = .data[[color_col]]
  )) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    labs(
      title = paste("Monthly", stat, value_name, "in", bsn, info_text),
      x = "Month",
      y = paste(value_name, value_unit),
      fill = "Dataset"
    ) +
    custom_theme() +
    scale_fill_manual(
      values = plot_info[[color_col]]$colors,
      labels = plot_info[[color_col]]$labels
    ) +
    theme(
      axis.title.x = element_blank()
    )
  
  # Save the plot
  save_dir <- file.path(plot_dir, "seasonality", info_col)
  filename <- paste0("seasonality_bar_", bsn, "_", stat, "_", value_col, info_text, ".pdf")
  save_plot(p, save_dir, filename, width = 14, height = 6)
}





