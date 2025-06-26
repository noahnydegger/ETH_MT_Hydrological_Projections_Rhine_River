library(data.table)
library(zoo)
library(ggplot2)

source(here("R_scripts", "plotting_functions", "knmi_plot_metadata.R"))

# Compute annual or seasonal statistics
compute_annual_or_seasonal <- function(dt, value_col, group_cols, 
                                       statistic = "mean", months = 1:12, 
                                       seasonal = FALSE, monthly = FALSE, half_year = FALSE) {
  dt <- dt[month(date) %in% months]
  dt[, YYYY := year(date)]
  dt[, MM := month(date)]
  
  # Determine grouping
  if (monthly) {
    group_vars <- c("MM", "YYYY", "member", group_cols)
  } else if (seasonal) {
    dt[, season := fifelse(MM %in% c(12,1,2), "DJF",
                           fifelse(MM %in% c(3,4,5), "MAM",
                                   fifelse(MM %in% c(6,7,8), "JJA", "SON")))]
    group_vars <- c("season", "YYYY", "member", group_cols)
  } else if (half_year) {
    dt[, season := fifelse(MM %in% c(5,6,7,8,9,10), "summer", "winter")]
    group_vars <- c("season", "YYYY", "member", group_cols)
  } else {
    group_vars <- c("YYYY", "member", group_cols)
  }
  
  # Statistic computation
  value_name <- paste0(value_col, "_", statistic)
  
  if (statistic == "mean") {
    dt_out <<- dt[, .(value = mean(get(value_col), na.rm = TRUE)), by = group_vars]
  } else if (statistic == "min") {
    dt_out <- dt[, .(value = min(get(value_col), na.rm = TRUE)), by = group_vars]
  } else if (statistic == "max") {
    dt_out <- dt[, .(value = max(get(value_col), na.rm = TRUE)), by = group_vars]
  } else if (statistic == "7day_low") {
    dt[, rollmean7 := rollapply(get(value_col), width = 7, FUN = mean, fill = NA, align = "right"), by = group_vars]
    dt_out <- dt[, .(value = min(rollmean7, na.rm = TRUE)), by = group_vars]
  } else {
    stop("Invalid statistic: ", statistic)
  }
  
  # Rename the computed column
  setnames(dt_out, "value", value_name)
  
  return(dt_out)
}

compute_relative_mean <- function(dt_mean, value_col, comparison_col, comparison_ref, 
                                  seasonal = FALSE, monthly = FALSE) {
  value_col_rel <- paste0(value_col, "_rel")
  
  # Determine grouping column
  group_col <- if (seasonal) {
    if (!"season" %in% names(dt_mean)) stop("Missing 'season' column.")
    "season"
  } else if (monthly) {
    if (!"MM" %in% names(dt_mean)) stop("Missing 'MM' column.")
    "MM"
  } else {
    NULL
  }
  
  # Compute relative deviation
  if (is.null(group_col)) {
    ref_mean <- dt_mean[get(comparison_col) == comparison_ref, median(get(value_col), na.rm = TRUE)]
    dt_mean[, (value_col_rel) := 100 * (get(value_col) - ref_mean) / ref_mean]
  } else {
    dt_mean[, (value_col_rel) := {
      ref <- dt_mean[get(comparison_col) == comparison_ref & get(group_col) == .BY[[1]],
                     median(get(value_col), na.rm = TRUE)]
      100 * (get(value_col) - ref) / ref
    }, by = group_col]
  }
  
  return(dt_mean)
}

compute_member_differences <- function(dt, value_col, group_cols, linking_cols, comparison_col, comparison_ref,
                                       statistic = "mean", seasonal = FALSE, monthly = FALSE, half_year = FALSE) {
  
  # Prepare grouping
  dt[, YYYY := year(date)]
  dt[, MM := month(date)]
  
  if (seasonal) {
    dt[, season := fifelse(MM %in% c(12,1,2), "DJF",
                           fifelse(MM %in% c(3,4,5), "MAM",
                                   fifelse(MM %in% c(6,7,8), "JJA", "SON")))]
    group_vars <- c("season", "member", group_cols)
    linking_vars <- c("season", linking_cols)
  } else if (half_year) {
    dt[, season := fifelse(MM %in% c(5,6,7,8,9,10), "summer", "winter")]
    group_vars <- c("season", "member", group_cols)
    linking_vars <- c("season", linking_cols)
  } else if (monthly) {
    group_vars <- c("MM", "member", group_cols)
    linking_vars <- c("MM", linking_cols)
  } else {
    group_vars <- c("member", group_cols)
    linking_vars <- linking_cols
  }
  
  # Compute value_mean depending on statistic
  if (statistic == "mean") {
    dt_stat <- dt[, .(value_stat = mean(get(value_col), na.rm = TRUE)), by = group_vars]
    
  } else if (statistic == "min") {
    dt_stat <- dt[, .(value_stat = min(get(value_col), na.rm = TRUE)), by = group_vars]
    
  } else if (statistic == "max") {
    dt_stat <- dt[, .(value_stat = max(get(value_col), na.rm = TRUE)), by = group_vars]
    
  } else if (statistic == "sum") {
    dt_stat <- dt[, .(value_stat = sum(get(value_col), na.rm = TRUE)), by = group_vars]
    
  } else if (statistic == "7day_low") {
    dt[, rollmean7 := zoo::rollapply(
      get(value_col), width = 7, FUN = mean, fill = NA, align = "right"), by = group_vars]
    
    dt_stat <- dt[, .(value_stat = min(rollmean7, na.rm = TRUE)), by = group_vars]
    
  } else {
    stop("Invalid statistic: ", statistic)
  }
  
  # Statistic computation
  value_stat <- paste0(value_col, "_", statistic)
  # Rename the computed column
  setnames(dt_stat, "value_stat", value_stat)
  
  # Split target vs reference
  dt_target <- dt_stat[get(comparison_col) != comparison_ref]
  dt_ref    <- dt_stat[get(comparison_col) == comparison_ref]
  
  # Add group label column for merge
  dt_ref[, ref_group := do.call(paste, c(.SD, sep = "_")), .SDcols = linking_vars]
  dt_target[, target_group := do.call(paste, c(.SD, sep = "_")), .SDcols = linking_vars]
  
  # Merge target vs reference on group (cartesian product for member combinations)
  dt_merged <- merge(dt_target, dt_ref, 
                     by.x = "target_group", by.y = "ref_group",
                     allow.cartesian = TRUE,
                     suffixes = c("_target", "_ref"))
  
  # Define column names
  abs_diff_colname <- paste0(value_stat, "_abs_diff")
  rel_diff_colname <- paste0(value_stat, "_rel_diff")
  
  # Compute differences and assign to named columns
  dt_merged[, (abs_diff_colname) := get(paste0(value_stat, "_target")) - get(paste0(value_stat, "_ref"))]
  dt_merged[, (rel_diff_colname) := 100 * (get(paste0(value_stat, "_target")) - get(paste0(value_stat, "_ref"))) / get(paste0(value_stat, "_ref"))]
  
  # Add target_ref_pair column
  dt_merged[, member_pair := paste0(member_target, "_", member_ref)]
  
  # Get target columns and new names
  target_cols <- grep("_target$", names(dt_merged), value = TRUE)
  target_cols <- setdiff(target_cols, c("member_target", "member_ref")) # exclude
  new_target_cols <- sub("_target$", "", target_cols)
  
  # Prepare output
  keep_cols <- c("member_target", "member_ref", "member_pair", "target_group", abs_diff_colname, rel_diff_colname, target_cols)
  out_dt <- dt_merged[, ..keep_cols]
  
  # Rename columns
  setnames(out_dt, c("member_target", "member_ref", "target_group"),
           c("target_member", "ref_member", "group"))
  setnames(out_dt, target_cols, new_target_cols)
  
  return(out_dt)
}

plot_annual_boxplots <- function(dt_annual, plot_dir, bsn, color_col, color_col_levels, comparison_ref, value_col, group_cols, stat, info_text = "", rel = FALSE) {
  
  value_name <- plot_info$column_info$names[[value_col]]
  value_unit <- plot_info$column_info$units[[value_col]]
  
  y_text <- paste(value_name, value_unit)
  
  stat_col <- paste0(value_col, "_", stat)
  
  if (rel) {
    stat_col <- paste0(stat_col, "_rel_diff")
    #info_text <- paste0(info_text, "_rel")
    value_unit <- "[%]"
    y_text <- "Change [%]"
  }
  
  dt_annual[, (color_col) := factor(get(color_col), levels = color_col_levels)]
  
  # Plot annual boxplots
  p <- ggplot(dt_annual, aes(x = .data[[color_col]], y = .data[[stat_col]], fill = .data[[color_col]])) +
    geom_boxplot(position = position_dodge(width = 0.8), fatten = 2, size = 0.8) +  
    labs(
      title = paste("Annual", stat, value_name, bsn, info_text),
      x = "Dataset",
      y = y_text,
      fill = "Dataset"
    )  +
    custom_theme() +
    theme(
      axis.title.x = element_blank()  # Remove the x-axis title
    ) +
    scale_fill_manual(
      values = plot_info[[color_col]]$colors,
      labels = plot_info[[color_col]]$labels
    ) +
    #ylim(0.0, 0.7) +
    scale_x_discrete(labels = NULL)  
  
  # Save the plot
  save_dir <- file.path(plot_dir, "annual_boxplots", value_col)
  filename <- paste0("annual_", bsn, "_", stat_col, info_text, ".pdf")
  save_plot(p, save_dir, filename, width = 8, height = 6)
  
}

plot_annual_horizon_boxplots <- function(dt_annual, plot_dir, bsn, color_col, color_col_levels, comparison_ref, value_col, group_cols, stat, info_text = "", y_lim = NULL, rel = FALSE, abs = FALSE) {
  
  value_name <- plot_info$column_info$names[[value_col]]
  value_unit <- plot_info$column_info$units[[value_col]]
  
  y_text <- paste(value_name, value_unit)
  
  stat_col <- paste0(value_col, "_", stat)
  
  if (rel) {
    stat_col <- paste0(stat_col, "_rel_diff")
    info_text <- paste0(info_text, "change")
    value_unit <- "[%]"
    y_text <- paste("change in", value_name, "[%]")
  }
  
  if (abs) {
    stat_col <- paste0(stat_col, "_abs_diff")
    info_text <- paste0(info_text, " change")
    value_unit <- value_unit
    y_text <- paste("change in", value_name, value_unit)
  }
  
  dt_annual[, (color_col) := factor(get(color_col), levels = color_col_levels)]
  dt_annual[, horizon := factor(horizon, levels = c("ref", "2033", "2050", "2100", "2150"),
                                labels = c("Ref", "2033", "2050", "2100", "2150"))]
  
  ref_q05 <- dt_annual[get(color_col) == comparison_ref, median(.SD[[1]], na.rm = TRUE), .SDcols = stat_col]
  ref_q25 <- dt_annual[get(color_col) == comparison_ref, quantile(.SD[[1]], probs = 0.25, na.rm = TRUE), .SDcols = stat_col]
  ref_q75 <- dt_annual[get(color_col) == comparison_ref, quantile(.SD[[1]], probs = 0.75, na.rm = TRUE), .SDcols = stat_col]
  
  # Plot annual boxplots
  p <- ggplot(dt_annual, aes(x = .data[[color_col]], y = .data[[stat_col]], fill = .data[[color_col]])) +
    # geom_boxplot(position = position_dodge(width = 0.8), fatten = 2, size = 0.8) +
    # geom_vline(xintercept = c(1.5, 2.5, 3.5, 4.5), color = "grey80") +  # <-- vertical lines between groups
    geom_hline(yintercept = ref_q25, linetype = "dashed", color = "grey20") +
    geom_hline(yintercept = ref_q75, linetype = "dashed", color = "grey20") +
    geom_boxplot(width = 0.6, outlier.size = 0.5) +
    facet_grid(~ horizon, scales = "free_x", space = "free_x", switch = "x") +
    labs(
      title = NULL,# paste("Annual", stat, value_name, bsn, info_text),
      x = NULL,
      y = y_text,
      fill = NULL
    )  +
    custom_theme() +
    theme(
      legend.position = "top",
      strip.placement = "outside",
      strip.background = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.spacing = unit(1.0, "lines"),
      #panel.grid.major.y = element_blank(),
      #panel.grid.minor.y = element_blank(),
      legend.direction = "horizontal",
      legend.box = "horizontal"
    ) +
    guides(
      fill = guide_legend(nrow = 1),
      color = guide_legend(nrow = 1)
    ) +
    (if (!is.null(y_lim)) 
      ylim(y_lim) else NULL) +
    scale_fill_manual(
      values = plot_info[[color_col]]$colors,
      labels = plot_info[[color_col]]$labels
    )

  # Save the plot
  save_dir <- file.path(plot_dir, "annual_horizon_boxplots", value_col)
  filename <- paste0("annual_horizon_", bsn, "_", stat_col, info_text, ".pdf")
  save_plot(p, save_dir, filename, width = 12, height = 6)
  
}

plot_annual_horizon_boxplots_combination <- function(dt, plot_dir, bsn, color_col, value_col, stat, y_lim = NULL, rel = FALSE, abs = FALSE,
                                                     title, legend, y_label) {
  
  value_name <- plot_info$column_info$names[[value_col]]
  value_unit <- plot_info$column_info$units[[value_col]]
  
  y_text <- paste(value_name, value_unit)
  
  stat_col <- paste0(value_col, "_", stat)
  
  if (rel) {
    stat_col <- paste0(stat_col, "_rel_diff")
    info_text <- paste0(info_text, "change")
    value_unit <- "[%]"
    y_text <- paste("change", "[%]")
  }
  
  if (abs) {
    stat_col <- paste0(stat_col, "_abs_diff")
    info_text <- paste0(info_text, " change")
    value_unit <- value_unit
    y_text <- paste("change", value_unit)
  }
  
  if(y_label == T){
    ylab <- y_text
  } else if(y_label == F){
    ylab <- ""
  }
  
  # Legend logic
  legend_theme <- if (legend) {
    theme(
      legend.position = c(1, 1.13),
      legend.justification = "right"
    )
  } else {
    theme(legend.position = "none")
  }
  
  # Plot annual boxplots
  p <- ggplot(dt, aes(x = .data[[color_col]], y = .data[[stat_col]], fill = .data[[color_col]]))
  
  if (rel) {
    p <- p + geom_hline(yintercept = 0, linewidth = 0.3, color = "grey50")
  }
  
  p <- p + geom_boxplot(width = 0.6, outlier.size = 0.5) +
    facet_grid(~ horizon, scales = "free_x", space = "free_x", switch = "x") +
    labs(
      title = title,# paste("Annual", stat, value_name, bsn, info_text),
      x = NULL,
      y = ylab,
      fill = NULL
    ) +
    custom_theme() +
    legend_theme +
    theme(
      strip.placement = "outside",
      strip.background = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.spacing = unit(1.0, "lines"),
      #panel.grid.major.y = element_blank(),
      #panel.grid.minor.y = element_blank(),
      legend.direction = "horizontal",
      legend.box = "horizontal",
      
      legend.key = element_rect(colour = NA, fill = NA),
      legend.background = element_rect(fill = "transparent", colour = NA),
      #legend.text = element_text(size = 8),
      panel.background = element_rect(fill = "white", colour = "grey96"),
      panel.grid.major = element_line(size = 0.2, linetype = 'dotted', colour = "grey80"),
      panel.grid.minor = element_line(size = 0.2, linetype = 'dotted', colour = "grey80"),
      axis.line.x = element_line(colour = "black", size = 0.3),
      axis.line.y = element_line(colour = "black", size = 0.3),
      #axis.title = element_text(size = 10),
      plot.title = element_text(vjust = 2, hjust = 0),
      plot.margin = unit(c(15, 5.5, 5.5, 5.5), "pt")
    ) +
    guides(
      fill = guide_legend(nrow = 1),
      color = guide_legend(nrow = 1)
    ) +
    scale_fill_manual(
      values = plot_info[[color_col]]$colors,
      labels = plot_info[[color_col]]$labels
    )
  
  if (!is.null(y_lim)) {
    p <- p + ylim(y_lim)
  }
  
  return(p)
  
}

combined_annual_horizon_boxplot <- function(dt, plot_dir, bsn) {

  titles <- c(
    paste0("(a) temperature"),
    paste0("(b) precipitation")
  )
  lg <- c(T, F) # legend list
  lab <- c(T, T) # y-label list
  relative <- c(F, T) # relative change
  absolute <- c(T, F) # absolute change
  
  value_cols <- c("tair_avg", "prec_avg")
  y_lims <- list(c(0, 8), NULL)
  
  color_col <- "scen_var"
  color_col_levels <- c("ref_ref","L_Paris", "L_wet", "L_dry", "M_wet", "M_dry", "H_wet", "H_dry")
  
  group_cols <- c("basin", "scenario", "variant", "horizon", "scen_var", "scen_var_hor", "hydro_model", "run_type")
  linking_cols <- c("basin", "run_type")
  
  comparison_ref = "ref_ref"
  
  stat = "mean"

  # create all plots
  pls <- list() # empty list
  for (i in seq_along(value_cols)) {
    value_col <<- value_cols[i]
    
    dt_diff <<- compute_member_differences(dt, value_col, group_cols, linking_cols, comparison_col = color_col, comparison_ref = comparison_ref, stat)
    
    dt_diff[, (color_col) := factor(get(color_col), levels = color_col_levels)]
    dt_diff[, horizon := factor(horizon, levels = c("ref", "2033", "2050", "2100", "2150"),
                           labels = c("Ref", "2033", "2050", "2100", "2150"))]
    
    pls[[i]] <- plot_annual_horizon_boxplots_combination(dt_diff, plot_dir, bsn, color_col, value_col, stat, y_lim = y_lims[[i]], rel = relative[i], abs = absolute[i], 
                                                         titles[i], legend = lg[i], y_label = lab[i])
    
  }
  
  p <- plot_grid(pls[[1]], pls[[2]], ncol = 1, 
                 align = "v",   # align vertically
                 axis = "l")
  
  # Save the plot
  save_dir <- file.path(plot_dir, "annual_horizon_boxplots", "tair_prec")
  filename <- paste0("annual_horizon_", bsn, "_tair_prec.pdf")
  save_plot(p, save_dir, filename, width = 12, height = 8)
}

plot_seasonal_boxplots <- function(dt_seasonal, plot_dir, bsn, color_col, color_col_levels, value_col, group_cols, stat, info_text = "", rel = FALSE) {
  
  value_name <- plot_info$column_info$names[[value_col]]
  value_unit <- plot_info$column_info$units[[value_col]]
  
  y_text <- paste(value_name, value_unit)
  
  stat_col <- paste0(value_col, "_", stat)
  
  if (rel) {
    stat_col <- paste0(stat_col, "_rel_diff")
    info_text <- paste0(info_text, "change")
    value_unit <- "[%]"
    y_text <- "Change [%]"
  }
  
  # Ensure color column has defined factor levels
  dt_seasonal[, (color_col) := factor(get(color_col), levels = color_col_levels)]
  
  # Convert group_cols to combined label for facets if needed
  dt_seasonal[, group_combined := do.call(paste, c(.SD, sep = "_")), .SDcols = group_cols]
  
  # Define season as factor with desired order
  dt_seasonal[, season := factor(season, levels = c("DJF", "MAM", "JJA", "SON"))]
  
  # Create a combined season-dataset label for x-axis
  dt_seasonal[, season_dataset := interaction(season, get(color_col), lex.order = TRUE)]
  
  num_groups <- length(color_col_levels)
  
  # Plot grouped seasonal boxplots
  p <- ggplot(dt_seasonal, aes(x = season, y = .data[[stat_col]], fill = .data[[color_col]])) +
    geom_boxplot(
      position = position_dodge(width = 0.8),
      width = 0.6,  # narrower boxes
      fatten = 2,
      size = 0.8
    ) +
    labs(
      title = paste("Seasonal", stat, value_name, bsn, info_text),
      x = "Season",
      y = y_text,
      fill = "Dataset"
    ) +
    custom_theme() +
    scale_fill_manual(
      values = plot_info[[color_col]]$colors,
      labels = plot_info[[color_col]]$labels
    )
  
  # Save the plot
  save_dir <- file.path(plot_dir, "seasonal_boxplots", value_col)
  filename <- paste0("seasonal_", bsn, "_", stat_col, info_text, ".pdf")
  save_plot(p, save_dir, filename, width = 10, height = 6)
}

# Function to plot monthly and yearly boxplots from daily data.table
plot_month_year_boxplots <- function(dt_month, dt_year, plot_dir, bsn, color_col, color_col_levels, comparison_ref, value_col, group_cols, stat, info_text = "", rel = FALSE) {
  
  value_name <- plot_info$column_info$names[[value_col]]
  value_unit <- plot_info$column_info$units[[value_col]]
  
  y_text <- paste(value_name, value_unit)
  
  stat_col <- paste0(value_col, "_", stat)
  
  if (rel) {
    stat_col <- paste0(stat_col, "_rel_diff")
    info_text <- paste0(info_text, "change")
    value_unit <- "[%]"
    y_text <- "Change [%]"
    
    # Exclude comparison_ref from plotting
    dt_month <- dt_month[get(color_col) != comparison_ref]
    dt_year <- dt_year[get(color_col) != comparison_ref]
  }
  
  # Ensure color_col is a factor with the right order
  dt_month[, (color_col) := factor(get(color_col), levels = color_col_levels)]
  dt_year[, (color_col) := factor(get(color_col), levels = color_col_levels)]
  
  dt_month[, MM := factor(month.abb[MM], levels = month.abb)]  # e.g., Jan, Feb
  
  # Plotting
  p <- ggplot(dt_month, aes(x = MM, y = .data[[stat_col]], fill = .data[[color_col]])) +
    geom_boxplot(
      position = position_dodge(width = 0.8),
      width = 0.6,  # narrower boxes
      fatten = 2,
      size = 0.8
    ) +
    labs(
      title = NULL,# paste(stat, value_name, bsn, info_text),
      x = "Month",
      y = y_text,
      fill = ""
    ) +
    custom_theme() +
    theme(
      legend.position = "top",
      axis.title.x = element_blank()  # Remove the x-axis title
    ) +
    scale_x_discrete() +
    scale_fill_manual(
      values = plot_info[[color_col]]$colors,
      labels = plot_info[[color_col]]$labels
    ) +
    guides(
      fill = guide_legend(nrow = 1)
    )
  
  # Add yearly boxplot
  p <- p +
    geom_boxplot(
      data = dt_year,
      aes(x = "Year", y = .data[[stat_col]], fill = .data[[color_col]]),
      position = position_dodge(width = 0.8),
      width = 0.6,  # narrower boxes
      fatten = 2,
      size = 0.8
    )
  
  # Save the plot
  save_dir <- file.path(plot_dir, "monthly_boxplots", value_col)
  filename <- paste0("monthly_", bsn, "_", stat_col, info_text, ".pdf")
  save_plot(p, save_dir, filename, width = 18, height = 6)
}

plot_variables_change_boxplot <- function(dt, color_col, title, legend = T, y_label = T, y_limits = NULL){
  
  if(y_label == T){
    ylab <- "change [mm]"
  } else if(y_label == F){
    ylab <- ""
  }
  # facet labels
  hor.labs <- c("observation" = "Observation", "hindcast" = "hindcast", "ref" = "Reference", "2150" = "2150", "2100" = "2100", "2050" = "2050")
  
  # Get relevant horizon levels from the data
  horizons_in_data <- intersect(names(hor.labs), unique(dt$horizon))
  horizons_in_data <- names(hor.labs)[names(hor.labs) %in% horizons_in_data]
  
  # Legend logic
  legend_theme <- if (legend) {
    theme(
      legend.position = c(1, 1.03),
      legend.justification = "right"
    )
  } else {
    theme(legend.position = "none")
  }

  # plot
  p <- ggplot(data = dt) +
    geom_boxplot(aes(x = variable, y = abs_diff, fill = .data[[color_col]]), lwd = 0.28, outlier.size = 0.28, fatten = 1.2) +
    #scale_fill_manual(values=c("#b1dee7ff","#f8990fff","#96d776ff"), name = "", labels = c("2035", "2060", "2085")) +
    scale_fill_manual(
      values = plot_info[[color_col]]$colors,
      labels = plot_info[[color_col]]$labels
    ) +
    labs(title = title, x = "", y = ylab) +
    geom_hline(yintercept = 0, lwd = 0.3, col = "grey50") + # add horizontal line to empty ggplot
    theme(
      panel.background = element_rect(fill = "white", colour = "grey96"),
      panel.grid.major = element_line(size = 0.2, linetype = 'dotted', colour = "grey80"), 
      panel.grid.minor = element_line(size = 0.2, linetype = 'dotted',colour = "grey80"),
      axis.line.x = element_line(colour = "black", size = 0.3), axis.line.y = element_line(colour = "black", size = 0.3)) +
    guides(fill = guide_legend(direction = "horizontal", title = NULL)) + # , nrow = 1
    (if (!is.null(y_limits)) coord_cartesian(ylim = y_limits) else NULL) +
    legend_theme +
    theme(
          #legend.position = leg, legend.justification = "right", 
          legend.key = element_rect(colour = NA, fill = NA), 
          legend.background = element_rect(fill = "transparent"), legend.text = element_text(size = 8),
          plot.margin = unit(c(15, 5.5, 5.5, 5.5), "pt"), plot.title = element_text(size = 12, vjust = 2), axis.title = element_text(size = 10)) +
    facet_grid(horizon ~ ., labeller = labeller(horizon = hor.labs))
  
  # change facet colors
  g <- ggplot_gtable(ggplot_build(p))
  strip_right <- which(grepl('strip-r', g$layout$name))
  fills <- c("#fecd07ff", "#6fb8c2ff", "#934c94ff")
  fills <- rev(unname(plot_info[["horizon"]]$colors[horizons_in_data]))
  k <- 1
  for (i in strip_right) {
    j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
    g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
    k <- k + 1
  }
  
  # output plot
  return(as.ggplot(g))
  
}

get_combined_long_diff_dt <- function(dt, value_cols, group_cols, linking_cols,
                                      color_col, comparison_ref, stat, seasonal = FALSE, half_year = FALSE) {
  # Short variable labels (customize order)
  vars_map <- c("P-kor" = "P", "GLAC" = "G", "P-SME" = "S", "EREA" = "ET", "RGES" = "Q") # , "GLAC" = "G"
  vars_short <- unname(vars_map)
  
  dt_list <- list()
  
  for (value_col in value_cols) {
    if (all(is.na(dt[[value_col]]))) next
    
    # Compute differences
    dt_diff <- compute_member_differences(
      dt,
      value_col = value_col,
      group_cols = group_cols,
      linking_cols = linking_cols,
      comparison_col = color_col,
      comparison_ref = comparison_ref,
      statistic = stat,
      seasonal = seasonal,
      half_year = half_year
    )
    
    # Extract needed columns
    abs_col <- paste0(value_col, "_", stat, "_abs_diff")
    rel_col <- paste0(value_col, "_", stat, "_rel_diff")
    
    # Make sure columns exist
    #if (!all(c(abs_col, rel_col) %in% names(dt_diff))) next
    
    cols_keep <- unique(c(group_cols, "horizon"))
    if (seasonal || half_year) {
      cols_keep <- c(cols_keep, "season")
    }
    dt_sub <- dt_diff[, ..cols_keep]
    
    # Add computed values
    dt_sub[, abs_diff := dt_diff[[abs_col]]]
    dt_sub[, rel_diff := dt_diff[[rel_col]]]
    dt_sub[, variable := vars_map[[value_col]]]
    
    dt_list[[value_col]] <- dt_sub
  }
  
  # Combine all
  dt_long <- rbindlist(dt_list, use.names = TRUE, fill = TRUE)
  dt_long[, variable := factor(variable, levels = vars_short)]
  
  return(dt_long)
}

combined_variables_change_boxplot_annual <- function(dt, plot_dir, bsn, time_period) {
  
  titles <- c(
    paste0("(a) ", bsn)
  )
  lg <- c(T) # legend list
  lab <- c(T) # y-label list
  
  if (time_period == "ref") {
    # Only keep relevant horizons
    dt_final <- dt[horizon %in% c("ref", "hindcast", "observation")]
    
    color_col <- "horizon"
    color_col_levels <- c("observation", "hindcast", "ref", "2050", "2100", "2150")
    
  } else if (time_period == "future") {
    # Remove "observation" entirely
    dt_final <- dt[!horizon %in% c("observation", "hindcast", "ref", "2033")]
    
    color_col <- "scen_var"
    color_col_levels <- c("none_none","L_none", "L_wet", "L_dry", "M_wet", "M_dry", "H_wet", "H_dry")
  }
  
  dt_final[, (color_col) := factor(get(color_col), levels = color_col_levels)]

  # create all plots
  pls <- list() # empty list
  for (i in seq_along(titles)) {
    
    dt_final[, abs_diff := abs_diff * 365]
    
    pls[[i]] <- plot_variables_change_boxplot(dt_final, color_col, titles[i], legend = lg[i], y_label = lab[i])
  }
  
  p <- plot_grid(pls[[1]], ncol = 1)
  
  # Save the plot
  save_dir <- file.path(plot_dir, "variables_change")
  filename <- paste0("vars_change_annual", bsn, "_", time_period, ".pdf")
  
  save_plot(p, save_dir, filename, width = 8.27, height = 6)
  
}

combined_variables_change_boxplot_half_year <- function(dt, plot_dir, bsn, time_period) {
  
  titles <- c(
    paste0("(a) summer"),
    paste0("(b) winter")
  )
  lg <- c(F, T) # legend list
  lab <- c(T, F) # y-label list
  
  seasons <- c("summer", "winter")
  
  if (time_period == "ref") {
    # Only keep relevant horizons
    dt_final <- dt[horizon %in% c("ref", "hindcast", "observation")]
    
    color_col <- "horizon"
    color_col_levels <- c("observation", "hindcast", "ref", "2050", "2100", "2150")
    
  } else if (time_period == "future") {
    # Remove "observation" entirely
    dt_final <- dt[!horizon %in% c("observation", "hindcast", "ref", "2033")]
    
    color_col <- "scen_var"
    color_col_levels <- c("none_none","L_none", "L_wet", "L_dry", "M_wet", "M_dry", "H_wet", "H_dry")
  }
  
  dt_final[, (color_col) := factor(get(color_col), levels = color_col_levels)]
  dt_final[, abs_diff := abs_diff * 183]
  
  y_min <- min(dt_final$abs_diff, na.rm = TRUE)
  y_max <- max(dt_final$abs_diff, na.rm = TRUE)
  y_limits <- c(y_min, y_max)
  
  # create all plots
  pls <- list() # empty list
  for (i in seq_along(seasons)) {
    
    dt_season <- dt_final[season == seasons[i]]
    
    pls[[i]] <- plot_variables_change_boxplot(dt_season, color_col, titles[i], legend = lg[i], y_label = lab[i], y_limits = y_limits)
  }
  
  p <- plot_grid(pls[[1]], pls[[2]], ncol = 2)
  
  # Save the plot
  save_dir <- file.path(plot_dir, "variables_change")
  filename <- paste0("vars_change_half_year", bsn, "_", time_period, ".pdf")
  
  save_plot(p, save_dir, filename, width = 8.27, height = 6)
  
}

combined_variables_change_boxplot_seasonal <- function(dt, plot_dir, bsn, time_period) {
  
  titles <- c(
    paste0("(c) JJA"),
    paste0("(d) SON"),
    paste0("(a) DJF"),
    paste0("(b) MAM")
  )
  lg <- c(F, F, F, T) # legend list
  lab <- c(T, F, T, F) # y-label list
  
  seasons <- c("JJA", "SON", "DJF", "MAM")
  
  if (time_period == "ref") {
    # Only keep relevant horizons
    dt_final <- dt[horizon %in% c("ref", "hindcast", "observation")]
    
    color_col <- "horizon"
    color_col_levels <- c("observation", "hindcast", "ref", "2050", "2100", "2150")
    
  } else if (time_period == "future") {
    # Remove "observation" entirely
    dt_final <- dt[!horizon %in% c("observation", "hindcast", "ref", "2033")]
    
    color_col <- "scen_var"
    color_col_levels <- c("none_none","L_none", "L_wet", "L_dry", "M_wet", "M_dry", "H_wet", "H_dry")
  }
  
  dt_final[, (color_col) := factor(get(color_col), levels = color_col_levels)]
  dt_final[, abs_diff := abs_diff * 91]
  
  y_min <- min(dt_final$abs_diff, na.rm = TRUE)
  y_max <- max(dt_final$abs_diff, na.rm = TRUE)
  y_limits <- c(y_min, y_max)
  
  # create all plots
  pls <- list() # empty list
  for (i in seq_along(seasons)) {
    
    dt_season <- dt_final[season == seasons[i]]
    
    pls[[i]] <- plot_variables_change_boxplot(dt_season, color_col, titles[i], legend = lg[i], y_label = lab[i], y_limits = y_limits)
  }
  
  p <- plot_grid(pls[[3]], pls[[4]], pls[[1]], pls[[2]], ncol = 2)
  
  # Save the plot
  save_dir <- file.path(plot_dir, "variables_change")
  filename <- paste0("vars_change_seasonal", bsn, "_", time_period, ".pdf")
  
  save_plot(p, save_dir, filename, width = 8.27, height = 9)
  
}


