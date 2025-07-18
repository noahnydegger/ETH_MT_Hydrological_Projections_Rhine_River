library(data.table)
library(patchwork)
library(grid)
library(cowplot)
library(ggplotify)

source(here("R_scripts", "plotting_functions", "knmi_plot_metadata.R"))

compute_percentile_ref_value <- function(dt, bsn, value_col, comparison_col, comparison_ref, q_bot, q_top) {
  # Subset the data based on basin and comparison reference
  dt_sub <- dt[basin == bsn & get(comparison_col) == comparison_ref]
  
  # Compute percentiles
  q_values <- quantile(dt_sub[[value_col]], probs = c(q_bot, q_top), na.rm = TRUE)
  
  # Return as a named list or vector
  ref_value <- list(
    q_bot = q_values[1],
    q_top = q_values[2]
  )
  
  return(ref_value)
}

compute_number_of_days <- function(dt, bsn, group_cols, value_col, comparison_col, comparison_ref, q_bot, q_top) {
  # Compute reference thresholds
  ref_values <<- compute_percentile_ref_value(dt, bsn, value_col, comparison_col, comparison_ref, q_bot, q_top)
  
  # Add date parts
  dt[, year := year(date)]
  dt[, month := month(date)]
  
  # Assign seasons
  dt[, season := fifelse(month %in% c(12, 1, 2), "DJF",
                             fifelse(month %in% c(3, 4, 5), "MAM",
                                     fifelse(month %in% c(6, 7, 8), "JJA", "SON")))]
  
  dt[, season := factor(season, levels = c("DJF", "MAM", "JJA", "SON"))]
  
  # Compare values to reference percentiles
  dt[, below_qbot := get(value_col) < ref_values$q_bot]
  dt[, above_qtop := get(value_col) > ref_values$q_top]
  
  # Seasonal aggregation
  group_season <- c(group_cols,"year", "season", "member")
  dt_season <- dt[, .(
    days_below_qbot = sum(below_qbot, na.rm = TRUE),
    days_above_qtop = sum(above_qtop, na.rm = TRUE)
  ), by = group_season]
  
  # Yearly aggregation
  group_year <- c(group_cols, "year", "member")
  dt_year <- dt[, .(
    days_below_qbot = sum(below_qbot, na.rm = TRUE),
    days_above_qtop = sum(above_qtop, na.rm = TRUE)
  ), by = group_year]
  dt_year[, season := "Year"]
  dt_year[, season := factor(season, levels = c("DJF", "MAM", "JJA", "SON", "Year"))]
  
  # Combine season + year
  result <- rbind(dt_season, dt_year, use.names = TRUE, fill = TRUE)
  result[, season := factor(season, levels = c("DJF", "MAM", "JJA", "SON", "Year"))]
  
  return(result)
}

plot_seasonal_bars_with_error <- function(dt_seasonal, plot_dir, bsn, color_col, color_col_levels, value_col, group_cols, info_text = "", rel = FALSE) {
  
  # Fetch metadata
  value_name <- plot_info$column_info$names[[value_col]]
  value_unit <- plot_info$column_info$units[[value_col]]
  y_text <- "days per year"
  
  if (rel) {
    value_unit <- "[%]"
    y_text <- "Change [%]"
    info_text <- paste0(info_text, "change")
  }
  
  # Ensure proper factor ordering
  dt_seasonal[, (color_col) := factor(get(color_col), levels = color_col_levels)]
  dt_seasonal[, season := factor(season, levels = c("DJF", "MAM", "JJA", "SON", "Year"))]
  dt_seasonal[, group_combined := do.call(paste, c(.SD, sep = "_")), .SDcols = group_cols]
  
  # Compute summary statistics per group
  dt_summary <<- dt_seasonal[, .(
    mean = mean(days_below_qbot, na.rm = TRUE),
    q10 = quantile(days_below_qbot, 0.1, na.rm = TRUE),
    q90 = quantile(days_below_qbot, 0.9, na.rm = TRUE)
  ), by = c(group_cols, "season")]
  
  dt_summary[, group_combined := do.call(paste, c(.SD, sep = "_")), .SDcols = group_cols]
  
  # Plot barplot with error bars (q10 - q90)
  p <- ggplot(dt_summary, aes(x = season, y = mean, fill = .data[[color_col]])) +
    geom_bar(stat = "identity", 
             position = position_dodge(width = 0.8), 
             width = 0.6, color = "black") +
    geom_errorbar(aes(ymin = q10, ymax = q90), 
                  position = position_dodge(width = 0.8), 
                  width = 0.2, color = "black") +
    scale_fill_manual(
      values = plot_info[[color_col]]$colors,
      labels = plot_info[[color_col]]$labels
    ) +
    labs(
      x = NULL,
      y = y_text,
      fill = "Dataset"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.major.x = element_blank(),
      axis.line = element_line(color = "black", linewidth = 0.5),
      legend.position = "top",
      legend.key.width = unit(1.5, "cm"),
      axis.text.x = element_text(size = 12)
    )
  
  # Save the plot
  save_dir <- file.path(plot_dir, "seasonal_barplots", value_col)
  filename <- paste0("seasonal_bar_", bsn, "_", value_col, info_text, ".pdf")
  save_plot(p, save_dir, filename, width = 10, height = 6)
}

barplot_season <- function(dt, color_col, title, legend = T, y_label = T, y_limits = NULL) {
  
  if(y_label == T){
    ylab <- "days per year"
  } else if(y_label == F){
    ylab <- ""
  }
  # facet labels
  rcp.labs <- c("RCP2.6", "RCP4.5", "RCP8.5")
  names(rcp.labs) <- c("RCP26", "RCP45", "RCP85")
  
  #hor.labs <- c("2150", "2100", "2050")
  #names(hor.labs) <- c("2150", "2100", "2050")
  
  hor.labs <- c("ref" = "Reference", "2150" = "2150", "2100" = "2100", "2050" = "2050")
  
  # Get relevant horizon levels from the data
  horizons_in_data <- intersect(names(hor.labs), unique(dt$facet_horizon))
  horizons_in_data <- names(hor.labs)[names(hor.labs) %in% horizons_in_data]
  
  
  scen.labs <- c("L" = "Low", "M" = "Moderate", "H" = "High")
  
  # if (legend == T) {
  #   leg <- c(1, 1.03)
  # } else {
  #   leg <- NULL  # Or skip setting this in theme()
  # }
  
  # Legend logic
  legend_theme <- if (legend) {
    theme(
      legend.position = c(1.10, 1.11),
      legend.justification = "right"
    )
  } else {
    theme(legend.position = "none")
  }
  
  # plot
  pl <- ggplot(data = dt, aes(x = season, y = mean, fill = .data[[color_col]])) +
    geom_bar(stat = "identity", position = position_dodge()) +
    geom_errorbar(aes(ymin = q10, ymax = q90), width = 0.2, linewidth = 0.3, position = position_dodge(0.9)) +
    #scale_fill_manual(values=c("grey", "#b1dee7ff","#f8990fff","#96d776ff"), name = "", labels = c("reference period", "2035", "2060", "2085")) +
    scale_fill_manual(
      values = plot_info[[color_col]]$colors,
      labels = plot_info[[color_col]]$labels
    ) +
    (if (!is.null(y_limits)) coord_cartesian(ylim = y_limits) else NULL) +
    facet_grid(facet_horizon ~ ., labeller = labeller(facet_horizon = hor.labs)) +
    labs(title = title, x = "", y = ylab) +
    theme(
      panel.background = element_rect(fill = "white", colour = "grey96"),
      panel.grid.major = element_line(linewidth = 0.1, linetype = 'dotted', colour = "grey80"), 
      panel.grid.minor = element_line(linewidth = 0.1, linetype = 'dotted',colour = "grey80"),
      axis.line.x = element_line(colour = "black", size = 0.3), axis.line.y = element_line(colour = "black", size = 0.3)) +
    guides(fill = guide_legend(direction = "horizontal", title = NULL)) +
    legend_theme +  # Add this instead
    theme(
          #legend.position = leg,
          #legend.justification = "right", 
          legend.key = element_rect(colour = NA, fill = NA), strip.text.y = element_text(size = 10), axis.text.y = element_text(size = 10),
          legend.background = element_rect(fill = "transparent"), legend.text = element_text(size = 10), legend.key.size = unit(0.2, 'cm'), axis.text.x = element_text(size = 10),
          plot.margin = unit(c(15, 5.5, 5.5, 5.5), "pt"), plot.title = element_text(size = 10, vjust = 2), axis.title = element_text(size = 10))
  
  # change facet colors
  g <- ggplot_gtable(ggplot_build(pl))
  strip_right <- which(grepl('strip-r', g$layout$name))
  #fills <- c("#fecd07ff", "#6fb8c2ff", "#934c94ff")
  fills <- c("#b1dee7ff", "#f8990fff", "#96d776ff")
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

combined_bar_plot_season <- function(dt, plot_dir, bsn, q_bot, q_top, time_period) {
  bot_days <- sprintf("%02d", round(q_bot * 100))
  top_days <- sprintf("%02d", round(q_top * 100))
  
  titles <- c(
    paste0("(a) low flow Q", top_days),
    paste0("(b) high flow Q", bot_days)
  )
  lg <- c(T, F) # legend list
  lab <- c(T, T) # y-label list
  r_number <- c(1, 4, 6, 8)
  days_col <- c("days_below_qbot", "days_above_qtop")
  
  if (time_period == "ref") {
    # Only keep relevant horizons
    dt_final <- dt[horizon %in% c("ref", "hindcast", "observation")]
    dt_final[, facet_horizon := "ref"]
    
    color_col <- "horizon"
    color_col_levels <- c("observation", "hindcast", "ref", "2050", "2100", "2150")
    
    lg <- c(F, T) # legend list
    lab <- c(T, F) # y-label list
    
    q90_vals <- sapply(days_col, function(col) {
      quantile(dt[[col]], 0.9, na.rm = TRUE)
    })
    
    # Set y-axis limits from 0 to the maximum q90 value
    y_limits <- c(0, max(q90_vals, na.rm = TRUE))
    
    plot_width <- 8.27
    plot_height <- 2.5
    plot_cols <- 2
    
  } else if (time_period == "future") {
    # Remove "observation" entirely
    dt_facet <- dt[!horizon %in% c("observation", "hindcast", "2033")]
    
    # Get rows where horizon == "ref"
    ref_rows <- dt_facet[horizon == "ref"]
    
    # Define future horizons
    future_horizons <- c("2050", "2100", "2150")
    
    # Duplicate "ref" rows for each target horizon
    ref_expanded <- ref_rows[rep(1:.N, each = length(future_horizons))]
    ref_expanded[, facet_horizon := rep(future_horizons, times = nrow(ref_rows))]
    
    # Set facet_horizon for the rest
    dt_facet[horizon != "ref", facet_horizon := horizon]
    
    # Combine ref + other future rows
    dt_final <- rbind(dt_facet[horizon != "ref"], ref_expanded)
    
    color_col <- "scen_var"
    color_col_levels <- c("ref_ref","L_Paris", "L_wet", "L_dry", "M_wet", "M_dry", "H_wet", "H_dry")
    
    lg <- c(T, F) # legend list
    lab <- c(T, T) # y-label list
    
    y_limits <- NULL
    
    plot_width <- 6
    plot_height <- 7
    plot_cols <- 1
  }
  
  # create all plots
  pls <- list() # empty list
  for (i in seq_along(days_col)) {
    col <- days_col[i]
    
    dt_summary <<- dt_final[, .(
      mean = mean(get(col), na.rm = TRUE),
      q10 = quantile(get(col), 0.1, na.rm = TRUE),
      q90 = quantile(get(col), 0.9, na.rm = TRUE)
    ), by = c(group_cols, "season", "facet_horizon")]
    
    dt_summary[, (color_col) := factor(get(color_col), levels = color_col_levels)]
    dt_summary[, season := factor(season, levels = c("DJF", "MAM", "JJA", "SON", "Year"))]
    
    pls[[i]] <- barplot_season(dt_summary, color_col, titles[i], legend = lg[i], y_label = lab[i], y_limits)
  }
  
  p <- plot_grid(pls[[1]], pls[[2]], ncol = plot_cols)
  
  # Save the plot
  save_dir <- file.path(plot_dir, "seasonal_barplots", value_col)
  filename <- paste0("seasonal_bar_combined_", bsn, paste0("_Q", q_bot*100, "_Q", q_top*100), "_", time_period, ".pdf")
  
  save_plot(p, save_dir, filename, width = plot_width, height = plot_height)  # 8.27, 3

  # for(i in 1:length(data)){
  #   if(grepl("Drought", names(data)[i]) == T){
  #     type <- "Drought"
  #   } else if(grepl("Flood", names(data)[i]) == T){
  #     type <- "Flood"
  #   }
  #   pl <- barplot(data[[i]], titles[i], type)
  #   pdf(paste0("../Figures/drought_limits_", names(data)[i], ".pdf"), width = 8.27, height = 5.5)
  #   print(pl)
  #   dev.off()
  # }
}



