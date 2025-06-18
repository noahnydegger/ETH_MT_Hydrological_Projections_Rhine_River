

##________________________________________
# FUNCTION to create monthly means
monthly <- function(data) {
  output <- data %>%
    group_by(month = format(as.Date(date), "%m")) %>%
    summarise_at(vars(colnames(data)), mean, na.rm = T)
  output <- output[,-2]
  return(output)
}


##________________________________________
# FUNCTION to reformat monthly data and create abs & rel data
monthly_reformat <- function(ref, early, mid, late, mymonths){
  
  ref <- melt(ref,  id.vars = 'month', variable.name = 'scenarios', value.name = "ref")
  early <- melt(early,  id.vars = 'month', variable.name = 'scenarios', value.name = "early")
  mid <- melt(mid,  id.vars = 'month', variable.name = 'scenarios', value.name = "mid")
  late <- melt(late,  id.vars = 'month', variable.name = 'scenarios', value.name = "late")
  
  combined <- data.frame(ref, early = early$early, mid = mid$mid, late = late$late)
  combined$month <- factor(mymonths[as.numeric(combined$month)], levels = mymonths)
  combined$scenarios <- as.character(combined$scenarios)
  combined$rcp <- substr(combined$scenarios, nchar(combined$scenarios) - 4, nchar(combined$scenarios))
  
  # absolut data
  abs <- melt(combined, id.vars = c('month', 'rcp'), measure.vars = c('ref','early','mid', 'late'), variable.name = 'Periode')
  
  # relative data v1 (X - ref)
  diff <- data.frame(month = combined$month,
                     rcp = combined$rcp,
                     early = combined$early - combined$ref,
                     mid = combined$mid - combined$ref,
                     late = combined$late - combined$ref)
  diff <- melt(diff, id.vars =c('month', 'rcp'), measure.vars=c('early','mid', 'late'), variable.name = 'Periode', value.name = "value")
  
  # relative data v2 (X : ref)
  rel <- data.frame(month = combined$month,
                    rcp = combined$rcp,
                    early = (combined$early / combined$ref * 100) - 100,
                    mid = (combined$mid / combined$ref * 100) - 100,
                    late = (combined$late / combined$ref * 100) - 100)
  rel <- melt(rel, id.vars = c('month', 'rcp'), measure.vars = c('early', 'mid', 'late'), variable.name = 'Periode')
  
  return(list(abs = abs, diff = diff, rel = rel))
}


##________________________________________
# FUNCTION to create yearly mean data
yearly <- function(data) {
  output <- data %>%
    summarise_at(vars(colnames(data)[-1]), mean, na.rm = T)
  output$month <- as.factor("Year")
  return(output)
}


##________________________________________
# FUNCTION to reformat yearly data
yearly_reformat <- function(ref, early, mid, late){
  
  ref <- melt(ref ,  id.vars = 'month', variable.name = 'scenarios', value.name = "ref")
  early <- melt(early ,  id.vars = 'month', variable.name = 'scenarios', value.name = "early")
  mid <- melt(mid ,  id.vars = 'month', variable.name = 'scenarios', value.name = "mid")
  late <- melt(late ,  id.vars = 'month', variable.name = 'scenarios', value.name = "late")
  
  combined <- data.frame(ref, early = early$early, mid = mid$mid, late = late$late)
  combined$scenarios <- as.character(combined$scenarios)
  combined$rcp <- substr(combined$scenarios, nchar(combined$scenarios)-4, nchar(combined$scenarios))
  
  
  # absolut data
  abs <- melt(combined, id.vars = c('month', 'rcp'), measure.vars = c('ref','early','mid', 'late'), variable.name = 'Periode')
  
  # relative data v1 (X - ref)
  diff <- data.frame(month = combined$month,
                     rcp = combined$rcp,
                     early = combined$early - combined$ref,
                     mid = combined$mid - combined$ref,
                     late = combined$late - combined$ref)
  diff <- melt(diff, id.vars =c('month', 'rcp'), measure.vars=c('early','mid', 'late'), variable.name = 'Periode', value.name = "value")
  
  # relative data v2 (X : ref)
  rel <- data.frame(month = combined$month,
                     rcp = combined$rcp,
                     early = (combined$early / combined$ref * 100) - 100,
                     mid = (combined$mid / combined$ref * 100) - 100,
                     late = (combined$late / combined$ref * 100) - 100)
  rel <- melt(rel, id.vars = c('month', 'rcp'), measure.vars = c('early', 'mid', 'late'), variable.name = 'Periode')
  
  return(list(abs = abs, diff = diff, rel = rel))
}


##________________________________________
# FUNCTION to create seasonal means
seasonaly <- function(data) {
  output <- data %>%
    group_by(season) %>%
    summarise_at(vars(colnames(data)[-42]), mean, na.rm = T)
  output <- output[,-c(2, 42)]
  return(output)
}


##________________________________________
# FUNCTION to reformat seasonal data and create abs & rel data
seasonaly_reformat <- function(ref, early, mid, late, myseasons){
  
  ref <- melt(ref,  id.vars = 'season', variable.name = 'scenarios', value.name = "ref")
  early <- melt(early,  id.vars = 'season', variable.name = 'scenarios', value.name = "early")
  mid <- melt(mid,  id.vars = 'season', variable.name = 'scenarios', value.name = "mid")
  late <- melt(late,  id.vars = 'season', variable.name = 'scenarios', value.name = "late")
  
  combined <- data.frame(ref, early = early$early, mid = mid$mid, late = late$late)
  combined$season <- factor(combined$season, levels = myseasons)
  combined$scenarios <- as.character(combined$scenarios)
  combined$rcp <- substr(combined$scenarios, nchar(combined$scenarios) - 4, nchar(combined$scenarios))
  
  # absolut data
  abs <- melt(combined, id.vars = c('season', 'rcp'), measure.vars = c('ref','early','mid', 'late'), variable.name = 'period')
  
  # relative data v1 (X - ref)
  diff <- data.frame(season = combined$season,
                     rcp = combined$rcp,
                     early = combined$early - combined$ref,
                     mid = combined$mid - combined$ref,
                     late = combined$late - combined$ref)
  diff <- melt(diff, id.vars =c('season', 'rcp'), measure.vars=c('early','mid', 'late'), variable.name = 'period', value.name = "value")
  
  # relative data v2 (X : ref)
  rel <- data.frame(season = combined$season,
                    rcp = combined$rcp,
                    early = (combined$early / combined$ref * 100) - 100,
                    mid = (combined$mid / combined$ref * 100) - 100,
                    late = (combined$late / combined$ref * 100) - 100)
  rel <- melt(rel, id.vars = c('season', 'rcp'), measure.vars = c('early', 'mid', 'late'), variable.name = 'period')
  
  return(list(abs = abs, diff = diff, rel = rel))
}



##________________________________________
# FUNCTION to calculate the median of corresponding climate chain values
median_percentiles <- function(data){
  
  newdata <- data %>% 
    group_by(date, rcp, period, q) %>%
    summarise(median = median(value))
  
  return(newdata)
}



##________________________________________
# FUNCTION to make boxplots
boxpl <- function(data, station, title, plot_type, legend = T){
  
  p <- ggplot(data[[station]][[plot_type]]) # start ggplot with data
  
  # different axis labels depending on plot type
  if(plot_type == "abs"){ # absolute
    ylab <- expression("Abfluss ["* m^3/s *"]")
    leg_col_label <- scale_fill_manual(values=c("grey", "#b1dee7ff","#f8990fff","#96d776ff"), name = "", labels = c("Referenzperiode", "2035", "2060", "2085"))
  } else if(plot_type == "diff"){ # difference
    ylab <- "change in water level [m]"
    leg_col_label <- scale_fill_manual(values=c("#b1dee7ff","#f8990fff","#96d776ff"), name = "", labels = c("2035", "2060", "2085"))
    p <- p + geom_hline(yintercept = 0, lwd = 0.3, col = "grey50") # add horizontal line to empty ggplot
  } else if(plot_type == "rel"){ # relative
    ylab <- "change in outflow [%]"
    leg_col_label <- scale_fill_manual(values=c("#b1dee7ff","#f8990fff","#96d776ff"), name = "", labels = c("2035", "2060", "2085"))
    p <- p + geom_hline(yintercept = 0, lwd = 0.3, col = "grey50") # add horizontal line to empty ggplot
  }
  
  # new rcp labels
  rcp.labs <- c("RCP2.6", "RCP4.5", "RCP8.5")
  names(rcp.labs) <- c("RCP26", "RCP45", "RCP85")
  
  ifelse(legend == T, leg <- c(1, 1.03), leg <- "none")
  
  # complete ggplot and split by facet plot (after rcp)
  p <- p + geom_boxplot(aes(x = month, y = value, fill = Periode), lwd = 0.28, outlier.size = 0.28, fatten = 1.2) +
    leg_col_label +
    labs(title = title, x = "", y = ylab) +
    theme(
      panel.background = element_rect(fill = "white", colour = "grey96"),
      panel.grid.major = element_line(size = 0.2, linetype = 'dotted', colour = "grey80"), 
      panel.grid.minor = element_line(size = 0.2, linetype = 'dotted',colour = "grey80"),
      axis.line.x = element_line(colour = "black", size = 0.3), axis.line.y = element_line(colour = "black", size = 0.3)) +
    guides(fill = guide_legend(direction = "horizontal")) +
    theme(legend.position = leg, legend.justification = "right", legend.key = element_rect(colour = NA, fill = NA), 
          legend.background = element_rect(fill = "transparent"), legend.text = element_text(size = 8),
          plot.margin = unit(c(15, 5.5, 5.5, 5.5), "pt"), plot.title = element_text(size = 12, vjust = 2), axis.title = element_text(size = 10))
  
  p <- p + facet_grid(rcp ~ ., labeller = labeller(rcp = rcp.labs))
  
  # change facet colors
  g <- ggplot_gtable(ggplot_build(p))
  strip_right <- which(grepl('strip-r', g$layout$name))
  fills <- c("#fecd07ff", "#6fb8c2ff", "#934c94ff")
  k <- 1
  for (i in strip_right) {
    j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
    g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
    k <- k + 1
  }
  
  # output plot
  return(as.ggplot(g))
}


##________________________________________
# FUNCTION to make boxplots
boxpl_season <- function(data, station, title, plot_type, y_lim = F, legend = T, y_label = T){
  
  p <- ggplot(data[[station]][[plot_type]]) # start ggplot with data
  
  if(y_lim[1] != F){
    p <- p + ylim(y_lim)
  }
  
  # different axis labels depending on plot type
  if(plot_type == "abs"){ # absolute
    ylab <- expression("Abfluss ["* m^3/s *"]")
    leg_col_label <- scale_fill_manual(values=c("grey", "#b1dee7ff","#f8990fff","#96d776ff"), name = "", labels = c("Referenzperiode", "2035", "2060", "2085"))
  } else if(plot_type == "diff"){ # difference
    if(y_label == T){
      ylab <- "change in water level [m]"
    } else if(y_label == F){
      ylab <- ""
    }
    leg_col_label <- scale_fill_manual(values=c("#b1dee7ff","#f8990fff","#96d776ff"), name = "", labels = c("2035", "2060", "2085"))
    p <- p + geom_hline(yintercept = 0, lwd = 0.3, col = "grey50") # add horizontal line to empty ggplot
    } else if(plot_type == "rel"){ # relative
    if(y_label == T){
      ylab <- "change in outflow [%]"
    } else if(y_label == F){
      ylab <- ""
    }
    leg_col_label <- scale_fill_manual(values=c("#b1dee7ff","#f8990fff","#96d776ff"), name = "", labels = c("2035", "2060", "2085"))
    p <- p + geom_hline(yintercept = 0, lwd = 0.3, col = "grey50") # add horizontal line to empty ggplot
  }
  
  # new rcp labels
  rcp.labs <- c("RCP2.6", "RCP4.5", "RCP8.5")
  names(rcp.labs) <- c("RCP26", "RCP45", "RCP85")
  
  ifelse(legend == T, leg <- c(1, 1.03), leg <- "none")
  
  # complete ggplot and split by facet plot (after rcp)
  p <- p + geom_boxplot(aes(x = season, y = value, fill = period), lwd = 0.28, outlier.size = 0.28, fatten = 1.2) +
    leg_col_label +
    labs(title = title, x = "", y = ylab) +
    theme(
      panel.background = element_rect(fill = "white", colour = "grey96"),
      panel.grid.major = element_line(size = 0.1, linetype = 'dotted', colour = "grey80"), 
      panel.grid.minor = element_line(size = 0.1, linetype = 'dotted',colour = "grey80"),
      axis.line.x = element_line(colour = "black", size = 0.3), axis.line.y = element_line(colour = "black", size = 0.3)) +
    guides(fill = guide_legend(direction = "horizontal")) +
    theme(legend.position = leg, legend.justification = "right", legend.key = element_rect(colour = NA, fill = NA), strip.text.y = element_text(size = 8), axis.text.y = element_text(size = 8),
          legend.background = element_rect(fill = "transparent"), legend.text = element_text(size = 8), axis.text.x = element_text(size = 8),
          plot.margin = unit(c(15, 5.5, 5.5, 5.5), "pt"), plot.title = element_text(size = 10, vjust = 2), axis.title = element_text(size = 8))
  
  p <- p + facet_grid(rcp ~ ., labeller = labeller(rcp = rcp.labs))

  # change facet colors
  g <- ggplot_gtable(ggplot_build(p))
  strip_right <- which(grepl('strip-r', g$layout$name))
  fills <- c("#fecd07ff", "#6fb8c2ff", "#934c94ff")
  k <- 1
  for (i in strip_right) {
    j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
    g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
    k <- k + 1
  }
  
  # output plot
  return(as.ggplot(g))
}



##________________________________________
# FUNCTION to make lineplots
lineplot <- function(data, station, title, lake = F) {
  
  # data for polygons
  data_polygon <- data.frame(date = c(data[[station]][data[[station]]$q == "q10", ]$date, rev(data[[station]][data[[station]]$q == "q90", ]$date)),
                             median = c(data[[station]][data[[station]]$q == "q10", ]$median, rev(data[[station]][data[[station]]$q == "q90", ]$median)),
                             period = c(data[[station]][data[[station]]$q == "q10", ]$period, rev(data[[station]][data[[station]]$q == "q90", ]$period)),
                             rcp = c(data[[station]][data[[station]]$q == "q10", ]$rcp, rev(data[[station]][data[[station]]$q == "q90", ]$rcp)))
  # lake
  if (lake == T) {
    ylab <- "water level [m]"
  } else {
    ylab <- expression("outflow [" * mm~d^"-1" * "]")
  }
  
  # new rcp labels
  rcp.labs <- c("RCP2.6", "RCP4.5", "RCP8.5")
  names(rcp.labs) <- c("RCP26", "RCP45", "RCP85")
  
  # complete ggplot and split by facet plot (after rcp)
  p <- ggplot() +
    geom_polygon(data = data_polygon, aes(x = date, y = median, group = period, fill = period), alpha = 0.12) +
    geom_line(data = data[[station]][data[[station]]$q == "q10", ], aes(x = date, y = median, group = period, col = period), lwd = 0.3, linetype = "dotted") +
    geom_line(data = data[[station]][data[[station]]$q == "q50", ], aes(x = date, y = median, group = period, col = period), lwd = 0.4) +
    geom_line(data = data[[station]][data[[station]]$q == "q90", ], aes(x = date, y = median, group = period, col = period), lwd = 0.3, linetype = "dotted") +
    scale_color_manual(
      values = c("grey40", "#b1dee7ff", "#f8990fff", "#96d776ff"), name = "", labels = c("ref", "2035", "2060", "2085"),
      guide = guide_legend(override.aes = list(linetype = "solid"))
    ) +
    scale_fill_manual(values = c("grey40", "#b1dee7ff", "#f8990fff", "#96d776ff"), name = "", labels = c("ref", "2035", "2060", "2085")) +
    scale_x_date(date_breaks = "1 months", minor_breaks = NULL, date_labels = mymonths, expand = c(0.01, 0, 0.05, 0)) +
    annotate("text", label = "q10", size = 3, x = as.Date(paste0(format(Sys.Date() + 366, "%Y"), "-01-10")), 
             y = mean(data[[station]]$median[(data[[station]]$date == paste0(format(Sys.Date(), "%Y"), "-12-31")) & (data[[station]]$q == "q10")])) +
    annotate("text", label = "q50", size = 3, x = as.Date(paste0(format(Sys.Date() + 366, "%Y"), "-01-10")), 
             y = mean(data[[station]]$median[(data[[station]]$date == paste0(format(Sys.Date(), "%Y"), "-12-31")) & (data[[station]]$q == "q50")])) +
    annotate("text", label = "q90", size = 3, x = as.Date(paste0(format(Sys.Date() + 366, "%Y"), "-01-10")), 
             y = mean(data[[station]]$median[(data[[station]]$date == paste0(format(Sys.Date(), "%Y"), "-12-31")) & (data[[station]]$q == "q90")])) +
    labs(title = title, x = "", y = ylab) +
    theme(
      panel.background = element_rect(fill = "white", colour = "grey96"),
      panel.grid.major = element_line(size = 0.2, linetype = "dotted", colour = "grey80"),
      panel.grid.minor = element_line(size = 0.2, linetype = "dotted", colour = "grey80"),
      axis.line.x = element_line(colour = "black", size = 0.3), axis.line.y = element_line(colour = "black", size = 0.3),
      axis.ticks.length.x = unit(0.15, "cm"), axis.text.x = element_text(vjust = 2, hjust = -0.3)
    ) +
    theme(
      legend.position = c(1, 1.03), legend.justification = "right", legend.key = element_rect(colour = NA, fill = NA), legend.direction = "horizontal",
      legend.background = element_rect(fill = "transparent"), legend.text = element_text(size = 8),
      plot.margin = unit(c(15, 5.5, 5.5, 5.5), "pt"), plot.title = element_text(size = 12, vjust = 2), axis.title = element_text(size = 10)
    )
  
  p <- p + facet_grid(rcp ~ ., labeller = labeller(rcp = rcp.labs))
  
  # change facet colors
  g <- ggplot_gtable(ggplot_build(p))
  # g <- ggplotGrob(p)
  strip_right <- which(grepl("strip-r", g$layout$name))
  fills <- c("#fecd07ff", "#6fb8c2ff", "#934c94ff")
  k <- 1
  for (i in strip_right) {
    j <- which(grepl("rect", g$grobs[[i]]$grobs[[1]]$childrenOrder))
    g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
    k <- k + 1
  }
  
  # output plot
  return(as.ggplot(g))
}



##________________________________________
# FUNCTION to make barplots
barplot <- function(data, title, DF) {
  # new rcp labels
  rcp.labs <- c("RCP2.6", "RCP4.5", "RCP8.5")
  names(rcp.labs) <- c("RCP26", "RCP45", "RCP85")
  
  # plot
  pl <- ggplot(data = data, aes(x = Month, y = mean, fill = Time_Period)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    geom_errorbar(aes(ymin = q10, ymax = q90), width = 0.2, position = position_dodge(0.9)) +
    scale_fill_manual(values=c("grey", "#b1dee7ff","#f8990fff","#96d776ff"), name = "", labels = c("reference period", "2035", "2060", "2085")) +
    facet_grid(rcp ~ ., labeller = labeller(rcp = rcp.labs)) +
    labs(title = title, x = "", y = "days per year") +
    theme(
      panel.background = element_rect(fill = "white", colour = "grey96"),
      panel.grid.major = element_line(linewidth = 0.2, linetype = 'dotted', colour = "grey80"), 
      panel.grid.minor = element_line(linewidth = 0.2, linetype = 'dotted',colour = "grey80"),
      axis.line.x = element_line(colour = "black", size = 0.3), axis.line.y = element_line(colour = "black", size = 0.3)) +
    guides(fill = guide_legend(direction = "horizontal")) +
    theme(legend.position = c(1, 1.03), legend.justification = "right", legend.key = element_rect(colour = NA, fill = NA), 
          legend.background = element_rect(fill = "transparent"), legend.text = element_text(size = 8),
          plot.margin = unit(c(15, 5.5, 5.5, 5.5), "pt"), plot.title = element_text(size = 12, vjust = 2), axis.title = element_text(size = 10))
  
  #ylim
 if(DF == "Flood"){
    pl <- pl + ylim(0, 10)
  }
  
  # change facet colors
  g <- ggplot_gtable(ggplot_build(pl))
  strip_right <- which(grepl('strip-r', g$layout$name))
  fills <- c("#fecd07ff", "#6fb8c2ff", "#934c94ff")
  k <- 1
  for (i in strip_right) {
    j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
    g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
    k <- k + 1
  }
  
  # output plot
  return(as.ggplot(g)) 
}


##________________________________________
# FUNCTION to make barplots
barplot_season <- function(data, title, legend = T, y_label = T) {
  
  if(y_label == T){
    ylab <- "days per year"
  } else if(y_label == F){
    ylab <- ""
  }
  # new rcp labels
  rcp.labs <- c("RCP2.6", "RCP4.5", "RCP8.5")
  names(rcp.labs) <- c("RCP26", "RCP45", "RCP85")
  
  ifelse(legend == T, leg <- c(1, 1.03), leg <- "none")
  
  # plot
  pl <- ggplot(data = data, aes(x = season, y = mean, fill = Time_Period)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    geom_errorbar(aes(ymin = q10, ymax = q90), width = 0.2, linewidth = 0.3, position = position_dodge(0.9)) +
    scale_fill_manual(values=c("grey", "#b1dee7ff","#f8990fff","#96d776ff"), name = "", labels = c("reference period", "2035", "2060", "2085")) +
    facet_grid(rcp ~ ., labeller = labeller(rcp = rcp.labs)) +
    labs(title = title, x = "", y = ylab) +
    theme(
      panel.background = element_rect(fill = "white", colour = "grey96"),
      panel.grid.major = element_line(linewidth = 0.1, linetype = 'dotted', colour = "grey80"), 
      panel.grid.minor = element_line(linewidth = 0.1, linetype = 'dotted',colour = "grey80"),
      axis.line.x = element_line(colour = "black", size = 0.3), axis.line.y = element_line(colour = "black", size = 0.3)) +
    guides(fill = guide_legend(direction = "horizontal")) +
    theme(legend.position = leg, legend.justification = "right", legend.key = element_rect(colour = NA, fill = NA), strip.text.y = element_text(size = 8), axis.text.y = element_text(size = 8),
          legend.background = element_rect(fill = "transparent"), legend.text = element_text(size = 8), legend.key.size = unit(0.2, 'cm'), axis.text.x = element_text(size = 8),
          plot.margin = unit(c(15, 5.5, 5.5, 5.5), "pt"), plot.title = element_text(size = 10, vjust = 2), axis.title = element_text(size = 8))
  
  # change facet colors
  g <- ggplot_gtable(ggplot_build(pl))
  strip_right <- which(grepl('strip-r', g$layout$name))
  fills <- c("#fecd07ff", "#6fb8c2ff", "#934c94ff")
  k <- 1
  for (i in strip_right) {
    j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
    g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
    k <- k + 1
  }
  
  # output plot
  return(as.ggplot(g)) 
}


##________________________________________
# FUNCTION to make barplots
boxpl_variables <- function(data, title, legend = T, y_label = T){
  
  # new rcp labels
  rcp.labs <- c("RCP2.6", "RCP4.5", "RCP8.5")
  names(rcp.labs) <- c("RCP26", "RCP45", "RCP85")
  
  # legend option
  ifelse(legend == T, leg <- c(1, 1.03), leg <- "none")
  
  # y-label option
  if(y_label == T){
    ylab <- "change [mm]"
  } else if(y_label == F){
    ylab <- ""
  }
  
  # plot
  p <- ggplot(data = data) +
    geom_boxplot(aes(x = Variable, y = diff, fill = period), lwd = 0.28, outlier.size = 0.28, fatten = 1.2) +
    scale_fill_manual(values=c("#b1dee7ff","#f8990fff","#96d776ff"), name = "", labels = c("2035", "2060", "2085")) +
    labs(title = title, x = "", y = ylab) +
    geom_hline(yintercept = 0, lwd = 0.3, col = "grey50") + # add horizontal line to empty ggplot
    theme(
      panel.background = element_rect(fill = "white", colour = "grey96"),
      panel.grid.major = element_line(size = 0.2, linetype = 'dotted', colour = "grey80"), 
      panel.grid.minor = element_line(size = 0.2, linetype = 'dotted',colour = "grey80"),
      axis.line.x = element_line(colour = "black", size = 0.3), axis.line.y = element_line(colour = "black", size = 0.3)) +
    guides(fill = guide_legend(direction = "horizontal")) +
    theme(legend.position = leg, legend.justification = "right", legend.key = element_rect(colour = NA, fill = NA), 
          legend.background = element_rect(fill = "transparent"), legend.text = element_text(size = 8),
          plot.margin = unit(c(15, 5.5, 5.5, 5.5), "pt"), plot.title = element_text(size = 12, vjust = 2), axis.title = element_text(size = 10)) +
    facet_grid(RCP ~ ., labeller = labeller(RCP = rcp.labs))
  
  # change facet colors
  g <- ggplot_gtable(ggplot_build(p))
  strip_right <- which(grepl('strip-r', g$layout$name))
  fills <- c("#fecd07ff", "#6fb8c2ff", "#934c94ff")
  k <- 1
  for (i in strip_right) {
    j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
    g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
    k <- k + 1
  }
  
  # output plot
  return(as.ggplot(g))
}







