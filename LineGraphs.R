#
#
# LINE GRAPHS
# Visualize over-time data from model runs.
#
#



# =============================================================================
# --- load data ---

TimeData <- read.csv(paste(PathData,
                           "FullRuns.csv",
                           sep = ""))



# =============================================================================
# --- create and save graphs of model runs ---

# Loop through all model runs.
for (i in unique(TimeData$run.number)) {
  # Create line plot for this model run.
  run.plot <- ggplot(TimeData[TimeData$run.number == i,],
                     aes(x = step,
                         y = mean.p.conform)) +
    geom_rect(aes(NULL,
                  NULL,
                  xmin = step,
                  xmax = dplyr::lead(step),
                  ymin = -Inf,
                  ymax = Inf,
                  fill = environment)) +
    geom_line(aes(y = mean.action), col = "orangered3") +
    geom_line(col = "black") +
    xlab("Time Step") +
    ylab("Mean Value") +
    scale_x_continuous(limits = c(0, 2000), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    scale_fill_gradient(low = "grey99", high = "grey92") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_rect(fill = "white",
                                          color = "white"),
          plot.title = element_text(hjust = 0.5),
          legend.position = "none",
          plot.margin = unit(c(0.25, 0.25, 0.25, 0.25), "in"))
  
  # Save Image
  ggsave(paste(PathFigures, "Run", i , ".png", sep = ""),
         height = 4,
         width = 6)
  print(run.plot)
  dev.off()
}



# =============================================================================
# --- create and save graphs of mortality rates ---

# Loop through all model runs.
for (i in unique(TimeData$run.number)) {
  # Create line plot for this model run.
  run.plot <- ggplot(TimeData[TimeData$run.number == i,],
                     aes(x = step)) +
    geom_rect(aes(NULL,
                  NULL,
                  xmin = step,
                  xmax = dplyr::lead(step),
                  ymin = -Inf,
                  ymax = Inf,
                  fill = environment)) +
    geom_line(aes(y = contrarian.mortality), col = "orangered3") +
    geom_line(aes(y = conformist.mortality), col = "black") +
    xlab("Time Step") +
    ylab("Mean Value") +
    scale_x_continuous(limits = c(0, 2000), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0.025, 0.225), expand = c(0, 0)) +
    scale_fill_gradient(low = "grey99", high = "grey92") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_rect(fill = "white",
                                          color = "white"),
          plot.title = element_text(hjust = 0.5),
          legend.position = "none",
          plot.margin = unit(c(0.25, 0.25, 0.25, 0.25), "in"))
  
  # Save Image
  ggsave(paste(PathFigures, "Mortality", i , ".png", sep = ""),
         height = 4,
         width = 6)
  print(run.plot)
  dev.off()
}