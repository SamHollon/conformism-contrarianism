#
#
# FIGURES
# Visualize the relationships between the model variables.
#
#



# =============================================================================
# --- load data ---

TransitionData <- read.csv(paste(PathData,
                                 "ConformByPTransition.csv",
                                 sep = ""))
MortalityData <- read.csv(paste(PathData,
                                "Mortality.csv",
                                sep = ""))
TimeData <- read.csv(paste(PathData, "FullRuns.csv", sep = ""))



# =============================================================================
# --- combine data sets ---

# Get the data for time step 7500 (for all runs) from Mortality Data.
mortality.end <- subset(MortalityData, step == 7500,
                        select = c(run.number,
                                   step,
                                   p.transition,
                                   init.prop.,
                                   init.prop.conform,
                                   sample.size,
                                   population,
                                   mean.p.conform))

# Add null missing column
mortality.end$var.p.conform = NA

# Add the above data to TransitionData.
TransitionData <- rbind(TransitionData, mortality.end)



# =============================================================================
# --- conformity vs. probability of environmental transition ---

# Scatter plot of mean probability of conformity at the end of the simulation
# (time step 7500) vs. the environmental transition probability. The probability
# of conformity began at 1 (i.e., fixation), and contrarians later invaded.
mean.conform.vs.transition <- ggplot(TransitionData,
                                     aes(x = p.transition,
                                         y = mean.p.conform)) +
  geom_smooth(method = "loess",
              formula = "y ~ x",
              se = F,
              col = "darkgrey") +
  geom_point(col = "black") +
  xlab("Environmental Transition Probability") +
  ylab("Mean Probability of Conformity") +
  scale_y_continuous(limits = c(0, 1)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "grey99", colour = "grey80"))


# Save scatter plot
ggsave(paste(PathFigures, "MeanConformVsTransition.png", sep = ""),
       height = 4, width = 6.5)
print(mean.conform.vs.transition)
dev.off()



# =============================================================================
# --- mortality by social type ---

# Get the mean of each variable for each run.
avg.mort <- aggregate(MortalityData, by = list(MortalityData$run), mean,
                      na.rm = T)

# Create a tidy data frame of mortality rates for both social types by run.
# and transition probability.
tidy.mort <- data.frame("run.number" = avg.mort$run.number,
                        "type" = "conformist",
                        "p.transition" = avg.mort$p.transition,
                        "mortality" = avg.mort$conformist.mortality)
tidy.mort <- rbind(tidy.mort,
                   data.frame("run.number" = avg.mort$run.number,
                              "type" = "contrarian",
                              "p.transition" = avg.mort$p.transition,
                              "mortality" = avg.mort$contrarian.mortality))

# Create labeller for transition probabilities.
transition.labs <- c(`0.01` = "T = 0.01",
                     `0.25` = "T = 0.25",
                     `0.75` = "T = 0.75",
                     `0.99` = "T = 0.99")

# Create jittered strip plots comparing mortality between the two groups
# for each transition probability.
mort.plot <- ggplot(tidy.mort, aes(x = type, y = mortality, color = type)) +
  geom_jitter() +
  facet_grid(~ p.transition, labeller = as_labeller(transition.labs)) +
  ylab("Time-Average Mortality Rate") +
  scale_color_manual(values = c("black", "orangered3")) +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        strip.text = element_text(color = "black"),
        strip.background = element_rect(fill = "grey90", color = "darkgrey"),
        legend.position = "none")


# Save the strip plot
ggsave(paste(PathFigures, "MortalityBySocialType.png", sep = ""),
       height = 4, width = 6.5)
print(mort.plot)
dev.off()


# Get mean and standard deviation for each social type for each of the three
# transition probabilities.

# Conformists at p.transition = 0.01: mean = 0.08259952, sd = 0.002511913.
mean(tidy.mort$mortality[tidy.mort$type == "conformist" &
                           tidy.mort$p.transition == 0.01])
sd(tidy.mort$mortality[tidy.mort$type == "conformist" &
                         tidy.mort$p.transition == 0.01])

# Contrarians at p.transition = 0.01: mean = 0.156766, sd = 0.001441084.
mean(tidy.mort$mortality[tidy.mort$type == "contrarian" &
                           tidy.mort$p.transition == 0.01])
sd(tidy.mort$mortality[tidy.mort$type == "contrarian" &
                         tidy.mort$p.transition == 0.01])

# Conformists at p.transition = 0.25: mean = 0.1192904, sd = 0.0008687777.
mean(tidy.mort$mortality[tidy.mort$type == "conformist" &
                           tidy.mort$p.transition == 0.25])
sd(tidy.mort$mortality[tidy.mort$type == "conformist" &
                         tidy.mort$p.transition == 0.25])

# Contrarians at p.transition = 0.25: mean = 0.1268624, sd = 0.000903642
mean(tidy.mort$mortality[tidy.mort$type == "contrarian" &
                           tidy.mort$p.transition == 0.25])
sd(tidy.mort$mortality[tidy.mort$type == "contrarian" &
                         tidy.mort$p.transition == 0.25])

# Conformists at p.transition = 0.75: mean = 0.1262848, sd = 0.000649301
mean(tidy.mort$mortality[tidy.mort$type == "conformist" &
                           tidy.mort$p.transition == 0.75])
sd(tidy.mort$mortality[tidy.mort$type == "conformist" &
                         tidy.mort$p.transition == 0.75])

# Contrarians at p.transition = 0.75: mean = 0.1202956, sd = 0.0005298751
mean(tidy.mort$mortality[tidy.mort$type == "contrarian" &
                           tidy.mort$p.transition == 0.75])
sd(tidy.mort$mortality[tidy.mort$type == "contrarian" &
                         tidy.mort$p.transition == 0.75])

# Conformists at p.transition = 0.99: mean = 0.127074, sd = 0.0004552011
mean(tidy.mort$mortality[tidy.mort$type == "conformist" &
                           tidy.mort$p.transition == 0.99])
sd(tidy.mort$mortality[tidy.mort$type == "conformist" &
                         tidy.mort$p.transition == 0.99])

# Contrarians at p.transition = 0.99: mean = 0.1194428, sd = 0.0003029008
mean(tidy.mort$mortality[tidy.mort$type == "contrarian" &
                           tidy.mort$p.transition == 0.99])
sd(tidy.mort$mortality[tidy.mort$type == "contrarian" &
                         tidy.mort$p.transition == 0.99])



# =============================================================================
# --- overall mortality by offspring conformity ---

# For each run, get the mean mortality for all agents of either social type.
overall.mort <-
  data.frame("run.number" = avg.mort$run.number,
             "mean.p.conform" = avg.mort$mean.p.conform,
             "mortality" = avg.mort$conformist.mortality *
               avg.mort$mean.conform +
               avg.mort$contrarian.mortality *
               (1 - avg.mort$mean.conform))


# Create scatter plot
mean.mortality.vs.conform <- ggplot(overall.mort,
                                    aes(x = mean.p.conform,
                                        y = mortality)) +
  geom_line(linetype = "dashed", size = 1, col = "darkgrey", aes(y = 0.125)) +
  geom_smooth(method = "lm",
              formula = y ~ poly(x, 5, raw = T),
              se = F,
              col = "darkgrey") +
  geom_point(col = "black") +
  xlab("Mean Probability of Conformity") +
  ylab("Time-Average Mortality") +
  # scale_y_continuous(limits = c(0, 1)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "grey99", colour = "grey80"))

# Save scatter plot
ggsave(paste(PathFigures, "MeanMortalityVsConform.png", sep = ""),
       height = 4, width = 6.5)
print(mean.mortality.vs.conform)
dev.off()



# =============================================================================
# --- create and save graphs of individual model runs ---

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
    geom_line(aes(y = mean.action), col = "black") +
    geom_line(col = "orangered1") +
    xlab("Time Step") +
    ylab("Proportion") +
    scale_x_continuous(limits = c(0, 2000), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    scale_fill_gradient(low = "grey99", high = "grey92") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_rect(fill = "white",
                                          color = "white"),
          legend.position = "none",
          plot.margin = unit(c(0.25, 0.25, 0.25, 0.25), "in"))
  
  # Save Image
  ggsave(paste(PathFigures, "Run", i , ".png", sep = ""),
         height = 4, width = 6.5)
  print(run.plot)
  dev.off()
}



# ==== end ====================================================================