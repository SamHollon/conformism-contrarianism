#
#
# REGRESSIONS
# Visualize the relationship between the model variables in scatter plots
# and perform regressions.
#
#



# =============================================================================
# --- load data ---

TransitionData <- read.csv(paste(PathData,
                                 "Invasion_ConformByPTransition.csv",
                                 sep = ""))
SampleSizeData <- read.csv(paste(PathData,
                                 "Invasion_ConformBySampleSize.csv",
                                 sep = ""))



# =============================================================================
# --- mean conformity vs. probability of environmental transition ---

# Scatter plot of mean probability of conformity at the end of the simulation
# (time step 7500) vs. the environmental transition probability. The probability
# of conformity began at 1 (i.e., fixation) and contrarians later invaded.
ggplot(TransitionData, aes(x = p.transition,
                            y = mean.p.conform)) +
  geom_point(col = "grey") +
  geom_smooth(method = "lm",
              formula = "y ~ x",
              se = F,
              col = "black") +
  xlab("Environmental Transition Probability") +
  ylab("Mean Probability of Conformity") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "grey99",
                                        colour = "grey80"),
        plot.title = element_text(hjust = 0.5))


# R^2 = 0.7863903.
cor(TransitionData$mean.p.conform, TransitionData$p.transition)^2



# =============================================================================
# --- sd conformity vs. probability of environmental transition ---

# Scatter plot of standard deviation in probability of conformity at the end of
# the simulation (time step 7500) vs. the environmental transition probability.
# The probability of conformity began at 1 (i.e., fixation) and contrarians
# later invaded.
ggplot(TransitionData, aes(x = p.transition,
                           y = sqrt(var.p.conform))) +
  geom_point(col = "grey") +
  geom_smooth(method = "loess",
              formula = "y ~ x",
              se = F,
              col = "black") +
  xlab("Environmental Transition Probability") +
  ylab("SD Probability of Conformity") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "grey99",
                                        colour = "grey80"),
        plot.title = element_text(hjust = 0.5))



# =============================================================================
# --- mean conformity vs. sample size ---

# Scatter plot of mean probability of conformity at the end of the simulation
# (time step 7500) vs. individuals' sample size. The probability of conformity
# began at 1 (i.e., fixation) and contrarians later invaded.
ggplot(SampleSizeData, aes(x = sample.size,
                           y = mean.p.conform)) +
  geom_point(col = "grey") +
  geom_smooth(method = "lm",
              formula = "y ~ x",
              se = F,
              col = "black") +
  xlab("Sample Size") +
  ylab("Mean Probability of Conformity") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "grey99",
                                        colour = "grey80"),
        plot.title = element_text(hjust = 0.5))


# R^2 = 0.2742303.
cor(SampleSizeData$mean.p.conform, SampleSizeData$sample.size)^2



# =============================================================================
# --- sd conformity vs. sample size ---

# Scatter plot of standard deviation in probability of conformity at the end of
# the simulation (time step 7500) vs. individuals' sample size. The probability
# of conformity began at 1 (i.e., fixation) and contrarians later invaded.
ggplot(SampleSizeData, aes(x = sample.size,
                           y = sqrt(var.p.conform))) +
  geom_point(col = "grey") +
  geom_smooth(method = "lm",
              formula = "y ~ x",
              se = F,
              col = "black") +
  xlab("Sample Size") +
  ylab("SD Probability of Conformity") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "grey99",
                                        colour = "grey80"),
        plot.title = element_text(hjust = 0.5))
