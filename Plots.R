#
#
# PLOTS
# Visualize the relationships between the model variables.
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
TimeData <- read.csv(paste(PathData,
                           "FullRuns.csv",
                           sep = ""))



# =============================================================================
# --- mean conformity vs. probability of environmental transition ---

# Scatter plot of mean probability of conformity at the end of the simulation
# (time step 7500) vs. the environmental transition probability. The probability
# of conformity began at 1 (i.e., fixation), and contrarians later invaded.
mean.conform.vs.transition <- ggplot(TransitionData,
                                     aes(x = p.transition,
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
        panel.background = element_rect(fill = "grey99", colour = "grey80"))

# R^2 = 0.8394212.
cor(TransitionData$mean.p.conform, TransitionData$p.transition)^2



# =============================================================================
# --- sd conformity vs. probability of environmental transition ---

# Scatter plot of standard deviation in probability of conformity at the end of
# the simulation (time step 7500) vs. the environmental transition probability.
# The probability of conformity began at 1 (i.e., fixation), and contrarians
# later invaded.
sd.conform.vs.transition <- ggplot(TransitionData,
                                   aes(x = p.transition,
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
        panel.background = element_rect(fill = "grey99", colour = "grey80"))



# =============================================================================
# --- mean conformity vs. sample size ---

# Scatter plot of mean probability of conformity at the end of the simulation
# (time step 7500) vs. individuals' sample size. The probability of conformity
# began at 1 (i.e., fixation), and contrarians later invaded.
mean.conform.vs.sample.size <- ggplot(SampleSizeData,
                                      aes(x = sample.size,
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
        panel.background = element_rect(fill = "grey99", colour = "grey80"))


# R^2 = 0.2742303.
cor(SampleSizeData$mean.p.conform, SampleSizeData$sample.size)^2



# =============================================================================
# --- sd conformity vs. sample size ---

# Scatter plot of standard deviation in probability of conformity at the end of
# the simulation (time step 7500) vs. individuals' sample size. The probability
# of conformity began at 1 (i.e., fixation), and contrarians later invaded.
sd.conform.vs.sample.size <- ggplot(SampleSizeData,
                                    aes(x = sample.size,
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
        panel.background = element_rect(fill = "grey99", colour = "grey80"))



# =============================================================================
# --- save scatter plots ---

ggsave(paste(PathFigures, "MeanConformVsTransition", i , ".png", sep = ""),
    height = 4, width = 6)
print(mean.conform.vs.transition)
dev.off()

ggsave(paste(PathFigures, "SDConformVsTransition", i , ".png", sep = ""),
    height = 4, width = 6)
print(sd.conform.vs.transition)
dev.off()

ggsave(paste(PathFigures, "MeanConformVsSampleSize.png", sep = ""),
    height = 4, width = 6)
print(mean.conform.vs.sample.size)
dev.off()

ggsave(paste(PathFigures, "SDConformVsSampleSize.png", sep = ""),
    height = 4, width = 6)
print(sd.conform.vs.sample.size)
dev.off()



# =============================================================================
# --- mortality by social type ---

# Prepare data for histograms.
conformist.mortality <- TimeData$conformist.mortality
contrarian.mortality <- TimeData$contrarian.mortality

# Start PNG
png(paste(PathFigures, "MortalityBySocialType.png", sep = ""),
    height = 7, width = 6, units = "in", res = 300)

# Create a plot area with two rows and one column.
par(mfrow = c(2,1))

# Create histograms of mortality by social type.
mort.conformists <- hist(remove.outliers(conformist.mortality, dst = 0.5),
                         freq = F, xlim = c(0, 0.25), breaks = 12, las = 1,
                         main = "Conformists", xlab = "Mortality Rate",
                         ylab = "Probability Density", col = "grey80")
mort.contrarians <- hist(remove.outliers(contrarian.mortality, dst = 0.5),
                         freq = F, xlim = c(0, 0.25), breaks = 12, las = 1,
                         main = "Contrarians", xlab = "Mortality Rate",
                         ylab = "Probability Density", col = "grey80")
# End PNG
dev.off()