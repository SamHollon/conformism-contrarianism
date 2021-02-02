#
#
# PLOTS
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



# =============================================================================
# --- conformity vs. probability of environmental transition ---

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


# R^2 = 0.8394212.
cor(TransitionData$mean.p.conform, TransitionData$p.transition)^2

# Regression line equation: y = 0.9181 - 0.8342x
lm(mean.p.conform ~ p.transition, TransitionData)


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
                     `0.75` = "T = 0.75")

# Create jittered strip plots comparing mortality between the two groups
# for each transition probability.
mort.plot <- ggplot(tidy.mort, aes(x = type, y = mortality)) +
  geom_jitter() +
  facet_grid(~ p.transition, labeller = as_labeller(transition.labs)) +
  ylab("Time-Average Mortality Rate") +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        strip.text = element_text(color = "black"),
        strip.background = element_rect(fill = "grey90", color = "darkgrey"))


# Start PNG
ggsave(paste(PathFigures, "MortalityBySocialType.png", sep = ""),
       height = 4, width = 6.5)

# Add the plot to the PNG
print(mort.plot)

# END PNG
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



# ==== end ====================================================================