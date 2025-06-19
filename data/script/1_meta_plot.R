##############################
# Title: 
# Create meta-results figure
# with adjacent table
#
# Author: 
# Daniel P. Martin
##############################

rm(list = ls())

library(ggplot2)
library(gridExtra)
library(dplyr)

# setwd to file path housing the folders: Scripts, Data, and Figures

# Read in data on team methods and results

my_data <- read.csv("Data/Crowdsourcing Reported Effects.csv")

####################################
# Conversion rules:
#
# d to OR
# d * pi/sqrt(3)
# (2r/sqrt(1 - r^2) ) * pi/sqrt(3)
#
# r to d
# 2r/sqrt(1 - r^2)
###################################

# Convert each separately, then bind together

OR_results <- filter(my_data, Effect.size.units == 'OR' | Effect.size.units == 'IRR') %>%
  mutate(OR = Estimate,
         OR_lo = Low,
         OR_hi = Hi)

D_results <- filter(my_data, Effect.size.units == 'D') %>%
  mutate(OR = exp(Estimate * pi/sqrt(3)),
         OR_lo = exp(Low * pi/sqrt(3)),
         OR_hi = exp(Hi * pi/sqrt(3)))

R_results <- filter(my_data, Effect.size.units == 'R') %>%
  mutate(OR = exp((2 * Estimate)/sqrt(1 - Estimate^2) * pi/sqrt(3)),
         OR_lo = exp((2 * Low)/sqrt(1 - Low^2) * pi/sqrt(3)),
         OR_hi = exp((2 * Hi)/sqrt(1 - Hi^2) * pi/sqrt(3)))

my_results <- arrange(rbind(OR_results, D_results, R_results), Team)

# Save converted results to code subgroups for analysis
write.csv(my_results, "Data/Crowdsourcing Effects in OR.csv", row.names = FALSE)

################################################################################

# Create meta-plot

data_total <- data.frame(Team = factor(c(my_data$Team, 0), levels = c(my_data$Team, 0)),
                         Approach = c(as.character(my_data$Analytic.Approach), NA),
                         OR = c(my_results$OR, NA),
                         Low = c(my_results$OR_lo, NA),
                         Hi = c(my_results$OR_hi, NA))

data_total <- arrange(data_total, OR)

data_total$Team <- factor(data_total$Team,
                          levels = c(rev(as.numeric(as.character(data_total$Team))[-30]), 0))

# Re-label to include all on the same figure, with the last two having an asterisk 

data_total[na.omit(data_total$Hi) > 6, ]$Hi <- 5

p <- ggplot(data_total, aes(OR, Team)) + 
  geom_point(size = 3) +
  geom_errorbarh(aes(xmax = Hi, xmin = Low), height = 0.5) +
  geom_vline(xintercept = 1, linetype = "longdash") +
  scale_x_continuous(breaks = seq(-3, 6, 1), labels = seq(-3,6,1)) +
  geom_text(x = 5.1, y = 1, label = "*", size = 6) +
  geom_text(x = 5.1, y = 2, label = "*", size = 6) + 
  labs(x = "Odds Ratio", y = "") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.margin = unit(c(0,0,0,0), "lines"))

# Create label table

lab <- data.frame(V0 = data_total$Team,
                  V05 = rep(c(1.9, 2, 2.5), each = nrow(data_total)),
                  V1 = c(as.character(data_total$Team)[-30], "Team",
                         as.character(data_total$Approach)[-30], "Analytic Approach",
                         format(round(data_total$OR[-30], 2), nsmall = 2), "OR"))

data_table <- ggplot(lab, aes(x = V05, y = V0, label = format(V1, nsmall = 1))) +
  geom_text(size = 4, hjust = 0, vjust = 0.5) + theme_bw() +
  geom_hline(aes(yintercept = c(29.5))) + geom_hline(aes(yintercept = c(30.5))) + 
  theme(panel.grid.major = element_blank(), 
        legend.position = "none",
        panel.border = element_blank(), 
        axis.text.x = element_text(colour="white"),
        axis.text.y = element_blank(), 
        axis.ticks = element_line(colour="white"),
        plot.margin = unit(c(0,0,0,0), "lines")) +
  labs(x = "",y = "") +
  coord_cartesian(xlim = c(1.9, 2.6))


# Save plot as a pdf

pdf('Figures/results_summary.pdf', width = 18, height = 8)
grid.arrange(data_table, p, ncol = 2)
dev.off()
