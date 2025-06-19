##############################
# Title: 
# Create subjective belief in
# effect over time figure
#
# Author: 
# Daniel P. Martin
##############################

rm(list = ls())

library(ggplot2)
library(car)
library(gridExtra)
library(dplyr)
library(reshape2)

# setwd to file path housing the folders: Scripts, Data, and Figures

# Read in subjective beliefs data, already in long format (1 = unlikely, 5 = likely)

my_data <- read.csv("Data/beliefs updated.csv")

# Add consistent jitter to each individual

jitter <- rep(rnorm(n = length(unique(my_data$Team)), mean = 0, sd = 0.08), 4)

my_data <- mutate(my_data,
                  RQ1_jitter = RQ1 + jitter,
                  RQ2a_jitter = RQ2a + jitter,
                  RQ2b_jitter = RQ2b + jitter)

data_long <- melt(data = my_data, id.vars = c("Team", "Time"))
data_long$Time <- Recode(as.character(data_long$Time),
                          "'Analytic Approach' = 'Analytic\\nApproach';
                          'Final Report' = 'Final\\nReport';
                          'After Discussion' = 'After\\nDiscussion'")

data_long$Time <- factor(data_long$Time, levels = c("Registration", "Analytic\nApproach", "Final\nReport", "After\nDiscussion"))

# Rescale scale for intuitive value

data_long$value <- data_long$value - 3

######################
# RQ1 plot for belief
# trajectories and 
# frequency tiles
######################

# Create plot for trajectories
  
rq1_traj <- ggplot(data = data_long[data_long$variable == "RQ1_jitter", ], aes(x = Time, y = value, group = 1)) + 
  geom_line(aes(group = Team), color = "#7e7e7e") + ylim(-2.5, 2.5) + 
  stat_summary(fun.y = mean, size = 3, color = "black", geom = "line") + theme_bw() + 
  ylab("Researcher Subjective Belief in the Effect for Research Question 1 \n(-2 = Very Unlikely, 2 = Very Likely)") + 
  theme(text = element_text(size = 13),
        axis.title.x = element_text(vjust = -0.5),
        axis.title.y = element_text(vjust = 1.3)) 

# Create plot for frequency tiles

rq1_freq <- group_by(data_long, Time, variable, value) %>%
  summarise(freq = length(value)) %>%
  filter(variable == "RQ1")

rq1_tile <- ggplot(data = rq1_freq, aes(x = Time, y = value)) + geom_tile(aes(fill = freq), color = "black") +
  scale_fill_gradient(name = "Number of\nTeams", low = "#f6f6f6", high = "#000000") + theme_bw() + ylab("") +
  theme(text = element_text(size = 13),
        axis.title.x = element_text(vjust = -0.5),
        axis.title.y = element_text(vjust = 1.3)) 

# Save plot as a pdf

pdf("Figures/belief_plot_RQ1.pdf", width = 14, height = 6)
grid.arrange(rq1_traj, rq1_tile, nrow = 1)
dev.off()

######################
# RQ2 plot for belief
# trajectories and 
# frequency tiles
######################

# Create function to label facets for RQ2a and RQ2b

research_question <- list(
  'RQ1' = "Research Quesion 1",
  'RQ2a' = "Research Question 2a",
  'RQ2b' = "Research Question 2b"
)

facet_labeller <- function(variable, value){
  
  return(research_question[value])
  
}

# Belief trajectories for RQ2a and RQ2b

rq2_traj <- ggplot(data = data_long[data_long$variable %in% c("RQ2a_jitter", "RQ2b_jitter"), ], aes(x = Time, y = value, group = 1)) + 
  geom_line(aes(group = Team), color = "#7e7e7e") + ylim(-2.5, 2.5) + 
  stat_summary(fun.y = mean, size = 3, color = "black", geom = "line") + theme_bw() + 
  facet_grid(variable ~ ., labeller = facet_labeller) + 
  ylab("Researcher Subjective Belief in the Effect for Research Question 2 \n(-2 = Strongly Disagree, 2 = Strongly Agree)") + 
  theme(text = element_text(size = 13),
        axis.title.x = element_text(vjust = -0.5),
        axis.title.y = element_text(vjust = 1.3),
        strip.background = element_blank(),
        strip.text.x = element_blank()) 

# Create plot for frequency tiles

rq2_freq <- group_by(data_long, Time, variable, value) %>%
  summarise(freq = length(value)) %>%
  filter(variable %in% c("RQ2a", "RQ2b"))

rq2_tile <- ggplot(data = rq2_freq, aes(x = Time, y = value)) + geom_tile(aes(fill = freq), color = "black") +
  scale_fill_gradient(name = "Number of\nTeams", low = "#f6f6f6", high = "black") + theme_bw() + ylab("") +
  facet_grid(variable ~ ., labeller = facet_labeller) + ylim(-2.5, 2.5) + 
  theme(text = element_text(size = 13),
        axis.title.x = element_text(vjust = -0.5),
        axis.title.y = element_text(vjust = 1.3)) 

grid.arrange(rq2_traj, rq2_tile, nrow = 1)

# Save plots as a pdf for supplemental section

pdf("Figures/belief_plot_RQ2.pdf", width = 14, height = 12)
grid.arrange(rq2_traj, rq2_tile, nrow = 1)
dev.off()

