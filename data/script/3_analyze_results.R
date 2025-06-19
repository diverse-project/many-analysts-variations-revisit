##############################
# Title: 
# Analyses for results section
#
# Author: 
# Daniel P. Martin
##############################

rm(list = ls())

library(dplyr)
library(car)
library(poLCA)
library(psychometric)

# setwd to file path housing the folders: Scripts, Data, and Figures

# Read in data

my_data <- read.csv("Data/Crowdsourcing Effects in OR with Subgroups.csv")

############################
# Get overall descriptives 
# and subgroup descriptives
############################

# Overall: OR = 1.31, range = [0.89, 2.93]

median(my_data$OR) # OR = 1.31
range(my_data$OR)

# 0 negative, 9/29 (31%) null, 20/29 (69%) positive

sum(my_data$OR_hi < 1)
sum(my_data$OR_lo < 1 & my_data$OR_hi > 1)
sum(my_data$OR_lo > 1)

# By handling distribution

group_by(my_data, Distribution) %>%
  summarise(median_effect = median(OR),
            mad_effect = mad(OR),
            N = length(OR),
            num_sig = sum(OR_lo > 1))

# By handling non-independence

group_by(my_data, Non_independence) %>%
  summarise(median_effect = median(OR),
            mad_effect = mad(OR),
            N = length(OR),
            num_sig = sum(OR_lo > 1))

######################
# Subjective beliefs 
# over time analysis
######################

# Assess subjective beliefs (M, SD) at each time point

belief_data <- read.csv("Data/beliefs.csv", header = TRUE)

belief_data$Team <- as.numeric(gsub("[A-z]", "", belief_data$Team))

# Center subjective beliefs

belief_data$RQ1 <- belief_data$RQ1 - 3

group_by(belief_data, Time) %>%
  summarise(mean_belief = mean(RQ1),
            sd_belief = sd(RQ1))

table(belief_data[, c("RQ1", "Time")])

# Correlate final effect size with beliefs at each stage

data_total <- merge(my_data, belief_data, by = "Team", all = TRUE)

group_by(data_total, Time) %>%
  summarise(cor_time = round(cor(RQ1, OR, method = "spearman"), 2))

# Get CIs

spearman_CI <- function(rho, n){
  
  return (c(tanh(atanh(rho) - 1.96/(sqrt(n - 3))),
           tanh(atanh(rho) + 1.96/(sqrt(n - 3)))))

}

round(spearman_CI(rho = 0.14, n = 28), 2)
round(spearman_CI(rho = -0.20, n = 28), 2)
round(spearman_CI(rho = 0.43, n = 28), 2)
round(spearman_CI(rho = 0.41, n = 28), 2)

# Correlations with the lower bound

group_by(data_total, Time) %>%
  summarise(cor_low = round(cor(RQ1, OR_lo, method = "spearman"), 2))

# Get CIs

round(spearman_CI(rho = 0.29, n = 28), 2)
round(spearman_CI(rho = -0.10, n = 28), 2)
round(spearman_CI(rho = 0.52, n = 28), 2)
round(spearman_CI(rho = 0.58, n = 28), 2)

###########################
# Analyze final results
# of 10 teams who performed
# a re-analysis
###########################

final_results <- read.csv("Data/After Final Beliefs Comparison.csv")

# Mean and sd difference between the two time points

apply(final_results[, c("result_final", "result_after_final")], 2, median)
apply(final_results[, c("result_final", "result_after_final")], 2, mad)

# Count those changing significance (5 changes, 4 became positive, 1 negative)

sum(final_results$final_sig != final_results$after_final_sig)

final_results[final_results$final_sig != final_results$after_final_sig, ]

# Correlation of beliefs between the two time points

cor(final_results[, c("belief_final", "belief_after_final")])

cohens_d <- function(x, y) {
  
  lx <- length(x) - 1
  ly <- length(y) - 1
  md  <- abs(mean(x) - mean(y))       
  csd <- lx * var(x) + ly * var(y)
  csd <- csd/(lx + ly)
  csd <- sqrt(csd)                     
  return(md/csd)   
  
}

cohens_d(final_results$belief_final, final_results$belief_after_final)

apply(final_results[, c("belief_final", "belief_after_final")], 2, mean)

#######################
# Analyze descriptives 
# with team confidence
#######################

# Read in ratings of analysis quality (i.e., confidence in analytic choices)

confidence <- read.csv("Data/Analysis Quality Ratings.csv")
confidence$Team <- as.numeric(gsub("[A-z]", "", confidence$Team))

# Calculate M and SD of number of reviews

mean(confidence$RQ1_Count)
sd(confidence$RQ1_Count)

# Merge confidence with effect size data (missing team 32)

conf_data <- merge(my_data, confidence, by = "Team")

# Calculate statistics with respect to confidence (cutoff at 4)

filter(conf_data, RQ1_Confidence >= 4) %>%
  summarise(median(OR), mad(OR))

filter(conf_data, RQ1_Confidence < 4) %>%
  summarise(median(OR), mad(OR))

# Correlation of 0.05, rho of .10 (more robust)

cor(conf_data[, c("RQ1_Confidence", "OR")], method = "pearson")
cor(conf_data[, c("RQ1_Confidence", "OR")], method = "spearman")

pairs(conf_data[, c("RQ1_Confidence", "OR")])

# Get rho CI

round(spearman_CI(rho = 0.10, n = 28), 2)

#######################
# Analyze descriptives 
# for stats expertise
#######################

expertise <- read.csv("Data/Quant Expertise.csv")
names(expertise)[3:7] <- c("Degree", "Position", "Understats", "Gradstats", "Papers")

# Calculate frequencies for degree, position, teaching, and papers

table(expertise[, "Degree"])
prop.table(table(expertise[, "Degree"]))

table(expertise[, "Position"])
prop.table(table(expertise[, "Position"]))

table(expertise[, "Understats"])
prop.table(table(expertise[, "Understats"]))

table(expertise[, "Gradstats"])
prop.table(table(expertise[, "Gradstats"]))

table(expertise[, "Papers"])
prop.table(table(expertise[, "Papers"]))


# Get max (or min) value for each team

expertise_clean <- group_by(expertise, Team) %>%
  summarise(Degree = min(Degree, na.rm = TRUE),
            Position = min(Position, na.rm = TRUE),
            Understats = max(Understats, na.rm = TRUE),
            Gradstats = max(Gradstats, na.rm = TRUE),
            Papers = max(Papers, na.rm = TRUE))

summary(expertise_clean)

# Merge expertise with effect size

expertise_effect <- merge(my_data, expertise_clean, by = "Team")

# Reduce the number of categories for papers written (at least one) and academic position (merge all professors)
# so there are enough df for the latent class analysis

expertise_effect$Papers <- Recode(expertise_effect$Papers,
                                  "1 = '1'; 6 = '3'; else = '2'")

expertise_effect$Position <- Recode(expertise_effect$Position,
                                    "4 = '2'; 5 = '3'; 6 = '4'; else = '1'")

lca_formula <- cbind(Degree, Position, Gradstats, Papers) ~ 1
lca_results <- poLCA(lca_formula, expertise_effect, nrep = 100)

# Second group more likely to have a member
# who has a PhD, is a professor, has taught grad stats, and 
# has at least one methodological publications
# Do they differ in median effect size or significance?

expertise_effect$expertise_group <- lca_results$predclass

group_by(expertise_effect, expertise_group) %>%
  summarise(median_effect = median(OR),
            mad_effect = mad(OR),
            N = length(OR),
            num_null = sum(OR_lo < 1 & OR_hi > 1),
            num_sig = sum(OR_lo > 1))

##########################
# Examine final thoughts
##########################

# Examine reasons behind effect

final_thoughts <- read.csv("Data/Final Research Conclusions.csv")
thoughts_sub <- final_thoughts[, c(1, grep("Whichofthefollowing", names(final_thoughts)))]

names(thoughts_sub) <- c("id", "pos_biased", "pos_unobserved", "pos_unknown", "pos_outlier", "pos_observed",
                         "little_evid", "no_evid", "neg_rel")

round(rbind(apply(thoughts_sub[,-1], 2, mean, na.rm = TRUE),
      apply(thoughts_sub[,-1], 2, sd, na.rm = TRUE)), 2)

# Examine appropriateness of the data set

suitable <- final_thoughts[, c(1, grep("suitableforanswering", names(final_thoughts)))]
names(suitable) <- c("id", "RQ1", "RQ2a", "RQ2b")

# unconfident - somewhat unconfident
prop.table(table(suitable$RQ1)) # 32%
prop.table(table(suitable$RQ2a)) # 75%
prop.table(table(suitable$RQ2b)) # 72%

#######################
# Calculate predicted 
# probabilities for a 
# single player over
# a 40 game season
#######################

# Using intercept value of -5.76 
# from Team 3

# log odds, odds, and probability
# for lightest skin tone

# log odds
-5.76 + 0*0.27
# odds
exp(-5.76)
# prob
light_prob <- exp(-5.76) / (1 + exp(-5.76))

# log odds, odds, and probability
# for darkest skin tone

# log odds
-5.76 + 1*0.27
# odds
exp(-5.49)
# prob
dark_prob <- exp(-5.49) / (1 + exp(-5.49))

# check OR = 1.31
exp(-5.49)/exp(-5.76)

# Probability of at least one card in n games:
# 1 - (1 - probability of receiving card)^n

# darkest skin tone: 15.2%
1 - (1 - dark_prob)^40

# lightest skin tone: 11.8%
1 - (1 - light_prob)^40










