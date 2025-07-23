# CrowdStorming Data Analysis - Final Approach

# Research Questions:
# 1. Are soccer referees more likely to give red cards to dark skin toned players than light skin toned players?
# 2. Are soccer referees from countries high in skintone prejudice more likely to award red cards to dark skin toned players?

# Load required packages
library(lme4)       # For mixed effects models
library(psych)      # For reliability analysis
library(sandwich)   # For robust standard errors
library(lmtest)     # For model comparisons

# Import the data
setwd('/home/mrenzo/many-analysts-variations-revisit/')
data <- read.csv(file="data/dataset/1. Crowdsourcing Dataset July 01, 2014 Incl.Ref Country/CrowdstormingDataJuly1st.csv")

# Data exploration
head(data)
summary(data)

# Check inter-rater reliability for skin tone ratings
cronbach_alpha <- psych::alpha(data[, c("rater1", "rater2")])
print(cronbach_alpha)  # High reliability confirmed (r = 0.92)

# Create averaged skin tone rating (continuous)
data$skin_tone <- rowMeans(data[, c("rater1", "rater2")], na.rm = TRUE)

# Convert factors
data$player <- as.factor(data$player)
data$refNum <- as.factor(data$refNum)
data$position <- as.factor(data$position)

# Scale skin tone ratings if needed (though PDF doesn't mention this)
# But according to PDF, keep ratings as continuous between 1-5

# ------------------------------------------------------------------
# FINAL ANALYSIS - HYPOTHESIS 1
# Are soccer referees more likely to give red cards to dark skin toned players?

# Poisson GLMM with random effects for referees and players
model1 <- glmer(redCards ~ skin_tone + position + (1 | refNum) + (1 | player), 
                data = data, 
                family = poisson, 
                offset = log(games),
                control = glmerControl(optimizer = "bobyqa"))

# Model summary
summary(model1)

# Calculate exponentiated coefficients and CIs
fixed_effects <- fixef(model1)
conf_int <- confint(model1, method = "Wald")

# Skin tone effect (main result)
skin_tone_effect <- exp(c(fixed_effects["skin_tone"], conf_int["skin_tone", ]))
names(skin_tone_effect) <- c("exp(beta)", "2.5% CI", "97.5% CI")
print(skin_tone_effect)

# ------------------------------------------------------------------
# FINAL ANALYSIS - HYPOTHESIS 2
# Are referees from high-prejudice countries more likely to give red cards to dark skin players?

# Center the skin tone variable for interaction
data$skin_tone_c <- data$skin_tone - mean(data$skin_tone, na.rm = TRUE)

# Model with IAT interaction
model2a <- glmer(redCards ~ skin_tone_c * meanIAT + position + (1 | refNum) + (1 | player),
                 data = data,
                 family = poisson,
                 offset = log(games),
                 control = glmerControl(optimizer = "bobyqa"))

# Model with Explicit bias interaction
model2b <- glmer(redCards ~ skin_tone_c * meanExp + position + (1 | refNum) + (1 | player),
                 data = data,
                 family = poisson,
                 offset = log(games),
                 control = glmerControl(optimizer = "bobyqa"))

# Summarize results
summary(model2a)
summary(model2b)

# Interaction effects
int_iat <- exp(c(fixef(model2a)["skin_tone_c:meanIAT"], 
                confint(model2a, method = "Wald")["skin_tone_c:meanIAT", ]))
names(int_iat) <- c("exp(beta)", "2.5% CI", "97.5% CI")

int_exp <- exp(c(fixef(model2b)["skin_tone_c:meanExp"], 
                confint(model2b, method = "Wald")["skin_tone_c:meanExp", ]))
names(int_exp) <- c("exp(beta)", "2.5% CI", "97.5% CI")

print("IAT interaction effect:")
print(int_iat)

print("Explicit bias interaction effect:")
print(int_exp)

# ------------------------------------------------------------------
# Additional diagnostic checks

# Check for overdispersion in final models
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model, type = "pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df = rdf, lower.tail = FALSE)
  c(chisq = Pearson.chisq, ratio = prat, rdf = rdf, p = pval)
}

# Check main model
print("Overdispersion check for main model:")
print(overdisp_fun(model1))

# Check interaction models
print("Overdispersion check for IAT interaction model:")
print(overdisp_fun(model2a))
print("Overdispersion check for Exp interaction model:")
print(overdisp_fun(model2b))

# Goodness of fit (deviance)
print("Deviance for models:")
print(c("Model1" = deviance(model1), 
        "Model2a" = deviance(model2a),
        "Model2b" = deviance(model2b)))