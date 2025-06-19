# Crowdstorming ZIP Models – Reproduction in R

# Load packages
library(dplyr)
library(pscl)
library(broom)

# Set working directory and load data
setwd("/home/mrenzo/Project/")
data <- read.csv("Dataset/1. Crowdsourcing Dataset July 01, 2014 Incl.Ref Country/CrowdstormingDataJuly1st.csv")

# Filter out rows with missing rater scores
data <- data %>%
  filter(!is.na(rater1), !is.na(rater2)) %>%
  mutate(
    dark_skin_raw = (rater1 + rater2) / 2,
    redCards = redCards,
    games = games,
    weight = weight,
    position = factor(position),
    referee = factor(refNum),
    mean_IAT = meanIAT,
    mean_Exp = meanExp
  )

# -----------------------
# MODELS 1 & 2 (scale 1–5)
# -----------------------
data_1_2 <- data %>%
  mutate(dark_skin = 1 + 4 * dark_skin_raw)  # rescale to [1,5]

# Model 1 – no interactions
m1 <- zeroinfl(redCards ~ dark_skin + weight + games + mean_IAT + mean_Exp + position |
                             dark_skin + weight + games + mean_IAT + mean_Exp + position,
               data = data_1_2, dist = "poisson", EM = TRUE)

# Model 2 – with interactions
m2 <- zeroinfl(redCards ~ dark_skin * mean_IAT + dark_skin * mean_Exp + weight + games + mean_IAT + mean_Exp + position |
                             dark_skin * mean_IAT + dark_skin * mean_Exp + weight + games + mean_IAT + mean_Exp + position,
               data = data_1_2, dist = "poisson", EM = TRUE)

# -----------------------
# MODELS 3 & 4 (scale 0–1) + referee fixed effects
# -----------------------
data_3_4 <- data %>%
  mutate(dark_skin = dark_skin_raw)  # keep scale [0,1]

# Model 3 – no interactions
m3 <- zeroinfl(redCards ~ dark_skin + weight + games + mean_IAT + mean_Exp + position + referee |
                             dark_skin + weight + games + mean_IAT + mean_Exp + position + referee,
               data = data_3_4, dist = "poisson", EM = TRUE)

# Model 4 – with interactions
m4 <- zeroinfl(redCards ~ dark_skin * mean_IAT + dark_skin * mean_Exp + weight + games + mean_IAT + mean_Exp + position + referee |
                             dark_skin * mean_IAT + dark_skin * mean_Exp + weight + games + mean_IAT + mean_Exp + position + referee,
               data = data_3_4, dist = "poisson", EM = TRUE)

# -----------------------
# Display results
# -----------------------
cat("\n===== MODEL 1 (ZIP – no interaction – scale 1–5) =====\n")
print(summary(m1))

cat("\n===== MODEL 2 (ZIP – interactions – scale 1–5) =====\n")
print(summary(m2))

cat("\n===== MODEL 3 (ZIP – no interaction – scale 0–1 + referee) =====\n")
print(summary(m3))

cat("\n===== MODEL 4 (ZIP – interactions – scale 0–1 + referee) =====\n")
print(summary(m4))

# Optional: Extract tidy coefficients for tables
tidy_results <- list(
  Model1 = tidy(m1),
  Model2 = tidy(m2),
  Model3 = tidy(m3),
  Model4 = tidy(m4)
)

# Save to CSV if needed
# write.csv(tidy_results$Model4, "model4_results.csv")
