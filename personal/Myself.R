# Load required libraries
library(dplyr)
library(sandwich)
library(lmtest)
library(multiwayvcov)   # for clustering SEs
library(broom)
library(car)

# Load data
setwd("/home/mrenzo/Project/")
data <- read.csv("Dataset/1. Crowdsourcing Dataset July 01, 2014 Incl.Ref Country/CrowdstormingDataJuly1st.csv",nrows = 60000)

# Clean and construct variables
data <- data %>%
  mutate(
    skin = rowMeans(cbind(rater1, rater2), na.rm = TRUE),  # 1 if darker skin, 0 otherwise
    redCardBin = ifelse(redCards > 0, 1, 0),                                 # binary outcome
    playerShort = factor(playerShort),
    position = factor(position),
    refNum = factor(refNum)                                                 # factorize referee ID
  ) %>%
  filter(!is.na(skin), !is.na(redCardBin), !is.na(weight), !is.na(height),
         !is.na(games), !is.na(goals), !is.na(victories), !is.na(refNum))

# Clustered SE function (by player)
cluster_se <- function(model, cluster_var) {
  cl_vcov <- cluster.vcov(model, cluster = data[[cluster_var]])
  coeftest(model, cl_vcov)
}

# Re-estimer le modèle avec interactions skin × refNum
model_interact <- lm(redCardBin ~ skin * refNum + position + weight + height + games + goals + victories, data = data)

# Extraire les coefficients des interactions skin:refNum
interaction_terms <- tidy(model_interact) %>%
  filter(grepl("skin:refNum", term)) %>%
  mutate(
    refNum = gsub("skin:refNum", "", term),
    refNum = gsub("[()]", "", refNum)
  )

# Ajouter les IC (approximation normale)
confint_interact <- confint(model_interact)
interaction_ci <- confint_interact[grep("skin:refNum", rownames(confint_interact)), ]
interaction_terms <- interaction_terms %>%
  mutate(ci_low = interaction_ci[, 1],
         ci_high = interaction_ci[, 2])

# Compter le nombre d’observations pour chaque refNum
ref_sizes <- data %>%
  group_by(refNum) %>%
  summarise(n = n()) %>%
  ungroup()

# Fusionner les tailles avec les effets
interaction_terms <- left_join(interaction_terms, ref_sizes, by = "refNum")

# Trier les arbitres selon l’effet le plus élevé de skin
interaction_terms_sorted <- interaction_terms %>%
  arrange(desc(estimate))

# Afficher les 10 arbitres avec l’effet le plus élevé
head(interaction_terms_sorted, 10)
