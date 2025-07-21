setwd("yourdir") # Remplacez « yourdir » par la direction où se trouve le CSV
library(readr)
library(pscl)

data <- read_csv(file="data/dataset/1. Crowdsourcing Dataset July 01, 2014 Incl.Ref Country/CrowdstormingDataJuly1st.csv", col_types = cols(.default = col_double()))

# Standardizer les ratings entre 0 et 1 pour les modèles 1 et 2, mais garder le 0 à 1 pour le modèle 3 et 4
data$darkSkin[data$darkSkin > 1] <- 1
data$darkSkin[data$darkSkin < 0] <- 0

# Supprimer les lignes où les ratings sont manquants
data <- na.omit(data)

# Créer la variable de la régression (multinomiale pour le modèle 4)
data$redCards_bipolar <- ifelse(data$redCards > 0, 1, 0)

# Appliquer le modèle bilog de la distribution ZIP
fit.zip.4 <- zeroinfl(redCards_bipolar ~ darkSkin + weight + position + games + meanIAT + meanExp , data=data)
summary(fit.zip.4)

fit.zip.4.int <- zeroinfl(redCards_bipolar ~ weight + position + games + darkSkin*meanIAT + darkSkin*meanExp , data=data)
summary(fit.zip.4.int)