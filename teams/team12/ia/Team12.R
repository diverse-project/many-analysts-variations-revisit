# 1. Préparation de l’environnement
library(pscl)
setwd('/home/mrenzo/many-analysts-variations-revisit/')
data <- read.csv("data/dataset/1. Crowdsourcing Dataset July 01, 2014 Incl.Ref Country/CrowdstormingDataJuly1st.csv")

# 2. Prétraitement
# Création de la variable "darkSkin" par moyenne des évaluations
data$darkSkin <- rowMeans(cbind(data$rater1, data$rater2), na.rm = TRUE)*4+1

# Suppression des cas sans évaluation de couleur de peau
data <- subset(data, !is.na(darkSkin))

# Normalisation entre 0 et 1 pour la version finale (Modèles 3 et 4)
data$darkSkin_scaled <- rowMeans(cbind(data$rater1, data$rater2), na.rm = TRUE)

# 3. Variables explicatives
# - weight, position, games: contrôles
# - meanIAT, meanExp: biais implicites et explicites

# 4. Modèles initiaux (1 et 2) avec l’échelle de couleur 1 à 5
fit.zip.1 <- zeroinfl(redCards ~ darkSkin + weight + position + games + meanIAT + meanExp, data=data)
fit.zip.2 <- zeroinfl(redCards ~ weight + position + games + darkSkin*meanIAT + darkSkin*meanExp, data=data)

# 5. Modèles finaux (3 et 4) avec darkSkin entre 0 et 1 et effets fixes par arbitre
data$referee <- factor(data$refNum)

fit.zip.3 <- zeroinfl(redCards ~ darkSkin_scaled + weight + position + games + meanIAT + meanExp + referee, data=data)
fit.zip.4 <- zeroinfl(redCards ~ weight + position + games + darkSkin_scaled*meanIAT + darkSkin_scaled*meanExp + referee, data=data)

# 6. Résumés des modèles
summary(fit.zip.1)
summary(fit.zip.2)
summary(fit.zip.3)
summary(fit.zip.4)
