library(lme4)

# Lecture des données
setwd('/home/mrenzo/many-analysts-variations-revisit/')
data <- read.csv("data/dataset/1. Crowdsourcing Dataset July 01, 2014 Incl.Ref Country/CrowdstormingDataJuly1st.csv")

# Skintone moyenne
data$skintone <- rowMeans(cbind(data$rater1, data$rater2), na.rm = TRUE)

# Sous-échantillon nettoyé
d4.test <- subset(data, !is.na(skintone) & !is.na(position) & !is.na(leagueCountry) & !is.na(redCards))

# Conversion en facteur
d4.test$position <- as.factor(d4.test$position)
d4.test$leagueCountry <- as.factor(d4.test$leagueCountry)
d4.test$playerShort <- as.factor(d4.test$playerShort)
d4.test$refNum <- as.factor(d4.test$refNum)

# Variable binaire : au moins un carton rouge ou non
d4.test$RC_bin <- as.integer(d4.test$redCards > 0)

# Modèle
model <- glmer(
  RC_bin ~ position + leagueCountry + skintone + (1 | playerShort) + (1 | refNum),
  data = d4.test,
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa")
)

# Résumé
summary(model)
