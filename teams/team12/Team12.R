library(pscl)
library(psy)

setwd("/home/mrenzo/Project/")

data = read.csv(file="/home/mrenzo/Project/Dataset/1. Crowdsourcing Dataset July 01, 2014 Incl.Ref Country/CrowdstormingDataJuly1st.csv")

# looking at the data

data$refNum = factor(data$refNum)
levels(data$refNum)

by(data$player, data$refNum, summary)
by(data$redCards, data$refNum, sum)

data$rater1skincolor = ifelse(data$rater1 < 3/5, "light skin", ifelse(data$rater1 > 3/5, "dark skin", NA))
data$rater2skincolor = ifelse(data$rater2 < 3/5, "light skin", ifelse(data$rater2 > 3/5, "dark skin", NA))

data$skinrating = rowMeans(data[,18:19])
data$skincolor = ifelse(data$skinrating > 3/5, "dark skin", ifelse(data$skinrating < 3/5, "light skin", NA))

# Créer une variable binaire pour darkSkin
data$darkSkin = ifelse(data$skincolor == "dark skin", 1, 0)

# Séparer les données selon la couleur de peau
data_dark = subset(data, darkSkin == 1)
data_light = subset(data, darkSkin == 0)

# Modèle ZIP pour joueurs à peau foncée
fit.zip.4.dark <- zeroinfl(redCards ~ weight + position + games + meanIAT + meanExp , data=data_dark)
summary(fit.zip.4.dark)

# Modèle ZIP pour joueurs à peau claire
fit.zip.4.light <- zeroinfl(redCards ~ weight + position + games + meanIAT + meanExp , data=data_light)
summary(fit.zip.4.light)

# Modèle complet
ll_full <- logLik(fit.zip.4.dark)

# Modèle nul (avec seulement intercepts)
fit.null <- zeroinfl(redCards ~ 1 | 1, data = data_dark)
ll_null <- logLik(fit.null)

# Pseudo-R² de McFadden
R2_mcfadden <- 1 - as.numeric(ll_full / ll_null)
R2_mcfadden
# Modèle complet
ll_full <- logLik(fit.zip.4.light)

# Modèle nul (avec seulement intercepts)
fit.null <- zeroinfl(redCards ~ 1 | 1, data = data_light)
ll_null <- logLik(fit.null)

# Pseudo-R² de McFadden
R2_mcfadden <- 1 - as.numeric(ll_full / ll_null)
R2_mcfadden
# faible, mais acceptable pour des modèles de comptage ou ZIP
#le modèle explique environ 17.5%/14.3 % de l'information par rapport au modèle nul.
