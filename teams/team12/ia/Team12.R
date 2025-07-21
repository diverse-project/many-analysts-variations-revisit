# 1. Préparation de l’environnement
library(pscl)
setwd('/home/mrenzo/many-analysts-variations-revisit/')
data <- read.csv("data/dataset/1. Crowdsourcing Dataset July 01, 2014 Incl.Ref Country/CrowdstormingDataJuly1st.csv")

# Preprocessing: Filter out rows with NA for rater1 and rater2
data <- subset(data, !is.na(rater1) & !is.na(rater2))

# Create darkSkin variable (scaled from 1 to 5 for models 1 & 2)
data$darkSkin_1to5 <- (data$rater1 + data$rater2) / 2 * 4 + 1

# Model 1: ZIP without interaction, 1–5 scaled skin
library(pscl)
fit.zip.1 <- zeroinfl(redCards ~ darkSkin_1to5 + weight + position + games + meanIAT + meanExp,
                      data = data)
summary(fit.zip.1)

# Model 2: ZIP with interaction, 1–5 scaled skin
fit.zip.2 <- zeroinfl(redCards ~ weight + position + games + darkSkin_1to5*meanIAT + darkSkin_1to5*meanExp,
                      data = data)
summary(fit.zip.2)