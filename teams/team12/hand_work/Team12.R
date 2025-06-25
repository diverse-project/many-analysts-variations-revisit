library(pscl)
library(psy)

setwd('/home/mrenzo/many-analysts-variations-revisit/')
data <- read.csv(file="data/dataset/1. Crowdsourcing Dataset July 01, 2014 Incl.Ref Country/CrowdstormingDataJuly1st.csv")

# looking at the data

data$refNum = factor(data$refNum)
levels(data$refNum)

by(data$player, data$refNum, summary)
by(data$redCards, data$refNum, sum)

data$rater1skincolor = ifelse(data$rater1 < 3/5, "light skin", ifelse(data$rater1 > 3/5, "dark skin", NA))
data$rater2skincolor = ifelse(data$rater2 < 3/5, "light skin", ifelse(data$rater2 > 3/5, "dark skin", NA))

data$skinrating = rowMeans(data[,18:19])*5
data$skincolor = ifelse(data$skinrating > 3, "dark skin", ifelse(data$skinrating < 3, "light skin", NA))

# Créer une variable binaire pour darkSkin
data$darkSkin = ifelse(data$skincolor == "dark skin", 1, 0)

# Séparer les données selon la couleur de peau
data_dark = subset(data, darkSkin == 1)
data_light = subset(data, darkSkin == 0)


fit.zip.1 <- zeroinfl(redCards ~ skinrating + weight + games + meanIAT + meanExp, data=data)
summary(fit.zip.1)

fit.zip.2 <- zeroinfl(redCards ~ skinrating + weight + games + meanIAT + meanExp + darkSkin*meanIAT + darkSkin*meanExp , data=data)
summary(fit.zip.2)

fit.zip.3 <- zeroinfl(redCards ~ darkSkin + weight + games + meanIAT + meanExp, data=data)
summary(fit.zip.3)

fit.zip.4 <- zeroinfl(redCards ~ darkSkin + weight + games + meanIAT + meanExp  + darkSkin*meanIAT + darkSkin*meanExp , data=data)
summary(fit.zip.4)
