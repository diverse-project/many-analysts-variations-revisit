library(pscl)

setwd('/home/mrenzo/many-analysts-variations-revisit/')
data <- read.csv(file="data/dataset/1. Crowdsourcing Dataset July 01, 2014 Incl.Ref Country/CrowdstormingDataJuly1st.csv")

# looking at the data

data$refNum = factor(data$refNum)
levels(data$refNum)

by(data$player, data$refNum, summary)
by(data$redCards, data$refNum, sum)


data$skinrating = rowMeans(data[,18:19])*4+1
data$skincolor = ifelse(data$skinrating > 2, "dark skin", ifelse(data$skinrating < 2, "light skin", NA))


fit.zip.4 <- zeroinfl(redCards ~ skincolor + weight + position + games + meanIAT + meanExp , data=data)
summary(fit.zip.4)

fit.zip.4.int <- zeroinfl(redCards ~ weight + position + games + darkSkin*meanIAT + darkSkin*meanExp , data=data)
summary(fit.zip.4.int)
