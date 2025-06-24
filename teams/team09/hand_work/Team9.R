require(lme4)

setwd('/home/mrenzo/many-analysts-variations-revisit/')
RC <- read.csv(file="data/dataset/1. Crowdsourcing Dataset July 01, 2014 Incl.Ref Country/CrowdstormingDataJuly1st.csv")

glmer(RC ~ position + leagueCountry + skintone + (1|playerShort) + (1|refNum), d4.test, family=binomial(link="logit"), control=glmerControl(optimizer="bobyqa"))

#rien de plus n'est donné