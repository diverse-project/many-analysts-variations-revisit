##VARIABLE CREATION AND RECODING##
library(nlme)

soc <- read.csv("/home/mrenzo/Project/Dataset/1. Crowdsourcing Dataset July 01, 2014 Incl.Ref Country/CrowdstormingDataJuly1st.csv")

##label variables as factors##
soc$refCountry<-factor(soc$refCountry)
soc$refNum<-factor(soc$refNum)

##aggregate and standardize IVS and DVS##
soc$meanrate<-rowMeans(cbind(soc$rater1, soc$rater2))
soc$ztone<-scale(soc$meanrate)
soc$zrcard<-scale(soc$redCards)
soc$zycard<-scale(soc$yellowCards)
soc$zyrcard<-scale(soc$yellowReds)
soc$zgames<-scale(soc$games)
soc$zIAT<-scale(soc$meanIAT)
soc$zExp<-scale(soc$meanExp)

##Body-Mass Index##
soc$bmi<-(soc$weight/soc$height^2)*10000

##total carded fouls (bookings)##
soc$totbook<-soc$redCards+soc$yellowCards+2*soc$yellowReds
soc$zbook<-scale(soc$totbook)

##player losing percentage within each referee dyad##
soc$losep<-soc$defeats/soc$games

##player scoring rate within each referee dyad##
soc$gpgame<-soc$goals/soc$games

##recoding skin tone into African-appearing vs. Non-African-appearing categories
##(based on visual inspection of coder rating categories, NB - drops all players from "neither light nor dark" category)
soc$tonecode<-ifelse(soc$meanrate<3, "white", ifelse(soc$meanrate>3, "black", NA))
soc$tonecode<-factor(soc$tonecode)

##EVALUATING RANDOM COMPONENTS OF VARIANCE##

##fixed intercept model##
intercept.model.gls<-gls(redCards~games, data=soc, na.action=na.exclude)

##random intercept by referee##
intercept.model<-lme(redCards~games, random=~1|refNum, data=soc, na.action=na.exclude)

##random intercept model with referee country##
intercept.country<-update(intercept.model, random=~1|refCountry)

##model comparison##
anova(intercept.model, intercept.model.gls)
anova(intercept.country, intercept.model.gls)

##random skintone slope models##
slope.model<-lme(redCards~games+meanrate, random=~meanrate|refNum, data=soc, na.action=na.exclude)
slope.country<-lme(redCards~games+meanrate, random=~meanrate|refCountry, data=soc, na.action=na.exclude)

##model comparisons##
noslope.model<-update(slope.model, random=~1|refNum)
anova(slope.model, noslope.model)
noslope.country<-update(slope.country, random=~1|refCountry)
anova(slope.country, noslope.country)

##random three-level intercept with referee nested within referee country##
three.level.intercept<-lme(redCards~games, random=~1|refCountry/refNum, data=soc, na.action=na.exclude)

##model comparisons##
anova(three.level.intercept, intercept.model)
anova(three.level.intercept, intercept.country)

##random three-level intercept model with random skintone slope##
three.level.slope<-lme(redCards~games+meanrate, random=~meanrate|refCountry/refNum, data=soc, na.action=na.exclude)

##model comparison##
noslope.threelevel<-update(three.level.slope, random=~1|refCountry/refNum)
anova(three.level.slope, noslope.threelevel)

##EXAMINING POSSIBLE CONFOUNDS WITH SKIN TONE##

##to retain all cases for player-level matrix a revised dataset--poscode--was manually created in which NAs for position were recoded to "unknown"
poscode = soc
##create a player-level matrix##
plevel<-aggregate(cbind(goals, games)~playerShort+club+position, poscode,sum, na.action=na.pass)

#RAJOUTER EN PLUS
poscode$birthday <- as.Date(poscode$birthday, format = "%d.%m.%Y")
reference_date <- as.Date("2014-01-01")
poscode$daysold <- as.numeric(difftime(reference_date, poscode$birthday, units = "days"))
playerage <- poscode[, c("playerShort", "daysold")]


##multiple steps required to retain all players despite missing data##
plevel1<-aggregate(bmi~playerShort+club+position, poscode,mean, na.action=na.pass)
plevel2<-aggregate(meanrate~playerShort+club+position, poscode,mean, na.action=na.pass)
plevel3<-aggregate(defeats~playerShort+club+position, poscode,sum, na.action=na.pass)
plevel4<-aggregate(daysold~playerShort, playerage, mean, na.action=na.pass)##age converted in separate file##

##retaining only nonredundant variables##
plevel1<-plevel1[,c(1,4)]
plevel2<-plevel2[,c(1,4)]
plevel3<-plevel3[,c(1,4)]

##merging into a single matrix##
plevel<-merge(plevel, plevel1, by="playerShort")
plevel<-merge(plevel, plevel2, by="playerShort")
plevel<-merge(plevel, plevel3, by="playerShort")
plevel<-merge(plevel, plevel4, by="playerShort")

##calculating player-level indices (aggregate goals per game and losing percentage)##
plevel$plevgpgame<-plevel$goals/plevel$games
plevel$plevlosep<-plevel$defeats.x/plevel$games

require(psych)
##correlating player factors with skin tone (requires psych package)##
with(plevel, corr.test(cbind(meanrate.y, plevgpgame, plevlosep, bmi.x, daysold.x)))
