glmer(RC ~ position + leagueCountry + skintone + (1|playerShort) + (1|refNum), d4.test, family=binomial(link="logit"), control=glmerControl(optimizer="bobyqa"))
