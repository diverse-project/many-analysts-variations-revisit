
library(dplyr)
library(tidyr)
library(lubridate)
library(fixest)      
library(sandwich)    
library(lmtest)      
library(marginaleffects)  

setwd("/home/mrenzo/Project/")
data = read.csv(file="/home/mrenzo/Project/Dataset/1. Crowdsourcing Dataset July 01, 2014 Incl.Ref Country/CrowdstormingDataJuly1st.csv")

data <- data %>%
  mutate(
    birthday = dmy(birthday),
    birth_year = year(birthday),
    age = 2013 - birth_year,
    age2 = age^2,
    height = as.numeric(height),
    weight = as.numeric(weight),
    height2 = height^2,
    weight2 = weight^2,
    rater1 = as.numeric(rater1),
    rater2 = as.numeric(rater2),
    rateravg = (rater1 + rater2)/2
  )

num_vars <- c("rater1","rater2","height","weight","meanIAT","nIAT","seIAT","meanExp","nExp","seExp")
for(v in num_vars) data[[v]][ data[[v]]=="NA" ] <- NA

data <- data %>% mutate(dyadnum = row_number())

data <- data %>%
  group_by(dyadnum) %>%
  mutate(
    totyell = sum(yellowCards),
    totred = sum(redCards),
    totyellred = sum(yellowReds)
  ) %>% ungroup()

data_long <- data %>% uncount(games, .id = "order")

data_long <- data_long %>%
  group_by(dyadnum) %>%
  mutate(
    withinorder = row_number(),
    dyadyellowcards = max(yellowCards),
    dyadyellowreds = max(yellowReds),
    dyadredcards   = max(redCards)
  ) %>%
  ungroup() %>%
  mutate(
    yellowCards = ifelse(dyadyellowcards>0,
                         ifelse(withinorder <= dyadyellowcards,1,0),
                         0),
    yellowReds  = ifelse(dyadyellowreds>0,
                         ifelse(withinorder <= dyadyellowreds,1,0),
                         0),
    redCards    = ifelse(dyadredcards>0,
                         ifelse(withinorder <= dyadredcards,1,0),
                         0)
  )

data_long <- data_long %>%
  mutate(
    anyred = ifelse(redCards==1 | yellowReds==1,1,0),
    anycard = ifelse(redCards==1 | yellowReds==1 | yellowCards==1,1,0)
  )

data_long <- data_long %>%
  mutate(
    clubnum = as.factor(club),
    leaguecountrynum = as.factor(leagueCountry),
    positionnum = as.factor(position)
  )

data_ref_norm <- data_long %>%
  filter(!is.na(meanIAT), !is.na(meanExp)) %>%
  distinct(refCountry, meanIAT, meanExp) %>%
  mutate(
    zmeaniat = scale(meanIAT)[, 1],
    zmeanexp = scale(meanExp)[, 1]
  ) %>%
  select(refCountry, zmeaniat, zmeanexp)

data_long <- data_long %>%
  left_join(data_ref_norm, by = "refCountry")



lm1 <- feols(redCards ~ rateravg | 0, data = data_long,
             cluster = ~playerShort)
lm2 <- feols(redCards ~ rateravg + height + height2 + weight + weight2 + age + age2 | leaguecountrynum + positionnum, data = data_long,
             cluster = ~playerShort)
log1 <- feglm(redCards ~ rateravg + height + height2 + weight + weight2 + age + age2,
              data = data_long, family = binomial(), cluster = ~playerShort)

lm_nonlin <- feols(redCards ~ factor(rater1) + height + height2 + weight + weight2 + age + age2 | leaguecountrynum + positionnum + clubnum,
                   data = data_long, cluster = ~playerShort)

lm_anyred <- feols(anyred ~ rateravg + height + height2 + weight + weight2 + age + age2 | leaguecountrynum + positionnum + clubnum,
                   data = data_long, cluster = ~playerShort)
log_anyred <- feglm(anyred ~ rateravg + height + height2 + weight + weight2 + age + age2,
                    data = data_long, family = binomial(), cluster = ~playerShort)

lm_yellow <- feols(yellowCards ~ rateravg + height + height2 + weight + weight2 + age + age2 | leaguecountrynum + positionnum + clubnum,
                   data = data_long, cluster = ~playerShort)
log_yellow <- feglm(yellowCards ~ rateravg + height + height2 + weight + weight2 + age + age2,
                    data = data_long, family = binomial(), cluster = ~playerShort)

data_long <- data_long %>%
  mutate(dark = ifelse(rater1<=2/5,0,1))

imp_light <- feols(redCards ~ zmeaniat, data = filter(data_long, dark==0), cluster = ~playerShort)
imp_dark  <- feols(redCards ~ zmeaniat, data = filter(data_long, dark==1), cluster = ~playerShort)

exp_light <- feols(redCards ~ zmeanexp, data = filter(data_long, dark==0), cluster = ~playerShort)
exp_dark  <- feols(redCards ~ zmeanexp, data = filter(data_long, dark==1), cluster = ~playerShort)

marg_red <- avg_slopes(log1, variables = "rateravg")
summary(marg_red)


library(modelsummary)

models <- list(
  "LPM simple (redCards)" = lm1,
  "LPM contrôles (redCards)" = lm2,
  "Logit (redCards)" = log1,
  "LPM nonlin (rater1)" = lm_nonlin,
  "LPM (anyred)" = lm_anyred,
  "Logit (anyred)" = log_anyred,
  "LPM (yellowCards)" = lm_yellow,
  "Logit (yellowCards)" = log_yellow,
  "IAT light" = imp_light,
  "IAT dark" = imp_dark,
  "EXP light" = exp_light,
  "EXP dark" = exp_dark
)

modelsummary(models, stars = TRUE, statistic = "std.error", gof_omit = "IC|Log.Lik|RMSE")
