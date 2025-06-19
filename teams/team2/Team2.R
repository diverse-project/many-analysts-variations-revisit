
library(dplyr)
library(tidyr)
library(lubridate)
library(fixest)      # pour régressions avec effets fixes et clusters
library(sandwich)    # pour SE robustes
library(lmtest)      # pour coeftest
library(marginaleffects)  # pour effets marginaux avec fixest

# 1. Import des données
setwd("/home/mrenzo/Project/")
data = read.csv(file="/home/mrenzo/Project/Dataset/1. Crowdsourcing Dataset July 01, 2014 Incl.Ref Country/CrowdstormingDataJuly1st.csv")

# 2. Préparation et nettoyage
# Conversion des dates
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

# Remplacer NA strings par NA réels
num_vars <- c("rater1","rater2","height","weight","meanIAT","nIAT","seIAT","meanExp","nExp","seExp")
for(v in num_vars) data[[v]][ data[[v]]=="NA" ] <- NA

# 3. Expansion en observations joueur-match
# Générer dyad ID
data <- data %>% mutate(dyadnum = row_number())

# Calculer totaux par dyad
data <- data %>%
  group_by(dyadnum) %>%
  mutate(
    totyell = sum(yellowCards),
    totred = sum(redCards),
    totyellred = sum(yellowReds)
  ) %>% ungroup()

# Expansion
data_long <- data %>% uncount(games, .id = "order")

# Répartir cartes par match
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

# Calculer anyred et anycard
data_long <- data_long %>%
  mutate(
    anyred = ifelse(redCards==1 | yellowReds==1,1,0),
    anycard = ifelse(redCards==1 | yellowReds==1 | yellowCards==1,1,0)
  )

# Variables de groupe pour FE
data_long <- data_long %>%
  mutate(
    clubnum = as.factor(club),
    leaguecountrynum = as.factor(leagueCountry),
    positionnum = as.factor(position)
  )

# 4. Normalisation des scores IAT et Exp par pays du referee
# Extraire pays referees
data_ref_norm <- data_long %>%
  filter(!is.na(meanIAT), !is.na(meanExp)) %>%
  distinct(refCountry, meanIAT, meanExp) %>%
  mutate(
    zmeaniat = scale(meanIAT)[, 1],
    zmeanexp = scale(meanExp)[, 1]
  ) %>%
  select(refCountry, zmeaniat, zmeanexp)

# Fusion
data_long <- data_long %>%
  left_join(data_ref_norm, by = "refCountry")



# 5. Régressions section 1 : Red Cards
# LPM bivarié
lm1 <- feols(redCards ~ rateravg | 0, data = data_long,
             cluster = ~playerShort)
# LPM contrôles
lm2 <- feols(redCards ~ rateravg + height + height2 + weight + weight2 + age + age2 | leaguecountrynum + positionnum, data = data_long,
             cluster = ~playerShort)
# Logit
log1 <- feglm(redCards ~ rateravg + height + height2 + weight + weight2 + age + age2,
              data = data_long, family = binomial(), cluster = ~playerShort)

# 6. Nonlinear (categorical rater1)
lm_nonlin <- feols(redCards ~ factor(rater1) + height + height2 + weight + weight2 + age + age2 | leaguecountrynum + positionnum + clubnum,
                   data = data_long, cluster = ~playerShort)

# 7. Any Red Cards
lm_anyred <- feols(anyred ~ rateravg + height + height2 + weight + weight2 + age + age2 | leaguecountrynum + positionnum + clubnum,
                   data = data_long, cluster = ~playerShort)
log_anyred <- feglm(anyred ~ rateravg + height + height2 + weight + weight2 + age + age2,
                    data = data_long, family = binomial(), cluster = ~playerShort)

# 8. Yellow Cards
lm_yellow <- feols(yellowCards ~ rateravg + height + height2 + weight + weight2 + age + age2 | leaguecountrynum + positionnum + clubnum,
                   data = data_long, cluster = ~playerShort)
log_yellow <- feglm(yellowCards ~ rateravg + height + height2 + weight + weight2 + age + age2,
                    data = data_long, family = binomial(), cluster = ~playerShort)

# 9. Biais implicite/explicite (interactions)
# Créer variable dark
data_long <- data_long %>%
  mutate(dark = ifelse(rater1<=2/5,0,1))

# Implicit bias redcards
imp_light <- feols(redCards ~ zmeaniat, data = filter(data_long, dark==0), cluster = ~playerShort)
imp_dark  <- feols(redCards ~ zmeaniat, data = filter(data_long, dark==1), cluster = ~playerShort)

# Explicit bias redcards
exp_light <- feols(redCards ~ zmeanexp, data = filter(data_long, dark==0), cluster = ~playerShort)
exp_dark  <- feols(redCards ~ zmeanexp, data = filter(data_long, dark==1), cluster = ~playerShort)

# Pour marginals logit
marg_red <- avg_slopes(log1, variables = "rateravg")
summary(marg_red)


# Fin du script
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
