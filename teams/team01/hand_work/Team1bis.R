# prep_and_analysis.R
# Script R pour reproduire les analyses de Pope & Pope (European Football Leagues...)
# Suivant exactement leur méthodologie décrite dans le PDF

# 0) Charger les packages nécessaires
if (!requireNamespace('fixest', quietly = TRUE)) install.packages('fixest', repos='https://cloud.r-project.org/')
library(fixest)

# 1) Charger les données pré-traitées
df <- readRDS('/home/mrenzo/Project/Docker/DockerTeam1/output/df_clean.rds')

# 2) Question 1: Effet de la couleur de peau (skin tone) sur probabilité de recevoir une carte
# Formule (1): Yi = α + β skintone_i + δ Xi + µ_ref + η_league + ν_club + θ_position + ε
# Avec Yi = redcards (straight red), anyred (any red), yellowcards
# Contrôles Xi = height, height2, weight, weight2, age, age2

outcomes_q1 <- c('redCards', 'anyred', 'yellowCards')
models_q1_lpm <- list()
models_q1_logit <- list()

for (y in outcomes_q1) {
  # Linear Probability Model
  formula_lpm <- as.formula(paste(y, '~ rateravg + height + height2 + weight + weight2 + age + age2 | refCountry + leagueCountry + club + position'))
  models_q1_lpm[[y]] <- feols(formula_lpm, data = df)
  
  # Logit (logistic regression) version
  formula_logit <- update(formula_lpm, paste0(y, ' ~ .'), family = binomial('logit'))
  models_q1_logit[[y]] <- feglm(formula_lpm, data = df, family = binomial('logit'))
}

# 3) Analyse non-linéaire: skin tone catégoriel (cotes 1-5)
# On transforme rateravg (moyenne) en facteur de rating discret
df$skintone_factor <- factor(df$rateravg, levels = 1:5)
models_q1_nonlinear <- list()
for (y in outcomes_q1) {
  f_nl <- as.formula(paste(y, '~ skintone_factor + height + height2 + weight + weight2 + age + age2 | refCountry + leagueCountry + club + position'))
  models_q1_nonlinear[[y]] <- feols(f_nl, data = df)
}

# 4) Question 2: Effet de la préjugé implicite (zmeaniat) et explicite (zmeanexp)
# Formule (2): Yi = α + β imp_i (ou exp_i) + rho_player + ε
# Estimations séparées pour joueurs clairs (rating 1-2) et foncés (3-5)

# Séparer jeux de données
df_light <- subset(df, rateravg < 3/5)
df_dark  <- subset(df, rateravg >= 3/5)

models_q2 <- list()
for (bias in c('zmeaniat', 'zmeanexp')) {
  for (grp in c('light', 'dark')) {
    data_grp <- if (grp=='light') df_light else df_dark
    for (y in c('redCards', 'anyred', 'yellowCards')) {
      f_q2 <- as.formula(paste(y, '~', bias, '| playerid'))
      models_q2[[paste(bias, grp, y, sep='_')]] <- feols(f_q2, data = data_grp)
    }
  }
}

# 5) Afficher les résultats
# Tables 1-3: LPM pour redcards, anyred, yellowcards
table_q1_lpm <- etable(models_q1_lpm, tex = FALSE)
cat('## Résultats Q1 (LPM):\n')
print(table_q1_lpm)

# Tables 4-6: Logit pour même outcomes
cat('\n## Résultats Q1 (Logit):\n')
print(etable(models_q1_logit, tex = FALSE))

# Table 7-9: Non-linéaire
table_q1_nl <- etable(models_q1_nonlinear, tex = FALSE)
cat('\n## Résultats Q1 (Non-linéaire):\n')
print(table_q1_nl)

# Tables 10-12: Q2 pour light/dark et imp/exp
table_q2 <- etable(models_q2, tex = FALSE)
cat('\n## Résultats Q2 (Impact du biais implicite/explicite):\n')
print(table_q2)
