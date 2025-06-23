if (!requireNamespace('fixest', quietly = TRUE)) install.packages('fixest', repos='https://cloud.r-project.org/')
library(fixest)

df <- readRDS('/home/mrenzo/Project/Docker/DockerTeam1/output/df_clean.rds')


outcomes_q1 <- c('redCards', 'anyred', 'yellowCards')
models_q1_lpm <- list()
models_q1_logit <- list()

for (y in outcomes_q1) {
  formula_lpm <- as.formula(paste(y, '~ rateravg + height + height2 + weight + weight2 + age + age2 | refCountry + leagueCountry + club + position'))
  models_q1_lpm[[y]] <- feols(formula_lpm, data = df)
  
  formula_logit <- update(formula_lpm, paste0(y, ' ~ .'), family = binomial('logit'))
  models_q1_logit[[y]] <- feglm(formula_lpm, data = df, family = binomial('logit'))
}

df$skintone_factor <- factor(df$rateravg, levels = 1:5)
models_q1_nonlinear <- list()
for (y in outcomes_q1) {
  f_nl <- as.formula(paste(y, '~ skintone_factor + height + height2 + weight + weight2 + age + age2 | refCountry + leagueCountry + club + position'))
  models_q1_nonlinear[[y]] <- feols(f_nl, data = df)
}

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

table_q1_lpm <- etable(models_q1_lpm, tex = FALSE)
cat('## Résultats Q1 (LPM):\n')
print(table_q1_lpm)

cat('\n## Résultats Q1 (Logit):\n')
print(etable(models_q1_logit, tex = FALSE))

table_q1_nl <- etable(models_q1_nonlinear, tex = FALSE)
cat('\n## Résultats Q1 (Non-linéaire):\n')
print(table_q1_nl)

table_q2 <- etable(models_q2, tex = FALSE)
cat('\n## Résultats Q2 (Impact du biais implicite/explicite):\n')
print(table_q2)
