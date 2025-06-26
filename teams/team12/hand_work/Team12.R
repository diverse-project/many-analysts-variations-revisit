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


# Fonction manuelle pour extraire les résultats d'un modèle zeroinfl
extract_zip_manual <- function(model, model_name) {
  summ <- summary(model)
  
  # Partie logit (zéro-inflation)
  logit <- as.data.frame(summ$coefficients$zero)
  logit$term <- rownames(logit)
  logit$part <- "logit"
  
  # Partie poisson (comptes)
  count <- as.data.frame(summ$coefficients$count)
  count$term <- rownames(count)
  count$part <- "log"
  
  # Combine
  results <- bind_rows(logit, count) %>%
    mutate(
      model = model_name,
      estimate = round(Estimate, 3),
      p.value = ifelse(`Pr(>|z|)` < 0.001, "<.001", round(`Pr(>|z|)`, 3))
    ) %>%
    select(model, part, term, estimate, p.value)
  
  return(results)
}

# Appliquer à chaque modèle
models <- list(
  Model1 = fit.zip.1,
  Model2 = fit.zip.2,
  Model3 = fit.zip.3,
  Model4 = fit.zip.4
)

library(dplyr)
library(tidyr)
library(purrr)

results_all <- imap_dfr(models, extract_zip_manual)

# Réorganiser sous forme de tableau large
results_wide <- results_all %>%
  pivot_wider(
    names_from = model,
    values_from = c(estimate, p.value),
    names_glue = "{.value}_{model}"
  ) %>%
  arrange(part, term)

# Sauvegarder au bon endroit
write.csv(results_wide, "teams/team12/outputs/tableau_modeles.csv", row.names = FALSE)

