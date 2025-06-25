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
