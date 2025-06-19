# Charger les packages
library(haven)
library(dplyr)
library(readr)

# Définir les chemins
data_in  <- '/home/mrenzo/Project/Dataset/1. Crowdsourcing Dataset July 01, 2014 Incl.Ref Country/CrowdstormingDataJuly1st.csv'
data_out <- '/home/mrenzo/Project/Docker/DockerTeam1/output'

# Importer les données depuis CSV brut
# On suppose que les noms de colonnes correspondent à ceux du .dta original
# et que la colonne 'birthday' est au format 'DD.MM.YYYY'
df <- read_csv(data_in, na = c("", "NA"))

# Générer playerid
# on créé un facteur puis un entier
df <- df %>% mutate(playerid = as.integer(factor(player)))

# Scinder la date de naissance
parts <- strsplit(df$birthday, ".", fixed = TRUE)
df <- df %>%
  mutate(
    birth_day   = as.integer(sapply(parts, `[`, 1)),
    birth_month = as.integer(sapply(parts, `[`, 2)),
    birth_year  = as.integer(sapply(parts, `[`, 3))
  )

# Colonnes texte "NA" traitées par readr en NA déjà
# Convertir en numérique
df <- df %>%
  mutate(across(c(birth_month, birth_day, birth_year, height, weight,
                  rater1, rater2, meanIAT, nIAT, seIAT,
                  meanExp, nExp, seExp, yellowCards,
                  redCards, yellowReds, games), as.numeric))

# Générer âge et âge^2
df <- df %>% mutate(age = 2013 - birth_year,
                    age2 = age^2)

# Moyenne des notations de peau
df <- df %>% mutate(rateravg = (rater1 + rater2) / 2)

# Expansion par nombre de games
df <- df %>%
  mutate(dyadnum = row_number()) %>%
  group_by(dyadnum) %>%
  mutate(
    tot_yellow    = sum(yellowCards, na.rm = TRUE),
    tot_red       = sum(redCards, na.rm = TRUE),
    tot_yellred   = sum(yellowReds, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  tidyr::uncount(games, .id = 'order') %>%
  group_by(dyadnum) %>%
  mutate(
    withinorder       = order,
    dyad_yell_max     = max(yellowCards, na.rm = TRUE),
    dyad_red_max      = max(redCards, na.rm = TRUE),
    dyad_yellred_max  = max(yellowReds, na.rm = TRUE)
  ) %>%
  ungroup()

# Réassignation des cartes par ordre
df <- df %>%
  mutate(
    yellowCards = ifelse(dyad_yell_max != 0 & withinorder <= dyad_yell_max, 1, 0),
    redCards    = ifelse(dyad_red_max  != 0 & withinorder <= dyad_red_max,  1, 0),
    yellowReds  = ifelse(dyad_yellred_max != 0 & withinorder <= dyad_yellred_max, 1, 0)
  )

# Générer anyred et anycard
df <- df %>%
  mutate(anyred  = as.integer(redCards == 1 | yellowReds == 1),
         anycard = as.integer(redCards == 1 | yellowReds == 1 | yellowCards == 1))

# Variables de groupe
df <- df %>%
  mutate(
    clubnum          = as.integer(factor(club)),
    leaguecountrynum = as.integer(factor(leagueCountry)),
    positionnum      = as.integer(factor(position))
  )

# Carrés
df <- df %>% mutate(height2 = height^2, weight2 = weight^2)

# Normalisation IAT et EXP par pays
df <- df %>%
  # On regroupe par pays
  group_by(refCountry) %>%
  # On crée directement les colonnes zmeaniat et zmeanexp
  mutate(
    zmeaniat = mean(meanIAT, na.rm = TRUE),
    zmeanexp = mean(meanExp, na.rm = TRUE)
  ) %>%
  ungroup()


# Sauvegarde finale
saveRDS(df, file = file.path(data_out, 'df_clean.rds'))
write_csv(df, file = file.path(data_out, 'df_clean.csv'))

