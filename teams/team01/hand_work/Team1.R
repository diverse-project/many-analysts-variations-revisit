library(haven)
library(dplyr)
library(readr)

setwd('/home/mrenzo/many-analysts-variations-revisit/')
data_in  <- 'data/dataset/1. Crowdsourcing Dataset July 01, 2014 Incl.Ref Country/CrowdstormingDataJuly1st.csv'
data_out <- 'teams/team01/output'

df <- read_csv(data_in, na = c("", "NA"))

df <- df %>% mutate(playerid = as.integer(factor(player)))

parts <- strsplit(df$birthday, ".", fixed = TRUE)
df <- df %>%
  mutate(
    birth_day   = as.integer(sapply(parts, `[`, 1)),
    birth_month = as.integer(sapply(parts, `[`, 2)),
    birth_year  = as.integer(sapply(parts, `[`, 3))
  )

df <- df %>%
  mutate(across(c(birth_month, birth_day, birth_year, height, weight,
                  rater1, rater2, meanIAT, nIAT, seIAT,
                  meanExp, nExp, seExp, yellowCards,
                  redCards, yellowReds, games), as.numeric))

df <- df %>% mutate(age = 2013 - birth_year,
                    age2 = age^2)

df <- df %>% mutate(rateravg = (rater1 + rater2) / 2)

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

df <- df %>%
  mutate(
    yellowCards = ifelse(dyad_yell_max != 0 & withinorder <= dyad_yell_max, 1, 0),
    redCards    = ifelse(dyad_red_max  != 0 & withinorder <= dyad_red_max,  1, 0),
    yellowReds  = ifelse(dyad_yellred_max != 0 & withinorder <= dyad_yellred_max, 1, 0)
  )

df <- df %>%
  mutate(anyred  = as.integer(redCards == 1 | yellowReds == 1),
         anycard = as.integer(redCards == 1 | yellowReds == 1 | yellowCards == 1))

df <- df %>%
  mutate(
    clubnum          = as.integer(factor(club)),
    leaguecountrynum = as.integer(factor(leagueCountry)),
    positionnum      = as.integer(factor(position))
  )

df <- df %>% mutate(height2 = height^2, weight2 = weight^2)

df <- df %>%
  group_by(refCountry) %>%
  mutate(
    zmeaniat = mean(meanIAT, na.rm = TRUE),
    zmeanexp = mean(meanExp, na.rm = TRUE)
  ) %>%
  ungroup()

if (!dir.exists(data_out)) {
  dir.create(data_out, recursive = TRUE)
}

saveRDS(df, file = file.path(data_out, 'df_clean.rds'))

