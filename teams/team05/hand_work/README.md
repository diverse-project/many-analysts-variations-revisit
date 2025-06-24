# Team 5

First of setwd is the path to the repo so you should change it for your sitatuation

## Script: `team5.R`

---

* Loaded from: `CrowdstormingDataJuly1st.csv`
* Key variables:

  * `redCards`: number of red cards
  * `games`: number of games played
  * `rater1`, `rater2`: skin tone ratings (1 = light, 5 = dark)
  * `meanIAT`, `meanExp`: bias scores by country
  * `refCountry`: referee’s country

## Preparation

* Averages skin tone ratings → `avgrate`
* Repeats each player row by number of games
* Creates binary variable: 1 if red card in a game, else 0
* Normalizes skin tone to \[0, 1] → `avgrate01`
* Removes rows with missing values

## Descriptive Stats

* Compares **observed vs. expected red cards** by skin tone
* Uses **Chi-squared test** and **barplot**

## Models

* `gm0`: baseline model (random effects: player, referee)
* `gm1`–`gm3`: adds skin tone and interactions
* `gm3`: best model – includes random slope by **referee country**
* `gm4`: tests interaction with **implicit bias**
* `gm5`: tests interaction with **explicit bias**
---

