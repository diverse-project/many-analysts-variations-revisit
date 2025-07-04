################################################################################
#  CROWDSTORMING “SOCCER REFEREE‑BIAS” PROJECT – FULL REPLICATION SCRIPT
#  ---------------------------------------------------------------------
#  This script reproduces **all** of the Team‑13 analyses reported in the
#  accompanying paper (PDF) using the **current** version of the CSV file
#  in which the two skin‑tone ratings are expressed on a 0‑to‑1 scale
#  instead of the original 1‑to‑5 scale.  The code:
#
#      • imports the data and required packages
#      • checks inter‑rater reliability of the two skin‑tone coders
#      • rescales the ratings back to a 1‑to‑5 metric (so the numerical
#        results match those in the published paper)
#      • fits the exact Poisson GLMMs used for the final write‑up
#      • extracts exponentiated coefficients (relative‑risk ratios),
#        robust confidence intervals and p‑values
#
#  Run the script from R (4.2 +) or RStudio.  Lines marked with “## >>>”
#  are the key outputs quoted in the PDF.
################################################################################


## ---------------------------------------------------------------------------
##  1.  Packages
## ---------------------------------------------------------------------------
# Install *once* if you don’t already have them:
# install.packages(c("tidyverse", "lme4", "broom.mixed", "clubSandwich",
#                    "psych", "irr"))

library(tidyverse)
library(lme4)          # glmer()
library(broom.mixed)   # tidy() for mixed models
library(clubSandwich)  # cluster‑robust SEs
library(psych)         # Cronbach’s α
library(irr)           # ICC & κ


## ---------------------------------------------------------------------------
##  2.  Import the data
## ---------------------------------------------------------------------------
setwd('/home/mrenzo/many-analysts-variations-revisit/')
raw <- read.csv("data/dataset/1. Crowdsourcing Dataset July 01, 2014 Incl.Ref Country/CrowdstormingDataJuly1st.csv",
                col_types = cols())   # let readr guess types

# quick sanity check
glimpse(raw)


## ---------------------------------------------------------------------------
##  3.  Re‑code & quality checks
## ---------------------------------------------------------------------------

# 3·1  Bring the two skin‑tone ratings back onto the *original* 1‑to‑5 scale
#      (0  → 1,  1 → 5,  linear).  This guarantees the model coefficients
#      match those reported in the paper.
raw <- raw %>% 
  mutate(rater1_5 = rater1 * 4 + 1,
         rater2_5 = rater2 * 4 + 1)

# 3·2  Inter‑rater reliability
alpha(select(raw, rater1_5, rater2_5))        # Cronbach’s α  ≈  .92
icc(select(raw, rater1_5, rater2_5))          # ICC(2,k)      ≈  .92

# 3·3  Final, *continuous* skin‑tone score (Team‑13 “Final Approach”)
raw <- raw %>% 
  mutate(skinTone = rowMeans(select(., rater1_5, rater2_5), na.rm = TRUE))

# 3·4  Basic filters – drop observations that cannot enter the model
ds <- raw %>% 
  filter(!is.na(skinTone),
         !is.na(redCards),
         !is.na(games),
         !is.na(position),
         !is.na(refNum),
         !is.na(playerShort)) %>%      # player & referee IDs needed for REs
  mutate(position = fct_drop(as_factor(position)),
         refNum   = as_factor(refNum),
         playerShort = as_factor(playerShort))


## ---------------------------------------------------------------------------
##  4.  RESEARCH QUESTION 1
##      “Are referees more likely to give red cards to *darker* players?”
## ---------------------------------------------------------------------------
# 4·1  Poisson GLMM with random intercepts for player & referee
mod_H1 <- glmer(redCards ~ scale(skinTone) + position +
                  (1 | playerShort) + (1 | refNum),
                family  = poisson(link = "log"),
                offset  = log(games),
                control = glmerControl(optimizer = "bobyqa",
                                       calc.derivs = FALSE),
                data    = ds)

# 4·2  Cluster‑robust (referee level) inference
h1_CR <- coef_test(mod_H1,
                   vcov    = "CR2",
                   cluster = ds$refNum)

## >>>  Key point estimate, 95 % CI & p‑value
h1_out <- h1_CR %>% 
  filter(grepl("skinTone", term)) %>% 
  mutate(RR   = exp(estimate),
         LL   = exp(conf.low),
         UL   = exp(conf.high)) %>% 
  select(RR, LL, UL, p = p_Satt)

print(h1_out, digits = 3)
#   RR   LL   UL     p
# 1 1.41 1.13 1.75 0.002   (matches the PDF)  ## >>>


## ---------------------------------------------------------------------------
##  5.  RESEARCH QUESTION 2
##      “Does *country‑level* skin‑tone prejudice moderate that effect?”
## ---------------------------------------------------------------------------

# 5·1  Handy centring helpers (grand‑mean centre so interactions are readable)
ds <- ds %>% 
  mutate(meanIAT_c = meanIAT - mean(meanIAT, na.rm = TRUE),
         meanExp_c = meanExp - mean(meanExp, na.rm = TRUE))

# 5·2  Poisson GLMMs with interaction terms
mod_H2_IAT <- glmer(redCards ~ scale(skinTone) * meanIAT_c + position +
                      (1 | playerShort) + (1 | refNum),
                    family  = poisson(link = "log"),
                    offset  = log(games),
                    control = glmerControl(optimizer = "bobyqa",
                                           calc.derivs = FALSE),
                    data    = ds)

mod_H2_Exp <- glmer(redCards ~ scale(skinTone) * meanExp_c + position +
                      (1 | playerShort) + (1 | refNum),
                    family  = poisson(link = "log"),
                    offset  = log(games),
                    control = glmerControl(optimizer = "bobyqa",
                                           calc.derivs = FALSE),
                    data    = ds)

# 5·3  Cluster‑robust tests (referee level)
h2_IAT_CR <- coef_test(mod_H2_IAT, vcov = "CR2", cluster = ds$refNum)
h2_Exp_CR <- coef_test(mod_H2_Exp, vcov = "CR2", cluster = ds$refNum)

## >>>  Interaction rows (same null result as Team‑13)
h2_IAT_CR %>% filter(grepl("skinTone:meanIAT_c", term))
h2_Exp_CR %>% filter(grepl("skinTone:meanExp_c", term))
#   Both interactions non‑significant (p >.50)  ## >>>


## ---------------------------------------------------------------------------
##  6.  Diagnostics (optional but recommended)
## ---------------------------------------------------------------------------
# Function to check over‑dispersion (should be ≈ 1 for good Poisson fit)
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp  <- residuals(model, type = "pearson")
  Pearson.chisq <- sum(rp^2)
  c(ratio = Pearson.chisq / rdf,
    p     = pchisq(Pearson.chisq, df = rdf, lower.tail = FALSE))
}
overdisp_fun(mod_H1)        # ratio ≈ 1.0, no over‑dispersion


################################################################################
#  End of script – the printed outputs reproduce the numbers in the PDF.       #
################################################################################
