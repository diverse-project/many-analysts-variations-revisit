
# ------------------------------------------------------------
# 1. Setup
# ------------------------------------------------------------
# Install/load required packages
install.packages(c("lme4", "rstan", "bayesplot", "ggplot2"))
library(lme4)       # for preliminary GLMMs
library(rstan)      # for running Stan
library(bayesplot)  # for posterior checks
library(ggplot2)

# for faster Stan compilation
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# ------------------------------------------------------------
# 2. Data import & cleaning
# ------------------------------------------------------------
# Assume you have a CSV with at least:
#   player_id, referee_id, games, red_cards, rater1, rater2, position, league, ref_country, meanIAT, meanExp
# Load data
setwd("/home/mrenzo/Project/")
dat <- read.csv("Dataset/1. Crowdsourcing Dataset July 01, 2014 Incl.Ref Country/CrowdstormingDataJuly1st.csv",nrows = 1000)

# Exclude missing as per complete-case assumption (∼79% retained) :contentReference[oaicite:1]{index=1}
dat <- na.omit(dat)

# First approach: average raters and round to nearest integer 1–5 :contentReference[oaicite:2]{index=2}
dat$raterMean_cat <- round(rowMeans(dat[, c("rater1", "rater2")]))
dat$raterMean_cat <- pmin(pmax(dat$raterMean_cat, 1), 5)

# For final approach: continuous 0–1 scale
dat$raterMean_cont <- (rowMeans(dat[, c("rater1", "rater2")]) - 1) / 4

# Subset for Question 2: only dark‐skinned (mean > 3) :contentReference[oaicite:3]{index=3}
dat2 <- subset(dat, rowMeans(dat[, c("rater1", "rater2")]) > 3)
# Supprimer les niveaux non utilisés
dat$position <- factor(dat$position)
dat$leagueCountry <- factor(dat$leagueCountry)
dat$raterMean_cat <- factor(dat$raterMean_cat)
# ------------------------------------------------------------
# 3. Preliminary lme4 fits
# ------------------------------------------------------------
# 3.1 Initial categorical skin‐tone model with overdispersion
dat$obsID <- 1:nrow(dat)
m1 <- glmer(
  cbind(redCards, games - redCards) ~ factor(raterMean_cat) 
  + (1 | position) + (1 | dat$leagueCountry)
  + (1 | obsID), 
  data = dat, family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa")
)

summary(m1)

# 3.2 Numerically‐coded skin tone in second‐level (using a random‐slope trick)
# here we let the categorical estimates be partially pooled toward linearity
m2 <- glmer(
  cbind(redCards, games - redCards) ~ 1 
  + (1 + raterMean_cat | raterMean_cat) 
  + (1 | position) + (1 | league)
  + (1 | obsID),
  data = dat, family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa")
)

summary(m2)

# ------------------------------------------------------------
# 4. Stan models for Research Question 1
# ------------------------------------------------------------
# 4.1 Prepare data list for Stan (initial categorical model)
stan_data1 <- list(
  n           = nrow(dat),
  y           = dat$red_cards,
  games       = dat$games,
  raterMean   = dat$raterMean_cat,
  positionN   = length(unique(dat$position)),
  position    = as.integer(factor(dat$position)),
  leagueN     = length(unique(dat$league)),
  leagueCountry = as.integer(factor(dat$league))
)

# Compile Stan model (use your .stan file or inline string)
stan_mod1 <- stan_model("stan_binomial1.stan")

# Sample
fit1 <- sampling(
  stan_mod1, data = stan_data1, 
  chains = 4, warmup = 200, iter = 700,
  control = list(adapt_delta = 0.95)
)
print(fit1, pars = c("b0", "bRater", "bleague", "bpos", "sigmaerror"), probs = c(0.025,0.5,0.975))

# 4.2 Second‐level numeric skin‐tone model
stan_data1b <- stan_data1
# no change to data; model code handles muRater linearly

stan_mod1b <- stan_model("stan_binomial1b.stan")
fit1b <- sampling(stan_mod1b, data = stan_data1b,
                  chains = 4, warmup = 200, iter = 700,
                  control = list(adapt_delta = 0.95))
print(fit1b, pars = c("gRater1", "bRater", "b0"), probs = c(0.025,0.5,0.975))

# ------------------------------------------------------------
# 5. Stan models for Research Question 2
# ------------------------------------------------------------
# Prepare data list for Stan (dark‐skinned subset)
stan_data2 <- list(
  n            = nrow(dat2),
  y            = dat2$red_cards,
  games        = dat2$games,
  positionN    = length(unique(dat2$position)),
  position     = as.integer(factor(dat2$position)),
  leagueN      = length(unique(dat2$league)),
  leagueCountry= as.integer(factor(dat2$league)),
  countryN     = length(unique(dat2$ref_country)),
  refCountry   = as.integer(factor(dat2$ref_country)),
  meanIAT      = tapply(dat2$meanIAT, dat2$ref_country, mean),
  meanExp      = tapply(dat2$meanExp, dat2$ref_country, mean)
)

stan_mod2 <- stan_model("stan_binomial2.stan")

fit2 <- sampling(stan_mod2, data = stan_data2,
                 chains = 4, warmup = 200, iter = 700,
                 control = list(adapt_delta = 0.95))
print(fit2, pars = c("g_country_raw", "gIAT_raw", "gExp_raw", "bcountry"), probs = c(0.025,0.5,0.975))

# ------------------------------------------------------------
# 6. Posterior predictive checks
# ------------------------------------------------------------
ppc_dens_overlay(
  y = dat$red_cards / dat$games,
  yrep = posterior_predict(fit1b)[ , , "red_cards"] / dat$games[1], # adjust indexing
  binwidth = 0.001
)
