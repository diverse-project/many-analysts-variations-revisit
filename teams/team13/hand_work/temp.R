# --- Load libraries ---
library(psy)
library(sandwich)

# --- Load data ---
setwd('/home/mrenzo/many-analysts-variations-revisit/')
data <- read.csv("data/dataset/1. Crowdsourcing Dataset July 01, 2014 Incl.Ref Country/CrowdstormingDataJuly1st.csv",nrows = 4000)

# --- Preprocessing: skin tone & red cards ---
data$refNum <- factor(data$refNum)
data$skinrating <- rowMeans(data[, c("rater1", "rater2")])*5
data$skincolor <- ifelse(data$skinrating < 3, "light skin", ifelse(data$skinrating > 3, "dark skin", NA))

# --- RESEARCH QUESTION 1 ---
# Poisson model: red cards ~ skin color + position (offset = log(games))
model1 <- glm(redCards ~ skincolor + position, data = data, offset = log(games), family = "poisson")
cov1 <- vcovHC(model1, type = "HC0")
se1 <- sqrt(diag(cov1))
result1 <- cbind(
  Estimate = coef(model1),
  `Robust SE` = se1,
  `Pr(>|z|)` = 2 * pnorm(abs(coef(model1) / se1), lower.tail = FALSE),
  LL = coef(model1) - 1.96 * se1,
  UL = coef(model1) + 1.96 * se1
)
print(result1)

# --- RESEARCH QUESTION 2 ---
# Aggregate data by refCountry and skincolor
agg <- aggregate(cbind(games, redCards) ~ refCountry + skincolor, data = data, sum, na.rm = TRUE)
agg_wide <- reshape(agg, timevar = "skincolor", idvar = "refCountry", direction = "wide")
names(agg_wide) <- c("refCountry", "games.darkskin", "redCards.darkskin", "games.lightskin", "redCards.lightskin")

# Merge with IAT/Exp data
iat_exp <- aggregate(cbind(meanIAT, seIAT, meanExp, seExp) ~ refCountry, data = data, mean)
merged <- merge(agg_wide, iat_exp, by = "refCountry")

# Calculate red card rates & ratio
merged$p.darkskin <- merged$redCards.darkskin / merged$games.darkskin
merged$p.lightskin <- merged$redCards.lightskin / merged$games.lightskin
merged$ratio <- merged$p.darkskin / merged$p.lightskin
merged <- subset(merged, !is.infinite(ratio) & !is.nan(ratio) & refCountry != 133)

# Poisson model: redCards.darkskin ~ meanIAT + p.lightskin
model2 <- glm(redCards.darkskin ~ meanIAT + p.lightskin, offset = log(games.darkskin), data = merged, family = "poisson")
cov2 <- vcovHC(model2, type = "HC0")
se2 <- sqrt(diag(cov2))
result2 <- cbind(
  Estimate = coef(model2),
  `Robust SE` = se2,
  `Pr(>|z|)` = 2 * pnorm(abs(coef(model2) / se2), lower.tail = FALSE),
  LL = coef(model2) - 1.96 * se2,
  UL = coef(model2) + 1.96 * se2
)
print(result2)

# Poisson model: redCards.darkskin ~ meanExp + p.lightskin
model3 <- glm(redCards.darkskin ~ meanExp + p.lightskin, offset = log(games.darkskin), data = merged, family = "poisson")
cov3 <- vcovHC(model3, type = "HC0")
se3 <- sqrt(diag(cov3))
result3 <- cbind(
  Estimate = coef(model3),
  `Robust SE` = se3,
  `Pr(>|z|)` = 2 * pnorm(abs(coef(model3) / se3), lower.tail = FALSE),
  LL = coef(model3) - 1.96 * se3,
  UL = coef(model3) + 1.96 * se3
)
print(result3)
    
