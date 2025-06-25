require(car)
require(lme4)

setwd('/home/mrenzo/many-analysts-variations-revisit/')
data <- read.csv(file="data/dataset/1. Crowdsourcing Dataset July 01, 2014 Incl.Ref Country/CrowdstormingDataJuly1st.csv",nrows=10000)

# player-referee combo variable
data$p_ref <- paste(data$playerShort, data$refNum, sep = ".")

# average skintone rating
data$avgrate <- rowMeans(cbind(data$rater1, data$rater2), na.rm = TRUE)

# repeat rows for each game using `rep()` vectorisé
data.games <- data[rep(1:nrow(data), times = data$games), ]
data.games$games <- 1  # chaque ligne = un match

# build redCards vector (1 pour chaque carton rouge, 0 sinon)
data.games$redCards <- unlist(mapply(function(rc, g) c(rep(1, rc), rep(0, g - rc)),
                                     data$redCards, data$games))

# exclude cases with missing values
data.games.nona <- subset(data.games, !is.na(avgrate) & !is.na(meanIAT))

# rescale skin-tone-rating to range 0–1
data.games.nona$avgrate01 <- (data.games.nona$avgrate - 1) / 4

# use redcard-games only for plot of expected and observed frequencies
data.red <- data.games.nona[data.games.nona$redCards > 0, ]

# save frequencies
x <- table(as.integer(data.games.nona$avgrate))
y <- table(as.integer(data.red$avgrate))

# Chi-Square test: Compare observed frequencies with expected probability
null.probs <- x / sum(x)
chi <- chisq.test(y, p = null.probs)
print(chi)
print(data.frame(ratings = as.numeric(names(x)), expected = round(as.vector(chi$expected), 1), observed = as.vector(chi$observed)))

# Barplot
cols <- c("grey20", "grey70")
b <- barplot(t(cbind(chi$expected / chi$expected * 100, chi$observed / chi$expected * 100)),
             beside = TRUE,
             col = cols,
             xlab = "Skin-tone ratings (mean)",
             ylab = "Percent",
             ylim = c(0, 150),
             main = paste("Red cards and skin-tones (n=", sum(chi$observed), ").", sep = ""))
legend("topright", legend = c("expected", "observed"), fill = cols)

# glmer fits for Research Question 1 (gm3 best model):
gm0 <- glmer(redCards ~ 1 + (1 | playerShort) + (1 | refNum),
             family = binomial, data = data.games.nona)

gm1 <- glmer(redCards ~ 1 + avgrate01 + (1 | playerShort) + (1 | refNum),
             family = binomial, data = data.games.nona)

gm2 <- glmer(redCards ~ 1 + avgrate01 + (1 | playerShort) + (1 + avgrate01 | refNum),
             family = binomial, data = data.games.nona)

gm3 <- glmer(redCards ~ 1 + avgrate01 + (1 | playerShort) + (1 | refNum) + (1 + avgrate01 | refCountry),
             family = binomial, data = data.games.nona)

# Research Question 2a
refag <- aggregate(data.games.nona$meanIAT, list(data.games.nona$refCountry), mean)
colnames(refag) <- c("refCountry", "meanIAT")
refag$ranefavgrate01 <- ranef(gm3, drop = FALSE)$refCountry[, 2]
scatter.smooth(refag$meanIAT, refag$ranefavgrate01)

gm4 <- glmer(redCards ~ 1 + avgrate01 * meanIAT + (1 | playerShort) + (1 | refNum) + (1 + avgrate01 | refCountry),
             family = binomial, data = data.games.nona)

# Research Question 2b
refag <- aggregate(data.games.nona$meanExp, list(data.games.nona$refCountry), mean)
colnames(refag) <- c("refCountry", "meanExp")
refag$ranefavgrate01 <- ranef(gm3, drop = FALSE)$refCountry[, 2]
scatter.smooth(refag$meanExp, refag$ranefavgrate01)

gm5 <- glmer(redCards ~ 1 + avgrate01 * meanExp + (1 | playerShort) + (1 | refNum) + (1 + avgrate01 | refCountry),
             family = binomial, data = data.games.nona)
