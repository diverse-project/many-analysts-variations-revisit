setwd('/home/mrenzo/many-analysts-variations-revisit/')
data <- read.csv(file="data/dataset/1. Crowdsourcing Dataset July 01, 2014 Incl.Ref Country/CrowdstormingDataJuly1st.csv", nrows = 10000)
dir.create("teams/team13/outputs", recursive = TRUE)
output_file <- "teams/team13/outputs/mon_fichier_resultats.txt"

sink(output_file)
cat("=== Résultats des régressions (Team 13) ===\n\n")

# Packages
library(lme4)
library(sandwich)
library(lmtest)

# === Initial Approach (dichotomous skin tone) ===
cat("## Initial Approach (Skin Tone dichotomique) ##\n")

data$skinrating <- rowMeans(data[, c("rater1", "rater2")], na.rm = TRUE)
data$skincolor <- ifelse(data$skinrating > 3/5, "dark skin", ifelse(data$skinrating < 3/5, "light skin", NA))
summary(factor(data$skincolor))
data.clean <- subset(data, !is.na(skincolor))
data.clean$skincolor <- relevel(factor(data.clean$skincolor), ref = "light skin")
data <- data.clean
poisson_init <- glm(redCards ~ skincolor + position, data=data, offset=log(games), family="poisson")
cov0 <- vcovHC(poisson_init, type = "HC0")
se0 <- sqrt(diag(cov0))
est0 <- coef(poisson_init)

exp_est <- exp(est0["skincolordark skin"])
pval <- 2 * pnorm(abs(est0["skincolordark skin"] / se0["skincolordark skin"]), lower.tail = FALSE)

cat("🔹 Résultat : exp(beta skincolordark) =", round(exp_est, 3), "\n")
cat("🔹 p-value =", round(pval, 3), "\n")

cat("\n📘 Le rapport Team 13 (Initial Approach) indique que l'effet de la couleur de peau est exp(beta) ≈ 1.2 et non significatif.\n")
cat("👉 Nous retrouvons : exp(beta) =", round(exp_est, 3), ", p-value =", round(pval, 3), "\n\n")

cat("Résumé complet du modèle initial :\n")
print(coeftest(poisson_init, vcov = cov0))
cat("\n\n")

# === Hypothesis 2 : Agrégation pays ===
cat("## Hypothesis 2 - Agrégation par pays ##\n")

agg1 <- aggregate(cbind(games, redCards) ~ refCountry + skincolor, data = data.clean, sum)
agg_wide <- reshape(agg1, idvar = "refCountry", timevar = "skincolor", direction = "wide")
agg_info <- aggregate(cbind(meanIAT, meanExp, seExp) ~ refCountry, data, mean)
df2 <- merge(agg_wide, agg_info, by = "refCountry")
names(df2) <- c("refCountry", "games.dark", "red.dark", "games.light",
                "red.light", "meanIAT", "meanExp", "seExp")

# Modèle IAT
glm_iat <- glm(red.dark ~ meanIAT + I(red.light / games.light),
               offset = log(games.dark), family = poisson, data = df2)
cov1 <- vcovHC(glm_iat, type = "HC0")
se1 <- sqrt(diag(cov1))
est1 <- coef(glm_iat)["meanIAT"]
pval1 <- 2 * pnorm(abs(est1 / se1["meanIAT"]), lower.tail = FALSE)
exp_est1 <- exp(est1)

cat("🔹 Agrégation IAT : exp(beta meanIAT) =", round(exp_est1, 3), ", p =", round(pval1, 3), "\n")
cat("\n📘 Le rapport Team 13 (Aggregated IAT) indique que l'effet de IAT est positif et significatif : exp(beta) ≈ 1.5, p < 0.05\n")
cat("👉 Nous obtenons : exp(beta) =", round(exp_est1, 3), ", p =", round(pval1, 3), "\n\n")

# Modèle Exp
glm_exp <- glm(red.dark ~ meanExp + I(red.light / games.light),
               offset = log(games.dark), family = poisson, data = df2)
cov2 <- vcovHC(glm_exp, type = "HC0")
se2 <- sqrt(diag(cov2))
est2 <- coef(glm_exp)["meanExp"]
pval2 <- 2 * pnorm(abs(est2 / se2["meanExp"]), lower.tail = FALSE)
exp_est2 <- exp(est2)

cat("🔹 Agrégation Exp : exp(beta meanExp) =", round(exp_est2, 3), ", p =", round(pval2, 3), "\n")
cat("\n📘 Le rapport Team 13 (Aggregated Explicit Bias) indique que l'effet de meanExp est aussi significatif avec exp(beta) ≈ 1.6\n")
cat("👉 Nous obtenons : exp(beta) =", round(exp_est2, 3), ", p =", round(pval2, 3), "\n\n")

cat("Résumé modèle IAT :\n")
print(coeftest(glm_iat, vcov = cov1))
cat("\nRésumé modèle Exp :\n")
print(coeftest(glm_exp, vcov = cov2))
cat("\n\n")

# === Final Approach - GLMM + Skin Tone Continu ===
cat("## Final Approach - GLMM avec Skin Tone continu ##\n")

data$skin_c <- scale(data$skinrating, center = TRUE, scale = FALSE)

glmm_h1 <- glmer(redCards ~ skin_c + position + offset(log(games)) +
                   (1 | refNum) + (1 | player),
                 family = poisson, data = data)
# Récupération de l'erreur standard
se_skin_c <- sqrt(diag(vcov(glmm_h1)))["skin_c"]
z_skin_c <- fixef(glmm_h1)["skin_c"] / se_skin_c
p_skin_c <- 2 * pnorm(abs(z_skin_c), lower.tail = FALSE)

# Affichage avec la p-value
exp_beta_skin_c <- exp(fixef(glmm_h1)["skin_c"])
cat("🔹 Final H1 : exp(beta skin_c) =", round(exp_beta_skin_c, 3), "\n")
cat("🔹 p-value (approx. normale) =", round(p_skin_c, 3), "\n")

cat("\n📘 Le rapport Team 13 (Final GLMM H1) mentionne un effet exp(beta) ≈ 1.2 et significatif à p ≈ 0.04\n")
cat("👉 Nous obtenons : exp(beta) =", round(exp_beta_skin_c, 3), 
    "avec p ≈", round(p_skin_c, 3), "\n\n")

cat("Résumé GLMM H1 :\n")
print(summary(glmm_h1))
cat("\n\n")

# Interaction avec IAT
glmm_h2_iat <- glmer(redCards ~ skin_c * meanIAT + position + offset(log(games)) +
                       (1 | refNum) + (1 | player),
                     family = poisson, data = data)
cat("🔹 Final H2 IAT : Interaction skin_c × meanIAT\n")
cat("\n📘 Team 13 rapporte un effet d'interaction positif et significatif entre skin_c et IAT (p ≈ 0.03)\n")
cat("👉 Résumé du modèle :\n")
print(summary(glmm_h2_iat))
cat("\n\n")

# Interaction avec Exp
glmm_h2_exp <- glmer(redCards ~ skin_c * meanExp + position + offset(log(games)) +
                       (1 | refNum) + (1 | player),
                     family = poisson, data = data)
cat("🔹 Final H2 Exp : Interaction skin_c × meanExp\n")
cat("\n📘 Team 13 observe aussi un effet d’interaction avec le biais explicite (meanExp), mais plus faible que pour IAT.\n")
cat("👉 Résumé du modèle :\n")
print(summary(glmm_h2_exp))
cat("\n\n")

# Fermeture du fichier
sink()
