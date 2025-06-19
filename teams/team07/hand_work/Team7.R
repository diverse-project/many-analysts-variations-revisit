full_data <- read.table("/home/mrenzo/Project/Dataset/1. Crowdsourcing Dataset July 01, 2014 Incl.Ref Country/CrowdstormingDataJuly1st.csv", sep=",", header=TRUE)
#besoin de Premium et je ne peux pas l'installer 
full_data <- full_data[!is.na(full_data$rater1) & !is.na(full_data$rater2), ]
full_data$rater1 <- as.numeric(full_data$rater1) - 1
full_data$rater2 <- as.numeric(full_data$rater2) - 1
subset_index <- which(full_data$redCards > 0)

library(PReMiuM)
runInfoObj <- profRegr(
  yModel = "Binomial",
  outcome = "redCards",
  outcomeT = "games",
  xModel = "Discrete",
  # excludeY = TRUE,
  nSweeps = 5000,
  extraYVar = TRUE,
  seed = 123,
  nProgress = 1000,
  nBurn = 5000,
  data = full_data[subset_index,],
  covNames = c("rater1", "rater2"),
  run = TRUE
)

dissimObj <- calcDissimilarityMatrix(runInfoObj)
clusObj <- calcOptimalClustering(dissimObj)
riskProfileObj <- calcAvgRiskAndProfile(clusObj)
clusterOrderObj <- plotRiskProfile(riskProfileObj, "summaryPROVA.png", showRelativeRisk=TRUE)

