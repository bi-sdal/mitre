source("./R/00-simulateArlFunctions.R")
library(data.table)
nDraws = 1000
resamples = fread("./data/mitre/working/imputationAndResamplingResults/sqrtHINCP_RMSP/bg_1001001/resamples.csv")
colnames(resamples)[1] = "houseID"
assignments = fread("./data/mitre/working/imputationAndResamplingResults/sqrtHINCP_RMSP/caseAssignments.csv")

logisticRegressions = lapply(1:nDraws, function(x){
  rsName = paste0('resample', x)
  X = suppressMessages(dcast(resamples[,.(houseID, feature, get(rsName))], houseID ~ feature))
  
  y = assignments[,.(Call_No, houseID = get(rsName))]
  
  data = merge(X, data.table(houseID = y$houseID, caseInHouse = 1), all.x = TRUE)
  data[is.na(data)] = 0
  
  glm(caseInHouse ~ RMSP + sqrtHINCP, data = data, family = binomial)
})

save(logisticRegressions,file =  "./data/mitre/working/imputationAndResamplingResults/sqrtHINCP_RMSP/logisticRegressions.Rdata")
