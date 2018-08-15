
resamples = rbindlist(lapply(bgs, function(bg) fread(sprintf("%s/bg_%s/resamples.csv", featurePath, bg))))
assignments = fread("./data/mitre/working/imputationAndResamplingResults/sqrtHINCP_RMSP/caseAssignments.csv")

sfExport('resamples', 'assignments')

logisticRegressions = sfLapply(1:nDraws, function(x){
  rsName = paste0('resample', x)
  X = suppressMessages(dcast(resamples[,.(houseID, feature, get(rsName))], houseID ~ feature))
  
  y = assignments[,.(Call_No, houseID = get(rsName))]
  
  data = merge(X, data.table(houseID = y$houseID, caseInHouse = 1), all.x = TRUE)
  data[is.na(data)] = 0
  
  glm(caseInHouse ~ RMSP + sqrtHINCP, data = data, family = binomial, model = FALSE)
})

for(i in 1:nDraws) logisticRegressions[[i]]$data = NULL
