
resamples = rbindlist(lapply(bgs, function(bg) fread(sprintf("%s/bg_%s/resamples.csv", featurePath, bg))))
assignments = fread(paste0(featurePath, "/caseAssignments.csv"))

sfExport('resamples', 'assignments')

logisticRegressions = lapply(1:5, function(x){
  rsName = paste0('resample', x)
  X = suppressMessages(dcast(resamples[,.(houseID, feature, get(rsName))], houseID ~ feature))
  
  y = unique(assignments[,.(Call_No, houseID = get(rsName))], by = 'houseID')
  
  data = merge(X, data.table(houseID = y$houseID, caseInHouse = 1), all.x = TRUE)
  data[is.na(data)] = 0
  
  fmla = formula(paste0('caseInHouse ~ ', paste0(imputationColumns, collapse = '+')))
  
  glm(fmla, data = data, family = binomial, model = FALSE)
})

for(i in 1:nDraws) logisticRegressions[[i]]$data = NULL
