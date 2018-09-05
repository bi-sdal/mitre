# Source parameters

source("./src/SMARTscatter/parameterFiles/allDataFullRuns.R")

# Compile data

resamples = rbindlist(lapply(bgs, function(bg) fread(sprintf("%s/bg_%s/resamples.csv", featurePath, bg))))

assignmentPaths = list.files("./data/mitre/working/imputationAndResamplingResults/sqrtHINCP_RMSP_householdSize_singleParent_snKid_milWoman_unmarriedPartner_multiGenHouse/", pattern= "Assignments")
assignments = lapply(assignmentPaths, function(x) fread(paste0(featurePath, "/",x)))

# Logreg for child abuse

logisticRegressions <- lapply(1:nDraws, function(x){
  rsName = paste0('resample', x)
  X = suppressMessages(dcast(resamples[,.(houseID, feature, get(rsName))], houseID ~ feature))
  
  y = unique(assignments[[1]][,.(Call_No, houseID = get(rsName))], by = 'houseID')
  
  data = merge(X, data.table(houseID = y$houseID, caseInHouse = 1), all.x = TRUE)
  data[is.na(data)] = 0
  
  fmla = formula(paste0('caseInHouse ~ ', paste0(imputationColumns, collapse = '+')))
  
  out = glm(fmla, data = data, family = binomial, model = FALSE)
  return(list(coefs = coefficients(out), p_hat = fitted(out)))
})

# Extract coefficients and fits

caCoefs = sapply(logisticRegressions, function(x) return(x$coefs))
colnames(caCoefs) = paste0('resample', 1:nDraws)
caFits = cbind(unique(resamples$houseID), sapply(logisticRegressions, function(x) return(x$p_hat)))
colnames(caFits) = c("houseID", paste0('resample', 1:nDraws))
write.csv(caCoefs, "./data/mitre/working/imputationAndResamplingResults/sqrtHINCP_RMSP_householdSize_singleParent_snKid_milWoman_unmarriedPartner_multiGenHouse/caCoefficients.csv")
write.csv(caFits, "./data/mitre/working/imputationAndResamplingResults/sqrtHINCP_RMSP_householdSize_singleParent_snKid_milWoman_unmarriedPartner_multiGenHouse/caFits.csv")
rm(logisticRegressions, caCoefs, caFits)

# Logreg for ipv

logisticRegressions <- lapply(1:nDraws, function(x){
  rsName = paste0('resample', x)
  X = suppressMessages(dcast(resamples[,.(houseID, feature, get(rsName))], houseID ~ feature))
  
  y = unique(assignments[[2]][,.(Call_No, houseID = get(rsName))], by = 'houseID')
  
  data = merge(X, data.table(houseID = y$houseID, caseInHouse = 1), all.x = TRUE)
  data[is.na(data)] = 0
  
  fmla = formula(paste0('caseInHouse ~ ', paste0(imputationColumns, collapse = '+')))
  
  out = glm(fmla, data = data, family = binomial, model = FALSE)
  return(list(coefs = coefficients(out), p_hat = fitted(out)))
})

# Extract coefficients and fits

ipvCoefs = sapply(logisticRegressions, function(x) return(x$coefs))
colnames(ipvCoefs) = paste0('resample', 1:nDraws)
ipvFits = cbind(unique(resamples$houseID), sapply(logisticRegressions, function(x) return(x$p_hat)))
colnames(ipvFits) = c("houseID", paste0('resample', 1:nDraws))
write.csv(ipvCoefs, "./data/mitre/working/imputationAndResamplingResults/sqrtHINCP_RMSP_householdSize_singleParent_snKid_milWoman_unmarriedPartner_multiGenHouse/ipvCoefficients.csv")
write.csv(ipvFits, "./data/mitre/working/imputationAndResamplingResults/sqrtHINCP_RMSP_householdSize_singleParent_snKid_milWoman_unmarriedPartner_multiGenHouse/ipvFits.csv")
rm(logisticRegressions, ipvCoefs, ipvFits)

# Logreg for ea

logisticRegressions <- lapply(1:nDraws, function(x){
  rsName = paste0('resample', x)
  X = suppressMessages(dcast(resamples[,.(houseID, feature, get(rsName))], houseID ~ feature))
  
  y = unique(assignments[[3]][,.(Call_No, houseID = get(rsName))], by = 'houseID')
  
  data = merge(X, data.table(houseID = y$houseID, caseInHouse = 1), all.x = TRUE)
  data[is.na(data)] = 0
  
  fmla = formula(paste0('caseInHouse ~ ', paste0(imputationColumns, collapse = '+')))
  
  out = glm(fmla, data = data, family = binomial, model = FALSE)
  return(list(coefs = coefficients(out), p_hat = fitted(out)))
})

# Extract coefficients and fits

eaCoefs = sapply(logisticRegressions, function(x) return(x$coefs))
colnames(eaCoefs) = paste0('resample', 1:nDraws)
eaFits = cbind(unique(resamples$houseID), sapply(logisticRegressions, function(x) return(x$p_hat)))
colnames(eaFits) = c("houseID", paste0('resample', 1:nDraws))
write.csv(eaCoefs, "./data/mitre/working/imputationAndResamplingResults/sqrtHINCP_RMSP_householdSize_singleParent_snKid_milWoman_unmarriedPartner_multiGenHouse/eaCoefficients.csv")
write.csv(eaFits, "./data/mitre/working/imputationAndResamplingResults/sqrtHINCP_RMSP_householdSize_singleParent_snKid_milWoman_unmarriedPartner_multiGenHouse/eaFits.csv")
rm(logisticRegressions, eaCoefs, eaFits)
