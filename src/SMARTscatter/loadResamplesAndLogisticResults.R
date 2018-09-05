library(data.table)

getResampleByIndex = function(path, resampleIndex){
  bgs = list.files(path) %>%
    str_extract("\\d{7}$") %>%
    na.omit %>%
    as.numeric
  resamples = lapply(bgs, function(bg){
    tmp = fread(sprintf("%sbg_%s/resamples.csv", path, bg))[,.(houseID, feature, get(sprintf("resample%s", resampleIndex)))]
    out = data.table(dcast(tmp, houseID ~ feature, value.var = "V3"), blockGroup = bg)
    return(out)
  })
  return(rbindlist(resamples))
}

getResamplesByBG = function(path, blockGroup){
  
  bgResamples = fread(sprintf("%sbg_%s/resamples.csv", path, blockGroup))
  return(data.table(bgResamples, blockGroup = blockGroup))
  
}

getLogiCoefs = function(path, model){
  if(!(model %in% c("ca", "ipv", "ea"))){
    warning("Model not recognized. Model must be one of ca, ipv, or ea.")
    return(0)
  }
  coefs = fread(sprintf("%s%sCoefficients.csv", path, model))
}

getLogiFits = function(path, model){
  if(!(model %in% c("ca", "ipv", "ea"))){
    warning("Model not recognized. Model must be one of ca, ipv, or ea.")
    return(0)
  }
  fits = fread(sprintf("%s%sFits.csv", path, model))
  fits[,V1 := NULL]
  return(fits)
}

getCaseAssignments = function(path, model){
  if(!(model %in% c("ca", "ipv", "ea"))){
    warning("Model not recognized. Model must be one of ca, ipv, or ea.")
    return(0)
  }
  assignmentPath <- function(model) {
    switch(model,
           ca = "childAbuseCaseAssignments.csv",
           ipv = "ipvCaseAssignments.csv",
           ea = "elderAbuseCaseAssignments.csv")
  }
  fits = fread(sprintf("%s%s", path, assignmentPath(model)))
  return(fits)
}




#
# Examples
#

path = "./data/mitre/working/imputationAndResamplingResults/sqrtHINCP_RMSP_householdSize_singleParent_snKid_milWoman_unmarriedPartner_multiGenHouse/"
resampleIndex = 1
blockGroup = 1001001
# Model is one of ca, ipv, or ea
model = "ca"

simData = getResampleByIndex(path, resampleIndex)
bgSims = getResamplesByBG(path, blockGroup)
caCoefs = getLogiCoefs(path, model)
caFits = getLogiFits(path, model)
caCaseAssignments = getCaseAssignments(path, model)

# More examples
# Coefficient histogram

hist(as.numeric(caCoefs[V1 == "multiGenHouse", -1]))
# p-value
mean(as.numeric(caCoefs[V1 == "multiGenHouse", -1]) < 0)
hist(as.numeric(caCoefs[V1 == "milWoman", -1]))

# Etimated ca probabilities
caFits
hist(as.numeric(caFits[houseID == 1, -1]))
system.time(medians <- sapply(unique(caFits$houseID)[1:1000], function(x){
  
  mean(as.numeric(caFits[houseID == x, -1]))
  
}))
hist(medians)

# Look at resampled data

simData
