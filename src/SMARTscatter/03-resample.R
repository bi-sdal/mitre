source("src/SMARTscatter/01-prepareAndLoadData.R")

miceImp = fread("./data/mitre/working/imputationAndResamplingResults/bg1001001_sqrtHINCP_RMSP/imputations")

# The imputation variable is continuous, so it must be binned according to breaks. Breaks are the lower bounds.
# We must also estimate a tail density

breaks = list(incomeBreaks = c(0,25000,50000,75000,100000,125000,150000,200000), roomBreaks = 1:9)
cutoffs = list(expCutoff = breaks$incomeBreaks[length(breaks$incomeBreaks)], roomCutoff = breaks$roomBreaks[length(breaks$roomBreaks)])
models = list("exponential", "geometric")
marginals = list(marginalIncome[1, -1], marginalRooms[1, -1])

highIncome = filter(clAtrackPums, source == "PUMS" & HINCP > cutoffs$expCutoff)[,'HINCP']
manyRooms = filter(clAtrackPums, source == "PUMS" & RMSP > cutoffs$roomCutoff)[,'RMSP']
parms = list(mleLambda = 1 / mean(highIncome - cutoffs$expCutoff), mleGeom = 1 / mean(manyRooms - cutoffs$roomCutoff))

resamplers = mapply(resamplerCtor, marginals, breaks, models, parms, SIMPLIFY = FALSE)

# Begin resampling step

nDraws = 1000
resampledDraws = list()
nRows = nrow(filter(clAtrackPums, BlockGroup == 1001001))

# Each element of houselist is a home. It is i by j where i is the number of features and j is the number of imputations

indepJointDensityResample = function(ID, imputedData, resampler, nDraws){
  
  imputations = filter(miceImp, houseID == ID)
  nFeatures = nrow(imputations)
  
  probs = sapply(1:nFeatures, function(x) {
    unname(sapply(imputations[x,-c(1:2)], resampler[[x]]$densityValue))
  })
  probs = apply(probs, 2, function(x) x/sum(x))
  probs = apply(probs, 1, prod)
  probs = probs/sum(probs)
  
  resampledCols = sample(3:ncol(imputations), nDraws, T, probs)
  
  out = imputations[, c(1:2, resampledCols)]
  colnames(out) = c("hosueID", "feature", paste0("resample", 1:(ncol(imputations) - 2)))
  rownames(out) = NULL
  return(out)
}


houseList = lapply(unique(miceImp$houseID), 
                   indepJointDensityResample, 
                   imputedData = miceImp, 
                   resampler = resamplers, 
                   nDraws = nDraws)

resamplesOut = do.call(rbind, houseList)
fwrite(resamplesOut, "./data/mitre/working/imputationAndResamplingResults/bg1001001_sqrtHINCP_RMSP/resamples.csv")

