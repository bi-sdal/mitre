# 1) Source Parameters, init snowfall

source("./src/SMARTscatter/parameterFiles/allDataFullRuns.R")

#
# Snowfall init
#

library(snowfall)
sfInit(parallel=TRUE, cpus=30)
sfSource("./R/00-simulateArlFunctions.R")
sfLibrary(dplyr)
sfLibrary(mice)
sfLibrary(data.table)
sfExport('bgs', 'featurePath', 'nImputations', 'nDraws', 'imputationColumns', 'regCols', 'miceMethods', 'clAtrackPums', 'breaks', 'cutoffs', 'models', 'marginals')


#
# 2) Perform MICE imputations
#
t0 = Sys.time()
sfSapply(bgs, function(bg){
  blockgroupPath = paste0(featurePath, "/bg_",bg)
  if(!dir.exists(blockgroupPath)) dir.create(blockgroupPath)
  SD = filter(clAtrackPums, BlockGroup == bg | source == "PUMS")
  nHomes = nrow(filter(SD, BlockGroup == bg))
  
  imputed_draws = imputeWithMICE(SD, 
                                 impCol = imputationColumns, 
                                 regressorCols = regCols, 
                                 imputations = nImputations, 
                                 method = miceMethods)
  
  ### If a transformed variable is imputed, make sure to do the INVERSE TRANSFORMATION before passing it to the resampler
  
  imputed_draws[[1]] = imputed_draws[[1]]^2
  
  imputationsOut = do.call(rbind, imputed_draws) %>%
    data.frame(houseID = filter(SD, BlockGroup == bg)$houseID, feature = rep(imputationColumns, each = nHomes), .) %>%
    data.table
  setnames(x = imputationsOut, colnames(imputationsOut), new = c("houseID", "feature", paste0("imputation", 1:nImputations)))
  # Write imputations to file
  fwrite(imputationsOut, paste0(blockgroupPath, "/imputations.csv"))
})

#
# 3) Resample according to marginals
#

sapply(bgs, function(bg){
  imputationsPath = paste0(featurePath, "/bg_",bg, "/imputations.csv")
  imputationsIn = fread(imputationsPath)
  
  # The imputation variable is continuous, so it must be binned according to breaks. Breaks are the lower bounds.
  # We must also estimate a tail density
  
  highIncome = filter(clAtrackPums, source == "PUMS" & HINCP > cutoffs$expCutoff)[,'HINCP']
  manyRooms = filter(clAtrackPums, source == "PUMS" & RMSP > cutoffs$roomCutoff)[,'RMSP']
  parms = list(mleLambda = 1 / mean(highIncome - cutoffs$expCutoff), mleGeom = 1 / mean(manyRooms - cutoffs$roomCutoff), NULL, NULL)
  
  resamplers = mapply(resamplerCtor, marginals, breaks, models, parms, SIMPLIFY = FALSE)
  
  # Begin resampling step
  
  
  resampledDraws = list()
  
  # Each element of houselist is a home. It is i by j where i is the number of features and j is the number of imputations
  
  houseList = lapply(unique(imputationsIn$houseID), 
                     indepJointDensityResample, 
                     imputedData = imputationsIn, 
                     resampler = resamplers, 
                     nDraws = nDraws)
  
  resamplesOut = do.call(rbind, houseList)
  # Write resamples to file
  fwrite(resamplesOut, paste0(featurePath, "/bg_",bg, "/resamples.csv"))
})

t1 = Sys.time() - t0
t1
