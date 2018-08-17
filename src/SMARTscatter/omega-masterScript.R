# Load packages and source function scripts
library(dplyr)
library(mice)
library(data.table)
library(snowfall)
source("./R/00-simulateArlFunctions.R")
source("./R/geocode.R")

#
# Load prepared data
#

source("./src/SMARTscatter/01-prepareAndLoadData.R")


# Define global parameters
t0 = Sys.time()
nImputations = 1500
nDraws = 750
regCols = c('VALP', 'TAXP2')
imputationColumns = c("sqrtHINCP", "RMSP", "householdSize", 'singleParent')
miceMethods = c('norm', 'cart', 'cart', 'cart', 'norm', 'norm')

# Paramerters for resampling

breaks = list(incomeBreaks = c(0,25000,50000,75000,100000,125000,150000,200000), roomBreaks = 1:9, sizeBreaks = NULL, spBreaks = NULL)
cutoffs = list(expCutoff = breaks$incomeBreaks[length(breaks$incomeBreaks)], roomCutoff = breaks$roomBreaks[length(breaks$roomBreaks)], sizeCutoff = NULL, spCutoff = NULL)
models = list("exponential", "geometric", 'constant', 'constant')
marginals = list(marginalIncome[1, -1], marginalRooms[1, -1] + 1, NULL, NULL)

# Make directiries (if needed)

featurePath = sprintf("./data/mitre/working/imputationAndResamplingResults/%s", paste0(imputationColumns, collapse = '_'))
if(!dir.exists(featurePath)) dir.create(featurePath)

bgs = na.omit(unique(clAtrackPums$BlockGroup))


# The maximum number of addresses to consider as possible candidates for the softmax.
maxCandidates = 20
# The decay penalty lowers the probability of an event happening at distant houses. Must be less than zero
decayPenalty = -50

#
# RUN THE CODE
#

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

#
# 4) Put cases in homes
#

# source("./src/SMARTscatter/04-putEventsInHouses.R")
# fwrite(houseAssignments, paste0(featurePath, "/caseAssignments.csv"))

#
# 5) Do logistic regression
#

# source("./src/SMARTscatter/05-logisticRegressions.R")
# save(logisticRegressions,file =  paste0(featurePath, "/logisticRegressions.Rdata"))

t1 = Sys.time() - t0
t1
