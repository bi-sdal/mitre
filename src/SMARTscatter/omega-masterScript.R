# Load packages and source function scripts
library(dplyr)
library(mice)
library(data.table)
source("./R/00-simulateArlFunctions.R")
source("./R/geocode.R")

# Define global parameters

nImputations = 2
nDraws = 2
imputationColumns = c("sqrtHINCP", "RMSP")
miceMethods = c('norm', 'cart', 'norm', 'norm')

# Make directiries (if needed)

featurePath = sprintf("./data/mitre/working/imputationAndResamplingResults/%s", paste0(imputationColumns, collapse = '_'))
if(!dir.exists(featurePath)) dir.create(featurePath)

#bgs = unique(clAtrackPums$BlockGroup)
bgs = c(1001001, 1001002)

# The maximum number of addresses to consider as possible candidates for the softmax.
maxCandidates = 20
# The decay penalty lowers the probability of an event happening at distant houses. Must be less than zero
decayPenalty = -50

#
# RUN THE CODE
#

#
# 1) Load prepared data
#

source("./src/SMARTscatter/01-prepareAndLoadData.R")

#
# 2) Perform MICE imputations
#

for(bg in bgs) {
  blockgroupPath = paste0(featurePath, "/bg_",bg)
  if(!dir.exists(blockgroupPath)) dir.create(blockgroupPath)
  source("./src/SMARTscatter/02-imputeWithMice.R")
  # Write imputations to file
  fwrite(imputationsOut, paste0(blockgroupPath, "/imputations.csv"))
}

#
# 3) Resample according to marginals
#

for(bg in bgs) {
  imputationsPath = paste0(featurePath, "/bg_",bg, "/imputations.csv")
  source("./src/SMARTscatter/03-resample.R")
  # Write resamples to file
  fwrite(resamplesOut, paste0(featurePath, "/bg_",bg, "/resamples.csv"))
}

#
# 4) Put cases in homes
#

source("./src/SMARTscatter/04-putEventsInHouses.R")
fwrite(houseAssignments, paste0(featurePath, "/caseAssignments.csv"))

#
# 5) Do logistic regression
#

source("./src/SMARTscatter/05-logisticRegressions.R")
save(logisticRegressions,file =  paste0(featurePath, "/logisticRegressions.Rdata"))
