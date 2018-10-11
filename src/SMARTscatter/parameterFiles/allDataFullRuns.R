# # Load packages and source function scripts
library(dplyr)
library(mice)
library(data.table)
library(snowfall)
source("./R/00-simulateArlFunctions.R")
#source("./src/SMARTscatter/R")

#
# Load prepared data
#

source("./src/SMARTscatter/01-prepareAndLoadData.R")


# Define global parameters
nImputations = 5
nDraws = 3
regCols = c('VALP', 'TAXP2')
imputationColumns = c("sqrtHINCP", "RMSP", "householdSize", 'singleParent', 'snKid', 'militaryService', 'unmarriedPartner', 'multiGenHouse')
regMethods = c('norm', 'norm')
imputationMethods = c('norm', 'cart', 'cart', 'cart', 'logreg', 'logreg', 'logreg', 'logreg')
miceMethods = c(imputationMethods, regMethods)

# Paramerters for resampling

breaks = list(incomeBreaks = c(0,25000,50000,75000,100000,125000,150000,200000), roomBreaks = 1:9, NULL, NULL, NULL, NULL, NULL, NULL)
cutoffs = list(expCutoff = breaks$incomeBreaks[length(breaks$incomeBreaks)], roomCutoff = breaks$roomBreaks[length(breaks$roomBreaks)], NULL, NULL, NULL, NULL, NULL, NULL)
models = list("exponential", "geometric", 'constant', 'constant', 'constant', 'constant', 'constant', 'constant')
marginals = list(marginalIncome[1, -1], marginalRooms[1, -1] + 1, NULL, NULL, NULL, NULL, NULL, NULL)

# Make directiries (if needed)

featurePath = sprintf("./data/mitre/working/imputationAndResamplingResults/%s", paste0(imputationColumns, collapse = '_'))
if(!dir.exists(featurePath)) dir.create(featurePath)

bgs = na.omit(unique(clAtrackPums$BlockGroup))


# The maximum number of addresses to consider as possible candidates for the softmax.
maxCandidates = 20
# The decay penalty lowers the probability of an event happening at distant houses. Must be less than zero
decayPenalty = -50