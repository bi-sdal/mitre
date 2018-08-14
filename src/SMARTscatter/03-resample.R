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



houseList = lapply(unique(miceImp$houseID), 
                   indepJointDensityResample, 
                   imputedData = miceImp, 
                   resampler = resamplers, 
                   nDraws = nDraws)

resamplesOut = do.call(rbind, houseList)

destFile1 = paste0(imputationColumns, collapse = '_')
destFile2 = paste0("bg_", bg)
fwrite(resamplesOut, sprintf("./data/mitre/working/imputationAndResamplingResults/%s/%s/imputations.csv", destFile1, destFile2))
