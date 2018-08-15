imputationsIn = fread(imputationsPath)

# The imputation variable is continuous, so it must be binned according to breaks. Breaks are the lower bounds.
# We must also estimate a tail density

breaks = list(incomeBreaks = c(0,25000,50000,75000,100000,125000,150000,200000), roomBreaks = 1:9)
cutoffs = list(expCutoff = breaks$incomeBreaks[length(breaks$incomeBreaks)], roomCutoff = breaks$roomBreaks[length(breaks$roomBreaks)])
models = list("exponential", "geometric")
marginals = list(marginalIncome[1, -1], marginalRooms[1, -1] + 1)

highIncome = filter(clAtrackPums, source == "PUMS" & HINCP > cutoffs$expCutoff)[,'HINCP']
manyRooms = filter(clAtrackPums, source == "PUMS" & RMSP > cutoffs$roomCutoff)[,'RMSP']
parms = list(mleLambda = 1 / mean(highIncome - cutoffs$expCutoff), mleGeom = 1 / mean(manyRooms - cutoffs$roomCutoff))

resamplers = mapply(resamplerCtor, marginals, breaks, models, parms, SIMPLIFY = FALSE)

# Begin resampling step


resampledDraws = list()
nRows = nrow(filter(clAtrackPums, BlockGroup == 1001001))

# Each element of houselist is a home. It is i by j where i is the number of features and j is the number of imputations



houseList = lapply(unique(imputationsIn$houseID), 
                   indepJointDensityResample, 
                   imputedData = imputationsIn, 
                   resampler = resamplers, 
                   nDraws = nDraws)

resamplesOut = do.call(rbind, houseList)


