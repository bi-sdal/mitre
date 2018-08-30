imputationsIn = fread(imputationsPath)

# The imputation variable is continuous, so it must be binned according to breaks. Breaks are the lower bounds.
# We must also estimate a tail density

highIncome = filter(clAtrackPums, source == "PUMS" & HINCP > cutoffs$expCutoff)[,'HINCP']
manyRooms = filter(clAtrackPums, source == "PUMS" & RMSP > cutoffs$roomCutoff)[,'RMSP']
parms = list(mleLambda = 1 / mean(highIncome - cutoffs$expCutoff), mleGeom = 1 / mean(manyRooms - cutoffs$roomCutoff), NULL, NULL)

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


