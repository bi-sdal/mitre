source("src/simulateArlingtons/multivariateArlingtonSim/01-prepareAndLoadData.R")

imputed_draws = imputeWithMICE(filter(clAtrackPums, BlockGroup == 1001001 | source == "PUMS"), c("sqrtHINCP", "RMSP"), c("VALP", "TAXP2"), 
                               imputations = 1000, method = c('norm', 'cart', 'norm', 'norm'))

###
### END IMPUTATION STEP. NEXT FIND MARGINALS.
###

# The imputation variable is continuous, so it must be binned according to breaks. Breaks are the lower bounds.
# We must also estimate a tail density

breaks = list(incomeBreaks = c(0,25000,50000,75000,100000,125000,150000,200000), roomBreaks = 1:9)
cutoffs = list(expCutoff = incomeBreaks[length(incomeBreaks)], roomCutoff = roomBreaks[length(roomBreaks)])
models = list("exponential", "geometric")
marginals = list(marginalIncome[1, -1], marginalRooms[1, -1])

highIncome = filter(clAtrackPums, source == "PUMS" & HINCP > expCutoff)[,'HINCP']
manyRooms = filter(clAtrackPums, source == "PUMS" & RMSP > roomCutoff)[,'RMSP']
parms = list(mleLambda = 1 / mean(highIncome - expCutoff), mleGeom = 1 / mean(manyRooms - roomCutoff))

resamplers = mapply(resamplerCtor, marginals, breaks, models, parms, SIMPLIFY = FALSE)


# hist(filter(clAtrackPums, BlockGroup == 1001001 | source == "PUMS")$HINCP, prob = TRUE, breaks = c(incomeBreaks, 1000000))
# lines(seq(0, 500000, length = 1000), sapply(seq(0, 500000, length = 1000), resamplers[[1]]$densityValue), col = 'red')
# 
# hist(filter(clAtrackPums, BlockGroup == 1001001 | source == "PUMS")$RMSP, prob = TRUE, breaks = c(roomBreaks,10:20), ylim = c(0, .25))
# lines(seq(0, 20, length = 100), sapply(seq(0, 20, length = 100), resamplers[[2]]$densityValue), col = 'red')

### If a transformed variable is imputed, make sure to do the INVERSE TRANSFORMATION before passing it to the resampler

imputed_draws[[1]] = imputed_draws[[1]]^2
nDraws = 1000
#blockGroups = filter(clAtrackPums, source != "PUMS")$BlockGroup
resampledDraws = list()
nRows = nrow(filter(clAtrackPums, BlockGroup == 1001001))
system.time(houseList <- lapply(1:nRows, indepJointDensityResample, imputedData = imputed_draws, resampler = resamplers, nDraws = nDraws))

featureList = lapply(1:length(houseList[[1]]), function(x){
  resampledRows = lapply(houseList, '[[', x)
  return(rbindlist(resampledRows))
})


