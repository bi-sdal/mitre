source("src/simulateArlingtons/multivariateArlingtonSim/01-prepareAndLoadData.R")

nImputations = 1000
imputationColumns = c("sqrtHINCP", "RMSP")
bg = 1001001
SD = filter(clAtrackPums, BlockGroup == bg | source == "PUMS")
nHomes = nrow(filter(SD, BlockGroup == bg))

imputed_draws = imputeWithMICE(SD, 
                               impCol = imputationColumns, 
                               regressorCols = c("VALP", "TAXP2"), 
                               imputations = nImputations, 
                               method = c('norm', 'cart', 'norm', 'norm'))

### If a transformed variable is imputed, make sure to do the INVERSE TRANSFORMATION before passing it to the resampler

imputed_draws[[1]] = imputed_draws[[1]]^2

imputationsOut = do.call(rbind, imputed_draws) %>%
  data.frame(houseID = filter(SD, BlockGroup == bg)$houseID, feature = rep(imputationColumns, each = nHomes), .) %>%
  data.table
setnames(x = imputationsOut, colnames(imputationsOut), new = c("houseID", "feature", paste0("imputation", 1:nImputations)))

# Write imputations to file
destFile = sprintf("bg%s_%s", bg, paste0(imputationColumns, collapse = '_'))


###
### END IMPUTATION STEP. NEXT FIND MARGINALS.
###

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


# hist(filter(clAtrackPums, BlockGroup == 1001001 | source == "PUMS")$HINCP, prob = TRUE, breaks = c(incomeBreaks, 1000000))
# lines(seq(0, 500000, length = 1000), sapply(seq(0, 500000, length = 1000), resamplers[[1]]$densityValue), col = 'red')
# 
# hist(filter(clAtrackPums, BlockGroup == 1001001 | source == "PUMS")$RMSP, prob = TRUE, breaks = c(roomBreaks,10:20), ylim = c(0, .25))
# lines(seq(0, 20, length = 100), sapply(seq(0, 20, length = 100), resamplers[[2]]$densityValue), col = 'red')

# Begin resampling step

nDraws = 1000
#blockGroups = filter(clAtrackPums, source != "PUMS")$BlockGroup
resampledDraws = list()
nRows = nrow(filter(clAtrackPums, BlockGroup == 1001001))

# Each element of houselist is a home. It is i by j where i is the number of features and j is the number of imputations

system.time(houseList <- lapply(1:nRows, indepJointDensityResample, imputedData = imputed_draws, resampler = resamplers, nDraws = nDraws))

featureList = lapply(1:length(houseList[[1]]), function(x){
  resampledRows = lapply(houseList, '[[', x)
  return(unlist(resampledRows))
})




