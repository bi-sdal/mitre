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
fwrite(imputationsOut, sprintf("./data/mitre/working/imputationAndResamplingResults/%s/imputations.csv", destFile))



