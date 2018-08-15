load(paste0(featurePath, "/logisticRegressions.Rdata"))

logisticRegressions

coefs = sapply(logisticRegressions, coefficients)
fits = sapply(logisticRegressions, fitted)

par(mfrow = c(2, 2))
for(i in 1:3){
  hist(coefs[i,], main = rownames(coefs)[i])
}

sapply(fits, range)
