# Three data elements:
# jointData contains a sample of both x and y (like PUMS)
# univariteData contains only x (like corelogic)
# yMargin has a sketch of the density of y
# The goal is to use x to impute y, and then resample from that with ymargin

library(mice)
library(data.table)
library(MASS)
nPop = 100000
nJoint = 1000
nPoint = 200

# Create the population

xPop = rgamma(nPop, 2, 5)
yPop = data.table(y1 = xPop * rbeta(nPop, 2, 2), y2 = xPop * rbeta(nPop, 2, 6))
par(mfrow = c(3, 1))
hist(xPop, prob = T)
sapply(colnames(yPop), function(x) hist(yPop[,get(x)], prob = T))
cor(cbind(xPop, yPop))


# Sample the joint density. 

jointData = data.table(yPop[1:nJoint,], x = xPop[1:nJoint])
xData = xPop[nJoint + (1:nPoint)]

# Since we know y's distribution, we can find it's true margins and adjust the sampler towards those.
# Drump up some cut points. This is arbitrary
breakPoints = sapply(1:ncol(yPop), function(x){
  out = c(-Inf, seq(0, max(yPop[,x, with = F]) * .8, length = 10), max(yPop[,x, with = F])) 
})

# Compute the proportion of the data which falls in each bin determined by breakPoints.
yMargin = sapply(1:ncol(yPop), function(x){
  diff(c(sapply(breakPoints[,x], function(z) mean(yPop[,x, with = F] < z))))
  })

# Use MICE to impute y

miceData = rbind(jointData, cbind(y1 = NA, y2 = NA, x = xData))

nImpute = 1000

imputeWithMICE = function(data, impCol, regressorCols, outName, imputations = 50){
  
  miceData = data[,c(impCol, regressorCols), with = F]
  
  mice.out <- mice(data=miceData, m = imputations, method="norm")
  return(mice.out$imp[impCol])
  
}

yImpute = imputeWithMICE(miceData, impCol = colnames(yPop), regressorCols = "x", imputations = nImpute)
# Now for each row we resample the columns, with probabilities proportional to the ones in the marginal distribution
# In the multivariate case, the same column from each list element is sampeled on a row by row basis.
# The probability of a column is proportional to the product of the marginals for each column

# yImputeRow = sapply(yImpute, '[',1,)

resampleRow = function(yImputeRow, breakPoints, margin){
  
  nSims = ncol(yImputeRow)
  nobs = nrow(yImputeRow)
  resampleProbs = matrix(NA, nobs, nSims)
  
  for(i in 1:ncol(breakPoints)){
    marginalBins = as.numeric(cut(unlist(yImputeRow[,i]), breakPoints[,i]))
    resampleProbs[,i] = margin[marginalBins, i]
  }
  
  resampleProbs = apply(resampleProbs, 1, prod)
  
  return(sample(1:nobs, nobs, replace = TRUE, prob = resampleProbs))
}

extractResampledPoint = function(pointId, imputedData, reweightedColumns){
  nSims = ncol(imputedData[[1]])
  nobs = length(imputedData)
  outData = matrix(NA, nSims, nobs)
  for(i in 1:nobs){
    outData[,i] = unlist(imputedData[[i]][pointId, ])[reweightedColumns[pointId,]]
  }
  return(outData)
}


resampledColumnIndices = t(sapply(1:nPoint, function(x){
  resampleRow(sapply(yImpute, '[',x,1:nJoint), breakPoints, yMargin)
}))


# Look at the imputed density for a row for y, and the 'true' density of that row given x

row = 1
yResample = extractResampledPoint(row, yImpute, resampledColumnIndices)
seq = seq(0, 1, length = 1000)

# True conditional distribution
# yPop = data.table(y1 = xPop * rbeta(nPop, 2, 10), y2 = rnorm(nPop, xPop, 1)^2)
# For y1
plot(seq, (1/jointData$x[row])*dbeta(seq/jointData$x[row], 2, 10), type = 'l', col = 'black', ylim = c(0, 5))


## Make the marginal histogram
nTestPoints = 10000
bins = sample(1:nrow(yMargin), nTestPoints, rep = T, prob = yMargin[,1])
marSamp = sapply(bins, function(x) runif(1, breakPoints[x], breakPoints[x+1]))

# Imputed conditional Distribution
hist(unlist(yImpute$y1[row,]), prob = T, add = T, col = rgb(1,0,0,0.3))
hist(yResample[,1], prob = T, add = T, col = rgb(0,0,1,0.3))
hist(marSamp, prob = T, add = T, col = rgb(0,1,0,0.3))
legend('topright', fill = c('black', 'red', 'blue', 'green'), legend = c("True", "Mice", "Resampled Mice", 'Marginal'), bty = 'n')





