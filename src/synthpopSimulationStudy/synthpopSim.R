# Three data elements:
# jointData contains a sample of both x and y (like PUMS)
# univariteData contains only x (like corelogic)
# yMargin has a sketch of the density of y
library(mice)
nPop = 100000
nJoint = 1000
nUni = 200

# Create the population

xPop = rgamma(nPop, 5, 5)
yPop = xPop * rbeta(nUni, 2, 10)
par(mfrow = c(2, 1))
hist(xPop, prob = T)
hist(yPop, prob = T)
cor(xPop, yPop)

# Sample the joint density. 

jointData = data.frame(y = yPop[1:nJoint], x = xPop[1:nJoint])
xData = xPop[nJoint + (1:nUni)]

# Since we know y's distribution, we can find it's true margins and adjust the sampler towards those.

breakPoints = c(-Inf, seq(0, .5, length = 10), 1.75176)
yMargin = diff(c(sapply(breakPoints, function(x) mean(yPop < x))))

# Use MICE to impute x

miceData = rbind(jointData, cbind(y = NA, x = xData))

nImpute = 1000

imputeWithMICE = function(data, impCol, regressorCols, outName, imputations = 50){
  
  miceData = data[,c(impCol, regressorCols)]
  
  mice.out <- mice(data=miceData, m = imputations, method="norm")
  return(as.matrix(mice.out$imp[[impCol]]))
  
}

yImpute = imputeWithMICE(miceData, impCol = "y", regressorCols = "x", imputations = nImpute)

# Now for each row we resample the columns, with probabilities proportional to the ones in the marginal distribution

resampleRow = function(yImputeRow, breakPoints, margin){
  
  marginalDensityBins = as.numeric(cut(yImputeRow, breakPoints))
  resampleProbs = margin[marginalDensityBins]
  
  return(sample(yImputeRow, length(yImputeRow), replace = TRUE, prob = resampleProbs))
}

yResample = t(apply(yImpute, 1, resampleRow, breakPoints, yMargin))


# Look at the imputed density for a row for y, and the 'true' density of that row given x

row = 3
seq = seq(0, 1, length = 1000)

# True conditional distribution
plot(seq, (1/jointData$x[row])*dbeta(seq/jointData$x[row], 2, 10), type = 'l', col = 'black')

## Make the marginal histogram
nTestPoints = 10000
bins = sample(1:length(yMargin), nTestPoints, rep = T, prob = yMargin)
marSamp = sapply(bins, function(x) runif(1, breakPoints[x], breakPoints[x+1]))

# Imputed conditional Distribution
hist(yImpute[row,], prob = T, add = T, col = rgb(1,0,0,0.3))
hist(yResample[row,], prob = T, add = T, col = rgb(0,0,1,0.3))
hist(marSamp, prob = T, add = T, col = rgb(0,1,0,0.3))
legend('topright', fill = c('black', 'red', 'blue', 'green'), legend = c("True", "Mice", "Resampled Mice", 'Marginal'), bty = 'n')





