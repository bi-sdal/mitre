# Three data elements:
# jointData contains a sample of both x and y (like PUMS)
# univariteData contains only x (like corelogic)
# yMargin has a sketch of the density of y
# The goal is to use x to impute y, and then resample from that with ymargin

library(mice)
#library(data.table)
library(MASS)
source("src/synthpopSimulationStudy/synthpopSimFunctions.R")
nPop = 100000
nJoint = 1000
nPoint = 200

# Create the population

xPop = rgamma(nPop, 2, 5)
#yPop = data.frame(y1 = xPop * rbeta(nPop, 2, 2), y2 = xPop * rbeta(nPop, 2, 6))
yPop = data.frame(y1 = abs(rnorm(nPop, xPop, .1)), y2 = abs(rnorm(nPop, xPop/2, .2)))
nFeatures = ncol(yPop)
par(mfrow = c(3, 1))
hist(xPop, prob = T)
sapply(colnames(yPop), function(x) hist(yPop[,x], prob = T))
cor(cbind(xPop, yPop))


# Sample the joint density. 

jointData = data.frame(yPop[1:nJoint,], x = xPop[1:nJoint])
xData = xPop[nJoint + (1:nPoint)]
miceData = rbind(jointData, cbind(y1 = NA, y2 = NA, x = xData))

# Since we know y's distribution, we can find it's true margins and adjust the sampler towards those.
# Drump up some cut points. This is arbitrary

resampler = lapply(1:nFeatures, function(x) autoBreaksAndMargins(yPop[,x]))


# Use MICE to impute y
nImpute = 1000
yImpute = imputeWithMICE(miceData, impCol = colnames(yPop), regressorCols = "x", imputations = nImpute)

# Now for each row we resample the columns, with probabilities proportional to the ones in the marginal distribution
# In the multivariate case, the same column from each list element is sampeled on a row by row basis.
# The probability of a column is proportional to the product of the marginals for each column


# Look at the imputed density for a row for y, and the 'true' density of that row given x

row = 1
yResample = indepJointDensityResample(row, yImpute, resampler)

# True conditional distribution


par(mfrow = c(1, 1))
margin = 2
hist(yPop[,margin], prob = T, col = rgb(0,0,0,.1))
hist(unlist(yImpute[[margin]][row,]), prob = T, add = T, col = rgb(1,0,0,0.3))
hist(yResample[margin,], prob = T, add = T, col = rgb(0,0,1,0.3))
legend('topright', fill = c('black', 'red', 'blue'), legend = c("True", "Mice", "Resampled Mice"), bty = 'n')
# Imputed conditional Distribution

median(yPop[,margin])
median(unlist(yImpute[[margin]][row,]))
median(yResample[margin,])
