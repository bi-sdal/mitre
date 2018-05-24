# Three data elements:
# jointData contains a sample of both x and y (like PUMS)
# univariteData contains only x (like corelogic)
# yMargin has a sketch of the density of y
library(mice)
library(MASS)
source("src/synthpopSimulationStudy/synthpopSimFunctions.R")
nPop = 50000
nJoint = 1000
nUni = 200

# Create the population

xPop = rgamma(nPop, 5, 5)
yPop = xPop * rbeta(nPop, 2, 10)
par(mfrow = c(2, 1))
hist(xPop, prob = T)
hist(yPop, prob = T)
cor(xPop, yPop)

# Sample the joint density. 

jointData = data.frame(y = yPop[1:nJoint], x = xPop[1:nJoint])
xData = xPop[nJoint + (1:nUni)]

# Use MICE to impute x

miceData = rbind(jointData, cbind(y = NA, x = xData))

nImpute = 1000
yImpute = imputeWithMICE(miceData, impCol = "y", regressorCols = "x", imputations = nImpute, method = 'norm')

# Now for each row we resample the columns, with probabilities proportional to the ones in the marginal distribution

resampler = autoBreaksAndMargins(yPop)
yResample = t(apply(yImpute, 1, resampleRow, resampler))

# Look at the imputed density for a row for y, and the 'true' density of that row given x

row = 2
seq = seq(0, 1, length = 1000)
par(mfrow = c(1, 1))
# True conditional distribution
plot(seq, (1/jointData$x[row])*dbeta(seq/jointData$x[row], 2, 10), type = 'l', col = 'black')

# Imputed conditional Distribution
hist(yImpute[row,], prob = T, add = T, col = rgb(1,0,0,0.1))
hist(yResample[row,], prob = T, add = T, col = rgb(0,0,1,0.1))
legend('topright', fill = c('black', 'red', 'blue'), legend = c("True", "Mice", "Resampled Mice"), bty = 'n')

median(yPop)
median(yImpute[row,])
median(yResample[row,])

