
#
# Imputation and resampling Functions
#

makeMarginalDensity = function(marginData, breaks, z = .1, type = "unifExp", expCutoff = breaks[length(breaks)], expParm = 1){
  
  # Compute the density for values less than the cutoff and average with a uniform
  
  ncols <- ncol(marginData)
  widths <- diff(breaks)
  marginal_uniform_density <- (1-z/2)*marginData[,2:(ncols-1)]/rowSums(marginData[,2:ncols])
  mudSums = rowSums(marginal_uniform_density)
  marginal_uniform_density <- sweep(marginal_uniform_density, 2, z/2*widths/sum(widths), FUN = "+")
  marginal_uniform_density = sweep(marginal_uniform_density, 1, rowSums(marginal_uniform_density)/(mudSums), FUN = "/")
  
  
  marginal_uniform_density_mat <- as.data.frame((as.matrix(marginal_uniform_density) %*% diag(1/widths)))
  names(marginal_uniform_density_mat) <- names(marginal_uniform_density)
  marginal_uniform_density_mat <- cbind(BlockGroup=marginData$BlockGroup,marginal_uniform_density_mat)
  
  # Get the proportion of data greater than the cutoff
  
  area_exp <- (1-z/2)*marginData[,ncols]/rowSums(marginData[,2:ncols]) + z/2
  
  return(list(margin = marginal_uniform_density_mat, exponentialMargin = area_exp, expCutoff = expCutoff, expParm = expParm, breaks = breaks))
  
}
findMarginalDensity = function(marginData, income, blockgroup){
  
  # get the piecewise uniform density, area_exp corresponding to this blockgroup
  ind <- match(blockgroup, marginData$margin$BlockGroup)
  uniform_density_bg <- as.matrix(marginData$margin[ind,-1])
  area_exp_bg <- marginData$exponentialMargin[ind]
  
  density_out <- rep(NA, length(income))
  # find the range income falls into, return uniform density
  cut_ind <- cut(income, marginData$breaks, labels=1:(length(marginData$breaks)-1))
  for(i in 1:length(density_out)){
    density_out[i] <- uniform_density_bg[i, cut_ind[i]]
  }
  # restricted to positive income
  density_out[income < 0] <- 0
  # exponential tail
  ind_highincome <- which(income >= 2e5)
  density_out[ind_highincome] <- area_exp_bg[ind_highincome] * marginData$expParm * exp(-marginData$expParm*(income[ind_highincome]-marginData$expCutoff))
  
  return(density_out)
}

imputeWithMICE = function(data, impCol, regressorCols, imputations = 50, ...){
  miceData = data[,c(impCol, regressorCols)]
  
  mice.out <- mice(data=miceData, m = imputations, ...)
  #mice.out <- mice(data=miceData, m = imputations)
  if(length(impCol) == 1) return(as.matrix(mice.out$imp[[impCol]]))
  if(length(impCol) > 1) return(as.matrix(mice.out$imp[impCol]))
  
}

# Function to find the distance between a police case and the homes datasets and reading those distances from the DB

computePoliceToResidenciesDist = function(policeLonLat, residenciesLonLat){
  
  dists = apply(residenciesLonLat, 1, function(x){
    distm(x, policeLonLat, distVincentySphere)/1000
  })
  
  return(dists)
}


getHomesInRadius = function(callNumber, policeData, resData, connection, radius = .2){
  distMatRow = match(callNumber, policeData$Call_No)
  dists = DBI::dbGetQuery(connection, sprintf("SELECT * FROM police_distances_long WHERE police = '%s';", distMatRow))$distance
  cols = which(dists < radius)
  if(length(cols) == 0){
    warning("No records were found in the given radius.")
    return(NULL)
  }
  return(cbind(resData[cols,], distance = dists[cols]))
}

softmax = function(vals, penalty){
  if(penalty > 0){
    warning("Penalty is greater than zero. Using the negative of the given penalty")
    penalty = -penalty
  }
  nums = exp(vals * penalty)
  return(nums/sum(nums))
}

# Functions for multivariate imputation and resampling

# Takes an ACS table and returns a density function. Includes uniform probability mixing and an exponential fit for the tail

densityFunctionFromTable = function(marginData, breaks, z = .1, expCutoff = breaks[length(breaks)], expParm = 1){
  yData = unlist(yData)
  # Break points are deciles by default
  if(missing(breakPoints)) breakPoints = c(0, quantile(yData, seq(0.1, .9, length = 9)))
  widths = diff(breakPoints)
  
  # Computes the proportion of data in each bin (non-trivial for user supplied points) and divides by the width to get the density value
  binProps = table(findInterval(yData, breakPoints))/length(yData)
  tailDensity = binProps[length(binProps)]
  marginDensity = binProps[-length(binProps)]/widths
  
  # The right tail is fit with an exponential distribution shifted to the rightmost breakpoint.
  # If no exponential parameter is provided, the function estimates one from the tail data
  if(missing(expParm)) expParm = 1 / (mean(yData[yData > max(breakPoints)]) - max(breakPoints))
  
  densityValue = function(y){
    if(y < 0) return(0)
    bin = findInterval(y, breakPoints)
    if(bin == length(binProps)) {
      return(tailDensity * expParm * exp(-expParm * (y - max(breakPoints))))
    }else{
      return(marginDensity[bin])
    }
  }
  
  out = list(breakPoints = breakPoints, densityValue = densityValue)
  
  return(out)
}


resampleRow = function(yImputeRow, resampler){
  
  resampleProbs = unlist(sapply(yImputeRow, resampler$densityValue))
  return(sample(yImputeRow, length(yImputeRow), replace = TRUE, prob = resampleProbs))
}

indepJointDensityResample = function(resampleRow, imputedData, resampler, nDraws){
  
  imputations = do.call(rbind, lapply(imputedData, function(x) x[resampleRow,]))
  nFeatures = length(imputedData)
  
  probs = sapply(1:nFeatures, function(x) {
    unname(sapply(imputations[x,], resampler[[x]]$densityValue))
  })
  probs = apply(probs, 2, function(x) x/sum(x))
  probs = apply(probs, 1, prod)
  probs = probs/sum(probs)
  
  resampledDraws = imputations[,sample(1:ncol(imputations), nDraws, T, probs)]
  return(as.matrix(resampledDraws))
}

resamplerCtor = function(marginalTable, breakPoints, densityType, parms){
  widths = diff(breakPoints)
  marginalTable = unlist(marginalTable)
  
  binProps = marginalTable/sum(marginalTable)
  tailDensity = binProps[length(binProps)]
  marginDensity = binProps[-length(binProps)]/widths
  
  densityValue = switch(densityType,
                        exponential = function(y){
                          if(y < 0) return(0)
                          bin = findInterval(y, breakPoints)
                          if(bin == length(binProps)) {
                            return(tailDensity * parms[1] * exp(-parms[1] * (y - max(breakPoints))))
                          }else{
                            return(marginDensity[bin])
                          }
                        },
                        geometric = function(y){
                          if(y < 0) return(0)
                          bin = findInterval(y, breakPoints)
                          if(bin == length(binProps)) {
                            return(
                              tailDensity * parms[1] * (1 - parms[1])^(y - max(breakPoints) )
                            )
                          }else{
                            return(marginDensity[bin])
                          }
                        }
  )
  
  out = list(breakPoints = breakPoints, densityValue = densityValue, parameters = parms)
}

