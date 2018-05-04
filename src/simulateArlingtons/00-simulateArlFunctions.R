
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
imputeWithMICE = function(data, impCol, regressorCols, outName, imputations = 50){
  
  miceData = data[,c(impCol, regressorCols)]
  
  mice.out <- mice(data=miceData, m = imputations, method="norm")
  return(as.matrix(mice.out$imp[[impCol]]))
  
}

# Function to find the distance between a police case and the homes datasets

computePoliceToResidenciesDist = function(policeLonLat, residenciesLonLat){
  
  dists = apply(residenciesLonLat, 1, function(x){
    distm(x, policeLonLat, distVincentySphere)/1000
  })
  
  return(dists)
}
