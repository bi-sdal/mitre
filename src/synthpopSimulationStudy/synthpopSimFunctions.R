autoBreaksAndMargins = function(yData, breakPoints, expParm){
  yData = unlist(yData)
  # Break points are deciles by default
  if(missing(breakPoints)) breakPoints = c(0, quantile(yData, seq(0.1, .9, length = 9)))
  widths = diff(breakPoints)
  
  # Computes the proportion of data in each bin and divides by the width to get the density value
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

# densityType is one of exponential, geometric
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









imputeWithMICE = function(data, impCol, regressorCols, imputations = 50, ...){
  
  miceData = data[,c(impCol, regressorCols)]
  
  mice.out <- mice(data=miceData, m = imputations, ...)
  if(length(impCol) == 1) return(as.matrix(mice.out$imp[[impCol]]))
  if(length(impCol) > 1) return(as.matrix(mice.out$imp[impCol]))
  
}

resampleRow = function(yImputeRow, resampler){
  
  resampleProbs = unlist(sapply(yImputeRow, resampler$densityValue))
  return(sample(yImputeRow, length(yImputeRow), replace = TRUE, prob = resampleProbs))
}

indepJointDensityResample = function(resampleRow, imputedData, resampler, nDraws = 1000){
  
  imputations = do.call(rbind, lapply(imputedData, function(x) x[resampleRow,]))
  
  probs = sapply(1:nFeatures, function(x) {
    unname(sapply(imputations[x,], resampler[[x]]$densityValue))
  })
  probs = apply(probs, 2, function(x) x/sum(x))
  probs = apply(probs, 1, prod)
  probs = probs/sum(probs)
  
  resampledDraws = imputations[,sample(1:ncol(imputations), nDraws, T, probs)]
  names(resampledDraws) = NULL
  out = list(resampledDraws[1,], resampledDraws[2,])
  names(out) = rownames(imputedData)
  return(out)
}
