# This is Ian rewriting Josh's code so that it makes sense to me.

library(dplyr)
library(mice)
library(data.table)
source("./src/synthpop_code/generalizedSynthPop/synthpopFunctions.R")

# Data elements. ACS_marginals is loaded in but doesn't seem to be used
# CLdata is corelogic housing data
marginalIncome = read.csv("./data/mitre/working/simulatedArlingtonData/marginalIncome.csv")
clAtrackPums = read.csv("./data/mitre/working/cleanedExampleData/clAtrackPums.csv")

# Do imputation
# Since we're using residencies with multiple units, we must divide the relevant values by the number of units
clAtrackPums$HINCP[!is.na(clAtrackPums$UNITS.NUMBER)] = clAtrackPums$HINCP[!is.na(clAtrackPums$UNITS.NUMBER)]/clAtrackPums$UNITS.NUMBER[!is.na(clAtrackPums$UNITS.NUMBER)]
clAtrackPums$VALP[!is.na(clAtrackPums$UNITS.NUMBER)] = clAtrackPums$VALP[!is.na(clAtrackPums$UNITS.NUMBER)]/clAtrackPums$UNITS.NUMBER[!is.na(clAtrackPums$UNITS.NUMBER)]
clAtrackPums$TAXP2[!is.na(clAtrackPums$UNITS.NUMBER)] = clAtrackPums$TAXP2[!is.na(clAtrackPums$UNITS.NUMBER)]/clAtrackPums$UNITS.NUMBER[!is.na(clAtrackPums$UNITS.NUMBER)]
clAtrackPums$sqrtHINCP = sqrt(clAtrackPums$HINCP)

imputed_draws = imputeWithMICE(clAtrackPums, "sqrtHINCP", c("VALP", "TAXP2"), outName = "sqrtHINCP", imputations = 1000)

###
### END IMPUTATION STEP. NEXT FIND MARGINALS.
###

# The imputation variable is continuous, so it must be binned according to breaks. 

breaks <- c(-1,25000,50000,75000,100000,125000,150000,200000)
expCutoff = 2e5

# next, fit exponential tail for values > 200,000 using the MLE for an exponential distribution, 1/x-bar
clAtrackPums %>% filter(source == "PUMS" & HINCP > expCutoff) %>% dplyr::select(HINCP) -> highInc

mle_lambda = 1/(mean(unlist(highInc) - expCutoff))

marginDist = makeMarginalDensity(marginalIncome, breaks = breaks, expParm = mle_lambda, expCutoff = expCutoff)

### If a transformed variable is imputed, make sure to do the INVERSE TRANSFORMATION before passing it to the resampler

imputations = imputed_draws^2
nDraws = 100
blockGroups = filter(clAtrackPums, source != "PUMS")$BlockGroup

resampleImputedDraws = function(imputations, nDraws = 20, blockGroups, marginDist){
  
  marginal_samp_prob <- matrix(NA, nrow = nrow(imputations), ncol = ncol(imputations))
  
  for(i in 1:ncol(imputations)){
    
    marginal_samp_prob[,i] = findMarginalDensity(marginDist, income = imputations[,i], blockgroup = blockGroups) # likelihoods
    if(i%%100==0) print(i)
    
  }
  
  income_draws <- matrix(NA,nrow=nrow(imputations),ncol=nDraws)
  
  for(i in 1:nrow(imputations)){
    income_draws[i,] <- sample(size=nDraws, x=imputations[i,], prob=marginal_samp_prob[i,], replace=TRUE)
  }
  
  colnames(income_draws) = paste0("incomeDraw", 1:nDraws)
  return(income_draws)
}

incomeDraws = resampleImputedDraws(imputations, nDraws = nDraws, blockGroups = blockGroups, marginDist = marginDist)


out = cbind(filter(clAtrackPums, source != "PUMS"), incomeDraws)


fwrite(out[,-c(1, 9)], "./data/mitre/working/simulatedArlingtonData/simulatedArlingtonIncome.csv")

# -----------------------------------------------------------------------
# plot imputed joint income for a single draw by blockgroup (histogram) vs marginal distributions
# -----------------------------------------------------------------------

income_draws_plot <- as.data.frame(cbind(income = income_draws[, 1], BlockGroup = bgs))


par(mfrow=c(5,4))
par(mar=c(3,3,1,1))
samp_bg <- sort(sample(unique(income_draws_plot$BlockGroup),20))
for(i in 1:20){
  
  xs <- seq(0,4e5,length=400)
  bg <- samp_bg[i]
  ys <- findMarginalDensity(marginDist, xs, rep(bg,length(xs)))
  ymax <- max( c(ys,hist( (income_draws_plot %>% filter(BlockGroup==bg))[,1], plot=FALSE )$density ) )
  hist( (income_draws_plot %>% filter(BlockGroup==bg))[,1], add=FALSE, freq = FALSE, xlim=c(0,4e5),ylim=c(0,ymax*1.1),
        col="lightblue",border="blue",main="",xlab="Income",ylab="Density",
        breaks = c(breaks,2.5e5,3e5,3.5e5,4e5,Inf) )
  lines(xs,ys,type="l",xlab="Income",ylab="Density",main="",lwd=2,col="red")
  text(paste(bg),x=3.3e5,y=max(ys)*.75,font=2,cex=1.5)
  
}


# -----------------------------------------------------------------------
# plot imputed conditional income (from mice) for a single draw by blockgroup (histogram) vs marginal distributions
# -----------------------------------------------------------------------

income_conditional_plot <- as.data.frame(cbind(income = imputed_draws[, 1]^2, BlockGroup=bgs))


par(mfrow=c(5,4))
par(mar=c(3,3,1,1))
#samp_bg <- sort(sample(unique(income_draws_plot$BlockGroup),20))
for(i in 1:20){
  xs <- seq(0,4e5,length=400)
  bg <- samp_bg[i]
  ys <- findMarginalDensity(marginDist, xs, rep(bg,length(xs)))
  ymax <- max( c(ys,hist( (income_draws_plot %>% filter(BlockGroup==bg))[,1], plot=FALSE )$density ) )
  hist( (income_conditional_plot %>% filter(BlockGroup==bg))[,1], add=FALSE, freq = FALSE, xlim=c(0,4e5),ylim=c(0,ymax*1.1),
        col="green",border="darkgreen",main="",xlab="Income",ylab="Density",
        breaks = c(breaks,2.5e5,3e5,3.5e5,4e5,Inf) )
  lines(xs,ys,type="l",xlab="Income",ylab="Density",main="",lwd=2,col="red")
  text(paste(bg),x=3.3e5,y=max(ys)*.75,font=2,cex=1.5)
}



# -----------------------------------------------------------------------
# plot joint distributions of imputed income, value, taxes vs PUMS
# -----------------------------------------------------------------------


CL_PUMS_plot <- CL_PUMS
CL_PUMS_plot$HINCP[CL_PUMS_plot$source=="CL"] <- income_draws[,1]
CL_PUMS_plot$color <- "black"
CL_PUMS_plot$color[CL_PUMS_plot$source=="CL"] <- "red"
CL_PUMS_plot$VALP[CL_PUMS_plot$VALP < max(CL_PUMS_plot$VALP)] <- pmin( rnorm(length(CL_PUMS_plot$VALP[CL_PUMS_plot$VALP < max(CL_PUMS_plot$VALP)]),
                                                                             mean=CL_PUMS_plot$VALP[CL_PUMS_plot$VALP < max(CL_PUMS_plot$VALP)],
                                                                             sd=CL_PUMS_plot$VALP/8), 1400000 )

par(xpd=FALSE)
pairs(~HINCP+VALP+TAXP2,data=CL_PUMS_plot,col=CL_PUMS_plot$color,pch=20,cex=0.5)
par(xpd=TRUE)
legend(x=0,y=1, legend=c("PUMS","CoreLogic (Imputed HINCP)"), fill=c("black","red"),cex=1.5)









