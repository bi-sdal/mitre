# impute HINCP, VALP, TAXP2 using mice for conditional draws, and a distribution for marginal draws
# marginal distribution: uniform with exponential tails
# problem: zero counts in bins are treated as zero likelihood, but are due to small ACS sample size
#   I implement a 'zero inflated' uniform; e.g. z/2% of income probability is a uniform draw anywhere from (0,2e5)
# second problem: zero counts in 200,000+ bin also treated as zero likelihood; inflate exponential tail by z/2%

library(dplyr)
library(mice)

load("./data/working/cleaned_CL_ACS.RData")

# -----------------------------------------------------------------------
# threshhold CoreLogic data to maximum in PUMS
# -----------------------------------------------------------------------

max(PUMS$VALP) # $1.4m
max(PUMS$TAXP2) # $33,368

CLdata$TOTAL.VALUE.CALCULATED[CLdata$TOTAL.VALUE.CALCULATED > max(PUMS$VALP)] <- max(PUMS$VALP)
CLdata$TAX.AMOUNT[CLdata$TAX.AMOUNT > max(PUMS$TAXP2)] <- max(PUMS$TAXP2)

# -----------------------------------------------------------------------
# generate 1000 (nsamp) conditional samples with mice
# -----------------------------------------------------------------------

# create combined data frame
CLdata$source <- "CL"
CLdata$HINCP <- NA
PUMS$source <- "PUMS"
PUMS$BlockGroup <- NA
CL_PUMS <- rbind( CLdata %>% dplyr::select(HINCP,VALP=TOTAL.VALUE.CALCULATED,TAXP2=TAX.AMOUNT,BlockGroup,source),
                  PUMS %>% dplyr::select(HINCP,VALP,TAXP2,BlockGroup,source) )
# transform income to satisfy linear regression assumptions
CL_PUMS$sqrtHINCP <- sqrt(CL_PUMS$HINCP)

# impute using Bayesian linear regression; add the imputed values to data frame
# be careful of memory constraints on number of draws (100 draws is 360Mb)

nsamp = 10
mice.out <- mice(data=CL_PUMS %>% dplyr::select(sqrtHINCP,VALP,TAXP2), m = nsamp, method="norm")
imputed_draws <- mice.out$imp$sqrtHINCP

for(i in 2:10){ #10 * nsamp total draws, this is done in a loop because doing it all at once (like m = 10 * nsamp in the function) can cause memory headaches
  mice.out <- mice(data=CL_PUMS %>% dplyr::select(sqrtHINCP,VALP,TAXP2), m = nsamp, method="norm")
  imputed_draws <- cbind(imputed_draws,mice.out$imp$sqrtHINCP)
}
imputed_draws[imputed_draws < 0] <- 0 # restrict household income to be positive

# -----------------------------------------------------------------------
# get the marginal distribution for income by block group; piecewise uniform with exponential tails
# -----------------------------------------------------------------------

# use counts from ACS table to estmiate a piecewise uniform distribution by blockgroup
# fit an exponential tail when income is $200,000+
income_marginal <- read.csv("./data/original/synthpop data/ACS_13_5YR_B19001_with_ann.csv",header=TRUE,skip=1)[,c(1:3,seq(6,37,by=2))]
income_marginal$BlockGroup <-  as.numeric(substr(income_marginal$Id2,6,12))
income_marginal <- cbind(BlockGroup=income_marginal$BlockGroup,income_marginal[,4:19])
income_marginal <- income_marginal %>% filter(BlockGroup != 9801001) # filter out blockgroup with 0 observations
# aggregate counts every 25k
income_marginal <- income_marginal %>% transmute(BlockGroup,"0-25k"=Estimate..Total.....10.000.to..14.999+Estimate..Total....Less.than..10.000+
                                                       Estimate..Total.....15.000.to..19.999+Estimate..Total.....20.000.to..24.999,
                                                     "25-50k"=Estimate..Total.....25.000.to..29.999+Estimate..Total.....30.000.to..34.999+
                                                       Estimate..Total.....35.000.to..39.999+Estimate..Total.....40.000.to..44.999+Estimate..Total.....45.000.to..49.999,
                                                     "50-75k"=Estimate..Total.....50.000.to..59.999+Estimate..Total.....60.000.to..74.999,
                                                     "75-100k"=Estimate..Total.....75.000.to..99.999,
                                                     "100-125k"=Estimate..Total.....100.000.to..124.999,
                                                     "125-150k"=Estimate..Total.....125.000.to..149.999,
                                                     "150-200k"=Estimate..Total.....150.000.to..199.999,
                                                     "200+k"=Estimate..Total.....200.000.or.more)
#breaks <- c(-1,10000,15000,20000,25000,30000,35000,40000,45000,50000,60000,75000,100000,125000,150000,200000)
breaks <- c(-1,25000,50000,75000,100000,125000,150000,200000)

ncols <- ncol(income_marginal)
z <- 0.1 # zero inflated; uniform from 0 to 2e5 by z/2, exponential tails by z/2 to capture zero counts in the ACS

# area under piecewise uniform (histogram) density: percent of observations under 200,000
area_hist <- (1-z/2)*rowSums(income_marginal[,2:(ncols-1)])/rowSums(income_marginal[,2:ncols])
# area under exponential tail: percent over 200,000
area_exp <- (1-z/2)*income_marginal[,ncols]/rowSums(income_marginal[,2:ncols]) + z/2


widths <- diff(breaks)
marginal_uniform_density <- (1-z/2)*income_marginal[,2:(ncols-1)]/rowSums(income_marginal[,2:ncols])
marginal_uniform_density <- sweep(marginal_uniform_density,2,z/2*widths/sum(widths),FUN = "+") # add constant uniform
marginal_uniform_density_mat <- as.data.frame((as.matrix(marginal_uniform_density) %*% diag(1/widths)))
names(marginal_uniform_density_mat) <- names(marginal_uniform_density)
marginal_uniform_density_mat <- cbind(BlockGroup=income_marginal$BlockGroup,marginal_uniform_density_mat)

# next, fit exponential tail for values > 200,000
samp_income <- PUMS$HINCP[PUMS$HINCP > 200000 & !is.na(PUMS$HINCP)] - 200000
# use the MLE for an exponential distribution, 1/x-bar
mle_lambda <- 1/mean(samp_income)
# plot the fitted exponential tail
hist(samp_income+2e5, freq=FALSE,breaks=30,xlab="Income",main="PUMS Fitted Exponential Tail")
xs <- seq(2e5,8e5,length=100)
lines(xs, mle_lambda*exp(-mle_lambda*(xs-2e5)),col=2,lwd=2)

# write a function for the marginal distribution for each blockgroup; make plots
# vectorize this function (vector of inputs for income and blockgroup)

marginal_dist <- function(income,blockgroup){
  # get the piecewise uniform density, area_exp corresponding to this blockgroup
  ind <- match(blockgroup,marginal_uniform_density_mat$BlockGroup)
  uniform_density_bg <- as.matrix(marginal_uniform_density_mat[ind,-1])
  area_exp_bg <- area_exp[ind]
  
  density_out <- rep(NA,length(income))
  # find the range income falls into, return uniform density
  cut_ind <- cut(income,breaks,labels=1:(length(breaks)-1))
  for(i in 1:length(density_out)){
    density_out[i] <- uniform_density_bg[i,cut_ind[i]]
  }
  # restricted to positive income
  density_out[income < 0] <- 0
  # exponential tail
  ind_highincome <- which(income >= 2e5)
  density_out[ind_highincome] <- area_exp_bg[ind_highincome] * mle_lambda * exp(-mle_lambda*(income[ind_highincome]-2e5))
  
  return(density_out)
}

#marginal_dist(140000,1002002)
#test <- marginal_dist(income=runif(n=nrow(CLdata),min=0,max=4e5),blockgroup=CLdata$BlockGroup)
#sum(log(test)) # loglikelihood

# -----------------------------------------------------------------------
# plot the marginal distributions by blockgroup for a sample of 20
# -----------------------------------------------------------------------

par(mfrow=c(5,4))
par(mar=c(3,3,1,1))
samp_bg <- sort(sample(marginal_uniform_density_mat$BlockGroup,20))
for(i in 1:20){
  xs <- seq(0,4e5,length=400)
  bg <- samp_bg[i]
  ys <- marginal_dist(xs,rep(bg,length(xs)))
  plot(xs,ys,type="l",xlab="Income",ylab="Density",main="",ylim=c(0,max(ys)))
  text(paste(bg),x=3.3e5,y=max(ys)*.75,font=2,cex=1.5)
}

# -----------------------------------------------------------------------
# draw imputed samples from the marginal distribution (independently for each household)
# -----------------------------------------------------------------------

ndraws <- 20

imputed_income <- imputed_draws^2
marginal_weights <- matrix(NA,nrow=nrow(imputed_draws),ncol=ncol(imputed_draws))
marginal_samp_prob <- matrix(NA,nrow=nrow(imputed_draws),ncol=ncol(imputed_draws))

for(i in 1:ncol(imputed_draws)){
  marginal_weights[,i] <- marginal_dist(income=imputed_income[,i],blockgroup=CLdata$BlockGroup) # likelihoods
  #marginal_samp_prob[,i] <- marginal_weights[,i]/sum(marginal_weights[,i])
  marginal_samp_prob[,i] <- marginal_weights[,i]
  if(i%%100==0){print(i)}
}

#sort(marginal_samp_prob[,1],decreasing = T)[1:10]
#sort(marginal_samp_prob[,1],decreasing = F)[1:10]
#max(marginal_samp_prob[,1])/min(marginal_samp_prob[,1])

# draw samples from weighted marginal distribution
income_draws <- matrix(NA,nrow=nrow(imputed_income),ncol=ndraws)
for(i in 1:nrow(imputed_income)){
  income_draws[i,] <- sample(size=ndraws, x=imputed_income[i,], prob=marginal_samp_prob[i,], replace=TRUE)
}

# -----------------------------------------------------------------------
# plot imputed joint income for a single draw by blockgroup (histogram) vs marginal distributions
# -----------------------------------------------------------------------

income_draws_plot <- as.data.frame(cbind(income=income_draws[,1],BlockGroup=CLdata$BlockGroup))

png("marginal_with_joint_imputed.png",width=800,height=600)
par(mfrow=c(5,4))
par(mar=c(3,3,1,1))
samp_bg <- sort(sample(unique(income_draws_plot$BlockGroup),20))
for(i in 1:20){
  xs <- seq(0,4e5,length=400)
  bg <- samp_bg[i]
  ys <- marginal_dist(xs,rep(bg,length(xs)))
  ymax <- max( c(ys,hist( (income_draws_plot %>% filter(BlockGroup==bg))[,1], plot=FALSE )$density ) )
  hist( (income_draws_plot %>% filter(BlockGroup==bg))[,1], add=FALSE, freq = FALSE, xlim=c(0,4e5),ylim=c(0,ymax*1.1),
        col="lightblue",border="blue",main="",xlab="Income",ylab="Density",
        breaks = c(breaks,2.5e5,3e5,3.5e5,4e5,Inf) )
  lines(xs,ys,type="l",xlab="Income",ylab="Density",main="",lwd=2,col="red")
  text(paste(bg),x=3.3e5,y=max(ys)*.75,font=2,cex=1.5)
}
dev.off()

# -----------------------------------------------------------------------
# plot imputed conditional income (from mice) for a single draw by blockgroup (histogram) vs marginal distributions
# -----------------------------------------------------------------------

income_conditional_plot <- as.data.frame(cbind(income=imputed_draws[,1]^2,BlockGroup=CLdata$BlockGroup))

png("marginal_with_mice_draw.png",width=800,height=600)
par(mfrow=c(5,4))
par(mar=c(3,3,1,1))
#samp_bg <- sort(sample(unique(income_draws_plot$BlockGroup),20))
for(i in 1:20){
  xs <- seq(0,4e5,length=400)
  bg <- samp_bg[i]
  ys <- marginal_dist(xs,rep(bg,length(xs)))
  ymax <- max( c(ys,hist( (income_draws_plot %>% filter(BlockGroup==bg))[,1], plot=FALSE )$density ) )
  hist( (income_conditional_plot %>% filter(BlockGroup==bg))[,1], add=FALSE, freq = FALSE, xlim=c(0,4e5),ylim=c(0,ymax*1.1),
        col="green",border="darkgreen",main="",xlab="Income",ylab="Density",
        breaks = c(breaks,2.5e5,3e5,3.5e5,4e5,Inf) )
  lines(xs,ys,type="l",xlab="Income",ylab="Density",main="",lwd=2,col="red")
  text(paste(bg),x=3.3e5,y=max(ys)*.75,font=2,cex=1.5)
}
dev.off()


# -----------------------------------------------------------------------
# plot joint distributions of imputed income, value, taxes vs PUMS
# -----------------------------------------------------------------------

png("conditional_imputed.png",width=800,height=800)
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
dev.off()




