library(dplyr)
library(data.table)
library(stringr)

## code to carry out a logistic regression by block group
## for each of the synthetic information Arlingtons generated.

## Assumes the file logisticByBlockGroup.R has been read in and the
## data objects: 
## datHouseBG[N,2] gives houseID and blockGroup for each of the N=44642 houses in Arlington
## domeLocs[2005,2] giving the houseID sampled for each DOME event.
## resamples[N*p,2+nreal]  holds the realizations of sampled Arlingtons
## synthHH[p,N,nreal] holding the p covariates for each of N households, for each
##                    of the nreal realizations.
## synthAC[ncases,nreal] holds the houseID's corresponding to the cases in the nreal realizations.

## N = number of households
## ncases = number of DOME calls
## p = number of variables considered (many build via smartScatter)
## nreal = number of MC (or MCMC) realizations.



# grab the covariates realization and the randomly assigned cases
#KVALS <- sort(sample(1:nreal,nreal,replace=FALSE))
devOut <- matrix(NA,nrow=4,ncol=length(KVALS))
for(kval in 1:length(KVALS)){
  print(kval); k=KVALS[kval]
  #print(c(k,kval))
 rsampName = paste('resample',k,sep="")
 y1 <- rep(0,N); y1[synthAC[,k]] <- 1
  # grab the features 
 fnames <- unique(resamples$feature)
 XX <- t(synthHH[,,k])
 colnames(XX) <- fnames
  # note: many houses have repeat offenses 
  # table(table(synthAC[,1])); for now treat as a 1
 df1 <- data.frame(datHouseBG,XX,y=y1)
  # summarize by block group
 df1 %>% group_by(blockGroup) %>%
   summarize(medInc = median(sqrtHINCP),single_parent=mean(singleParent),
             RMSP=mean(RMSP),householdSize=mean(householdSize),
             unmarriedPartner=mean(unmarriedPartner),snKid=mean(snKid),
             multiGenHouse=mean(multiGenHouse),milWoman=mean(milWoman),
             cases=sum(y),n=n()) %>%
   mutate(prob=cases/n) %>% filter(n > 10) -> df2
 
 
 
 #plot(df2$medInc,df2$prob)
 logit <- function(p) log(p/(1-p))
 #plot(df2$medInc,logit(df2$prob),xlim=c(70000,145000),xlab='median income',ylab='logit P(Abuse)')
 #plot(sqrt(df2$medInc),logit(df2$prob),xlim=sqrt(c(70000,145000)))
 
 fit0 <- glm(cbind(cases,n-cases) ~ medInc + RMSP, data=df2, family=binomial(link='logit'))
 fit1 <- glm(cbind(cases,n-cases) ~ medInc + RMSP + single_parent + householdSize +
               unmarriedPartner + snKid + multiGenHouse + milWoman, 
               data=df2, family=binomial(link='logit'))
  # load up the deviance and aic values
 devOut[,kval] = c(fit0$deviance,fit0$aic,fit1$deviance,fit1$aic)
}
  # make a plot
 PDF=TRUE
 if(PDF) pdf('fits.pdf',width=9,height=4)
   par(mfrow=c(1,2),oma=c(0,0,1.2,0),mar=c(4,4,1.7,1))
   ddev = devOut[1,] - devOut[3,]
   hist(ddev,main="dev(baseline) - dev(ss)",xlab="change in deviance")
   daic = devOut[2,] - devOut[4,]
   hist(daic,main="aic(baseline) - aic(ss)",xlab="change in AIC")
 if(PDF) dev.off()
   
   
  