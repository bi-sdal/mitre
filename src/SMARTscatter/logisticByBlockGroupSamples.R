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



# read data from logisticByBlockGroup_drug.R
nreal <- readRDS('./data/mitre/working/smart_analysis/nreal.RDS')
p <- readRDS('./data/mitre/working/smart_analysis/p.RDS')
N <- readRDS('./data/mitre/working/smart_analysis/N.RDS')
synthAC <- readRDS('./data/mitre/working/smart_analysis/synthAC.RDS')
resamples <- readRDS('./data/mitre/working/smart_analysis/resamples.RDS')
synthHH <- readRDS('./data/mitre/working/smart_analysis/synthHH.RDS')
datHouseBG <- readRDS('./data/mitre/working/smart_analysis/datHouseBG.RDS')
drugBGrate <- readRDS('./data/mitre/working/smart_analysis/drugBGrate.RDS')
housingACSbg <- readRDS('./data/mitre/working/smart_analysis/housingACSbg.RDS')

# grab the covariates realization and the randomly assigned cases
KVALS <- sort(sample(1:nreal,nreal,replace=FALSE))
devOut <- matrix(NA,nrow=4,ncol=length(KVALS))
coefOut <- matrix(NA,nrow=p+2,ncol=length(KVALS))

fit_notSmart <- list()
fit_smart <- list()

pb <- progress::progress_bar$new(format = "[:bar] :current/:total (:percent)", total = length(KVALS))

for(kval in 1:length(KVALS)){
  pb$tick(1)
  #print(kval); 
 k=KVALS[kval]
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
             multiGenHouse=mean(multiGenHouse),
             #milWoman=mean(milWoman),
             militaryService=mean(militaryService),
             cases=sum(y),n=n()) %>%
   # compute the raw DOME probability by block group
   mutate(probCL=cases/n) %>%
   # add the DRUG call rate by block group (computed in geocode_DRUG.r)
   left_join(drugBGrate[,c("BlockGroup","rate")],by=c("blockGroup"="BlockGroup")) %>% 
   rename(DRUGrate = rate) %>%
   # add the housing unit count from the ACS by block group
   left_join(housingACSbg[,c("blockGroup","nunit")],by=c("blockGroup"="blockGroup")) %>% 
    # modify the mean of the binary variables in df1
    mutate(single_parent=single_parent*n/nunit,
           unmarriedPartner=unmarriedPartner*n/nunit,
           snKid=snKid*n/nunit,multiGenHouse=multiGenHouse*n/nunit,
           #milWoman=milWoman*n/nunit,
           militaryService=militaryService*n/nunit,
           DRUGrate=DRUGrate*n/nunit) %>%
    # compute the raw DOME probability by block group
   mutate(prob=cases/nunit) %>%
   # filter out small block groups and the courthouse block group
   filter(nunit > 20,blockGroup != 1017013) -> df2
 
 
 
 
 #plot(df2$medInc,df2$prob)
 logit <- function(p) log(p/(1-p))
 #plot(df2$medInc,logit(df2$prob),xlim=c(70000,145000),xlab='median income',ylab='logit P(Abuse)')
 #plot(sqrt(df2$medInc),logit(df2$prob),xlim=sqrt(c(70000,145000)))
 
 fit0 <- glm(cbind(cases,nunit-cases) ~ medInc + RMSP + DRUGrate, data=df2, family=binomial(link='logit'))
 fit1 <- glm(cbind(cases,nunit-cases) ~ medInc + RMSP + DRUGrate + single_parent + householdSize +
               unmarriedPartner + snKid + multiGenHouse + militaryService, #milWoman, 
               data=df2, family=binomial(link='logit'))
 
 fit_notSmart <- append(fit_notSmart, list(fit0))
 fit_smart <- append(fit_smart, list(fit1))
 
  # load up the deviance and aic values
 devOut[,kval] = c(fit0$deviance,fit0$aic,fit1$deviance,fit1$aic)
 coefOut[,kval] = fit1$coefficients
}

stopifnot(length(fit_notSmart) == length(fit_smart))
stopifnot(length(fit_smart) == length(KVALS))

  # make a plot
 PDF=FALSE
 if(PDF) pdf('fits.pdf',width=9,height=4)
   par(mfrow=c(1,2),oma=c(0,0,1.2,0),mar=c(4,4,1.7,1))
   ddev = devOut[1,] - devOut[3,]
   hist(ddev,main="dev(baseline) - dev(ss)",xlab="change in deviance")
   daic = devOut[2,] - devOut[4,]
   hist(daic,main="aic(baseline) - aic(ss)",xlab="change in AIC")
 if(PDF) dev.off()
   
 pairs(t(coefOut),pch='.')
  
   
# dan's mess here
if(0){
   fit1 %>%
     broom::tidy(conf.int = TRUE) %>%
     ggplot(aes(exp(estimate), term, color = term)) +
     geom_point() +
     geom_errorbarh(aes(xmin = exp(conf.low), xmax = exp(conf.high))) +
     xlim(0, 10)
}

# final data sets
saveRDS(devOut, file = './data/mitre/final/logistic_regressions/deviance.RDS')
saveRDS(coefOut, file = './data/mitre/final/logistic_regressions/coefficients.RDS')

saveRDS(fit_notSmart, file = './data/mitre/final/logistic_regressions/fit_notSmart.RDS')
saveRDS(fit_smart, file = './data/mitre/final/logistic_regressions/fit_smart.RDS')

# temp datasets (only a single realization)
saveRDS(df, file = './data/mitre/working/logistic_regressions/example_df.RDS')
saveRDS(df2, file = './data/mitre/working/logistic_regressions/example_df2.RDS')
saveRDS(fit0, file = './data/mitre/working/logistic_regressions/example_fit0.RDS')
saveRDS(fit1, file = './data/mitre/working/logistic_regressions/example_fit1.RDS')

print("DONE. logisticByBlockGroupSamples.R")
