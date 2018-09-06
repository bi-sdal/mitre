library(dplyr)
library(data.table)
library(stringr)

## read in median income by blockgroup from ACS tables

## Get blockgroup list from file

bgs = list.files("./data/mitre/working/imputationAndResamplingResults/sqrtHINCP_RMSP_householdSize_singleParent_snKid_milWoman_unmarriedPartner_multiGenHouse//") %>%
  str_extract("\\d{7}") %>%
  na.omit %>%
  as.numeric
  
# resamples gives resampled Arlingtons: 44642 houses with income and # bedrooms
# houseID feature resample1 .... resample100 blockgroup
resamples = rbindlist(
  lapply(bgs, function(bg) {
    out = fread(sprintf("./data/mitre/working/imputationAndResamplingResults/sqrtHINCP_RMSP_householdSize_singleParent_snKid_milWoman_unmarriedPartner_multiGenHouse/bg_%s/resamples.csv", bg))
    out = data.table(out, blockGroup = bg)
    return(out)
  })
)

# also read in the "by block group" DRUG rates
drugBGrate <- read.csv('./data/mitre/working/PoliceData/drugBGrate.csv',stringsAsFactors = FALSE)

# read in ACS housing counts by block group
housingACSbg <- read.csv('./data/mitre/working/MiscData/housingACSbg.csv',stringsAsFactors = FALSE)

# variables: special_needs_kid; non_active_duty_woman; unmarried_partner;
# multigenerational_household;
# this is 1001 columns: Call_No resample1 ... resample1000
# this assigns a house for each DOME call, randomly, 1000 times.

# assignments = fread("./data/mitre/working/imputationAndResamplingResults/sqrtHINCP_RMSP_householdSize_singleParent_snKid_milWoman_unmarriedPartner_multiGenHouse/caseAssignments.csv")
# assignments = fread("./data/mitre/working/imputationAndResamplingResults/sqrtHINCP_RMSP_householdSize_singleParent/caseAssignments.csv")
assignments = fread("./data/mitre/working/imputationAndResamplingResults/sqrtHINCP_RMSP/caseAssignments.csv")

## make a house and blockgroup object
resamples %>% arrange(houseID,desc(feature)) -> resamples
resamples %>% select(houseID,blockGroup) %>% group_by(houseID) %>%
  summarize(blockGroup=first(blockGroup)) -> datHouseBG

## make a N x p x nreal object holding sampled realizations
nreal <- ncol(resamples) - 3   # number of synthetic realizations
N <- nrow(datHouseBG)          # number of households
p <- length(unique(resamples$feature))   # number of coveriates/features

 # carefully order the resamples so we can be sure we have all of the 
 # variables in an expected order
resamples %>% select(starts_with("resample")) -> resamples2
resamples2 <- as.numeric(as.matrix(resamples2))
synthHH <- array(resamples2,c(p,N,nreal))
 # now the cases - each column of cases selects houses at which a DOME event happened
assignments %>% select(starts_with("resample")) -> assignments2
assignments2 <- assignments2[,1:nreal]
synthAC <- as.matrix(assignments2)

browser()
## Now, combine events by block group
# par(mfrow=c(1,1),mar=c(4,4,1,1),oma=c(0,0,0,0))
# grab the covariates realization and the randomly assigned cases
# pick realization k=600 for this set of pictures
 k = 600;
 rsampName = paste('resample',k,sep="")
 y1 <- rep(0,N); y1[synthAC[,k]] <- 1
  # grab the features 
 fnames <- unique(resamples$feature)
 XX <- t(synthHH[,,k])
 colnames(XX) <- fnames
  # note: many houses have repeat offenses 
  # table(table(synthAC[,1])); for now treat as a 1
 rm(df1)
 df1 <- data.frame(datHouseBG,XX,y=y1)
  # summarize by block group
 df1 %>% group_by(blockGroup) %>%
   summarize(medInc = median(sqrtHINCP),single_parent=mean(singleParent),
             RMSP=mean(RMSP),householdSize=mean(householdSize),
             unmarriedPartner=mean(unmarriedPartner),snKid=mean(snKid),
             multiGenHouse=mean(multiGenHouse),milWoman=mean(milWoman),
             cases=sum(y),n=n()) %>%
    # compute the raw DOME probability by block group
   mutate(probCL=cases/n) %>%
    # add the DRUG call rate by block group (computed in geocode_DRUG.r)
   left_join(drugBGrate[,c("BlockGroup","rate")],by=c("blockGroup"="BlockGroup")) %>% 
   rename(DRUGrate = rate) %>%
    # add the housing unit count from the ACS by block group
   left_join(housingACSbg[,c("blockGroup","nunit")],by=c("blockGroup"="blockGroup")) %>% 
    # compute the raw DOME probability by block group
   mutate(prob=cases/nunit) %>%
    # filter out small block groups and the courthouse block group
   filter(nunit > 20,blockGroup != 1017013) -> df2
 
 
 plot(df2$medInc,df2$prob)
 logit <- function(p) log(p/(1-p))
 plot(df2$medInc,logit(df2$prob),xlim=c(70000,145000),xlab='median income',ylab='logit P(Abuse)')
 plot(sqrt(df2$medInc),logit(df2$prob),xlim=sqrt(c(70000,145000)))
 
 fit0 <- glm(cbind(cases,nunit-cases) ~ medInc + RMSP + DRUGrate, data=df2, family=binomial(link='logit'))
 fit1 <- glm(cbind(cases,nunit-cases) ~ medInc + RMSP + single_parent + householdSize +
             #unmarriedPartner + snKid + multiGenHouse + milWoman, 
             unmarriedPartner + snKid + multiGenHouse + milWoman + DRUGrate, 
             data=df2, family=binomial(link='logit'))
  # make a plot
 PDF=FALSE
 if(PDF) pdf('lrBlockGroup.pdf',width=9,height=4)
   par(mfrow=c(1,2),oma=c(0,0,1.2,0),mar=c(4,4,1.7,1))
   plot(df2$medInc,logit(df2$prob),xlim=c(70000,145000),xlab='median income',ylab='logit P(Abuse)')
   points(df2$medInc,logit(fit1$fitted.values),pch=16,cex=.7,col='green')
   mtext('logit scale',side=3,line=.2,outer=F)
   plot(df2$medInc,(df2$prob),xlim=c(70000,145000),xlab='median income',ylab='P(Abuse)')
   mtext('probability scale',side=3,line=.2,outer=F)
   points(df2$medInc,(fit1$fitted.values),pch=16,cex=.7,col='green')
   mtext('domestic violence call rate by median income for blockgroup',side=3,line=.1,outer=T)
 if(PDF) dev.off()
 # look at fitted values and plot on arlington's blockgroups
  # read in GIS info for Arlington
   library(acs)
   library(tigris)
   library(ggplot2)
   library(scales)
   
 arlBGgis <- block_groups("Virginia", c("Arlington County"))
 arlZipgis <- zctas(starts_with = 222)
 
 par(mfrow=c(1,1),oma=c(0,0,0,0),mar=c(1,1,1,1))
 plot(arlBGgis)
 
 # convert Arlington block groups to a dataframe
 arlBGgis@data$id = rownames(arlBGgis@data)
 arlBGgis.points = fortify(arlBGgis, region="id")
 arlBGgis.df = left_join(arlBGgis.points, arlBGgis@data, by="id")
 
 # merge with LR fit
 
 # need to append 51013 to the blockgroup labels used in df2
 df2$blockGroup2 <- paste("51013",df2$blockGroup,sep='')
 # append fitted values to df2
 df2$fitted.values <- fit1$fitted.values
 df2$fitted.values0 <- fit0$fitted.values
 # check: do these blockgroup id's match to the GIS id's
 sum(is.na(match(df2$blockGroup2,arlBGgis.df$GEOID)))
 table(match(xx,arlBGgis.df$GEOID))
 arlFit <- left_join(arlBGgis.df,df2,by=c("GEOID"="blockGroup2"))
  # make a plot on the map
 if(PDF) pdf('lr1fit.pdf',width=7,height=6)
 ggplot(arlFit) + 
   geom_polygon(aes(x=long,y=lat,group=group,fill=fitted.values),
   #geom_polygon(aes(x=long,y=lat,group=group,fill=prob),
                alpha=1,color="grey70",lwd=.5) +
   scale_fill_gradientn(colors=c('lightblue','red','yellow'),
                        values = scales::rescale(c(0, .09, .18, 0.25, 0.4)),
                        labels=percent,name=expression("Pr(DOME)"),
                       limits=c(0,max(fit0$fitted,fit1$fitted))) +
   coord_quickmap() + #coord_equal(ratio=1) +
   theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
         axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
         plot.title = element_text(lineheight=.8, face="bold", vjust=1, hjust = .5),
         plot.caption = element_text(hjust=0)) + #labels
   labs(title="Fitted Probability of Domestic Abuse Call", x="", y="") #labs(title="Fitted Probability of Domestic Abuse Call", x="", y="", caption="Source: US American Community Survey data for Fairfax County, VA, 2015")
if(PDF) dev.off()
 
 # make a plot of the "null" model
 # make a plot on the map
if(PDF) pdf('lrfit0.pdf',width=7,height=6)
 ggplot(arlFit) + 
    geom_polygon(aes(x=long,y=lat,group=group,fill=fitted.values0),
                 #geom_polygon(aes(x=long,y=lat,group=group,fill=prob),
                 alpha=1,color="grey70",lwd=.5) +
#    scale_fill_gradient(low="lightblue",high="red",labels=percent,name=expression("Pr(DOME)"),                      
#                        limits=c(0,max(fit0$fitted,fit1$fitted))) +
    scale_fill_gradientn(colors=c('lightblue','red','yellow'),
                         values = scales::rescale(c(0, .09, .18, 0.25, 0.4)),
                         labels=percent,name=expression("Pr(DOME)"),
                         limits=c(0,max(fit0$fitted,fit1$fitted))) +
    #coord_equal(ratio=1) + 
    coord_quickmap() +
    theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
          axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
          plot.title = element_text(lineheight=.8, face="bold", vjust=1, hjust = .5),
          plot.caption = element_text(hjust=0)) + #labels
    #labs(title="Fitted Probability of Domestic Abuse Call", x="", y="", caption="Source: US American Community Survey data for Fairfax County, VA, 2015")
    labs(title="Fitted Probability of Domestic Abuse Call", x="", y="")
 if(PDF) dev.off()


 # some code to ignore that tries to load zip codes for Arlington County
if(0){     
  # create covariates from resamples data frame
 resamples[1:10,] %>% select(houseID,blockGroup,one_of(rsampName)) -> tmp
 tmp %>% reshape(idvar="houseID",timevar = rsampName, direction = "wide") -> tmp2
 resamples[1:10,icol] %>% reshape(idvar="houseID",timevar = "feature", direction = "wide") -> tmp
 names(tmp) <- c("houseID","sqrtHINCP","RMSP")
  # which houses are DOME cases
 icase <- assignments[,k+1]
 tmp$y <- 0; tmp$y[]
 
 # seems to have worked....
 arlZipgis <- zctas(starts_with = 222); plot(arlZipgis)
 }
