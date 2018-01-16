# files:
# CoreLogic housing data: Arlington_CL_2013_Data.csv, full pop of real estate data for arlington houses
# PUMS household microdata: ss14hva.csv, for all of virginia (2009-2014 5 year estimates)
# published ACS table for household income by block group:
#     ACS_13_5YR_B19001_with_ann.csv, this is the variable we impute

# CoreLogic housing data is our universe for Arlington.
# It has measurements of property value and taxes paid; we will impute household income.

# method:
# Step 1: Impute household income onto our synthetic universe using samples from the PUMS.
# Repeat Step 1 to get many realizations of the synthetic universe.
# Step 2: For each household, resample imputed realizations weighted by the marginal distribution of household income by block group.

library(dplyr)
library(MASS)

dataDir = "./data/mitre/original/synthpop data/"

# read in CoreLogic data for Arlington; subset by single family and by variables of interest
# including home value (TOTAL.VALUE.CALCULATED), taxes paid (TAX.AMOUNT)
CLdata<-read.csv(paste0(dataDir, "Arlington_CL_2013_Data.csv")) %>%
  filter(PropertyType!="Multifamily",!is.na(TOTAL.VALUE.CALCULATED),!is.na(TAX.AMOUNT),!is.na(BlockGroup_rec)) %>%
  dplyr::select(BlockGroup=BlockGroup_rec,TOTAL.VALUE.CALCULATED,TAX.AMOUNT,LATITUDE,LONGITUDE) %>%
  arrange(BlockGroup)

# read in ACS pums for Arlington (two PUMAs)
# subset single family and by variables of interest
# including household income (HINCP), home value (VALP), taxes paid (TAXP)
PUMS <- read.csv(paste0(dataDir, "ss14hva.csv")) %>%
  dplyr::select(PUMA10,HINCP,VALP,TAXP) %>%
  filter(PUMA10 %in% c(1301,1302), !is.na(TAXP),!is.na(VALP),!is.na(HINCP), VALP < 1500000, HINCP >= 0)

# ------------------------------------------------------------------------------------------------
# TAXP is binned. To use it as a predictor in our model we convert it into $$ by taking the center value of each bin.
# ------------------------------------------------------------------------------------------------

PUMS$TAXP2[PUMS$TAXP==1] <- 0
PUMS$TAXP2[PUMS$TAXP==2] <- 25
PUMS$TAXP2[PUMS$TAXP==3] <- 75
PUMS$TAXP2[PUMS$TAXP==4] <- 125
PUMS$TAXP2[PUMS$TAXP==5] <- 175
PUMS$TAXP2[PUMS$TAXP==6] <- 225
PUMS$TAXP2[PUMS$TAXP==7] <- 275
PUMS$TAXP2[PUMS$TAXP==8] <- 325
PUMS$TAXP2[PUMS$TAXP==9] <- 375
PUMS$TAXP2[PUMS$TAXP==10] <- 425
PUMS$TAXP2[PUMS$TAXP==11] <- 475
PUMS$TAXP2[PUMS$TAXP==12] <- 525
PUMS$TAXP2[PUMS$TAXP==13] <- 575
PUMS$TAXP2[PUMS$TAXP==14] <- 625
PUMS$TAXP2[PUMS$TAXP==15] <- 675
PUMS$TAXP2[PUMS$TAXP==16] <- 725
PUMS$TAXP2[PUMS$TAXP==17] <- 775
PUMS$TAXP2[PUMS$TAXP==18] <- 825
PUMS$TAXP2[PUMS$TAXP==19] <- 875
PUMS$TAXP2[PUMS$TAXP==20] <- 925
PUMS$TAXP2[PUMS$TAXP==21] <- 975
PUMS$TAXP2[PUMS$TAXP==22] <- 1050
PUMS$TAXP2[PUMS$TAXP==23] <- 1150
PUMS$TAXP2[PUMS$TAXP==24] <- 1250
PUMS$TAXP2[PUMS$TAXP==25] <- 1350
PUMS$TAXP2[PUMS$TAXP==26] <- 1450
PUMS$TAXP2[PUMS$TAXP==27] <- 1550
PUMS$TAXP2[PUMS$TAXP==28] <- 1650
PUMS$TAXP2[PUMS$TAXP==29] <- 1750
PUMS$TAXP2[PUMS$TAXP==30] <- 1850
PUMS$TAXP2[PUMS$TAXP==31] <- 1950
PUMS$TAXP2[PUMS$TAXP==32] <- 2050
PUMS$TAXP2[PUMS$TAXP==33] <- 2150
PUMS$TAXP2[PUMS$TAXP==34] <- 2250
PUMS$TAXP2[PUMS$TAXP==35] <- 2350
PUMS$TAXP2[PUMS$TAXP==36] <- 2450
PUMS$TAXP2[PUMS$TAXP==37] <- 2550
PUMS$TAXP2[PUMS$TAXP==38] <- 2650
PUMS$TAXP2[PUMS$TAXP==39] <- 2750
PUMS$TAXP2[PUMS$TAXP==40] <- 2850
PUMS$TAXP2[PUMS$TAXP==41] <- 2950
PUMS$TAXP2[PUMS$TAXP==42] <- 3050
PUMS$TAXP2[PUMS$TAXP==43] <- 3150
PUMS$TAXP2[PUMS$TAXP==44] <- 3250
PUMS$TAXP2[PUMS$TAXP==45] <- 3350
PUMS$TAXP2[PUMS$TAXP==46] <- 3450
PUMS$TAXP2[PUMS$TAXP==47] <- 3550
PUMS$TAXP2[PUMS$TAXP==48] <- 3650
PUMS$TAXP2[PUMS$TAXP==49] <- 3750
PUMS$TAXP2[PUMS$TAXP==50] <- 3850
PUMS$TAXP2[PUMS$TAXP==51] <- 3950
PUMS$TAXP2[PUMS$TAXP==52] <- 4050
PUMS$TAXP2[PUMS$TAXP==53] <- 4150
PUMS$TAXP2[PUMS$TAXP==54] <- 4250
PUMS$TAXP2[PUMS$TAXP==55] <- 4350
PUMS$TAXP2[PUMS$TAXP==56] <- 4450
PUMS$TAXP2[PUMS$TAXP==57] <- 4550
PUMS$TAXP2[PUMS$TAXP==58] <- 4650
PUMS$TAXP2[PUMS$TAXP==59] <- 4750
PUMS$TAXP2[PUMS$TAXP==60] <- 4850
PUMS$TAXP2[PUMS$TAXP==61] <- 4950
PUMS$TAXP2[PUMS$TAXP==62] <- 5250
PUMS$TAXP2[PUMS$TAXP==63] <- 5750
PUMS$TAXP2[PUMS$TAXP==64] <- 6500
PUMS$TAXP2[PUMS$TAXP==65] <- 7500
PUMS$TAXP2[PUMS$TAXP==66] <- 8500
PUMS$TAXP2[PUMS$TAXP==67] <- 9500
# category 68 is $10000+;
# approximate this with a draw over the range of high values for taxes paid in CoreLogic
samp_taxes <- CLdata$TAX.AMOUNT[CLdata$TAX.AMOUNT > 10000 & !is.na(CLdata$TAX.AMOUNT)]
PUMS$TAXP2[PUMS$TAXP==68 & !is.na(PUMS$TAXP)] <- sample(x=samp_taxes,size=sum(PUMS$TAXP==68,na.rm=T))

# ------------------------------------------------------------------------------------------------
# get ACS marginals by block group (housholder income) from American FactFinder; 2013 5-year tables
# estimate the mean and standard deviation of ln(income) by block group;
# we will fit a lognormal distribution to income
# ------------------------------------------------------------------------------------------------

# ACS table: B19001 (household income)
income_marginal <- read.csv(paste0(dataDir, "ACS_13_5YR_B19001_with_ann.csv"),header=TRUE,skip=1)[,c(1:3,seq(6,37,by=2))]

# income is binned; convert it into $$ by taking the center value of each bin
for(i in 1:nrow(income_marginal)){ # loop over blockgroup
  marginal_samples <- c( rep(5000,income_marginal[i,4]),
                         rep(12500,income_marginal[i,5]),
                         rep(17500,income_marginal[i,6]),
                         rep(22500,income_marginal[i,7]),
                         rep(27500,income_marginal[i,8]),
                         rep(32500,income_marginal[i,9]),
                         rep(37500,income_marginal[i,10]),
                         rep(42500,income_marginal[i,11]),
                         rep(47500,income_marginal[i,12]),
                         rep(55000,income_marginal[i,13]),
                         rep(65000,income_marginal[i,14]),
                         rep(87500,income_marginal[i,15]),
                         rep(112500,income_marginal[i,16]),
                         rep(137500,income_marginal[i,17]),
                         rep(175000,income_marginal[i,18]))
  # the final bin is for $200,000+;
  # approximate it with a draw over the range of high values of income from ACS PUMS data
  samp_income <- PUMS$HINCP[PUMS$HINCP > 200000 & !is.na(PUMS$HINCP)]
  marginal_samples <- c(marginal_samples,sample(x=samp_income,size=income_marginal[i,19]))

  # compute the mean and sd of ln(income) for each block group
  income_marginal$log_mean[i] <- mean(log(marginal_samples))
  income_marginal$log_sd[i] <- sd(log(marginal_samples))
}

# join the ACS marginal information to CLdata by block group 
blockgroups <- unique(CLdata$BlockGroup)
ACS_marginals <- income_marginal %>% dplyr::select(BlockGroup=Id2,log_mean,log_sd)
ACS_marginals$BlockGroup <- as.numeric(substr(ACS_marginals$BlockGroup,6,12))
ACS_marginals <- ACS_marginals %>% filter(BlockGroup %in% CLdata$BlockGroup)
CLdata <- merge(CLdata,ACS_marginals,by="BlockGroup")

# save CLdata, PUMS, ACS_marginals data frames to use in inference
save(CLdata,PUMS,ACS_marginals,file = "./data/mitre/working/cleaned_CL_ACS.RData")

