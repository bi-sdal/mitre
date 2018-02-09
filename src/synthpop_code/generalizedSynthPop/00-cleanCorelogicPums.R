## This script is for the idiosyncratic preparation of the data from corelogic, PUMS, and the ACS marginal tables
## THIS SHOULD NEVER HAVE TO BE RUN AGAIN.



## Cleaning and preparation for the ACS marginal tables

income_marginal <- read.csv("./data/mitre/original/synthpop_data/ACS_13_5YR_B19001_with_ann.csv",header=TRUE,skip=1)[,c(1:3,seq(6,37,by=2))]
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

# Cleaning for Corelogic and the PUMS data, as well as their merge

dataDir = "./data/mitre/original/synthpop_data/"

# read in CoreLogic data for Arlington; subset by single family and by variables of interest
# including home value (TOTAL.VALUE.CALCULATED), taxes paid (TAX.AMOUNT)
CLdata<-read.csv(paste0(dataDir, "Arlington_CL_2013_Data.csv")) %>%
  filter(PropertyType!="Multifamily",!is.na(TOTAL.VALUE.CALCULATED),!is.na(TAX.AMOUNT),!is.na(BlockGroup_rec)) %>%
  dplyr::select(BlockGroup=BlockGroup_rec,TOTAL.VALUE.CALCULATED,TAX.AMOUNT,LATITUDE,LONGITUDE) %>%
  arrange(BlockGroup)


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

#
# Write the final cleaned data
#

write.csv(income_marginal, "./data/mitre/working/cleanedExampleData/income_marginal.csv", row.names  = F)
write.csv(CL_PUMS, "./data/mitre/working/cleanedExampleData/corelogicAndPUMS.csv", row.names  = F)




