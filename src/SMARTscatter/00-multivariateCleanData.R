library(data.table)
library(dplyr)
library(stringr)
## This script is for the idiosyncratic preparation of the data from corelogic, ATRACK, PUMS, and the ACS marginal tables


## Cleaning and preparation for the ACS marginal tables

marginalIncome = fread("./data/mitre/original/marginData/arlMarginalIncomeBG.csv", skip = 1)[,c(2,seq(6,37,by=2))]
marginalRoomCount = fread("./data/mitre/original/marginData/ACS_NO_ROOMS.csv")
marginalIncome$Id2 = as.numeric(substr(marginalIncome$Id2,6,12))
marginalRoomCount$Id2 = as.numeric(substr(marginalRoomCount$Id2,6,12))
# Group by 25k
marginalIncome = data.table(BlockGroup = marginalIncome$Id2,
                            '0to25' = rowSums(marginalIncome[,2:5]), 
                            '25to50' = rowSums(marginalIncome[,6:10]),
                            '50to75' = rowSums(marginalIncome[,11:12]),
                            '75to100' = rowSums(marginalIncome[,13]),
                            '100to125' = rowSums(marginalIncome[,14]),
                            '125to150' = rowSums(marginalIncome[,15]),
                            '150to200' = rowSums(marginalIncome[,16]),
                            '200plus' = rowSums(marginalIncome[,17]))
marginalRoomCount = marginalRoomCount[,.(BlockGroup = marginalRoomCount$Id2,
                                         oneRoom = `Estimate; Total: - 1 room`,
                                         twoRooms = `Estimate; Total: - 2 rooms`,
                                         threeRooms = `Estimate; Total: - 3 rooms`,
                                         fourRooms = `Estimate; Total: - 4 rooms`,
                                         fiveRooms = `Estimate; Total: - 5 rooms`,
                                         sixRooms = `Estimate; Total: - 6 rooms`,
                                         sevenRooms = `Estimate; Total: - 7 rooms`,
                                         eightRooms = `Estimate; Total: - 8 rooms`,
                                         ninePlusRooms = `Estimate; Total: - 9 or more rooms`)]




# Cleaning for Corelogic, ATRACK and the PUMS data

# read in CoreLogic data for Arlington; subset by single family and by variables of interest
# including home value (TOTAL.VALUE.CALCULATED), taxes paid (TAX.AMOUNT)

CLdata = fread("./data/mitre/original/synthpop_data/Arlington_CL_2013_Data.csv")

CLdata = CLdata[!is.na(TOTAL.VALUE.CALCULATED) & !is.na(TAX.AMOUNT) & !is.na(BlockGroup_rec),
                .(BlockGroup=BlockGroup_rec, TOTAL.VALUE.CALCULATED,  TAX.AMOUNT, LATITUDE, LONGITUDE, UNITS.NUMBER, PropertyType)]

# Set all missing unit numbers to 1 except for multifamily households
CLdata$UNITS.NUMBER[CLdata$PropertyType != "Multifamily" & is.na(CLdata$UNITS.NUMBER)] = 1
CLdata = na.omit(unique(CLdata[,PropertyType := NULL]))
setkey(CLdata, BlockGroup)

# Read in ATRACK

rentalData = fread("./data/mitre/working/PoliceData/CoreLogic_ATRACK_joined.csv")
rentalData = rentalData[,.(BlockGroup=BlockGroup_rec, TOTAL.VALUE.CALCULATED,  TAX.AMOUNT, LATITUDE, LONGITUDE, UNITS.NUMBER = units)]


rentalData = na.omit(unique(rentalData))
setkey(rentalData, BlockGroup)

#
# Read in PUMS
#

PUMS = fread("./data/mitre/original/PUMS2016/householdPUMS2016VA.csv")
PUMS = PUMS[PUMA %in% c(1301, 1302) & VALP < 1500000 & HINCP >= 0, .(SERIALNO, PUMA, HINCP, VALP, TAXP, RMSP)]

personPUMS = fread("./data/mitre/original/PUMS2016/personPUMS2016VA.csv")[SERIALNO %in% PUMS$SERIALNO]

# Make household level summaries based on the perosnPUMS data

householdSize = personPUMS[,.(householdSize = .N), by = SERIALNO]
singleParent = personPUMS[,
                          .(singleParent = ifelse(2 %in% .SD$RELP & !(1 %in% .SD$RELP), 1, 0)), 
                          by = SERIALNO]


# Don't have young parents
# findYoungParent = function(pums){
#   if(!(2 %in% pums$RELP)) return(0)
#   parentsAge = pums[RELP %in% c(0, 1), AGEP]
#   if(min(parentsAge) < 34) return(1)
#   return(0)
# }


# youngParent = personPUMS[,
#                          .(youngParent = do.call(findYoungParent, list(.SD))),
#                          by = SERIALNO]

# RAC1P = 1 -> white
nonwhite



# Drop SERIALNO after merging

PUMS = PUMS[householdSize, on = 'SERIALNO'][singleParent, on = 'SERIALNO'][,SERIALNO:=NULL]

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
# threshhold CoreLogic and ATRACK data to maximum in PUMS
# -----------------------------------------------------------------------

max(PUMS$VALP) # $1.4m
max(PUMS$TAXP2) # $33,368

CLdata$TOTAL.VALUE.CALCULATED[CLdata$TOTAL.VALUE.CALCULATED > max(PUMS$VALP)] <- max(PUMS$VALP)
CLdata$TAX.AMOUNT[CLdata$TAX.AMOUNT > max(PUMS$TAXP2)] <- max(PUMS$TAXP2)
rentalData$TOTAL.VALUE.CALCULATED[rentalData$TOTAL.VALUE.CALCULATED > max(PUMS$VALP)] <- max(PUMS$VALP)
rentalData$TAX.AMOUNT[rentalData$TAX.AMOUNT > max(PUMS$TAXP2)] <- max(PUMS$TAXP2)


# create combined data frame
CLdata$source <- "CL"
PUMS$source <- "PUMS"
rentalData$source <- "AT"


setnames(CLdata, c('TOTAL.VALUE.CALCULATED', 'TAX.AMOUNT'), c('VALP', 'TAXP2'))

clAtrackPums = rbind(CLdata, rentalData, PUMS, fill = TRUE)

# transform income to satisfy linear regression assumptions
clAtrackPums[,sqrtHINCP := sqrt(HINCP)]
clAtrackPums$houseID = 1:nrow(clAtrackPums)

#
# Write the final cleaned data
#

fwrite(marginalRoomCount, "./data/mitre/working/simulatedArlingtonData/marginalRooms.csv", row.names  = F)
fwrite(marginalIncome, "./data/mitre/working/simulatedArlingtonData/marginalIncome.csv", row.names  = F)
fwrite(clAtrackPums, "./data/mitre/working/cleanedExampleData/clAtrackPumsMultivariate.csv", row.names  = F)




