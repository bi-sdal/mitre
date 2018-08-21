#filter Police data for substance abuse and alcohol related calls
library(gdata)
library(dplyr)
library(sqldf)

#must be working out of wrong directory; had to load file and run it directly.
#source("./src/Police/geocode.r")

#
# Explore the police data 
#

# for now, trying to create data objects 
# drug related events with location
# alcohol related events with location - lower priority, not as clear of a risk factor


dataDir = '/home/sdal/projects/arl/arlington911/data/original/police/aug082016/'

dispatch = read.xls(sprintf("%scfs2015.xls", dataDir))

table(dispatch$Original_Call)

#ASLT (assault), DIP = drunk in public, DTOX = drunk / detox
dispatchDrunk = filter(dispatch,  Original_Call == "DIP" |  Original_Call == "DTOX")
reportNumbersDrunk = na.omit(dispatchDrunk$Report_No)

#test2
#just drug related violence calls
dispatchDrug = filter(dispatch, Original_Call == "DRUG")

dispatchDrugCompact = filter(dispatchDrug, dispatchDrug$Report_No != "NA")
reportNumbersDrug = na.omit(dispatchDrug$Report_No)

inper = read.xls(sprintf("%sinmast-inper62015.xls", dataDir))

#link related records for Alcohol and Drug police calls
inperDrunk = filter(inper, Report_No %in% reportNumbersDrunk)
inperDrug = filter(inper, Report_No %in% reportNumbersDrug)


#find suspects and arrestees that live in Arlington - their address is another data point of substance abuse risk in the community
inperDrugArlington = filter(inperDrug, Involvement == "ARR" | Involvement == "SUS", City == "ARLINGTON")

ArlingtonDrugEventList = dispatchDrugCompact[,c("Report_No", "Location")]
ArlingtonDrugSuspectsAddressList = inperDrugArlington[,c("Report_No", "Address")]

#exploring how events and suspects may both be at the same address
DrugEventsSubset <- sqldf('select a.Report_No, a.Location as DrugEventAddress, b.Address as DrugSuspectAddress from ArlingtonDrugEventList a,  ArlingtonDrugSuspectsAddressList b where a.Report_No = b.Report_No')
DrugEventsAndSuspects <- sqldf('select a.Report_No, a.Location as DrugEventAddress, b.Address as DrugSuspectAddress from ArlingtonDrugEventList a left join ArlingtonDrugSuspectsAddressList b on a.Report_No = b.Report_No')

#many events (>50%) are at the Courthouse [default/unknown] address
table(DrugEvents$DrugEventAddress)

#not sure if we need count, but adding it to be consistent with data structure of child abuse, assault records
ArlingtonDrugEventList$Count = 1
ArlingtonDrugSuspectsAddressList$Count = 1
colnames(ArlingtonDrugSuspectsAddressList) = colnames(ArlingtonDrugEventList)

#two final files
finalDrugSuspectsLocationList = ArlingtonDrugSuspectsAddressList
finalDrugEventLocationList = ArlingtonDrugEventList

#not sure what path is desired
#write.csv(finalDrugSuspectsLocationList,"finalDrugSuspectsLocationList.csv")
#write.csv(finalDrugEventLocationList, "finalDrugEventLocationList.csv")