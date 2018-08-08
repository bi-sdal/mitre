library(dplyr)
library(mice)
library(data.table)
library(readr)
library(sf)
require(ggplot2)
source("./src/simulateArlingtons/00-simulateArlFunctions.R")

getHomesInRadius = function(callNumber, policeData, resData, connection, radius = .2){
  distMatRow = match(callNumber, policeData$Call_No)
  #dists = distanceMatrix[distMatRow,]
  dists = DBI::dbGetQuery(connection, sprintf("SELECT * FROM police_distances_long WHERE police = '%s';", distMatRow))$distance
  cols = which(dists < radius)
  if(length(cols) == 0){
    warning("No records were found in the given radius.")
    return(NULL)
  }
  return(resData[cols,])
}


#Load data
#Police data
#Columns: Rep_Dist is a code describing what happened; Disposition???; Priority???; UNITS_NUMBER no apts;

policeData = fread("./data/mitre/working/PoliceData/policeData.csv")
policeLonLat = policeData[,.(LONGITUDE, LATITUDE)]
# Residencies data

incomeSims = fread("./data/mitre/working/simulatedArlingtonData/simulatedArlingtonIncome.csv")
residenciesLonLat = na.omit(incomeSims[,.(LONGITUDE, LATITUDE)])

# Attach a flag to each house indicating how many cases were at that address
callNos = unique(policeData$Call_No)
conDist = sdalr::con_db("mitre")

closestAddress = t(sapply(callNos, function(x){
  distMatRow = match(x, policeData$Call_No)
  dists = DBI::dbGetQuery(conDist, sprintf("SELECT * FROM police_distances_long WHERE police = '%s';", distMatRow))$distance
  out = c(x, which.min(dists), min(dists))
  names(out) = c("Call_No", "closestHouse", "DistKm")
  return(out)
}))
closestAddress = data.table(closestAddress)
closestAddress = merge(closestAddress, policeData[,.(Call_No, LONGITUDE, LATITUDE)])


houseIndex = data.table(closestHouse = 1:44642)

eventsAtAddress = merge(houseIndex, y = closestAddress[,.N, by = closestHouse], all = TRUE)
eventsAtAddress$N[is.na(eventsAtAddress$N)] = 0
incomeSims = cbind(incomeSims, eventsAtAddress = eventsAtAddress$N)

#
# WRITE TO FILE SO DAVE CAN PLOT
#

fwrite(incomeSims, "./data/mitre/working/simulatedArlingtonData/arlSimWithCaseCounts.csv")
fwrite(closestAddress, "./data/mitre/working/simulatedArlingtonData/closestHouseToCall.csv")

plot(incomeSims$UNITS.NUMBER[incomeSims$eventsAtAddress > 0], incomeSims$eventsAtAddress[incomeSims$eventsAtAddress > 0], xlab = "Units Number", ylab = "Number of Events")

# Make a chloropleth of DOMS counts in blockgroups


## Read Geospatial File from DB and Plot
# create db connection
conGeo <- sdalr::con_db("sdal")
# read shape from db
arlBlockGroups <- sf::st_read_db(conGeo, c("geospatial$census_cb", "cb_2016_51_bg_500k"))
# limit to Arlington
arlBlockGroups <- arlBlockGroups[arlBlockGroups$COUNTYFP=="013",]
# add in the police call locations

ggplot(arlBlockGroups) + geom_sf(aes(geometry = wkb_geometry, fill = ALAND))

radius = 1
homesInRadius = getHomesInRadius(callNumber, policeData, incomeSims, conDist, radius)








# convert to sf object
policeLonLat = policeData[,.(LONGITUDE, LATITUDE)]
policeCoords = na.omit(policeCoords)
policeSf = st_as_sf(policeLonLat, 
                    coords = c('LONGITUDE', 'LATITUDE'), 
                    crs = sf::st_crs(arlBlockGroups))
clSf = st_as_sf(na.omit(incomeSims[,.(LONGITUDE, LATITUDE)]),
                coords = c('LONGITUDE', 'LATITUDE'), 
                crs = sf::st_crs(arlBlockGroups))


# plot
# png("./output/arlRentalOwnedDomestic.png")
plot(sf::st_geometry(arlBlockGroups), axes = FALSE)
plot(sf::st_geometry(clSf), axes = TRUE, col = "red", add = TRUE, cex = .1)
plot(sf::st_geometry(policeSf), axes = TRUE, col = "blue", add = TRUE, pch = 19, cex = .3)
legend('topright', fill = c("red", "green", "blue"), legend = c("Owned Homes", "Rental Homes", "Domestic Disputes"), bty = 'n')
# dev.off()