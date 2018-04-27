library(data.table)
library(dplyr)
library(ggplot2)
library(geosphere)
library(snowfall)
library(microbenchmark)
library(sf)
# Compute distance matrix between police data and CL data. Returns the geodesic distance in kilometers
policeToClDist = function(policeLonLat){
  
  dists = apply(clLonLat, 1, function(x){
    distm(x, policeLonLat, distVincentySphere)/1000
  })
  
  return(dists)
}

# Initiate sf cluster for parallel computation of distances
# THIS COMPUTATION WAS ALREADY DONE AND THIS NEED NOT BE RUN AGAIN

# sfInit(parallel=TRUE, cpus=24)
# sfLibrary(data.table)
# sfLibrary(geosphere)
# sfClusterEval()
# sfExport('policeToClDist')
# Export unique lon/lat coords to clusters
# sfExport('clLonLat')
# Each row corresponds to a police event, each column a CL lat long
# system.time(policeToClUniqueDistances <- t(sfApply(policeLonLat, 1, policeToClDist)))

# Load data
# Police data
# Columns: Rep_Dist is a code describing what happened; Disposition???; Priority???; UNITS_NUMBER no apts; 
policeData = fread("./data/mitre/working/PoliceData/policeData.csv")

# CL Data
CLdata = fread("./data/mitre/original/synthpop_data/Arlington_CL_2013_Data.csv")[,-1]
colnames(CLdata)[9:10] = c('lat', 'lng')
policeLonLat = unique(policeData, by = c('lng', 'lat'))[,.(lng, lat)]
clLonLat = unique(CLdata, by = c('lng', 'lat'))[,.(lng, lat)]

# Rental data

rentalData = fread("./data/mitre/working/PoliceData/CoreLogic_ATRACK_joined.csv")
rentalLonLat = unique(rentalData, by = c('LATITUDE', 'LONGITUDE'))
rentalLonLat = rentalLonLat[,.(lng = LONGITUDE, lat = LATITUDE)]

# Read distance matrix. Each row corresponds to a unique lat/long from the police data
policeToClUniqueDistances = as.matrix(fread("./data/mitre/working/PoliceData/distMatrix.csv"))

# Make a table linking rows of the police data and CLdata to the appropriate positions in the distance matrix
policeRowIndices = merge(policeData[,.(lng, lat)], data.table(policeLonLat, policeIndex = 1:nrow(policeLonLat)), by = c('lng', 'lat'), sort = FALSE)$policeIndex
clIndices = merge(CLdata[,.(lng, lat)], data.table(clLonLat, clIndex = 1:nrow(clLonLat)), by = c('lng', 'lat'), sort = FALSE)$clIndex

getHomesInRadius = function(callNumber, radius = .2){
  distMatRow = policeRowIndices[match(callNumber, policeData$Call_No)]
  uniqueClLatLongInRadius = which(policeToClUniqueDistances[distMatRow,] < radius)
  if(length(uniqueClLatLongInRadius) == 0){
    warning("No records were found in the given radius.")
    return(NULL)
  }
  return(CLdata[which(clIndices %in% uniqueClLatLongInRadius)])
}

# Summaries to do:
# Hist of prices, hist of units, plot 
# Take a coordinate, get all the units in that radius as well as all the cases

callNos = unique(policeData$Call_No)
index = 2
clRecs = getHomesInRadius(callNos[index])
hist(clRecs$TOTAL.VALUE.CALCULATED/clRecs$UNITS.NUMBER)

# total summaries for arlington county
CLdata$TOTAL.VALUE.CALCULATED/CLdata$UNITS.NUMBER

## Read Geospatial File from DB and Plot
# create db connection
con <- sdalr::con_db("sdal")
# read shape from db
gs <- sf::st_read_db(con, c("geospatial$census_cb", "cb_2016_51_county_within_ua_500k"))
# limit to Arlington
gs <- gs[gs$COUNTYFP10=="013",]
# add in the police call locations

# convert to sf object
policeCoords = policeData[,.(lng, lat)]
policeCoords = na.omit(policeCoords)
policeSf = st_as_sf(policeCoords, 
                 coords = c("lng", "lat"), 
                 crs = sf::st_crs(gs))
clSf = st_as_sf(na.omit(CLdata[,.(lng, lat)]),
                coords = c("lng", "lat"), 
                crs = sf::st_crs(gs))
rentalSf = st_as_sf(na.omit(rentalLonLat),
                coords = c("lng", "lat"), 
                crs = sf::st_crs(gs))

# plot
# png("./output/arlRentalOwnedDomestic.png")
plot(sf::st_geometry(gs), axes = FALSE)
plot(sf::st_geometry(clSf), axes = TRUE, col = "red", add = TRUE, cex = .1)
plot(sf::st_geometry(rentalSf), axes = TRUE, col = "green", add = TRUE, cex = 1, pch = 19)
plot(sf::st_geometry(policeSf), axes = TRUE, col = "blue", add = TRUE, pch = 19, cex = .3)
legend('topright', fill = c("red", "green", "blue"), legend = c("Owned Homes", "Rental Homes", "Domestic Disputes"), bty = 'n')
# dev.off()



