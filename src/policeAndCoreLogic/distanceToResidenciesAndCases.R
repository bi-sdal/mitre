library(data.table)
library(dplyr)
library(ggplot2)
library(geosphere)
library(snowfall)
library(microbenchmark)
library(sf)
source("./R/00-simulateArlFunctions.R")

#Load data
#Police data
#Columns: Rep_Dist is a code describing what happened; Disposition???; Priority???; UNITS_NUMBER no apts;

policeToResidenciesDistances = fread('./data/mitre/final/police/dome_geocoded.csv')

# Manually remove events that happened at 1425 N Courthouse
policeData = na.omit(policeToResidenciesDistances[-c(50:65)])

policeLonLat = unique(policeData, by = 'Report_No')[,.(lon, lat)]
colnames(policeLonLat) = c('LONGITUDE', 'LATITUDE')
# Residencies data

clAtrackPums = fread("./data/mitre/working/cleanedExampleData/clAtrackPumsMultivariate.csv")[source != "PUMS"]
residenciesLonLat = na.omit(clAtrackPums[,.(LONGITUDE, LATITUDE)])


# Initiate sf cluster for parallel computation of distances
# THIS COMPUTATION WAS ALREADY DONE AND THIS NEED NOT BE RUN AGAIN

sfInit(parallel=TRUE, cpus=24)
sfLibrary(data.table)
sfLibrary(geosphere)
sfSource("./R/00-simulateArlFunctions.R")
#Export unique lon/lat coords to clusters
sfExport('policeLonLat')
sfExport('residenciesLonLat')

#Each row corresponds to a police event, each column a CL lat long
system.time(policeToResidenciesDistances <- t(sfApply(policeLonLat, 1, computePoliceToResidenciesDist, residenciesLonLat = residenciesLonLat)))

write.csv(cbind(unique(policeData$Report_No), policeToResidenciesDistances), "./data/mitre/working/PoliceData/distMatrix.csv", row.names = FALSE)



