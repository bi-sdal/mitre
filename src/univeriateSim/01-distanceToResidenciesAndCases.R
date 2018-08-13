library(data.table)
library(dplyr)
library(ggplot2)
library(geosphere)
library(snowfall)
library(microbenchmark)
library(sf)
source("./src/simulateArlingtons/00-simulateArlFunctions.R")

#Load data
#Police data
#Columns: Rep_Dist is a code describing what happened; Disposition???; Priority???; UNITS_NUMBER no apts;

policeData = fread("./data/mitre/working/PoliceData/policeData.csv")
policeLonLat = policeData[,.(LONGITUDE, LATITUDE)]
# Residencies data

clAtrackPums = fread("./data/mitre/working/cleanedExampleData/clAtrackPums.csv")
residenciesLonLat = na.omit(clAtrackPums[,.(LONGITUDE, LATITUDE)])


# Initiate sf cluster for parallel computation of distances
# THIS COMPUTATION WAS ALREADY DONE AND THIS NEED NOT BE RUN AGAIN

sfInit(parallel=TRUE, cpus=24)
sfLibrary(data.table)
sfLibrary(geosphere)
sfExport('policeToResidenciesDist')
#Export unique lon/lat coords to clusters
sfExport('policeLonLat')
sfExport('residenciesLonLat')

#Each row corresponds to a police event, each column a CL lat long
system.time(policeToResidenciesDistances <- t(sfApply(policeLonLat, 1, computePoliceToResidenciesDist, residenciesLonLat = residenciesLonLat)))
write.csv(tmp, "./data/mitre/working/PoliceData/distMatrix.csv", row.names = FALSE)



