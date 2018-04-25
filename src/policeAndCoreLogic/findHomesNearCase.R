library(data.table)
library(dplyr)
library(ggplot2)
library(geosphere)
library(snowfall)
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

# Load data
# Police data
# Columns: Rep_Dist is a code describing what happened; Disposition???; Priority???; UNITS_NUMBER no apts; 
policeData = fread("./data/mitre/working/PoliceData/policeData.csv")


# CL Data
CLdata = fread("./data/mitre/original/synthpop_data/Arlington_CL_2013_Data.csv")[,-1]
colnames(CLdata)[9:10] = c('lat', 'lng')
policeLonLat = unique(policeData, by = c('lng', 'lat'))[,.(lng, lat)]
clLonLat = unique(CLdata, by = c('lng', 'lat'))[,.(lng, lat)]

# Export unique lon/lat coords to clusters
# sfExport('clLonLat')
# Each row corresponds to a police event, each column a CL lat long
# system.time(policeToClUniqueDistances <- t(sfApply(policeLonLat, 1, policeToClDist)))

policeToClUniqueDistances = fread("./data/mitre/working/PoliceData/distMatrix.csv")

microbenchmark(distm(policeLonLat[3,], clLonLat[2,], fun = distVincentySphere)/1000, policeToClUniqueDistances[3, .(2)])







