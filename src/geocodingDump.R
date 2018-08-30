# Load police incident data and house locations
# publicHealthOutcomes = fread('./data/mitre/final/police/dome_replicated.csv')
# Geocode addresses (just once)
# geocodes = matrix(NA, ncol = 2, nrow = nrow(publicHealthOutcomes))
# colnames(geocodes) = c("LATITUDE", "LONGITUDE")
# for(i in 49:nrow(publicHealthOutcomes)){
#   geocodes[i,] = geocode_address(publicHealthOutcomes$Location[i])
#   Sys.sleep(5)
# }
# 
# geocode_address2 = function(address, api_key = sdalr::get_google_maps_key()) {
#   address <- stringr::str_replace_all(string = address, pattern = '\\s', replacement = '+')
#   res <- httr::GET(sprintf('https://maps.googleapis.com/maps/api/geocode/json?address=%s&key=%s',
#                            address,
#                            api_key))
#   con <- httr::content(res)
#   return(con)
# }
# 
# geocodes = list()
# for(i in 1:nrow(publicHealthOutcomes)){
#   geocodes[[i]] = geocode_address2(paste0(publicHealthOutcomes$Location[i], ", Arlington, VA"))
#   Sys.sleep(1)
# }
# 
# outcomesLatLong = lapply(geocodes, function(x){
#   if(length(x$results) == 0) return(c(NA, NA))
#   return(c(lat = x$results[[1]]$geometry$location$lat,
#            lon = x$results[[1]]$geometry$location$lng)
#   )
# })
# 
# outcomesLatLong = na.omit(data.table(publicHealthOutcomes, do.call(rbind, outcomesLatLong))[,Location := NULL])
# 
# fwrite(na.omit(outcomesLatLong), './data/mitre/final/police/dome_geocoded.csv')

#
# Geocode to get zip code
#

library(data.table)
library(ggmap)
library(sf)
library(zipcode)
library(dplyr)
library(stringr)

policeData = fread("./data/mitre/working/PoliceData/policeData.csv")
outcomesLatLong = fread('./data/mitre/final/police/dome_geocoded.csv')
clAtrack = fread("./data/mitre/working/cleanedExampleData/clAtrackPumsMultivariate.csv")[source != "PUMS"]
conDist = sdalr::con_db("mitre")

library("ggmap")

# generate a single example address

policeLonLat = as.matrix(policeData[,.(LONGITUDE, LATITUDE)])


res <- lapply(1:nrow(policeData), function(x){
  out = revgeocode(policeLonLat[x,], output="more")$postal_code
  return(out)
})

fails = which(sapply(res, length) == 0)

reverseGeocode = function(lat, lon, api_key = sdalr::get_google_maps_key()) {
  res <- httr::GET(sprintf('https://maps.googleapis.com/maps/api/geocode/json?latlng=%s,%s&key=%s',
                           lat,
                           lon,
                           api_key))
  con <- httr::content(res)
  return(con)
}

res2 = lapply(fails, function(x){
  out = reverseGeocode(policeLonLat[x,2], policeLonLat[x,1])
  return(out)
})

zips2 = numeric(length(res2))

for(i in 1:length(res2)){

  zips2[i] = tryCatch(res2[[i]][[1]][[1]]$address_components[[8]]$long_name, error = function(e) return(NA))
  print(i)
}
res = sapply(res, levels)

res[fails] = zips2
finalZips = as.numeric(unlist(res))








