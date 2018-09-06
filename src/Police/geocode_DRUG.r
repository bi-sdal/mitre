library(foreach)
library(doParallel)
library(data.table)
library(jsonlite)
library(foreach)


# GEOCODE FUNCTION ----------------------------------------------
google_geocode <- function(address_str = "1600+Amphitheatre+Parkway,+Mountain+View,+CA",
                           key = "AIzaSyC9FKW-kjQlEXjfM3OgMZBJ7xE6zCN1JQI") {
  url <- sprintf("https://maps.googleapis.com/maps/api/geocode/json?address=%s&key=%s",
                 address_str,
                 key)
  return <- jsonlite::fromJSON(URLencode(url))
  if (return$status[[1]]=="OK") {
    vals <- as.data.frame(t(return$results$address_components[[1]]$short_name))
    num_col <- length(return$results$address_components[[1]]$types)
    cols <- list()
    for(i in 1:num_col){cols <- c(cols, return$results$address_components[[1]]$types[[i]][[1]])}
    # cols <- unlist(return$results$address_components[[1]]$types)
    # cols <- cols[-grep("political", cols)]
    colnames(vals) <- cols
    vals$lat <- if(length(return$results$geometry$location$lat)==1)  return$results$geometry$location$lat else NA
    vals$lng <- if(length(return$results$geometry$location$lng)==1)  return$results$geometry$location$lng else NA
    vals
  } else {4
    return$status
  }
}

# AARON'S TEST DATA ----------------------------------------------
ids <- c("18857", "18858", "18859", "18860", "18861", "18862", "18863", "18864", "18865",
         "18866", "18867", "18868", "18869", "18870", "18871", "18872", "18873", "18874", "18875")
full_addresses <- c("4300 CARLIN SPRINGS North Road, apt 418 22203", "3635 FOUR MILE RUN South Drive 22206", "1545 KEY North Boulevard, apt 48 22209",
                    "4300 CARLIN SPRINGS North Road, apt 912 22203", "1500 FERN South Street, apt 611 22202", "1311 ODE North Street, apt 614 22209",
                    "2021 NELSON North Street 22207", "2140 THOMAS North Street 22207", "610 CARLIN SPRINGS South Road, apt 318 22204",
                    "GLEBE North Road, apt 28 22203", "850 GREENBRIER South Street, apt 205 22204", "2323 11TH North Street 22201",
                    "610 CARLIN SPRINGS South Road, apt 424 22204", "1706 UHLE North Street, apt 1015 22201", "411 GLEBE North Road, apt 3 22203",
                    "6058 6TH North Street 22203", "513 IVY North Street 22201", "860 GREENBRIER South Street, apt 407 22204",
                    "2021 NELSON North Street, apt 308 22207")
Addresses <- data.table(id = ids, full_address = full_addresses)
# DAVE'S TEST DATA -------------------------------

c1 %>% filter(Original_Call=='DRUG',year(Received_Date_Time)==2013) -> cfsDRUG2013
ids <- 1:nrow(cfsDRUG2013)
#ids <- 1:20
locDRUG2013 <-  cfsDRUG2013$Location[1:length(ids)]
full_addresses <- paste(locDRUG2013,'Arlington, VA')
Addresses <- data.table(id = ids, full_address = as.character(full_addresses))
# SET UP PROCESSOR CLUSTER ----------------------------------------------
myCluster<-makeCluster(5)
registerDoParallel(myCluster)

# SPLIT DATA INTO CHUNKS FOR PARALLEL PROCESSING ----------------------------------------------
chunk <- 5
iterator <- idiv(nrow(Addresses), chunkSize=chunk)
startRow <- 1
endRow <- chunk

# GEOCODE IN PARALLEL AND COLLECT TO RESULTS DATA TABLE ----------------------------------------------
if (exists("geocode_results") == T) rm(geocode_results)
try(for (i in 1:ceiling(nrow(Addresses) / chunk)) {
  myChunkOfData <- Addresses[startRow:endRow]
  print(sprintf("Processing rows %s to %s", startRow, endRow))
  s <- foreach(r = iter(myChunkOfData, by = "row")) %dopar% {
    # print(r)
    l <- list()
    
    w <- NULL
    attempt <- 1
    while (is.null(w) && attempt <= 3) {
      attempt <- attempt + 1
      try(w <- google_geocode(r$full_address[[1]]))
      if (is.null(colnames(w)))
      {
        w <- c(V1 = w[[1]])
      }
    }
    w$id <- r$id
    to.s <- w
  }
  
  t <- rbindlist(s, fill = TRUE)
  
  if (exists("geocode_results") == F)
    geocode_results <- t
  else
    geocode_results <- rbindlist(list(geocode_results, t), fill = TRUE)
  
  startRow <- endRow + 1
  endRow <- endRow + nextElem(iterator)
}
, silent = FALSE
)
geocode_results <- geocode_results[!is.na(id)]

# STOP CLUSTER ----------------------------------------------
stopCluster(myCluster)

## now save to the cfsDRUG2013.csv file
cfsDRUG2013$lat <- geocode_results$lat
cfsDRUG2013$lng <- geocode_results$lng
save(cfsDRUG2013,file='cfsDRUG2013.RData',ascii=TRUE)
icourthouse <- ((cfsDRUG2013$lat==38.88988) & (cfsDRUG2013$lng==-77.08318))
icourthouse <- cfsDRUG2013$Location=='1425 N COURTHOUSE RD'  #196 out of 571 (35%)
#icourthouse <- cfsDOME2013$Location=='1425 N COURTHOUSE RD'  #50 out of 2000

 # now, get counts of DRUG calls by block group
 # load in block groups
# arlBGgis <- block_groups("Virginia", c("Arlington County"))

 # make the spatial points dataset
# read in packages
library(rgdal)
library(rgeos)
library(maptools)
library(proj4)

drugpoints <- data.frame(Longitude = cfsDRUG2013$lng, Latitude = cfsDRUG2013$lat)
# Assignment modified according
coordinates(drugpoints) <- ~ Longitude + Latitude
# Set the projection of the SpatialPointsDataFrame using the projection of the shapefile
proj4string(drugpoints) <- proj4string(arlBGgis)
  # now check which points are in the block group polygons
drugCountBG <- over(drugpoints, arlBGgis)
drugtab <- table(drugCountBG$GEOID)
# block group 1721 holds the courthouse
drugBGcount <- data.frame(GEOID=names(drugtab),count=as.numeric(drugtab))

 ## Now, determine the number of units by blockgroup
units <- read.csv("./data/mitre/working/cleanedExampleData/clAtrackPumsMultivariate.csv")
units %>% group_by(BlockGroup) %>% summarize(n=n(),nunit=sum(UNITS.NUMBER,na.rm=T)) %>% filter(!is.na(BlockGroup)) -> unitBGcount
unitBGcount$GEOID = paste('51013',unitBGcount$BlockGroup,sep='')

unitBGcount %>% left_join(drugBGcount,by='GEOID') -> tmp
drugBGrate = tmp; drugBGrate$count <- ifelse(is.na(drugBGrate$count),0,drugBGrate$count)
 # get the rate - this could be problematic since the number of units doesn't appear accurate.
drugBGrate$rate = drugBGrate$count/drugBGrate$n
 # note, for the BG holding the courthouse (BlockGroup=1017013), the rate is higher than the number of units 
 # this is no big deal since we'll be deleting this blockgroup from the LR analyses

## need to add in the number of units by block group from the ACS!!
## and then append to this file, and recompute the rates

 # save the file: 
write.csv(drugBGrate,'./data/mitre/working/PoliceData/drugBGrate.csv',row.names=FALSE,quote=FALSE)
 # try reading the file to make sure it wrote out ok
#tmp <- read.csv('drugBGrate.csv',stringsAsFactors = FALSE)
