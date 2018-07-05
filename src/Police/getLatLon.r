# getting lat lon for a bunch of addresses 
# the object locDOME2013 created in look1.r

## separate out DOME CFS calls for 2013 (our trial year)

c1 %>% filter(Original_Call=='DOME',year(Received_Date_Time)==2013) -> cfsDOME2013
locDOME2013 <- cfsDOME2013$Location[1:3]
locDOME2013 <- paste(locDOME2013,'Arlington, VA')
nloc <- length(locDOME2013)

# use Aaron's code to run things

# after running geocode.r, results are in locDOME2013 and 
# geocode_results - both with 2043 observations

cfsDOME2013$lat <- geocode_results$lat
cfsDOME2013$lng <- geocode_results$lng
save(cfsDOME2013,file='cfsDOME2013.RData',ascii=TRUE)

# plot beats and incidents in Arlington
require("rgdal") # requires sp, will use proj.4 if installed
require("maptools")
require("ggplot2")
require("dplyr")
require("rgeos")
library(sp)
library(sf)

# instal sdalr tools
devtools::install_github('bi-sdal/sdalr')
## Read Geospatial File from DB and Plot
con <- sdalr::con_db("sdal") # create db connection
gs <- sf::st_read_db(con, c("geospatial$census_cb", "cb_2016_51_bg_500k")) # read geo from db (specify schema & table)
gs <- gs[gs$COUNTYFP=="013",] # limit to Arlington
plot(gs[, "ALAND"],col='white') #plot

 # add in the police call locations
plat <- c(38.91058, 38.89512, 38.85228, 38.89512, 38.84480, 38.84480, 38.84705,
          38.84788, 38.86379, 38.87954, 38.85526, 38.88854)
plng <- c(-77.13967, -77.07277, -77.05204, -77.07277, -77.09586, -77.09586,
          -77.07739, -77.08898, -77.07867, -77.10520, -77.11541, -77.14661)
points(plat,plng,pch=16,cex=.8,col='red')
plot(plat,plng,pch=16,col='red')

DT <- data.table(latitude = cfsDOME2013$lat,
                 longitude = cfsDOME2013$lng)
DT_sf = st_as_sf(DT, 
                 coords = c("longitude", "latitude"), 
                 #crs = sf::st_crs(gs), 
                 crs = 4269,
                 agr = "constant",na.fail=FALSE)

plot(st_geometry(DT_sf), pch = 16, cex=.8, col = 'red', add = TRUE)
points(DT_sf,pch=1,col='red')

# Josh's conversion code
proj4string <- CRS('+proj=longlat +ellps=WGS84')
ArlBG_boundary_latlong <- spTransform(gs, proj4string)
dome_latlong <- spTransform(DT, proj4string)

HS_boundary <- readOGR(dsn="~/sdal/projects/limbo/fairfax_alerts/GISData/Roanoke_City_HighSchools/", layer="schoolzones_HS")
HS_boundary_latlong <- spTransform(HS_boundary, proj4string)



## Read Geospatial File from DB and Plot
# create db connection
con <- sdalr::con_db("sdal")
# read shape from db
gs <- sf::st_read_db(con, c("geospatial$census_cb", "cb_2016_51_county_within_ua_500k"))
# limit to Arlington
gs <- gs[gs$COUNTYFP10=="013",]
# add in the police call locations
DT <- data.table(latitude = c(38.91058, 38.89512, 38.85228, 38.89512, 38.84480, 38.84480, 38.84705,
                              38.84788, 38.86379, 38.87954, 38.85526, 38.88854),
                 longitude = c(-77.13967, -77.07277, -77.05204, -77.07277, -77.09586, -77.09586,
                               -77.07739, -77.08898, -77.07867, -77.10520, -77.11541, -77.14661))
# convert to sf object
DT_sf = st_as_sf(DT, 
                 coords = c("longitude", "latitude"), 
                 crs = sf::st_crs(gs), 
                 agr = "constant")
# plot
plot(sf::st_geometry(gs), axes = TRUE, graticule = TRUE)
pdf('dome2013.pdf',width=4,height=4)
par(mfrow=c(1,1),oma=c(0,0,0,0),mar=c(0,0,0,0))
plot(sf::st_geometry(gs))
plot(sf::st_geometry(DT_sf), axes = TRUE, col = "blue", add = TRUE,pch=16,cex=.3)
dev.off()
