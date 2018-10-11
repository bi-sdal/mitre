# some code to identify a blockgroup in Arlington
# read in packages
library(rgdal)
library(rgeos)
library(maptools)
library(proj4)

# Arlington zip codes -----------------------
arlzip <- readShapePoly("~/Dave/SDALProjects/MITRE/ArlingtonZip/Zip_Codes/Zip_Codes.shp")
# this call loads in the projection info as well.
arlzip <- readOGR("~/Dave/SDALProjects/MITRE/ArlingtonZip/Zip_Codes/")
# the blockgroup spatial object is in arlBGgis
# now we can see the proj4string
proj4string(arlBGgis)
# and use it to convert to lat and lon
#arlzipTrans <- spTransform(arlzip)
# make some test points
pts <- locator(3); 
 # turn points into a dataframe
pts <- data.frame(Longitude=pts$x,Latitude=pts$y)
 # add coordinates
coordinates(pts) <- ~ Longitude + Latitude
 # add projection info
proj4string(pts) <- proj4string(arlBGgis)
 # now its a spatial object, suitable for using over or other methods.
summary(pts)
 # determine which block groups the points are in.
over(pts,arlBGgis)

##------ this stuff below is not vetted ------
# make this a spatial points object
dat_2 <- SpatialPointsDataFrame(pts[,c("x", "y")], pts)
coordinates(pts) <- ~ Longitude + Latitude

# in state units.  Need to convert to lat lon
# this does it for a collection of points
pj <- project(pts, proj4string(arlBGgis))
#pj <- project(pts, proj4string(arlBGgis), inverse=TRUE)
# for a spatial data frame: specify the coordinates you want
latlong = "+init=epsg:4326"
# make the transformation
aztrans = spTransform(arlzip, CRS(latlong))
plot(aztrans); points(pj)

over(dat_2,arlBGgis)
