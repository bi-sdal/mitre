library(ggplot2)
library(data.table)
library(sf)

source("./src/SMARTscatter/01-prepareAndLoadData.R")

imputationColumns = c("sqrtHINCP", "RMSP", "householdSize", 'singleParent', 'snKid', 'milWoman', 'unmarriedPartner', 'multiGenHouse')
featurePath = sprintf("./data/mitre/working/imputationAndResamplingResults/%s", paste0(imputationColumns, collapse = '_'))

load(paste0(featurePath, "/logisticRegressions.Rdata"))

logisticRegressions

coefs = sapply(logisticRegressions, coefficients)
fits = lapply(logisticRegressions, fitted)

par(mfrow = c(2, 2))
for(i in 2:5){
  hist(coefs[i,], main = rownames(coefs)[i])
}

sapply(fits, range)





# Make probability intensity plot

## Read Geospatial File from DB and Plot
# create db connection
conGeo <- sdalr::con_db("sdal")
# read shape from db
arlBlockGroups <- sf::st_read_db(conGeo, c("geospatial$census_cb", "cb_2016_51_bg_500k"))
# limit to Arlington
arlBlockGroups <- arlBlockGroups[arlBlockGroups$COUNTYFP=="013",]
# add in the police call locations

# convert to sf object
clLonLat = data.table(clAtrackPums)[,.(LONGITUDE, LATITUDE)]
clLonLat = na.omit(clLonLat)
clSf = st_as_sf(clLonLat, 
                    coords = c('LONGITUDE', 'LATITUDE'), 
                    crs = sf::st_crs(arlBlockGroups))
f
repRun = 2

ggplot(arlBlockGroups) + geom_sf(aes(geometry = wkb_geometry)) + 
  geom_point(data = clLonLat, aes(x = LONGITUDE, y = LATITUDE, colour = fits[[repRun]])) + 
  scale_color_gradient2(low = 'blue', high = 'red', midpoint = median(fits[[repRun]]))


# plot
# png("./output/arlRentalOwnedDomestic.png")
plot(sf::st_geometry(arlBlockGroups), axes = FALSE)
plot(sf::st_geometry(clSf), axes = TRUE, col = fits[[1]], add = TRUE, cex = .1)




plot(sf::st_geometry(policeSf), axes = TRUE, col = "blue", add = TRUE, pch = 19, cex = .3)
legend('topright', fill = c("red", "green", "blue"), legend = c("Owned Homes", "Rental Homes", "Domestic Disputes"), bty = 'n')
# dev.off()