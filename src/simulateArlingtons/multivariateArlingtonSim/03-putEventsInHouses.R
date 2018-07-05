# Police events data and DHS incident rates

# Load police incident data and house locations
policeData = fread("./data/mitre/working/PoliceData/policeData.csv")
clAtrack = fread("./data/mitre/working/cleanedExampleData/clAtrackPums.csv")[source != "PUMS"] %>% cbind(houseID = 1:nrow(clAtrack))
conDist = sdalr::con_db("mitre")

closestAddress = fread("./data/mitre/working/simulatedArlingtonData/closestHouseToCall.csv") %>%
  merge(policeData[,.(Call_No, caseLong = LONGITUDE, caseLat = LATITUDE)]) %>%
  merge(clAtrack[,.(closestHouse, houseLong = LONGITUDE, houseLat = LATITUDE)], by = c('houseID' = 'closestHouse')) %>%
  setorder(DistKm) %>%
  setnames("closestHouse", "houseID") %>%
  '['(DistKm < .5)

closestAddress

#
# Construct a probability distribution over houses based on the distance to those houses
#

# The maximum number of addresses to consider as possible candidates.
maxCandidates = 20
# The decay penalty lowers the probability of an event happening at distant houses. Must be less than zero
decayPenalty = -50

distancesToHomes = getHomesInRadius(130010020, policeData, clAtrack, conDist, .5) %>% setorder(distance)
homeSet = distancesToHomes[1:maxCandidates, .(houseID, distance)]
eventAtLocationProbs = softmax(homeSet$distance, decayPenalty)
plot(eventAtLocationProbs)
