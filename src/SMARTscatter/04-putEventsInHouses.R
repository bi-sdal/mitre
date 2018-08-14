# Load police incident data and house locations

policeData = fread("./data/mitre/working/PoliceData/policeData.csv")
clAtrack = fread("./data/mitre/working/cleanedExampleData/clAtrackPumsMultivariate.csv")[source != "PUMS"]
conDist = sdalr::con_db("mitre")

closestAddress = fread("./data/mitre/working/simulatedArlingtonData/closestHouseToCall.csv") %>%
  merge(policeData[,.(Call_No, caseLong = LONGITUDE, caseLat = LATITUDE)]) %>%
  merge(clAtrack[,.(houseID, houseLong = LONGITUDE, houseLat = LATITUDE)], by.x = 'closestHouse', by.y = 'houseID') %>%
  setorder(DistKm) %>%
  setnames("closestHouse", "houseID") %>%
  '['(DistKm < .5)
#
# Construct a probability distribution over houses based on the distance to those houses
#

system.time(probInHouseList <- lapply(unique(policeData$Call_No), 
                                      probCaseInHouseSoftmax,
                                      policeData = policeData, 
                                      resData = clAtrack,
                                      connection = conDist,
                                      radius = .5,
                                      maxCandidates = maxCandidates,
                                      decayPenalty = decayPenalty))

probInHouseList = do.call(rbind, probInHouseList)

#
# For each of nDraws times, assign a case to a house
#

houseAssignments = assignCasesToHouse(probInHouseList, nDraws)

