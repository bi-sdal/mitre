source("./R/00-simulateArlFunctions.R")
# Police events data and DHS incident rates

# Load police incident data and house locations
nDraws = 1000
policeData = fread("./data/mitre/working/PoliceData/policeData.csv")
clAtrack = fread("./data/mitre/working/cleanedExampleData/clAtrackPumsMultivariate.csv")[source != "PUMS"]
conDist = sdalr::con_db("mitre")

closestAddress = fread("./data/mitre/working/simulatedArlingtonData/closestHouseToCall.csv") %>%
  merge(policeData[,.(Call_No, caseLong = LONGITUDE, caseLat = LATITUDE)]) %>%
  merge(clAtrack[,.(houseID, houseLong = LONGITUDE, houseLat = LATITUDE)], by.x = 'closestHouse', by.y = 'houseID') %>%
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



probCaseInHouseSoftmax = function(callNumber, policeData, resData, connection, radius, maxCandidates, decayPenalty){
  tmp <<- callNumber
  distancesToHomes = getHomesInRadius(callNumber, policeData, resData, connection, radius)
  if(is.null(distancesToHomes)) return(NULL)
  maxCandidates = min(maxCandidates, nrow(distancesToHomes))
  setorder(distancesToHomes, distance)
  homeSet = distancesToHomes[1:maxCandidates, .(Call_No = callNumber, houseID, distance)]
  homeSet$probInHouse = softmax(homeSet$distance, decayPenalty)
  return(homeSet)
}

tmp = getHomesInRadius(132230134, policeData, clAtrack, conDist, .5)

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

assignCasesToHouse = function(probInHouseList, nDraws){
  caseNos = unique(probInHouseList$Call_No)
  nCases = length(caseNos)
  out = matrix(NA, nCases, nDraws)
  indices = probInHouseList[,sample(1:.N, nDraws, replace = TRUE,  prob = probInHouse),by = Call_No]
  for(i in 1:nCases){
    activeIndices = indices[Call_No == caseNos[i]]$V1
    out[i,] = probInHouseList[Call_No == caseNos[i]][activeIndices,houseID]
  }
  colnames(out) = paste0("resample", 1:nDraws)
 return(data.table(Call_No = caseNos, out)) 
}


houseAssignments = assignCasesToHouse(probInHouseList, nDraws)

fwrite(houseAssignments, "./data/mitre/working/imputationAndResamplingResults/sqrtHINCP_RMSP/caseAssignments.csv")

