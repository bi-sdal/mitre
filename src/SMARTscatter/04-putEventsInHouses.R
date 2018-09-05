

policeData = fread("./data/mitre/working/PoliceData/policeData.csv")
outcomesLatLong = fread('./data/mitre/final/police/dome_geocoded.csv')
clAtrack = fread("./data/mitre/working/cleanedExampleData/clAtrackPumsMultivariate.csv")[source != "PUMS"]
conDist = sdalr::con_db("mitre")

# closestAddress = fread("./data/mitre/working/simulatedArlingtonData/closestHouseToCall.csv") %>%
#   merge(policeData[,.(Call_No, caseLong = LONGITUDE, caseLat = LATITUDE)]) %>%
#   merge(clAtrack[,.(houseID, houseLong = LONGITUDE, houseLat = LATITUDE)], by.x = 'closestHouse', by.y = 'houseID') %>%
#   setorder(DistKm) %>%
#   setnames("closestHouse", "houseID") %>%
#   '['(DistKm < .5)

#
# Construct a probability distribution over houses based on the distance to those houses
#
# Child abuse

childAbuse = outcomesLatLong[child_victim_count == 1, .(Report_No, lon, lat)]
setnames(childAbuse, "Report_No", "Call_No")

probInHouseList <- do.call(rbind, lapply(unique(childAbuse$Call_No), 
                          probCaseInHouseSoftmax,
                          policeData = childAbuse, 
                          resData = clAtrack,
                          connection = conDist,
                          radius = .5,
                          table = "ca_ipv_ea_events",
                          maxCandidates = maxCandidates,
                          decayPenalty = decayPenalty)
                          )

caHouseAssignments = assignCasesToHouse(probInHouseList, nDraws)
fwrite(caHouseAssignments, paste0(featurePath, "/childAbuseCaseAssignments.csv"))

# IPV

ipv = outcomesLatLong[adult_victim_count == 1, .(Report_No, lon, lat)]
setnames(ipv, "Report_No", "Call_No")

probInHouseList <- do.call(rbind, lapply(unique(ipv$Call_No), 
                                         probCaseInHouseSoftmax,
                                         policeData = ipv, 
                                         resData = clAtrack,
                                         connection = conDist,
                                         radius = .5,
                                         table = "ca_ipv_ea_events",
                                         maxCandidates = maxCandidates,
                                         decayPenalty = decayPenalty)
)

ipvHouseAssignments = assignCasesToHouse(probInHouseList, nDraws)
fwrite(ipvHouseAssignments, paste0(featurePath, "/ipvCaseAssignments.csv"))

# Elder Abuse

elderAbuse = outcomesLatLong[elder_victim_count == 1, .(Report_No, lon, lat)]
setnames(elderAbuse, "Report_No", "Call_No")

probInHouseList <- do.call(rbind, lapply(unique(elderAbuse$Call_No), 
                                         probCaseInHouseSoftmax,
                                         policeData = elderAbuse, 
                                         resData = clAtrack,
                                         connection = conDist,
                                         radius = .5,
                                         table = "ca_ipv_ea_events",
                                         maxCandidates = maxCandidates,
                                         decayPenalty = decayPenalty)
)

eaHouseAssignments = assignCasesToHouse(probInHouseList, nDraws)
fwrite(eaHouseAssignments, paste0(featurePath, "/elderAbuseCaseAssignments.csv"))


