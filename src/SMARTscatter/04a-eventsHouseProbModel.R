source("./src/SMARTscatter/parameterFiles/allDataFullRuns.R")

caByZip = fread("./data/mitre/original/caByZip.csv")
colnames(caByZip) = c("ZIP", "caCases")

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