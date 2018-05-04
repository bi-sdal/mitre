library(dplyr)
library(data.table)
library(sf)
require(ggplot2)

# Load data
#Police data
policeData = fread("./data/mitre/working/PoliceData/policeData.csv")

# Simulated Arlington Data
incomeSims = fread("./data/mitre/working/simulatedArlingtonData/arlSimWithCaseCounts.csv")

# Load table linking case nos to houses, with distances. NOTE: the number in closestHouse corresponds to the appropriate row in simulated income data.
closestAddressToCase = fread("./data/mitre/working/simulatedArlingtonData/closestHouseToCall.csv")