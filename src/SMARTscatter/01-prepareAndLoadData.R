# CLdata is corelogic housing data
marginalIncome = read.csv("./data/mitre/working/simulatedArlingtonData/marginalIncome.csv")
marginalRooms = read.csv("./data/mitre/working/simulatedArlingtonData/marginalRooms.csv")
clAtrackPums = read.csv("./data/mitre/working/cleanedExampleData/clAtrackPumsMultivariate.csv")

# Since we're using residencies with multiple units, we must divide the relevant values by the number of units
clAtrackPums$HINCP[!is.na(clAtrackPums$UNITS.NUMBER)] = clAtrackPums$HINCP[!is.na(clAtrackPums$UNITS.NUMBER)]/clAtrackPums$UNITS.NUMBER[!is.na(clAtrackPums$UNITS.NUMBER)]
clAtrackPums$VALP[!is.na(clAtrackPums$UNITS.NUMBER)] = clAtrackPums$VALP[!is.na(clAtrackPums$UNITS.NUMBER)]/clAtrackPums$UNITS.NUMBER[!is.na(clAtrackPums$UNITS.NUMBER)]
clAtrackPums$TAXP2[!is.na(clAtrackPums$UNITS.NUMBER)] = clAtrackPums$TAXP2[!is.na(clAtrackPums$UNITS.NUMBER)]/clAtrackPums$UNITS.NUMBER[!is.na(clAtrackPums$UNITS.NUMBER)]
clAtrackPums$sqrtHINCP = sqrt(clAtrackPums$HINCP)